(* ::Package:: *)

(* Created by Gene Kopp, 2021 *)
(* Updated Aug 2024; updated Mar 2026 *)
(* Functions to find complex frames minimizing the p-frame potential *)


(* variable vector list *)
RePhiVar[d_, n_] := Table[a[j, k], {j, 1, d}, {k, 1, n}];
ImPhiVar[d_, n_] := Table[b[j, k], {j, 1, d}, {k, 1, n}];
PhiVar[d_, n_] := Table[a[j, k] + b[j, k] I, {j, 1, d}, {k, 1, n}];

(* p-frame potential in terms of variables *)
VarFP[d_, n_, p_] := Module[{ReVar, ImVar, ReG, ImG, norm},
  ReVar = RePhiVar[d, n];
  ImVar = ImPhiVar[d, n];
  ReG = Transpose[ReVar] . ReVar + Transpose[ImVar] . ImVar;
  ImG = Transpose[ReVar] . ImVar - Transpose[ImVar] . ReVar;
  norm = Diagonal[ReG + ImG];
  {ReG, ImG} = UpperTriangularize[#, 1] & /@ {ReG, ImG};
  Total[((ReG^2 + ImG^2) / Outer[Times, norm, norm])^(p/2), 2]
  ]

(* list of initial values for variables *)
varcons[Phi0_] := Module[{d, n},
  {d, n} = Dimensions[Phi0];
  Join[Transpose@{Flatten@RePhiVar[d, n], Re[#]},
       Transpose@{Flatten@ImPhiVar[d, n], Im[#]}
      ] &@ Flatten[Phi0]
  ]

(* random seed vector list *)
Options[rand] = Options[RandomReal];
rand[opts : OptionsPattern[]] :=
 RandomReal[NormalDistribution[], {d, n}, opts] +
  RandomReal[NormalDistribution[], {d, n}, opts] I
rand[d_, n_, opts : OptionsPattern[]] :=
 RandomReal[NormalDistribution[], {d, n}, opts] +
  RandomReal[NormalDistribution[], {d, n}, opts] I
ResourceFunction["AddCodeCompletion"]["rand"][RepeatOptions[rand, 2]];

(* minimize p-frame potential with QuasiNewton *)
(* vector list, p, options *)
Options[MinPhiQNp] = {Method -> "QuasiNewton", MaxIterations -> 1000};
Options[MinPhiQNp] = ReplaceOptions[Options[FindMinimum], Options[MinPhiQNp]];
MinPhiQNp[Phi0_, p_, opts : OptionsPattern[]] := Module[{d, n, min},
  {d, n} = Dimensions[Phi0];
  min = FindMinimum[VarFP[d, n, p], varcons[Phi0], opts,
     Method -> "QuasiNewton", MaxIterations -> 1000];
  min[[2]] = normalizeSO[PhiVar[d, n] /. min[[2]]];
  min
  ]
ResourceFunction["AddCodeCompletion"]["MinPhiQNp"][
  None, None, RepeatOptions[MinPhiQNp]];

(* minimize coherence with PrincipalAxis [OFTEN FAILS] *)
(* vector list, options *)
Options[MinPhiPA] = {Method -> "PrincipalAxis"};
Options[MinPhiPA] = ReplaceOptions[Options[FindMinimum], Options[MinPhiPA]];
MinPhiPA[Phi0_, opts : OptionsPattern[]] := Module[{d, n, min},
  {d, n} = Dimensions[Phi0];
  min = FindMinimum[Coherence[PhiVar[d, n]], varcons[Phi0],
     opts, Method -> "PrincipalAxis"];
  min[[2]] = normalizeSO[PhiVar[d, n] /. min[[2]]];
  min
  ]
ResourceFunction["AddCodeCompletion"]["MinPhiPA"][None, RepeatOptions[MinPhiPA]];

(* ETF conditions *)
tightnessIdealGenerators[d_, n_] := Flatten[Table[
    PhiVar[d, n][[All, k]] - (d/n) Sum[(PhiVar[d, n][[All, j]]\[Conjugate] . 
        PhiVar[d, n][[All, k]]) PhiVar[d, n][[All, j]], {j, 1, n}], {k, 1, n}]];

equiangularWelchIdealGenerators[d_, n_] := Flatten[Table[
    (PhiVar[d, n][[All, i]]\[Conjugate] . PhiVar[d, n][[All, j]])
        (PhiVar[d, n][[All, j]]\[Conjugate] . PhiVar[d, n][[All, i]]) -
        Welch[d, n]^2, {i, 1, n}, {j, i + 1, n}]];

unitNormIdealGenerators[d_, n_] := 
  Table[PhiVar[d, n][[All, i]]\[Conjugate] . PhiVar[d, n][[All, i]] - 1, {i, 1, n}];

(* The functions below attempt to refine the precision of ETFs using the
   equations an ETF satisfies. They currently do not work very well. You should
   use MinPhiQNp with p=4 instead [or figure out how to improve this]. *)
Options[etfRefine] = {WorkingPrecision -> MachinePrecision, 
   "TightnessIdealGenerators" -> False};
etfRefine[Phi0_, OptionsPattern[]] := Module[{d, n, idealGenerators, TID},
  {d, n} = Dimensions[Phi0];
  TID = If[OptionValue["TightnessIdealGenerators"],
    tightnessIdealGenerators[d, n],
    {}];
  idealGenerators = Join[unitNormIdealGenerators[d, n],
    equiangularWelchIdealGenerators[d, n], TID];
  PhiVar[d, n] /. FindRoot[idealGenerators .
     RandomInteger[10, {Length[idealGenerators], 2 d n}], varcons[Phi0],
    WorkingPrecision -> OptionValue[WorkingPrecision]]
  ]
ResourceFunction["AddCodeCompletion"]["etfRefine"][
  None, RepeatOptions[etfRefine]];
