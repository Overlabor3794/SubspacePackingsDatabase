(* ::Package:: *)

(* Created by Gene Kopp, 2021 *)
(* Updated Aug 2024; updated Mar 2026 *)
(* Functions to find complex frames minimizing the p-frame potential *)


(* variable vector list *)
PhiVar[d_, n_] := Table[a[i, j] + b[i, j] I, {i, 1, d}, {j, 1, n}];

(* list of initial values for variables *)
varcons[Phi0_] := Module[{d, n},
  {d, n} = Dimensions[Phi0];
  Join[
   Transpose@{Flatten@Table[a[i, j], {i, 1, d}, {j, 1, n}], 
     Re[Flatten[Phi0]]},
   Transpose@{Flatten@Table[b[i, j], {i, 1, d}, {j, 1, n}], 
     Im[Flatten[Phi0]]}
   ]
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
Options[MinPhiQNp] = Join[Options[MinPhiQNp], Options[FindMinimum]];
MinPhiQNp[Phi0_, p_, opts : OptionsPattern[]] := Module[{d, n, min},
  {d, n} = Dimensions[Phi0];
  min = FindMinimum[pFramePotential[PhiVar[d, n], p], varcons[Phi0], opts];
  min[[2]] = normalizeSO[PhiVar[d, n] /. min[[2]]];
  min
  ]
ResourceFunction["AddCodeCompletion"]["MinPhiQNp"][
  None, None, RepeatOptions[MinPhiQNp]];

(* minimize coherence with PrincipalAxis [OFTEN FAILS] *)
(* vector list, options *)
Options[MinPhiPA] = {Method -> "PrincipalAxis"};
Options[MinPhiPA] = Join[Options[MinPhiPA], Options[FindMinimum]];
MinPhiPA[Phi0_, opts : OptionsPattern[]] := Module[{d, n, min},
  {d, n} = Dimensions[Phi0];
  min = FindMinimum[Coherence[PhiVar[d, n]], varcons[Phi0], opts];
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
