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
(* The optional argument "generators" can be either a (case insensitive) string or
   a list of strings specifying the ideal generators to be returned by the function.
	 The possible strings are "Tight", "EquiangularWelch", and "UnitNorm" *)
idealGenerators[d_, n_, gen_ : {"Tight", "EquiangularWelch", "UnitNorm"}] :=
 Module[{ReVar, ImVar, ReVarT, ImVarT, ReG, ImG, absGsq, ReTight,
  ImTight, tight, welch, unit, genRules},
  ReVar = RePhiVar[d, n];
  ImVar = ImPhiVar[d, n];
  ReVarT = Transpose[ReVar];
  ImVarT = Transpose[ImVar];
  ReG = ReVarT . ReVar + ImVarT . ImVar;
  ImG = ReVarT . ImVar - ImVarT . ReVar;
  absGsq = ReG^2 + ImG^2;
  ReTight = ReVar . ReVarT + ImVar . ImVarT - (n/d) IdentityMatrix[d];
  ImTight = ImVar . ReVarT - ReVar . ImVarT;
  tight = ReTight^2 + ImTight^2;
  welch = (Flatten[absGsq[[#, # + 1 ;;]] & /@ Range[n]] - Welch[d, n]^2)^2;
  unit = (Diagonal[ReG] - 1)^2 + Diagonal[ImG]^2;
  genRules = {"tight" -> tight, "equiangularwelch" -> welch, "unitnorm" -> unit};
  ToLowerCase[gen] /. genRules
  ]
ResourceFunction["AddCodeCompletion"]["idealGenerators"][
  None, None, {"Tight", "EquiangularWelch", "UnitNorm"}];

(* Refines the precision of an ETF using the equations it satisfies. *)
(* Option "IdealGenerators" can be used to specify the generators used by the
   function. Default value is {"Tight", "EquiangularWelch", "UnitNorm"} *)
Options[refineETF] = ReplaceOptions[Options[FindMinimum], {MaxIterations -> 1000}];
Options[refineETF] = Prepend[Options[refineETF],
    "IdealGenerators" -> {"Tight", "EquiangularWelch", "UnitNorm"}];
refineETF[Phi0_, opts : OptionsPattern[]] :=
 Module[{d, n, fopts, gen, f, min},
  {d, n} = Dimensions[Phi0];
  gen = OptionValue["IdealGenerators"];
  If[gen === {}, gen = {"Tight", "EquiangularWelch", "UnitNorm"}];
  f = Total[idealGenerators[d, n, gen], Infinity];
  fopts = FilterRules[{opts}, Options[FindMinimum]];
  min = FindMinimum[f, varcons[Phi0], Evaluate[fopts], MaxIterations -> 1000];
  min[[2]] = PhiVar[d, n] /. min[[2]];
  min
  ]
ResourceFunction["AddCodeCompletion"]["refineETF"][
  None, RepeatOptions[refineETF]];
