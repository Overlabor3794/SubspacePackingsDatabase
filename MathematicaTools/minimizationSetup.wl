(* ::Package:: *)

(* Created by Gene Kopp, 2021 *)
(* Updated Aug 2024; updated Mar 2026 *)
(* Functions to find complex frames minimizing the p-frame potential *)


(* Variable vector list *)
RePhiVar[d_, n_] := Table[a[j, k], {j, 1, d}, {k, 1, n}];
ImPhiVar[d_, n_] := Table[b[j, k], {j, 1, d}, {k, 1, n}];
PhiVar[d_, n_] := Table[a[j, k] + b[j, k] I, {j, 1, d}, {k, 1, n}];

(* List of initial values for variables *)
varcons[Phi0_] := Block[{a, b, d, n},
  {d, n} = Dimensions[Phi0];
  Join[Transpose@{Flatten@RePhiVar[d, n], Re[#]},
       Transpose@{Flatten@ImPhiVar[d, n], Im[#]}
      ] &@ Flatten[Phi0]
  ]

(* Random seed vector list *)
Options[rand] = Options[RandomReal];
rand[opts : OptionsPattern[]] :=
 RandomReal[NormalDistribution[], {d, n}, opts] +
  RandomReal[NormalDistribution[], {d, n}, opts] I
rand[d_Integer, n_Integer, opts : OptionsPattern[]] :=
 RandomReal[NormalDistribution[], {d, n}, opts] +
  RandomReal[NormalDistribution[], {d, n}, opts] I
ResourceFunction["AddCodeCompletion"]["rand"][RepeatOptions[rand, 2]];

(* p-frame potential in terms of variables *)
VarFP[d_, n_, p_] := Block[{a, b, ReVar, ImVar, ReG, ImG, norm},
  ReVar = RePhiVar[d, n];
  ImVar = ImPhiVar[d, n];
  ReG = Transpose[ReVar] . ReVar + Transpose[ImVar] . ImVar;
  ImG = Transpose[ReVar] . ImVar - Transpose[ImVar] . ReVar;
  norm = Diagonal[ReG + ImG];
  {ReG, ImG} = UpperTriangularize[#, 1] & /@ {ReG, ImG};
  2 Total[((ReG^2 + ImG^2) / Outer[Times, norm, norm])^(p/2), 2]
  ]

(* Polynomial avatar for p-frame potential in terms of variables *)
(* Should have same minima for some sufficiently large c *)
VarPolyFP[d_, n_, p_, c_] := Block[{a, b, ReVar, ImVar, ReG, ImG, norm},
  ReVar = RePhiVar[d, n];
  ImVar = ImPhiVar[d, n];
  ReG = Transpose[ReVar] . ReVar + Transpose[ImVar] . ImVar;
  ImG = Transpose[ReVar] . ImVar - Transpose[ImVar] . ReVar;
  norm = Diagonal[ReG + ImG];
  {ReG, ImG} = UpperTriangularize[#, 1] & /@ {ReG, ImG};
  Total[(ReG^2 + ImG^2)^(p/2), 2] + c Total[(norm - 1)^2]
  ]

(* ETF conditions *)
(* The optional argument "generators" can be either a (case insensitive) string or
   a list of strings specifying the ideal generators to be returned by the function.
	 The possible strings are "Tight", "EquiangularWelch", and "UnitNorm" *)
idealGenerators[d_, n_, gen_ : {"Tight", "EquiangularWelch", "UnitNorm"}] :=
 Block[{a, b, ReVar, ImVar, ReVarT, ImVarT, ReG, ImG, absGsq,
  ReTight, ImTight, tight, welch, unit, genRules},
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

(* Core code of MinPhiQNp, MinPhiPoly, and refineETF *)
Options[iMinPhi] = ReplaceOptions[Options[FindMinimum], {MaxIterations -> 1000}];
Options[iMinPhi] = Join[{"EarlyAbort" -> True, Normalize -> False,
    "IdealGenerators" -> {"Tight", "EquiangularWelch", "UnitNorm"}}, 
   Options[iMinPhi]];
iMinPhi[Phi0_, p_, c_, fn_Symbol, opts : OptionsPattern[]] := Block[{a, b,
  d, n, wprec, gprec, var, target, f, obj, gen, min, aborted, smonitor, iopts},
  {d, n} = Dimensions[Phi0];
  wprec = OptionValue[WorkingPrecision];
  gprec = OptionValue[PrecisionGoal];
  If[gprec === Automatic, gprec = wprec/2];
  gprec = SetPrecision[gprec, Infinity];
  var = PhiVar[d, n];
  Which[
   fn === MinPhiQNp,
   target = n(n - 1) Welch[d, n]^p;
   f = pFramePotential[#, p] &;
   obj = VarFP[d, n, p],
   fn === MinPhiPoly,
   target = Welch[d, n];
   f = Coherence;
   obj = VarPolyFP[d, n, p, c],
   fn === refineETF,
   target = Welch[d, n];
   f = Coherence;
   gen = OptionValue["IdealGenerators"];
   obj = Total[idealGenerators[d, n, gen], Infinity];
   ];
  min = {f[#], #} & @ Phi0;
  aborted = False;
  If[TrueQ @ OptionValue["EarlyAbort"],
   smonitor := (
    OptionValue[StepMonitor];
    min = {f[#], #} & @ var;
    If[Abs[min[[1]] - target] < target*10^(-gprec) &&
       Precision[min[[1]]] > MachinePrecision,
     Abort[];
     ];
    ),
   smonitor := (
    OptionValue[StepMonitor];
    min = {f[#], #} & @ var;
    );
   ];
  iopts = {MaxIterations -> OptionValue[MaxIterations]};
  iopts = Join[{StepMonitor :> smonitor}, {opts}, iopts];
  iopts = FilterRules[iopts, Options[FindMinimum]];
  CheckAbort[
   min = FindMinimum[obj, varcons[Phi0], Evaluate[iopts]],
   aborted = True;
   ];
  If[! aborted,
   min[[2]] = var /. min[[2]];
   If[fn === MinPhiPoly || fn === refineETF,
    min[[1]] = f[min[[2]]]
    ];
   ];
  If[OptionValue[Normalize], min[[2]] = normalizeSO[min[[2]]]];
  min
  ] /; NumericQ[p] && NumericQ[c]
ResourceFunction["AddCodeCompletion"]["iMinPhi"][
  None, None, None, None, RepeatOptions[iMinPhi]];

(* Minimize p-frame potential with QuasiNewton *)
(* vector list, p, options *)
Options[MinPhiQNp] = {Method -> "QuasiNewton", MaxIterations -> 1000};
Options[MinPhiQNp] = ReplaceOptions[Options[FindMinimum], Options[MinPhiQNp]];
Options[MinPhiQNp] =
  Join[{"EarlyAbort" -> True, Normalize -> False}, Options[MinPhiQNp]];
MinPhiQNp[Phi0_, p_?NumericQ, opts : OptionsPattern[]] :=
 iMinPhi[Phi0, p, 0, MinPhiQNp, opts, Method -> OptionValue[Method]]
ResourceFunction["AddCodeCompletion"]["MinPhiQNp"][
  None, None, RepeatOptions[MinPhiQNp]];

(* Minimize coherence with PrincipalAxis [OFTEN FAILS] *)
(* vector list, options *)
Options[MinPhiPA] = {Method -> "PrincipalAxis"};
Options[MinPhiPA] = ReplaceOptions[Options[FindMinimum], Options[MinPhiPA]];
Options[MinPhiPA] = Prepend[Options[MinPhiPA], Normalize -> False];
MinPhiPA[Phi0_, opts : OptionsPattern[]] := Block[{a, b, d, n, iopts, min},
  {d, n} = Dimensions[Phi0];
  iopts = Append[{opts}, Method -> OptionValue[Method]];
  iopts = FilterRules[iopts, Options[FindMinimum]];
  min = FindMinimum[Coherence[PhiVar[d, n]], varcons[Phi0], Evaluate[iopts]];
  min[[2]] = PhiVar[d, n] /. min[[2]];
  If[OptionValue[Normalize], min[[2]] = normalizeSO[min[[2]]]];
  min
  ]
ResourceFunction["AddCodeCompletion"]["MinPhiPA"][None, RepeatOptions[MinPhiPA]];

(* Minimize polynomial avatar for p-frame potential with Newton *)
(* vector list, p, c, options *)
Options[MinPhiPoly] = {Method -> "Newton", MaxIterations -> 1000};
Options[MinPhiPoly] = ReplaceOptions[Options[FindMinimum], Options[MinPhiPoly]];
Options[MinPhiPoly] =
  Join[{"EarlyAbort" -> True, Normalize -> False}, Options[MinPhiPoly]];
MinPhiPoly[Phi0_, p_ : 4, c_ : 1, opts : OptionsPattern[]] :=
 iMinPhi[Phi0, p, c, MinPhiPoly, opts, Method -> OptionValue[Method]] /;
  NumericQ[p] && NumericQ[c]
ResourceFunction["AddCodeCompletion"]["MinPhiPoly"][
  None, None, None, RepeatOptions[MinPhiPoly]];

(* Refines the precision of an ETF using the equations it satisfies. *)
(* Option "IdealGenerators" can be used to specify the generators used by the
   function. Default value is {"Tight", "EquiangularWelch", "UnitNorm"} *)
Options[refineETF] = ReplaceOptions[Options[iMinPhi], {Method -> "Newton"}];
refineETF[Phi0_, opts : OptionsPattern[]] := iMinPhi[Phi0, 0, 0, refineETF, opts]
ResourceFunction["AddCodeCompletion"]["refineETF"][None, RepeatOptions[refineETF]];
