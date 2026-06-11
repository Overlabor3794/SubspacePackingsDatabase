(* ::Package:: *)

(* Created by Gene Kopp, Aug 2025 *)
(* Functions to convert tuples of inexact numbers to exact numbers, assuming they
   are Galois conjugate over a small field *)
(* Includes exactification of triple products *)


(* Position of the minimum element of a list of distinct real numbers *)
positionSmallest[l_] := FirstPosition[l, Min[l]][[1]]

(* Exactification using elementary symmetric polynomials and RootApproximant *)
(* If "RationalCoefficients" is set to True, instead use Rationalize on coefficeints *)
(* "SimplificationMethod" is the function used to simplfy the algebraic numbers
   to be output by exactifyTuple *)
(* If "NumericalRefinement" is set to true, instead of simplifying, approximate
   to higher precision and then apply RootApproximant again *)
(* Increase the precision by a factor of "RefinementFactor" when using
   "NumericalRefinement" *)
Options[exactifyTuple] = {RationalCoefficients -> False, 
   SimplificationMethod -> RootReduce, NumericalRefinement -> False, 
   RefinementFactor -> 10};
exactifyTuple[\[Alpha]_, OptionsPattern[]] := 
 Module[{deg, ESP, X, exactESP, f, roots, RC, SM, NR, RF},
  RC = OptionValue[RationalCoefficients];
  SM = OptionValue[SimplificationMethod];
  NR = OptionValue[NumericalRefinement];
  RF = OptionValue[RefinementFactor];
  deg = Length[\[Alpha]];
  ESP = CoefficientList[Expand[Times @@ (X - \[Alpha])], X];
  exactESP = If[RC,
    Rationalize[#, 10^(-0.75 Precision[\[Alpha][[1]]])] & /@ ESP,
    RootApproximant /@ ESP];
  f = Plus @@ (exactESP . #^Range[0, deg]) &;
  roots = If[NR,
    Table[RootApproximant[N[Root[f, j], RF*N@Precision[Plus @@ \[Alpha]]]], {j, 1, deg}],
    Table[SM[Root[f, j]], {j, 1, deg}]];
  Table[roots[[positionSmallest[Abs /@ (\[Alpha][[j]] - roots)]]], {j, 1, deg}]
  ]

(* Converts an array to a list of array positions -> distinct elements *)
Options[arrayPositionMap] = {WorkingPrecision -> Automatic};
arrayPositionMap[array_, OptionsPattern[]] := Module[{prec, aprec},
  prec = OptionValue[WorkingPrecision];
  aprec = Precision[array];
  If[prec === Automatic,
   prec = If[aprec >= MachinePrecision + 5, MachinePrecision, aprec - 5]
   ];
  If[aprec === MachinePrecision,
   Reap[MapIndexed[Sow[{#1, #2}, nChop[SetPrecision[#1, $MachinePrecision], prec]] &,
      array, {ArrayDepth[array]}], _, #2[[All, 2]] -> #2[[1, 1]] &][[2]]
   ,
   Reap[MapIndexed[Sow[{#1, #2}, nChop[#1, prec]] &, array, {ArrayDepth[array]}],
         _, #2[[All, 2]] -> #2[[1, 1]] &][[2]]
   ]
  ]

nChop[x_, prec_] := N[Chop[N[x, prec + 4], 10^(-prec)], prec]
SetAttributes[nChop, Listable];

(* Converts an array to a lookup table *)
Options[arraytoLUT] = {WorkingPrecision -> Automatic, PackArray -> True};
arraytoLUT[array_, OptionsPattern[]] := 
 Module[{wprec, narray, distinct, LUT, dims, positions, base},
  wprec = OptionValue[WorkingPrecision];
  If[wprec === Automatic, wprec = MachinePrecision];
  If[OptionValue[PackArray],
   narray = Developer`ToPackedArray[N[array, wprec + 5], Complex],
   narray = N[N[array, wprec + 5], wprec]
   ];
  dims = Dimensions[narray];
  narray = Flatten[narray];
  distinct = PositionIndex[narray];
  LUT = AssociationThread[Keys[distinct] -> Range[0, Length[distinct] - 1]];
  LUT = ArrayReshape[Lookup[LUT, narray], dims];
  positions = First /@ Values[distinct];
  base = Reverse@FoldList[Times, 1, Reverse@dims[[2 ;;]]];
  positions = Mod[Quotient[# - 1, base], dims] & /@ positions + 1;
  distinct = Extract[array, positions];
  {distinct, LUT}
  ]

(* Converts a lookup table to an array *)
arrayfromLUT[LUT_] := 
 LUT[[2]] /. AssociationThread[Range[0, Length[LUT[[1]]] - 1] -> LUT[[1]]]

(* Covert an array position map to a standard array *)
arrayFromPositionMap[PM_] := Normal@SparseArray@Flatten[Thread /@ PM, 1];

(* Exactify a position map of triple products, taking advantage of possible
   Galois conjugates *)
Options[exactifyTPPM] = {RationalCoefficients -> False, 
   SimplificationMethod -> RootReduce, NumericalRefinement -> False, 
   RefinementFactor -> 10};
exactifyTPPM[TPPM_, opts : OptionsPattern[]] := Module[{elts, positions, exactelts},
  elts = TPPM[[All, 2]];
  positions = TPPM[[All, 1]];
  exactelts = exactifyTuple[elts, opts];
  Thread[positions -> exactelts]
  ]

(* Exactify a triple product tensor, taking advantage of possible Galois conjugates *)
Options[exactifyTP] = {WorkingPrecision -> MachinePrecision,
   RationalCoefficients -> False, SimplificationMethod -> RootReduce,
   NumericalRefinement -> False, RefinementFactor -> 10};
exactifyTP[T_, opts : OptionsPattern[]] := arrayFromPositionMap[
  exactifyTPPM[
   arrayPositionMap[T, FilterRules[{opts}, Options[arrayPositionMap]]],
   FilterRules[{opts}, Options[exactifyTPPM]]
   ]
  ]

(* Exactify a position map of triple products naively using RootApproximant *)
exactifyTPPMalt[TPPM_] := Module[{elts, positions, exactelts},
  elts = TPPM[[All, 2]];
  positions = TPPM[[All, 1]];
  exactelts = RootApproximant /@ elts;
  Thread[positions -> exactelts]
  ]

(* Exactify a triple product tensor naively using RootApproximant *)
Options[exactifyTPalt] = {WorkingPrecision -> MachinePrecision};
exactifyTPalt[T_, opts : OptionsPattern[]] := 
 arrayFromPositionMap@exactifyTPPMalt@arrayPositionMap[T, opts]
