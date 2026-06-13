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

(* Converts an array to a lookup table *)
Options[arraytoLUT] = {WorkingPrecision -> Automatic, PackArray -> Automatic};
arraytoLUT[array_, OptionsPattern[]] := 
 Module[{aprec, wprec, pack ,narray, distinct, LUT, dims, positions, base},
  aprec = Precision[array];
  wprec = OptionValue[WorkingPrecision];
  pack = OptionValue[PackArray];
  Which[
   wprec === pack === Automatic,
   If[aprec >= MachinePrecision + 5,
    wprec = MachinePrecision;
    pack = True,
    wprec = Max[1, aprec - 5];
    pack = False;
    ],
   wprec === Automatic && pack =!= Automatic,
   wprec = If[aprec >= MachinePrecision + 5, MachinePrecision, Max[1, aprec - 5]],
   wprec =!= Automatic && pack === Automatic,
   pack = If[wprec === MachinePrecision, True, False]
   ];
  narray = SetPrecision[array, wprec + 5];
  If[pack === True,
   narray = Developer`ToPackedArray[narray, Complex],
   narray = Chop[narray, 10^(5 - Accuracy[array])];
   narray = SetPrecision[narray, wprec];
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

(* Exactify a position map of triple products, taking advantage of possible
   Galois conjugates *)
Options[exactifyLUT] = {RationalCoefficients -> False, 
   SimplificationMethod -> RootReduce, NumericalRefinement -> False, 
   RefinementFactor -> 10};
exactifyLUT[LUT_, opts : OptionsPattern[]] :=
 {exactifyTuple[LUT[[1]], opts], LUT[[2]]}

(* Exactify a triple product tensor, taking advantage of possible Galois conjugates *)
Options[exactifyTP] = {WorkingPrecision -> Automatic,
   RationalCoefficients -> False, SimplificationMethod -> RootReduce,
   NumericalRefinement -> False, RefinementFactor -> 10};
exactifyTP[TP_, opts : OptionsPattern[]] := Module[{fopts, tmp},
  fopts = FilterRules[{opts}, Options[arraytoLUT]];
  tmp = arraytoLUT[TP, fopts];
  fopts = FilterRules[{opts}, Options[exactifyLUT]];
  tmp = exactifyLUT[tmp, fopts];
  arrayfromLUT[tmp]
  ]

(* Exactify a position map of triple products naively using RootApproximant *)
exactifyLUTalt[LUT_] := {RootApproximant /@ LUT[[1]], LUT[[2]]}

(* Exactify a triple product tensor naively using RootApproximant *)
Options[exactifyTPalt] = {WorkingPrecision -> Automatic};
exactifyTPalt[TP_, opts : OptionsPattern[]] := 
 arrayfromLUT@exactifyLUTalt@arraytoLUT[TP, opts]
