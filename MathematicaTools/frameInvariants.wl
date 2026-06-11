(* ::Package:: *)

(* Created by Gene Kopp, Mar 2026 *)
(* Functions to compute invariants of frames *)


(* Takes in an array containing frame data and returns its type *)
(* The values returned are
   - "SO"    : Sythesis operator
   - "GM"    : Gram matrix
   - "TP"    : Triple product tensor
   - "TPS"   : Triple product slice
   - "TPPM"  : Triple product tensor position map
   - "TPSPM" : Triple product slice position map
   - $Failed : If array is none of the above
  *)
(* If array is a Gram matrix, then it must be the Gram matrix of a normalized
   frame; otherwise the function will return "TPS" *)
arrayType[array_List] := Module[{dims, dim},
  If[Length[array] == 0, Return[$Failed]];
  dims = Dimensions[array];
  dim = Length[dims];
  Which[
   dim == 1,
   If[! DeleteDuplicates[Head /@ array] === {Rule}, Return[$Failed]];
   If[Length[array[[1, 1, 1]]] == 3, Return["TPPM"]];
   If[Length[array[[1, 1, 1]]] == 2, Return["TPSPM"]],
   dim == 2,
   If[! MatrixQ[array], Return[$Failed]];
   If[dims[[1]] < dims[[2]], Return["SO"]];
   If[dims[[1]] == dims[[2]],
    If[DeleteDuplicates[N@Diagonal[array], Abs[#1 - #2] < 10^(-10) &] == {1},
     Return["GM"],
     Return["TPS"]]
    ],
   dim == 3 && dims[[1]] == dims[[2]] == dims[[3]], Return["TP"]
   ];
   Return[$Failed]
  ]

(* List of distict triple products, including degenerate ones *)
(* First argument can be an a frame, a Gram matrix, a triple product tensor,
   a triple product slice, a triple product position map, or a triple product
   slice position map *)
(* The available option is WorkingPrecision and is used to determine the number
   of signifcant digits used for comparing triple products *)
Options[distinctTP] = {WorkingPrecision -> Automatic, PackArray -> True};
distinctTP[array_, OptionsPattern[]] := 
 Module[{type, TP, nTP, wprec, dims, positions, base},
  type = arrayType[array];
  If[type === "TPPM", Return@array[[All, 2]]];
  Which[
   type === "TPSPM", TP = TPfromTPslice@arrayFromPositionMap[array],
   type === "SO", TP = TPfromSO[array],
   type === "GM", TP = TPfromGM[array],
   type === "TPS", TP = TPfromTPslice[array],
   type === "TP", TP = array,
   True, Return[$Failed]
   ];
  wprec = OptionValue[WorkingPrecision];
  If[wprec === Automatic, wprec = MachinePrecision];
  If[OptionValue[PackArray],
   nTP = Developer`ToPackedArray[N[TP, wprec + 5], Complex],
   nTP = N[N[TP, wprec + 5], wprec]
   ];
  dims = Dimensions[nTP];
  nTP = Flatten[nTP];
  positions = First /@ Values@PositionIndex[nTP];
  base = Reverse@FoldList[Times, 1, Reverse@dims[[2 ;;]]];
  positions = Mod[Quotient[# - 1, base], dims] & /@ positions + 1;
  Extract[TP, positions]
  ]

(* Number of distict triple products, including degenerate ones *)
numberTP[array_, opts : OptionsPattern[]] := Length@distinctTP[array, opts]

Options[momentCore] = {PrecisionGoal -> Automatic, Method -> Automatic, ND -> False};
momentCore[array_, m_, OptionsPattern[]] := Module[{prec, wprec, type, Tm, CS},
  prec = OptionValue[WorkingPrecision];
  If[prec === Automatic, prec = Precision[array]];
  wprec = prec + If[prec === MachinePrecision, 0, 5];
  type = arrayType[array];
  Which[
   type === "TPPM",
   Tm = array;
   Tm[[All, 2]] = N[Tm[[All, 2]], wprec]^m;
   Tm = arrayFromPositionMap[Tm],
   type === "TPSPM",
   Tm = array;
   Tm[[All, 2]] = N[Tm[[All, 2]], wprec]^m;
   Tm = TPfromTPslice@arrayFromPositionMap[Tm],
   type === "SO", Tm = TPfromGM[GMfromSO[N[array, wprec]]^m],
   type === "GM", Tm = TPfromGM[N[array, wprec]^m],
   type === "TPS", Tm = TPfromTPslice[N[array, wprec]^m],
   type === "TP", Tm = N[array, wprec]^m,
   True, Return[$Failed]
   ];
  CS = OptionValue[Method];
  If[CS === Automatic,
   If[prec === MachinePrecision,
    CS = "CompensatedSummation",
    CS = Automatic
    ]
   ];
  If[OptionValue[ND],
   Do[Tm[[i, i, All]] = Tm[[i, All, i]] = Tm[[All, i, i]] = 0, {i, Length[Tm]}]
   ];
  N[Total[Tm, 3, Method -> CS], prec]
  ]

(* Moments [sum of powers of triple products] *)
(* Passing the option Method -> "CompensatedSummation" uses "CompensatedSummation"
   method with Total. "CompensatedSummation" is always used if array has precision
   equal to MachinePrecision *)
moment[array_, m_, opts : OptionsPattern[]] := 
 momentCore[array, m, opts, ND -> False]

(* Nondiagonal moments [sum of powers of totally nondiagonal triple products] *)
(* Passing the option Method -> "CompensatedSummation" uses "CompensatedSummation"
   method with Total. "CompensatedSummation" is always used if array has precision
   equal to MachinePrecision *)
momentnd[array_, m_, opts : OptionsPattern[]] := 
 momentCore[array, m, opts, ND -> True]

(* Compute a general m-product with index set Indices_ *)
mproductfromGM[G_, Indices_] := Module[{m, wrapIndices},
  m = Length[Indices];
  wrapIndices = Append[Indices, Indices[[1]]];
  Product[G[[wrapIndices[[i]], wrapIndices[[i + 1]]]], {i, 1, m}]
  ]

mproductfromSO[Phi_, Indices_] := mproductfromSO[GMfromSO[Phi], Indices];

(* Compute the sum of all m-products with index set of shape Indices_ *)
(* For example, the index set {a, b, c, a, b, c} yields the second moment of the
   triple products *)
(* I think these generate all S_n-invariants [i.e., projective permutation
   unitary invariants] *)
generalSnInvariantfromGM[G_, Indices_] := Module[{IndexSet, n, k},
  IndexSet = DeleteDuplicates[Indices];
  n = Length[G];
  k = Length[IndexSet];
  Plus @@ (mproductfromGM[G, #] &
      /@ (Indices /. (Thread[IndexSet -> #] & /@ Tuples[Range[n], k])))
  ]

generalSnInvariantfromSO[Phi_, Indices_] := 
  generalSnInvariantfromGM[GMfromSO[Phi], Indices];
