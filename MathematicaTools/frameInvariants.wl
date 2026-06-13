(* ::Package:: *)

(* Created by Gene Kopp, Mar 2026 *)
(* Functions to compute invariants of frames *)


(* Takes in an array containing frame data and returns its type as a string *)
(* The values returned are
   - "SO"      : Sythesis operator (frame)
   - "GM"      : Gram matrix
   - "TP"      : Triple product tens
   - "TPS"     : Triple product slice
   - "TP LUT"  : Triple product tensor lookup table
   - "TPS LUT" : Triple product slice lookup table
   - $Failed   : If array is none of the above
  *)
(* If array is a Gram matrix, then it must be the Gram matrix of a normalized
   frame; otherwise the function will return "TPS" *)
arrayType[array_List] := Module[{dims, dim},
  If[Length[array] == 0, Return[$Failed]];
  dims = Dimensions[array];
  dim = Length[dims];
  Which[
   dim == 1 && dims[[1]] == 2,
   If[ArrayDepth[array[[2]]] == 2, Return["TPS LUT"]];
   If[ArrayDepth[array[[2]]] == 3, Return["TP LUT"]],
   dim == 2,
   If[! MatrixQ[array] && dims[[1]] == 2,
    If[ArrayDepth[array[[2]]] == 2, Return["TPS LUT"]];
    If[ArrayDepth[array[[2]]] == 3, Return["TP LUT"]]
    ];
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
Options[distinctTP] = {WorkingPrecision -> Automatic, PackArray -> Automatic};
distinctTP[array_, OptionsPattern[]] := 
 Module[{type, TP, nTP, aprec, wprec, pack, dims, positions, base},
  type = arrayType[array];
  If[type === "TP LUT", Return@array[[1]]];
  Which[
   type === "TPS LUT", TP = TPfromTPslice@arrayfromLUT[array],
   type === "SO",
   TP = TPfromSO[array],
   type === "GM", TP = TPfromGM[array],
   type === "TPS", TP = TPfromTPslice[array],
   type === "TP", TP = array,
   True, Return[$Failed]
   ];
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
  nTP = SetPrecision[TP, wprec + 5];
  If[pack === True,
   nTP = Developer`ToPackedArray[nTP, Complex],
   nTP = Chop[nTP, 10^(5 - Accuracy[array])];
   nTP = SetPrecision[nTP, wprec];
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
momentCore[array_, m_, OptionsPattern[]] := Module[{gprec, wprec, type, Tm, CS},
  gprec = OptionValue[PrecisionGoal];
  If[gprec === Automatic, gprec = Precision[array]];
  wprec = gprec + If[gprec === MachinePrecision, 0, 5];
  type = arrayType[array];
  Which[
   type === "TP LUT",
   Tm = array;
   Tm[[1]] = N[Tm[[1]], wprec]^m;
   Tm = arrayfromLUT[Tm],
   type === "TPS LUT",
   Tm = array;
   Tm[[1]] = N[Tm[[1]], wprec]^m;
   Tm = TPfromTPslice@arrayfromLUT[Tm],
   type === "SO", Tm = TPfromGM[GMfromSO[N[array, wprec]]^m],
   type === "GM", Tm = TPfromGM[N[array, wprec]^m],
   type === "TPS", Tm = TPfromTPslice[N[array, wprec]^m],
   type === "TP", Tm = N[array, wprec]^m,
   True, Return[$Failed]
   ];
  CS = OptionValue[Method];
  If[CS === Automatic,
   If[wprec === MachinePrecision,
    CS = "CompensatedSummation",
    CS = Automatic
    ]
   ];
  If[OptionValue[ND],
   Do[Tm[[i, i, All]] = Tm[[i, All, i]] = Tm[[All, i, i]] = 0, {i, Length[Tm]}]
   ];
  N[Total[Tm, 3, Method -> CS], gprec]
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
