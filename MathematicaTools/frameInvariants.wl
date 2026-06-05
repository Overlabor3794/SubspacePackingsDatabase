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
Options[distinctTP] = {WorkingPrecision -> Automatic};
distinctTP[array_, OptionsPattern[]] := Module[{type, TP, prec, aprec},
  type = arrayType[array];
  Which[
   type === "TPPM", Return@array[[All, 2]],
   type === "TPSPM", TP = TPfromTPslice@arrayFromPositionMap[array],
   type === "SO", TP = TPfromSO[array],
   type === "GM", TP = TPfromGM[array],
   type === "TPS", TP = TPfromTPslice[array],
   type === "TP", TP = array,
   True, Return[$Failed]
   ];
  prec = OptionValue[WorkingPrecision];
  aprec = Precision[array];
  If[prec === Automatic,
   prec = If[aprec >= MachinePrecision + 5, MachinePrecision, aprec - 5]
   ];
  If[aprec === MachinePrecision,
    TP = SetPrecision[Flatten[TP], $MachinePrecision + 5];
    N,
    TP = Flatten[TP];
    Identity
    ]@DeleteDuplicatesBy[TP, nChop[#, prec] &]
  ]

(* Number of distict triple products, including degenerate ones *)
numberTP[array_, opts : OptionsPattern[]] := Length@distinctTP[array, opts]

(* Moments [sum of powers of triple products] *)
momentfromSO[Phi_, m_] := Plus @@ (Flatten[TPfromSO[Phi]]^m);

(* Nondiagonal moments [sum of powers of totally nondiagonal triple products] *)
momentndfromSO[Phi_, m_] := Module[{T, n},
  T = TPfromSO[Phi];
  n = Length[T];
  Sum[If[(i - j) (j - k) (k - i) == 0, 0, T[[i, j, k]]^m],
   {i, 1, n}, {j, 1, n}, {k, 1, n}]
  ]

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
