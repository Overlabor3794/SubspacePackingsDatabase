(* ::Package:: *)

(* Created by Gene Kopp, Mar 2026 *)
(* Functions to compute invariants of frames *)


(* List of distict triple products, including degenerate ones *)
(* First argument can be an a frame, a Gram matrix, a triple product tensor,
   a triple product slice, a triple product position map, or a triple product
   slice position map *)
(* The available option is WorkingPrecision and is used to determine the number
   of signifcant digits used for comparing triple products *)
Options[distinctTP] = {WorkingPrecision -> Automatic};
distinctTP[array_, OptionsPattern[]] := Module[{dim, dims, TP, prec, aprec},
  dims = Dimensions[array];
  dim = Length[dims];
  Which[
   dim == 1,
   If[Length[array[[1, 1, 1]]] == 3, Return@array[[All, 2]]];
   If[Length[array[[1, 1, 1]]] == 2,
    TP = TPfromTPslice@arrayFromPositionMap[array]],
   dim == 2,
   If[dims[[1]] < dims[[2]], TP = TPfromSO[array]];
   If[dims[[1]] == dims[[2]],
    If[DeleteDuplicates[N@Diagonal[array], Abs[#1 - #2] < 10^(-10) &] == {1},
     TP = TPfromGM[array],
     TP = TPfromTPslice[array]]
    ],
   dim == 3 && dims[[1]] == dims[[2]] == dims[[3]], TP = array;
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
