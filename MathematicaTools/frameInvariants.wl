(* ::Package:: *)

(* Created by Gene Kopp, Mar 2026 *)
(* Functions to compute invariants of frames *)


(* List of distict triple products, including degenerate ones *)
Options[distinctTPfromSO] = {Tolerance -> 10^(-6)};
distinctTPfromSO[Phi_, OptionsPattern[]] := DeleteDuplicates[
   Chop@Flatten@TPfromSO[Phi], Abs[#1 - #2] < OptionValue[Tolerance] &];

(* Number of distict triple products, including degenerate ones *)
Options[numberTPfromSO] = {Tolerance -> 10^(-6)};
numberTPfromSO[Phi_, OptionsPattern[]] := 
  Length@distinctTPfromSO[Phi, Tolerance -> OptionValue[Tolerance]];

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
