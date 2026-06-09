(* ::Package:: *)

(* Created by Gene Kopp, Aug 2025; updated Mar 2026 *)
(* Functions to convert between vector list, Gram matrix, and triple products *)


(* TYPES *)
(* SO = sythesis operator = "frame" = "short, fat matrix"
   = matrix whose columns are frame vectors *)
(* all columns of SO must be unit vectors *)
(* AO = analysis operator = conjugate transpose of syntesis operator *)
(* all rows of AO must be unit vectors *)
(* GM = Gram matrix *)
(* TP = triple product tensor *)

(* CONVERSION FUNCTIONS *)
(* Normalized (unit) analysis operator from unnormalized analysis operator *)
normalizeAO[V_] := Normalize /@ V

(* Normalized (unit) synthesis operator from unnormalized synthesis operator *)
normalizeSO[Phi_] := normalizeAO[Phi\[ConjugateTranspose]]\[ConjugateTranspose]

(* Gram matrix from vector list *)
GMfromSO[Phi_] := Phi\[ConjugateTranspose] . Phi

(* Triple product tensor from Gram matrix *)
TPfromGM[G_] := Module[{n = Length[G]},
  Table[G[[i, j]] G[[j, k]] G[[k, i]], {i, 1, n}, {j, 1, n}, {k, 1, n}]
  ]

(* Triple product slice from Gram matrix *)
TPslicefromGM[G_, i_ : 1] := Module[{n = Length[G]},
  Table[G[[i, j]] G[[j, k]] G[[k, i]], {j, 1, n}, {k, 1, n}]
  ]

(* Triple product tensor from vector list *)
TPfromSO[Phi_] := TPfromGM[GMfromSO[Phi]]

(* Vector list from Gram matrix *)
Options[SOfromGM] = {Tolerance -> 10^(-6)};
SOfromGM[G_, OptionsPattern[]] := Module[{d, U, \[CapitalLambda], V},
  d = MatrixRank[G, Tolerance -> OptionValue[Tolerance]];
   {U, \[CapitalLambda], V} = SingularValueDecomposition[G, UpTo[d]];
   (U . Sqrt[\[CapitalLambda]])\[ConjugateTranspose]
   ]

(* Gram matrix from triple product tensor *)
(* currently implemented when SO has a vector not orthogonal to any other vector *)
Options[GMfromTP] = {Tolerance -> 10^(-6)};
GMfromTP[T_, OptionsPattern[]] := Module[{n, k},
  n = Dimensions[T][[1]];
  For[k = 1, k <= n, k++,
   If[! AnyTrue[Flatten@T[[All, k, k]], Abs[#] < OptionValue[Tolerance] &],
    Break[]];
   ];
  If[k > n, k = 1];
  Table[T[[i, j, k]]/(T[[i, k, k]] T[[j, k, k]])^(1/2),
      {i, 1, Length[T]}, {j, 1, Length[T]}]
  ]

(* Vector list from triple product tensor *)
(* currently implemented when SO has a vector not orthogonal to any other vector *)
Options[SOfromTP] = {Tolerance -> 10^(-6)};
SOfromTP[T_, OptionsPattern[]] := 
 SOfromGM[GMfromTP[T], Tolerance -> OptionValue[Tolerance]]

(* Faithful matrix plot for synthesis operator or Gram matrix *)
FrameVisualize[M_] := 
 MatrixPlot[Hue[(Arg[#] + Pi)/(2 Pi), Abs[#]] & /@ # & /@ M, Frame -> False]

(* Game of Sloanes representation of a synthesis operator *)
GoSfromSO[Phi_] := Join[Re[#], Im[#]] &@ Flatten[Phi\[Transpose]]

(* Synthesis operator from a Games of Sloanes representation *)
SOfromGoS[gos_, {d_, n_}] := 
 Transpose@ArrayReshape[gos[[;; d n]] + I gos[[d n + 1 ;;]], {n, d}]

(* Convert between triple product tensor and one slice of the triple
   product tensor *)
TPslicefromTP[TP_, i_ : 1] := TP[[i]]
TPfromTPslice[TPS_, i_ : 1] := Module[{T1, T2, T3, den},
  T1 = ConstantArray[TPS, Length[TPS]];
  T2 = Transpose[T1, {3, 1, 2}];
  T3 = Transpose[T1, {2, 3, 1}];
  den = 1/Outer[Times, #, #, #] &@TPS[[i]];
  TPS[[i, i]]*T1*T2*T3*den
  ]
