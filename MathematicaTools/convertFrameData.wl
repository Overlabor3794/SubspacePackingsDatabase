(* ::Package:: *)

(* Created by Gene Kopp, Aug 2025; updated Mar 2026 *)
(* Functions to convert between vector list, Gram matrix, and triple products *)


Get[FileNameJoin[NotebookDirectory[], "init.wl"]];


(* TYPES *)
(* SO = sythesis operator = "frame" = "short, fat matrix" = matrix whose columns are frame vectors *)
(* all columns of SO must be unit vectors *)
(* AO = analysis operator = conjugate transpose of syntesis operator *)
(* all rows of AO must be unit vectors *)
(* GM = Gram matrix *)
(* TP = triple product tensor *)
(* CONVERSION FUNCTIONS *)
(* Normalized (unit) analysis operator from unnormalized analysis operator *)
normalizeAO[V_]:=Normalize/@V
(* Normalized (unit) synthesis operator from unnormalized synthesis operator *)
normalizeSO[Phi_]:=normalizeAO[Phi\[ConjugateTranspose]]\[ConjugateTranspose]
(* Gram matrix from vector list *)
GMfromSO[Phi_]:=Phi\[ConjugateTranspose] . Phi
(* Triple product tensor from Gram matrix *)
TPfromGM[G_]:=Module[{n=Length[G]},Table[G[[i,j]]G[[j,k]]G[[k,i]],{i,1,n},{j,1,n},{k,1,n}]]
(* Triple product tensor from vector list *)
TPfromSO[Phi_]:=TPfromGM[GMfromSO[Phi]]
(* Vector list from Gram matrix *)
Options[SOfromGM]={Tolerance->10^(-6)};
SOfromGM[G_,OptionsPattern[]]:=Module[{d,U,\[CapitalLambda],V},
d=MatrixRank[G,Tolerance->OptionValue[Tolerance]];
{U,\[CapitalLambda],V}=SingularValueDecomposition[G,UpTo[d]];
(U . Sqrt[\[CapitalLambda]])\[ConjugateTranspose]
]
(* Gram matrix from triple product tensor *)
(* currently implemented when SO has a vector not orthogonal to any other vector *)
Options[GMfromTP]={Tolerance->10^(-6)};
GMfromTP[T_,OptionsPattern[]]:=Module[{n,k},
n=Dimensions[T][[1]];
For[k=1,k<=n,k++,
If[!AnyTrue[Flatten@T[[All,k,k]],Abs[#]<OptionValue[Tolerance]&],Break[]];
];
If[k>n,k=1];
Table[T[[i,j,k]]/(T[[i,k,k]]T[[j,k,k]])^(1/2),{i,1,Length[T]},{j,1,Length[T]}]
]
(* Vector list from triple product tensor *)
(* currently implemented when SO has a vector not orthogonal to any other vector *)
Options[SOfromTP]={Tolerance->10^(-6)};
SOfromTP[T_,OptionsPattern[]]:=SOfromGM[GMfromTP[T],Tolerance->OptionValue[Tolerance]]
(* Faithful matrix plot for synthesis operator or Gram matrix *)
FrameVisualize[M_]:=MatrixPlot[Hue[(Arg[#]+Pi)/(2Pi),Abs[#]]&/@#&/@M,Frame->False]
(* Game of Sloanes representation of a synthesis operator *)
GoSfromSO[Phi_]:=Join[Flatten[Re[Phi\[Transpose]]],Flatten[Im[Phi\[Transpose]]]]
(* Synthesis operator from a Games of Sloanes representation *)
SOfromGoS[gos_,{d_,n_}]:=Table[gos[[d (i-1)+j]]+gos[[d n +d(i-1)+j]]I,{j,1,d},{i,1,n}]
(* Convert between triple product tensor and one slice of the triple product tensor *)
TPslicefromTP[T_,i_:1]:=T[[i]]
TPfromTPslice[Tslice_,i_:1]:=Table[Tslice[[i,i]]Tslice[[j,k]]Tslice[[k,l]]Tslice[[l,j]]/(Tslice[[i,j]]Tslice[[i,k]]Tslice[[i,l]]),
{j,1,Length[Tslice]},{k,1,Length[Tslice]},{l,1,Length[Tslice]}]
