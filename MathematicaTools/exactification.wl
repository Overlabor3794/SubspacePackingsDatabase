(* ::Package:: *)

(* Created by Gene Kopp, Aug 2025 *)
(* Functions to convert tuples of inexact numbers to exact numbers, assuming they are Galois conjugate over a small field *)
(* Includes exactification of triple products *)


(* Standard shortcuts for displaying matrices *)
mf=MatrixForm;mp=MatrixPlot;
(* Position of the minimum element of a list of distinct real numbers *)
positionSmallest[l_]:=FirstPosition[l,Min[l]][[1]]
(* Exactification using elementary symmetric polynomials and RootApproximant *)
(* If optional variable ratcoeffs is set to True, instead use Rationalize on coefficeints *)
exactifyTuple[\[Alpha]_,ratcoeffs_:False]:=Module[{deg,ESP,exactESP,f,roots},
deg=Length[\[Alpha]];
ESP=CoefficientList[Expand[Times@@(X-\[Alpha])],X];
If[ratcoeffs,exactESP=Rationalize[#,10^(-0.75Precision[\[Alpha][[1]]])]&/@ESP,exactESP=RootApproximant/@ESP];
f=Plus@@(exactESP . #^Range[0,deg])&;
roots=Table[RootReduce[Root[f,j]],{j,1,deg}];
Table[roots[[positionSmallest[Abs/@(\[Alpha][[j]]-roots)]]],{j,1,deg}]
]
(* Converts an array to a list of distinct elements -> array positions *)
arrayPositionMap[l_,tolerance_:10^(-6)]:=Module[{elts,positions,level},
elts=DeleteDuplicates[Flatten[l],Abs[#1-#2]<tolerance&];
level=Length[Dimensions[l]];
positions=Table[Position[l,_?(Abs[#-elts[[j]]]<tolerance&),level],{j,1,Length[elts]}];
Thread[positions->elts]
]
(* Covert an array position map to a standard array *)
arrayFromPositionMap[PM_]:=Normal[SparseArray[Flatten[Thread/@PM,1]]];
(* Exactify a position map of triple products, taking advantage of possible Galois conjugates *)
exactifyTPPM[TPPM_]:=Module[{elts,positions,exactelts},
elts=#[[-1]]&/@TPPM;
positions=#[[1]]&/@TPPM;
exactelts=exactifyTuple[elts];
Thread[positions->exactelts]
]
(* Exactify a triple product tensor, taking advantage of possible Galois conjugates *)
exactifyTP[T_,tolerance_:10^(-6)]:=arrayFromPositionMap[exactifyTPPM[arrayPositionMap[T,tolerance]]]
(* Exactify a position map of triple products naively using RootApproximant *)
exactifyTPPMalt[TPPM_]:=Module[{elts,positions,exactelts},
elts=#[[-1]]&/@TPPM;
positions=#[[1]]&/@TPPM;
exactelts=RootApproximant/@elts;
Thread[positions->exactelts]
]
(* Exactify a triple product tensor naively using RootApproximant *)
exactifyTPalt[T_,tolerance_:10^(-6)]:=arrayFromPositionMap[exactifyTPPMalt[arrayPositionMap[T,tolerance]]]
