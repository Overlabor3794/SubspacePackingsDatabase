(* ::Package:: *)

(* Created by Gene Kopp, Aug 2025 *)
(* Functions to convert tuples of inexact numbers to exact numbers, assuming they are Galois conjugate over a small field *)
(* Includes exactification of triple products *)


Get[FileNameJoin[NotebookDirectory[], "init.wl"]];


(* Position of the minimum element of a list of distinct real numbers *)
positionSmallest[l_]:=FirstPosition[l,Min[l]][[1]]
(* Exactification using elementary symmetric polynomials and RootApproximant *)
(* If "RationalCoefficients" is set to True, instead use Rationalize on coefficeints *)
(* "SimplificationMethod" is the function used to simplfy the algebraic numbers to be output by exactifyTuple *)
(* If "NumericalRefinement" is set to true, instead of simplifying, approximate to higher precision and then apply RootApproximant again *)
(* Increase the precision by a factor of "RefinementFactor" when using "NumericalRefinement" *)
Options[exactifyTuple]={RationalCoefficients->False,SimplificationMethod->RootReduce,NumericalRefinement->False,RefinementFactor->10};
exactifyTuple[\[Alpha]_,OptionsPattern[]]:=Module[{deg,ESP,X,exactESP,f,roots,RC,SM,NR,RF},
RC=OptionValue[RationalCoefficients];
SM=OptionValue[SimplificationMethod];
NR=OptionValue[NumericalRefinement];
RF=OptionValue[RefinementFactor];
deg=Length[\[Alpha]];
ESP=CoefficientList[Expand[Times@@(X-\[Alpha])],X];
exactESP=If[RC,Rationalize[#,10^(-0.75Precision[\[Alpha][[1]]])]&/@ESP,RootApproximant/@ESP];
f=Plus@@(exactESP . #^Range[0,deg])&;
roots=If[NR,
Table[RootApproximant[N[Root[f,j],RF*N@Precision[Plus@@\[Alpha]]]],{j,1,deg}],
Table[SM[Root[f,j]],{j,1,deg}]];
Table[roots[[positionSmallest[Abs/@(\[Alpha][[j]]-roots)]]],{j,1,deg}]
]
(* Converts an array to a list of distinct elements -> array positions *)
Options[arrayPositionMap]={Tolerance->10^(-6)};
arrayPositionMap[l_,OptionsPattern[]]:=Module[{elts,positions,level,tolerance},
tolerance=OptionValue[Tolerance];
elts=DeleteDuplicates[Flatten[l],Abs[#1-#2]<tolerance&];
level=Length[Dimensions[l]];
positions=Table[Position[l,_?(Abs[#-elts[[j]]]<tolerance&),level],{j,1,Length[elts]}];
Thread[positions->elts]
]
(* Covert an array position map to a standard array *)
arrayFromPositionMap[PM_]:=Normal[SparseArray[Flatten[Thread/@PM,1]]];
(* Exactify a position map of triple products, taking advantage of possible Galois conjugates *)
Options[exactifyTPPM]={RationalCoefficients->False,SimplificationMethod->RootReduce,NumericalRefinement->False,RefinementFactor->10};
exactifyTPPM[TPPM_,opts:OptionsPattern[]]:=Module[{elts,positions,exactelts},
elts=#[[-1]]&/@TPPM;
positions=#[[1]]&/@TPPM;
exactelts=exactifyTuple[elts,opts];
Thread[positions->exactelts]
]
(* Exactify a triple product tensor, taking advantage of possible Galois conjugates *)
Options[exactifyTP]={Tolerance->10^(-6),RationalCoefficients->False,SimplificationMethod->RootReduce,NumericalRefinement->False,RefinementFactor->10};
exactifyTP[T_,opts:OptionsPattern[]]:=arrayFromPositionMap[
exactifyTPPM[
arrayPositionMap[T,Tolerance->OptionValue[Tolerance]],
FilterRules[{opts,Options[exactifyTP]},Options[exactifyTPPM]]
]
]
(* Exactify a position map of triple products naively using RootApproximant *)
exactifyTPPMalt[TPPM_]:=Module[{elts,positions,exactelts},
elts=#[[-1]]&/@TPPM;
positions=#[[1]]&/@TPPM;
exactelts=RootApproximant/@elts;
Thread[positions->exactelts]
]
(* Exactify a triple product tensor naively using RootApproximant *)
Options[exactifyTPalt]={Tolerance->10^(-6)};
exactifyTPalt[T_,OptionsPattern[]]:=arrayFromPositionMap[exactifyTPPMalt[arrayPositionMap[T,Tolerance->OptionValue[Tolerance]]]]
