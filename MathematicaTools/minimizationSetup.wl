(* ::Package:: *)

(* Created by Gene Kopp, 2021 *)
(* Updated Aug 2024; updated Mar 2026 *)
(* Functions to find complex frames minimizing the p-frame potential *)


(* Coherence of a sythesis operator Phi *)
Coherence[Phi_]:=Module[{V},
V=Phi\[ConjugateTranspose];
Max[Flatten[Table[Abs[V[[i]]\[Conjugate] . V[[j]]]/(Norm[V[[i]]]Norm[V[[j]]]),{i,1,Length[V]},{j,i+1,Length[V]}]]]
]
(* p-frame potential of a sythesis operator Phi *)
pFramePotential[Phi_,p_]:=Module[{V},
V=Phi\[ConjugateTranspose];
2Sum[(Abs[V[[i]]\[Conjugate] . V[[j]]]/(Norm[V[[i]]]Norm[V[[j]]]))^p,{i,1,Length[V]},{j,i+1,Length[V]}]
]
(* Welch bound [lower bound on the coherence], acheived if and only if Phi is an equiangular tight frame *)
Welch[d_,n_]:=Sqrt[(n-d)/(d(n-1))]


(* Values of d and n should be set before running this block. *)
(* variable vector list *)
PhiVar=Table[a[i,j]+b[i,j]I,{i,1,d},{j,1,n}];
(* list of initial values for variables *)
varcons[Phi0_]:=Join[Transpose[{Flatten[Table[a[i,j],{i,1,d},{j,1,n}]],Re[Flatten[Phi0]]}],
Transpose[{Flatten[Table[b[i,j],{i,1,d},{j,1,n}]],Im[Flatten[Phi0]]}]
]
(* minimize p-frame potential with QuasiNewton *)
(* vector list, p, precision *)
MinPhiQNp[Phi0_,p_,NN_]:=FindMinimum[pFramePotential[PhiVar,p],varcons[Phi0],
Method->"QuasiNewton",MaxIterations->1000,WorkingPrecision->NN]
(* vector list, p, precision, maximum allowed iterations *)
MinPhiQNp[Phi0_,p_,NN_,it_]:=FindMinimum[pFramePotential[PhiVar,p],varcons[Phi0],
Method->"QuasiNewton",MaxIterations->it,WorkingPrecision->NN]
(* vector list, p *)
(* using MachinePrecision *)
MinPhiQNpcheap[Phi0_,p_]:=FindMinimum[pFramePotential[PhiVar,p],varcons[Phi0],Method->"QuasiNewton",MaxIterations->1000]
(* minimize coherence with PrincipalAxis [OFTEN FAILS] *)
(* vector list, precision *)
MinPhiPA[Phi0_,NN_]:=FindMinimum[Coherence[PhiVar],varcons[Phi0],Method->"PrincipalAxis",WorkingPrecision->NN]
(* minimize coherence with PrincipalAxis [OFTEN FAILS] *)
(* vector list, precision, maximum allowed iterations *)
MinPhiPA[Phi0_,NN_,it_]:=FindMinimum[Coherence[PhiVar],varcons[Phi0],Method->"PrincipalAxis",MaxIterations->it,WorkingPrecision->NN]
(* random seed vector list *)
rand[]:=RandomReal[NormalDistribution[],{d,n}]+RandomReal[NormalDistribution[],{d,n}]I
(* ETF conditions *)
tightnessIdealGenerators=Flatten[Table[PhiVar[[All,k]]-(d/n)Sum[(PhiVar[[All,j]]\[Conjugate] . PhiVar[[All,k]])PhiVar[[All,j]],{j,1,n}],{k,1,n}]];
equiangularWelchIdealGenerators=Flatten[
Table[(PhiVar[[All,i]]\[Conjugate] . PhiVar[[All,j]])(PhiVar[[All,j]]\[Conjugate] . PhiVar[[All,i]])-Welch[d,n]^2,{i,1,n},{j,i+1,n}]];
unitNormIdealGenerators=Table[PhiVar[[All,i]]\[Conjugate] . PhiVar[[All,i]]-1,{i,1,n}];
(* The functions below attempt to refine the precision of ETFs using the equations an ETF satisfies.
They currently do not work very well. You should use MinPhiQNp with p=4 instead [or figure out how to improve this]. *)
etfRefine1[Phi0_,NN_]:=Module[{idealGenerators},
idealGenerators=Join[unitNormIdealGenerators,equiangularWelchIdealGenerators,tightnessIdealGenerators];
FindRoot[idealGenerators . RandomInteger[10,{Length[idealGenerators],2d n}],varcons[Phi0],WorkingPrecision->NN]
]
etfRefine[Phi0_,NN_]:=Module[{idealGenerators},
idealGenerators=Join[unitNormIdealGenerators,equiangularWelchIdealGenerators];
FindRoot[idealGenerators . RandomInteger[10,{Length[idealGenerators],2d n}],varcons[Phi0],WorkingPrecision->NN]
]
