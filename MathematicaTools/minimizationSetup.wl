(* ::Package:: *)

(* Created by Gene Kopp, 2021 *)
(* Updated Aug 2024; updated Mar 2026 *)
(* Functions to find complex frames minimizing the p-frame potential *)


(* Coherence of a sythesis operator Phi *)
Coherence[Phi_] := Module[{V},
  V = Normalize /@ ConjugateTranspose[Phi];
  Max@Abs@UpperTriangularize[V . V\[ConjugateTranspose], 1]
]

(* p-frame potential of a sythesis operator Phi *)
pFramePotential[Phi_, p_] := Module[{V = Phi\[ConjugateTranspose]},
  2 Sum[(Abs[V[[i]]\[Conjugate] . V[[j]]]/(Norm[V[[i]]] Norm[V[[j]]]))^p,
      {i, 1, Length[V]}, {j, i + 1, Length[V]}]
  ]

Clear[pFramePotential2]
Options[pFramePotential2] = Options[Total];
pFramePotential2[Phi_, p_, OptionsPattern[]] := Module[{V, CS},
  CS = OptionValue[Method];
  If[CS === Automatic && Precision[Phi] === MachinePrecision,
   CS = "CompensatedSummation"];
  V = Normalize /@ ConjugateTranspose[Phi];
  2 Total[Abs[UpperTriangularize[V . V\[ConjugateTranspose], 1]]^p, 2, Method -> CS]
  ]

(* Welch bound [lower bound on the coherence], acheived if and only if Phi is an
   equiangular tight frame *)
Welch[d_, n_] := Sqrt[(n - d)/(d (n - 1))]


(* variable vector list *)
PhiVar[d_, n_] := Table[a[i, j] + b[i, j] I, {i, 1, d}, {j, 1, n}];

(* list of initial values for variables *)
varcons[Phi0_] := Module[{d, n},
  {d, n} = Dimensions[Phi0];
  Join[
   Transpose@{Flatten@Table[a[i, j], {i, 1, d}, {j, 1, n}], 
     Re[Flatten[Phi0]]},
   Transpose@{Flatten@Table[b[i, j], {i, 1, d}, {j, 1, n}], 
     Im[Flatten[Phi0]]}
   ]
  ]

(* minimize p-frame potential with QuasiNewton *)
(* vector list, p, options *)
MinPhiQNp[Phi0_, p_, opts : OptionsPattern[]] := Module[{d, n, min},
  {d, n} = Dimensions[Phi0];
  min = FindMinimum[pFramePotential[PhiVar[d, n], p], varcons[Phi0],
    opts, Method -> "QuasiNewton", MaxIterations -> 1000, 
    WorkingPrecision -> MachinePrecision];
  {min[[1]], PhiVar[d, n] /. min[[2]]}
  ]

(* minimize coherence with PrincipalAxis [OFTEN FAILS] *)
(* vector list, options *)
MinPhiPA[Phi0_, opts : OptionsPattern[]] := Module[{d, n, min},
  {d, n} = Dimensions[Phi0];
  min = FindMinimum[Coherence[PhiVar[d, n]], varcons[Phi0],
    opts, Method -> "PrincipalAxis", MaxIterations -> Automatic, 
    WorkingPrecision -> MachinePrecision];
  {min[[1]], PhiVar[d, n] /. min[[2]]}
  ]

(* random seed vector list *)
rand[] := RandomReal[NormalDistribution[], {d, n}] +
    RandomReal[NormalDistribution[], {d, n}] I

rand[d_, n_] := RandomReal[NormalDistribution[], {d, n}] +
    RandomReal[NormalDistribution[], {d, n}] I

(* ETF conditions *)
tightnessIdealGenerators[d_, n_] := Flatten[Table[
    PhiVar[d, n][[All, k]] - (d/n) Sum[(PhiVar[d, n][[All, j]]\[Conjugate] . 
        PhiVar[d, n][[All, k]]) PhiVar[d, n][[All, j]], {j, 1, n}], {k, 1, n}]];

equiangularWelchIdealGenerators[d_, n_] := Flatten[Table[
    (PhiVar[d, n][[All, i]]\[Conjugate] . PhiVar[d, n][[All, j]])
        (PhiVar[d, n][[All, j]]\[Conjugate] . PhiVar[d, n][[All, i]]) -
        Welch[d, n]^2, {i, 1, n}, {j, i + 1, n}]];

unitNormIdealGenerators[d_, n_] := 
  Table[PhiVar[d, n][[All, i]]\[Conjugate] . PhiVar[d, n][[All, i]] - 1, {i, 1, n}];

(* The functions below attempt to refine the precision of ETFs using the
   equations an ETF satisfies. They currently do not work very well. You should
   use MinPhiQNp with p=4 instead [or figure out how to improve this]. *)
Options[etfRefine] = {WorkingPrecision -> MachinePrecision, 
   TightnessIdealGenerators -> False};
etfRefine[Phi0_, OptionsPattern[]] := Module[{d, n, idealGenerators, TID},
  {d, n} = Dimensions[Phi0];
  TID = If[OptionValue[TightnessIdealGenerators], 
    tightnessIdealGenerators[d, n],
    {}];
  idealGenerators = Join[unitNormIdealGenerators[d, n],
    equiangularWelchIdealGenerators[d, n], TID];
  PhiVar[d, n] /. FindRoot[idealGenerators .
     RandomInteger[10, {Length[idealGenerators], 2 d n}], varcons[Phi0],
    WorkingPrecision -> OptionValue[WorkingPrecision]]
  ]
