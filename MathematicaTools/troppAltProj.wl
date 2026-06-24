(* ::Package:: *)

(* A rewrite of "troppAltProj.m", available in
   https://github.com/gnikylime/GameofSloanes *)


(* Initial seed *)
Options[InitMat] = {WorkingPrecision -> MachinePrecision};
InitMat[d_, n_, opts : OptionsPattern[]] := Module[{\[Tau], T, X, t, x, cor},
  \[Tau] = 0.9;
  T = 10000;
  While[True,
   X = rand[d, 1, opts];
   X = X / Norm[X];
   t = 0;
   While[t < T,
    x = Flatten@rand[1, d];
    x = Normalize[x];
    cor = Max@Abs[X\[ConjugateTranspose] . x];
    If[cor < \[Tau],
     X = MapThread[Append, {X, x}];
     t = 0;
     If[Dimensions[X][[2]] == n, t = T],
     t++;
     ]
    ];
   If[Dimensions[X][[2]] == n, Break[]];
   ];
  X
  ]
ResourceFunction["AddCodeCompletion"]["InitMat"][
  None, None, RepeatOptions[InitMat]];

(* Alternating projections *)
Options[troppAltProj] = {WorkingPrecision -> Automatic,
   PrecisionGoal -> Automatic, MaxIterations -> 30000,
   UpdateInterval -> Infinity};
troppAltProj[d_, n_, \[Mu]_, Y_ : Automatic, opts : OptionsPattern[]] :=
 Block[{wprec, gprec, X, bound, G, error, dwprec, $MinPrecision, $MaxPrecision,
  const, interval, T, \[Lambda], V, first, s, D, U},
  wprec = OptionValue[WorkingPrecision];
  gprec = OptionValue[PrecisionGoal];
  Which[
   wprec === Automatic && gprec === Automatic,
   wprec = MachinePrecision; gprec = 15,
   wprec === Automatic && gprec =!= Automatic,
   If[gprec === MachinePrecision, wprec = MachinePrecision, wprec = 2*gprec],
   wprec =!= Automatic && gprec === Automatic,
   If[wprec === MachinePrecision, gprec = MachinePrecision, gprec = wprec/2]
   ];
  If[Y === Automatic, X = InitMat[d, n, WorkingPrecision -> wprec], X = Y];
  bound = 5*10^(-gprec);
  G = SetPrecision[X\[ConjugateTranspose] . X, wprec];
  error = Abs[Max@Abs@UpperTriangularize[G, 1] - \[Mu]]/\[Mu];
  dwprec := If[wprec === MachinePrecision,
   wprec, Min[-2*Log[10, error] + 10,
   wprec]];
  $MinPrecision = 0; $MaxPrecision = Infinity;
  $MinPrecision = $MaxPrecision = dwprec;
  G = SetPrecision[G, dwprec];
  const = ConstantArray[0, Dimensions[G]];
  interval = OptionValue[UpdateInterval];
  If[interval != Infinity,
   Print[CurrentDate[], " - Starting loop"]];
  T = OptionValue[MaxIterations];
  Do[
   G = G*\[Mu]/Clip[Abs[G], {\[Mu], Infinity}];
   Do[G[[i, i]] = 1, {i, 1, n}];
   G = (G + G\[ConjugateTranspose])/2;
   {\[Lambda], V} = Chop@Eigensystem[G];
   V = V[[Ordering[\[Lambda], All, Greater]]];
   first = \[Lambda][[1 ;; d]];
   s = Total[first];
   If[s > n,
    While[(s = Total[first]) > n,
     first = Ramp[first - (s - n)/Count[\[Lambda], _?Positive]];
     ],
    first += (n - s)/d;
    ];
   D = const;
   D[[1 ;; d, 1 ;; d]] = DiagonalMatrix[first];
   G = Transpose[V] . D . V\[Conjugate];
   If[Divisible[t, 100],
    error = Abs[Max@Abs@UpperTriangularize[G, 1] - \[Mu]]/\[Mu];
    If[error < bound, Break[]];
    $MinPrecision = 0; $MaxPrecision = Infinity;
    G = SetPrecision[G, dwprec];
    $MinPrecision = $MaxPrecision = dwprec;
    ];
   If[Divisible[t, interval],
    Print[Now, " - Iteration ", t,
               " - Precision of G = ", Precision[G],
               " - Effective precision = ", -N@Log[10, error]]
    ],
   {t, T}];
  D = DiagonalMatrix@Diagonal[G];
  G = # . G . # &@Inverse[Sqrt[D]];
  G = (G + G\[ConjugateTranspose])/2;
  {U, D, V} = SingularValueDecomposition[G, UpTo[d]];
  (U . Sqrt[D])\[ConjugateTranspose]
  ] /; MatrixQ[Y] || Y === Automatic
ResourceFunction["AddCodeCompletion"]["troppAltProj"][
  None, None, None, RepeatOptions[troppAltProj, 1]];
