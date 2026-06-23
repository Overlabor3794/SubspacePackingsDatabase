(* ::Package:: *)

(* Custom version of SetPrecision with optional argument for guard digits *)
setPrecision[expr_, prec_, guard_ : 0] :=
 If[NumericQ[prec],
  If[guard === 0,
   SetPrecision[expr, prec],
   SetPrecision[SetPrecision[expr, prec + guard], prec]
  ],
  expr
 ]

(* Extract packing dimensions (d,n) from file name *)
extractDimensions[filename_String] := ToExpression[StringCases[filename,
   RegularExpression["(\\d+)x(\\d+)"] -> {"$1", "$2"}][[1]]]

(* Extract the number of triple products from file name *)
extractNumberTP[filename_String] := ToExpression[StringCases[filename,
   RegularExpression["(\\d+)x(\\d+)_(\\d+)"] -> {"$3"}][[1, 1]]]

(* Replace file extension while preserving path structure *)
replaceExt[filename_, ext_] := Module[{split},
  split = FileNameSplit[filename];
  split[[-1]] = FileBaseName[split[[-1]]] <> "." <> ext;
  FileNameJoin[split]
  ]

(* Takes in an array containing frame data and returns its type as a string *)
(* The values returned are
   - "SO"      : Sythesis operator (frame)
   - "GM"      : Gram matrix
   - "TP"      : Triple product tensor
   - "TPS"     : Triple product slice
   - "TP LUT"  : Triple product tensor lookup table
   - "TPS LUT" : Triple product slice lookup table
   - $Failed   : If array is none of the above
  *)
(* If array is a Gram matrix, then it must be the Gram matrix of a normalized
   frame; otherwise the function will return "TPS" *)
arrayType[array_List] := Module[{dims, dim},
  If[Length[array] == 0, Return[$Failed]];
  dims = Dimensions[array];
  dim = Length[dims];
  Which[
   dim == 1 && dims[[1]] == 2,
   If[ArrayDepth[array[[2]]] == 2, Return["TPS LUT"]];
   If[ArrayDepth[array[[2]]] == 3, Return["TP LUT"]],
   dim == 2,
   If[dims[[1]] == 2 && ! MatrixQ[array],
    If[ArrayDepth[array[[2]]] == 2, Return["TPS LUT"]];
    If[ArrayDepth[array[[2]]] == 3, Return["TP LUT"]]
    ];
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

(* Coherence of a sythesis operator Phi *)
Coherence[Phi_] := Module[{V},
  V = Normalize /@ ConjugateTranspose[Phi];
  Max@Abs@UpperTriangularize[V . V\[ConjugateTranspose], 1]
]

(* p-frame potential of a sythesis operator Phi *)
Options[pFramePotential] = Options[Total];
pFramePotential[Phi_, p_, opts : OptionsPattern[]] := Module[{V, CS},
  CS = OptionValue[Method];
  If[CS === Automatic && Precision[Phi] === MachinePrecision,
   CS = "CompensatedSummation"];
  V = Normalize /@ ConjugateTranspose[Phi];
  2 Total[Abs[UpperTriangularize[V . V\[ConjugateTranspose], 1]]^p, 2, opts, Method -> CS]
  ]

(* Welch bound [lower bound on the coherence], acheived if and only if Phi is an
   equiangular tight frame *)
Welch[d_, n_] := Sqrt[(n - d)/(d (n - 1))]
