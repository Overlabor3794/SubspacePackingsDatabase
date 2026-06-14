(* ::Package:: *)

(* Extract packing dimensions (d,n) from file name *)
extractDimensions[filename_String] := ToExpression[StringCases[filename,
   RegularExpression["(\\d+)x(\\d+)"] -> {"$1", "$2"}][[1]]]

(* Extract the number of triple products from file name *)
extractNumberTP[filename_String] := ToExpression[StringCases[filename,
   RegularExpression["(\\d+)x(\\d+)_(\\d+)"] -> {"$3"}][[1, 1]]]

(* Function to import .gos, .tp, and .exa files *)
(* For .gos files, the fmt argument is not applicable and is ignored if passed *)
(* For .tp and .exa files, the available formats are "TP", "TP slice", and
   "Lookup table". The default is "TP" for .tp files and "TP slice" for .exa
   files *)
(* The available option is PrecisionGoal. If unspecified, then the precision of
   the input is unchanged *)
Options[importPacking] = {PrecisionGoal -> Automatic};
importPacking[filename_String, fmt_String : Automatic, d_Integer : Automatic,
  n_Integer : Automatic, opts : OptionsPattern[]] := Module[{ext},
   If[! FileExistsQ[filename],
    Message[importPacking::nffil, filename];
    Return[$Failed]];
   ext = FileExtension[filename];
   Which[
    ext == "gos" || ext == "txt",
    If[fmt =!= Automatic, Message[importPacking::ignarg, fmt]];
    gosImport[filename, d, n, opts],
    ext == "tp", tpImport[filename, fmt, opts],
    ext == "exa", exaImport[filename, fmt, opts],
    True, Message[importPacking::FileName, filename]; Return[$Failed]
    ]
   ]
ResourceFunction["AddCodeCompletion"]["importPacking"][
  "RelativeFileName", {"TP", "TP slice", "Lookup table"}];
importPacking::nffil = "File `1` not found during import";
importPacking::FileName = 
  "`1` is an invalid file name; *.txt, *.gos, *.tp, or *.exa expected";
importPacking::ignarg = "The argument `1` is ignored";
importPacking::fmt = "`1` is an invalid format. Valid formats are\
  \"TP\",  \"TP slice\",  and  \"Lookup table\"";

(* Internal function to import .gos files *)
Options[gosImport] = Options[importPacking];
gosImport[filename_, d_, n_, OptionsPattern[]] := Module[{gprec, plist, dd, nn},
  gprec = OptionValue[PrecisionGoal];
  plist = setPrecision[Import[filename, "List"], gprec, 5];
  If[d === Automatic || n === Automatic,
   {dd, nn} = extractDimensions[filename],
   {dd, nn} = {d, n}];
  SOfromGoS[plist, {dd, nn}]
  ]

(* Internal function to import .tp files *)
Options[tpImport] = Options[importPacking];
tpImport[filename_, fmt_, OptionsPattern[]] := Module[{gprec, LUT, lcfmt},
  gprec = OptionValue[PrecisionGoal];
  LUT = Import[filename, "List"];
  LUT = {ToExpression@LUT[[1]], ImportString[LUT[[2]], "JSON"]};
  LUT[[1]] = setPrecision[LUT[[1]], gprec, 5];
  lcfmt = If[fmt === Automatic, "tp", ToLowerCase[fmt]];
  Which[
   lcfmt == "tp", arrayfromLUT[LUT],
   lcfmt == "lookup table", LUT,
   lcfmt == "tp slice", TPslicefromTP@arrayfromLUT[LUT],
   True, Message[importPacking::fmt, fmt]; Return[$Failed]
   ]
  ]

(* Internal function to import .exa files *)
Options[exaImport] = Options[importPacking];
exaImport[filename_, fmt_, OptionsPattern[]] := Module[{gprec, LUT, lcfmt, output},
  gprec = OptionValue[PrecisionGoal];
  LUT = Import[filename, "List"];
  LUT = {ToExpression@LUT[[1]], ImportString[LUT[[2]], "JSON"]};
  LUT[[1]] = setPrecision[LUT[[1]], gprec + 5];
  lcfmt = If[fmt === Automatic, "tp slice", ToLowerCase[fmt]];
  Which[
   lcfmt == "tp", setPrecision[TPfromTPslice@arrayfromLUT[LUT], gprec],
   lcfmt == "lookup table",
   LUT[[1]] = setPrecision[LUT[[1]], gprec];
   LUT,
   lcfmt == "tp slice",
   LUT[[1]] = setPrecision[LUT[[1]], gprec];
   arrayfromLUT[LUT],
   True, Message[importPacking::fmt, fmt]; Return[$Failed]
   ]
  ]

(* Function to export Packings *)
(* Works with .gos, .tp, and .exa file types *)
(* First argument can be an a frame, a triple product tensor, a triple product
   slice, a triple product lookup table, or a triple product slice lookup table *)
(* The function uses the structure of the first argument to determine how it
   should be exported *)
(* If the global flag $exportPackingChecks is set to True (default), then the
   function performs checks between the structure of the array and the file name
   and warns with a prompt if a mismatch is detected *)
(* The available option is PrecisionGoal and is only relevant to .gos files *)
exportPacking[array_, filename_, opts : OptionsPattern[]] := Module[{cont, type},
  If[$exportPackingChecks && FileExistsQ[filename],
   cont = ChoiceDialog[filename <> " already exists.\nDo you want to replace it?",
      {"Yes" -> True, "No" -> False}];
   If[! cont, Return[$Failed]];
   ];
  type = arrayType[array];
  Which[
   type === "SO", gosExport[array, filename, opts],
   type === "TP", tpExport[array, filename],
   type === "TPS", exaExport[array, filename],
   type === "TP LUT" || type === "TPS LUT", lutExport[array, filename],
   True, Message[exportPacking::structure]; Return[$Failed]
   ]
  ]
ResourceFunction["AddCodeCompletion"]["exportPacking"][None, "RelativeFileName"];
exportPacking::structure = "The structure of the first argument is inconsistent \
with the supported file types";

dimDialong[filename_] := ChoiceDialog["Incorrect dimensions detected\
 in " <> FileNameTake[filename] <> ".\nDo you want to continue\
 exporting?", {"Yes" -> True, "No" -> False}];
numberTPDialong[filename_] := ChoiceDialog[
 "Incorrect number of distinct triple products detected in " <> 
  FileNameTake[filename] <> ".\nDo you want to continue exporting?",
  {"Yes" -> True, "No" -> False}];
extDialong[filename_] := ChoiceDialog["Incorrect extension detected \
in " <> FileNameTake[filename] <> ".\nDo you want to continue \
exporting?", {"Yes" -> True, "No" -> False}];

(* Internal function to export .gos files *)
Options[gosExport] = {PrecisionGoal -> MachinePrecision};
gosExport[Phi_, filename_, OptionsPattern[]] := Module[{prec},
  If[$exportPackingChecks,
   If[Dimensions[Phi] != extractDimensions[filename],
    If[! dimDialong[filename], Return[$Failed]];
    ];
   If[numberTP[Phi] != extractNumberTP[filename],
    If[! numberTPDialong[filename], Return[$Failed]];
    ];
   If[FileExtension[filename] != "gos",
    If[! extDialong[filename], Return[$Failed]];
    ];
   ];
  prec = OptionValue[PrecisionGoal];
  Export[filename, N[GoSfromSO[Phi], prec], "List"]
  ]

(* Internal function to export .tp files *)
tpExport[TP_, filename_] := Module[{d, n, distinct, array},
  If[$exportPackingChecks,
   n = Dimensions[TP][[1]];
   d = n / ((n - 1) TP[[1, 2, 2]] + 1);
   If[{d, n} != extractDimensions[filename],
    If[! dimDialong[filename], Return[$Failed]];
    ];
   If[FileExtension[filename] != "tp",
    If[! extDialong[filename], Return[$Failed]];
    ];
   ];
  {distinct, array} = arraytoLUT[TP];
  If[$exportPackingChecks && Length[distinct] != extractNumberTP[filename],
   If[! numberTPDialong[filename], Return[$Failed]];
   ];
  array = ExportString[array, "JSON", Compact -> True];
  Export[filename, {distinct, array}, "List"]
  ]

(* Internal function to export .exa files *)
exaExport[TPS_, filename_] := Module[{d, n, distinct, array},
  If[$exportPackingChecks,
   n = Dimensions[TPS][[1]];
   d = n / ((n - 1) TPS[[2, 2]] + 1);
   If[{d, n} != extractDimensions[filename],
    If[! dimDialong[filename], Return[$Failed]];
    ];
   If[FileExtension[filename] != "exa",
    If[! extDialong[filename], Return[$Failed]];
    ];
   ];
  {distinct, array} = arraytoLUT[TPS];
  array = ExportString[array, "JSON", Compact -> True];
  Export[filename, {distinct, array}, "List"]
  ]

(* Internal function to export lookup tables to .tp or .exa files *)
lutExport[LUT_, filename_] := Module[{distinct, array, dim, ext, d, n, \[Alpha]},
  {distinct, array} = LUT;
  If[$exportPackingChecks,
   dim = ArrayDepth[array];
   ext = FileExtension[filename];
   If[(ext != "tp" && ext != "exa") || (ext == "tp" && dim != 3) ||
     (ext == "exa" && dim != 2),
    If[! extDialong[filename], Return[$Failed]];
    ];
   n = Dimensions[array][[1]];
   Which[
    dim == 2,
    \[Alpha] = distinct[[array[[2, 2]] + 1]],
    dim == 3,
    \[Alpha] = distinct[[array[[1, 2, 2]] + 1]];
    If[Length[distinct] != extractNumberTP[filename],
     If[! numberTPDialong[filename], Return[$Failed]];
     ]
    ];
   d = n/((n - 1) \[Alpha] + 1);
   If[{d, n} != extractDimensions[filename],
    If[! dimDialong[filename], Return[$Failed]];
    ];
   ];
  array = ExportString[array, "JSON", Compact -> True];
  Export[filename, {distinct, array}, "List"]
  ]


setPrecision[expr_, prec_, guard_ : 0] :=
 If[NumericQ[prec],
  If[guard === 0,
   SetPrecision[expr, prec],
   SetPrecision[SetPrecision[expr, prec + guard], prec]
  ],
  expr
 ]
