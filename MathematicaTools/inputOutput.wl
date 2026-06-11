(* ::Package:: *)

(* Extract packing dimensions (d,n) from file name *)
extractDimensions[filename_String] := ToExpression[StringCases[filename,
   RegularExpression["(\\d+)x(\\d+)"] -> {"$1", "$2"}][[1]]]

(* Extract the number of triple products from file name *)
extractNumberTP[filename_String] := ToExpression[StringCases[filename,
   RegularExpression["(\\d+)x(\\d+)_(\\d+)"] -> {"$3"}][[1, 1]]]

(* Function to import .gos, .tp, and .exa files *)
(* For .gos files, the fmt argument is not applicable and is ignored if passed *)
(* For .tp files, the available formats are "TP", which gives the full TP tensor,
   and "Poisiton map", which gives the position map for the TP tensor *)
(* For .exa files, the available formats are "TP", which gives the full TP tensor,
   "Position map", which gives the position map for the TP slice, and "TP slice",
   which returns the TP slice *)
(* The available option is Precision. The default is MachinePrecision for .gos 
   files, and Infinity for .tp and .exa files *)
importPacking[filename_String, fmt_String : "TP", d_Integer : Automatic,
  n_Integer : Automatic, opts : OptionsPattern[]] := Module[{ext, lcfmt},
   If[! FileExistsQ[filename],
    Message[importPacking::nffil, filename];
    Return[$Failed]];
   ext = FileExtension[filename];
   lcfmt = ToLowerCase[fmt];
   Which[
    ext == "gos" || ext == "txt", gosImport[filename, d, n opts],
    ext == "tp", tpImport[filename, lcfmt, opts],
    ext == "exa", exaImport[filename, lcfmt, opts],
    True, Message[importPacking::FileName, filename]
    ]
   ]
ResourceFunction["AddCodeCompletion"]["importPacking"][
  "RelativeFileName", {"TP", "Position map", "TP slice"}];
importPacking::nffil = "File `1` not found during import";
importPacking::FileName = 
  "`1` is an invalid file name; *.gos, *.tp, or *.exa expected";
importPacking::tpFormat = "\"`1`\" is an invalid format. Valid formats for \
*.tp are \"TP\" and \"Position map\"";
importPacking::exaFormat = "`1` is an invalid format. Valid formats for *.exa are\
  \"TP\",  \"TP slice\",  and  \"Position map\"";

(* Internal function to import .gos files *)
Options[gosImport] = {Precision -> MachinePrecision};
gosImport[filename_, d_, n_, OptionsPattern[]] := Module[{dd, nn, plist},
  plist =
   SetPrecision[Import[filename, "List"], OptionValue[Precision]];
  If[d === Automatic || n === Automatic,
   {dd, nn} = extractDimensions[filename],
   {dd, nn} = {d, n}];
  SOfromGoS[plist, {dd, nn}]
  ]

(* Internal function to import .tp files *)
Options[tpImport] = {Precision -> Infinity};
tpImport[filename_, fmt_, OptionsPattern[]] := Module[{prec, TPPM},
  prec = OptionValue[Precision];
  TPPM = ToExpression /@ Import[filename, "List"];
  Which[
   fmt == "tp",
   TPPM[[All, 2]] = N[TPPM[[All, 2]], prec];
   arrayFromPositionMap[TPPM]
   ,
   fmt == "position map",
   TPPM[[All, 2]] = N[TPPM[[All, 2]], prec];
   TPPM
   ,
   True,
   Message[importPacking::tpFormat, fmt]
   ]
  ]

(* Internal function to import .exa files *)
Options[exaImport] = {Precision -> Infinity};
exaImport[filename_, fmt_, OptionsPattern[]] := Module[{prec, TPSPM},
  prec = OptionValue[Precision];
  TPSPM = ToExpression /@ Import[filename, "List"];
  Which[
   fmt == "tp",
   TPSPM[[All, 2]] = N[TPSPM[[All, 2]], 2*prec];
   N[TPfromTPslice@arrayFromPositionMap[TPSPM], prec]
   ,
   fmt == "position map",
   TPSPM[[All, 2]] = N[TPSPM[[All, 2]], prec];
   TPSPM
   ,
   fmt == "tp slice",
   TPSPM[[All, 2]] = N[TPSPM[[All, 2]], prec];
   arrayFromPositionMap[TPSPM]
   ,
   True,
   Message[importPacking::exaFormat, fmt]
   ]
  ]

(* Function to export Packings *)
(* Works with .gos, .tp, and .exa file types *)
(* First argument can be an a frame, a triple product tensor, a triple product
   slice, a triple product position map, or a triple product slice position map *)
(* The function uses the structure of the first argument to determine how it
   should be exported *)
(* If the global flag $exportPackingChecks is set to True (default), then the
   function performs checks between the structure of the array and the file name
   and warns with a prompt if a mismatch is detected *)
(* The available option is Precision and is only relevant to .gos files *)
exportPacking[array_, filename_, opts : OptionsPattern[]] := 
 Module[{cont, dims, dim},
  If[$exportPackingChecks && FileExistsQ[filename],
   cont = ChoiceDialog[filename <> " already exists.\nDo you want \
to replace it?", {"Yes" -> True, "No" -> False}];
   If[! cont, Return[$Failed]];
   ];
  dims = Dimensions[array];
  dim = Length[dims];
  Which[
   dim == 1, pmExport[array, filename],
   dim == 2 && dims[[1]] < dims[[2]], gosExport[array, filename, opts],
   dim == 2 && dims[[1]] == dims[[2]], exaExport[array, filename],
   dim == 3 && dims[[1]] == dims[[2]] == dims[[3]], 
   tpExport[array, filename],
   True, Message[exportPacking::structure]
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
Options[gosExport] = {Precision -> MachinePrecision};
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
  prec = OptionValue[Precision];
  Export[filename, N[GoSfromSO[Phi], prec], "List"]
  ]

(* Internal function to export .tp files *)
tpExport[TP_, filename_] := Module[{d, n, TPPM},
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
  TPPM = arrayPositionMap[TP];
  If[$exportPackingChecks && Length[TPPM] != extractNumberTP[filename],
   If[! numberTPDialong[filename], Return[$Failed]];
   ];
  Export[filename, TPPM, "List"]
  ]

(* Internal function to export .exa files *)
exaExport[TPS_, filename_] := Module[{d, n, TPSPM},
  If[$exportPackingChecks,
   n = Dimensions[TPS][[1]];
   d = n / ((n - 1) TP[[2, 2]] + 1);
   If[{d, n} != extractDimensions[filename],
    If[! dimDialong[filename], Return[$Failed]];
    ];
   If[FileExtension[filename] != "exa",
    If[! extDialong[filename], Return[$Failed]];
    ];
   ];
  TPSPM = arrayPositionMap[TPS];
  Export[filename, TPSPM, "List"]
  ]

(* Internal function to export array position maps to .tp or .exa files *)
pmExport[PM_, filename_] := Module[{dim, ext, extcheck, d, n, \[Alpha]},
  If[$exportPackingChecks,
   dim = Length@PM[[1, 1, 1]];
   ext = FileExtension[filename];
   extcheck = (ext != "tp" && ext != "exa") || (ext == "tp" && dim != 3) ||
       (ext == "exa" && dim != 2);
   If[extcheck,
    If[! extDialong[filename], Return[$Failed]];
    ];
   n = Max@PM[[All, 1]];
   Which[
    dim == 2,
    \[Alpha] = {2, 2} /. Flatten[Thread /@ PM];,
    dim == 3,
    \[Alpha] = {1, 2, 2} /. Flatten[Thread /@ PM];
    If[Length[PM] != extractNumberTP[filename],
     If[! numberTPDialong[filename], Return[$Failed]];
     ]
   ];
   d = n/((n - 1) \[Alpha] + 1);
   If[{d, n} != extractDimensions[filename],
    If[! dimDialong[filename], Return[$Failed]];
    ];
   ];
  Export[filename, PM, "List"]
  ]

(* Internal function to export lookup tables to .tp or .exa files *)
lutExport[LUT_, filename_] := Module[{TPs, array, dim, ext, d, n, \[Alpha]},
  {TPs, array} = LUT;
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
    \[Alpha] = TPs[[array[[2, 2]] + 1]],
    dim == 3,
    \[Alpha] = TPs[[array[[1, 2, 2]] + 1]];
    If[Length[TPs] != extractNumberTP[filename],
     If[! numberTPDialong[filename], Return[$Failed]];
     ]
    ];
   d = n/((n - 1) \[Alpha] + 1);
   If[{d, n} != extractDimensions[filename],
    If[! dimDialong[filename], Return[$Failed]];
    ];
   ];
  array = ExportString[array, "JSON", Compact -> True];
  Export[filename, {TPs, array}, "List"]
  ]
