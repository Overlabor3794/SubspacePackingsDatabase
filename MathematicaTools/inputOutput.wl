(* ::Package:: *)

(* Standard shortcut *)
mf=MatrixForm;
(* Directory containing text files of packings *)
packingsDirectory=FileNameJoin[ParentDirectory[NotebookDirectory[]],"Packings"];
(* Set as present working directory *)
SetDirectory[packingsDirectory];
(* Extract packing dimensions (d,n) from file name *)
extractDimensions[filename_String]:=ToExpression[StringCases[filename,RegularExpression["(\\d+)x(\\d+)"]->{"$1","$2"}][[1]]]
(* Extract the number of triple products from file name *)
extractNumberTP[filename_String]:=ToExpression[StringCases[filename,RegularExpression["(\\d+)x(\\d+)_(\\d+)"]->{"$3"}][[1,1]]]
(* Function to import .gos, .tp, and .exa files *)
(* For .gos files, the fmt argument is not applicable and is ignored if passed *)
(* For .tp files, the available formats are "TP", which gives the full TP tensor,
   and "Poisiton map", which gives the position map for the TP tensor *)
(* For .exa files, the available formats are "TP", which gives the full TP tensor,
   "Position map", which gives the position map for the TP slice, and
   "TP slice", which returns the TP slice *)
(* The available option is Precision. The default is MachinePrecision for .gos 
   files, and Infinity for .tp and .exa files *)
importPacking[filename_, opts : OptionsPattern[]] := 
 importPacking[filename, "TP", opts]
importPacking[filename_, fmt_, opts : OptionsPattern[]] := 
 Module[{ext, lcfmt},
  If[! FileExistsQ[filename],
   Message[importPacking::nffil, filename];
   Return[]];
  ext = FileExtension[filename];
  lcfmt = ToLowerCase[fmt];
  Which[
   ext == "gos", gosImport[filename, opts],
   ext == "tp", tpImport[filename, lcfmt, opts],
   ext == "exa", exaImport[filename, lcfmt, opts],
   True, Message[importPacking::FileName, filename]
   ]
  ]
ResourceFunction["AddCodeCompletion"]["importPacking"][
  "RelativeFileName"];
importPacking::nffil = "File `1` not found during Import";
importPacking::FileName = 
  "`1` is an invalid file name; *.gos, *.tp, or *.exa expected";
importPacking::tpFormat = 
  "\"`1`\" is an invalid format. Valid formats for \
*.tp are \"TP\" and \"Position map\"";
importPacking::exaFormat = 
  "`1` is an invalid format. Valid formats for *.exa are  \"TP\",  \
\"TP slice\",  and  \"Position map\"";

(* Internal function to import .gos files *)
Options[gosImport] = {Precision -> MachinePrecision};
gosImport[filename_String, OptionsPattern[]] := Module[{d, n, plist},
  {d, n} = extractDimensions[filename];
  plist = 
   SetPrecision[Import[filename, "List"], OptionValue[Precision]];
  SOfromGoS[plist, {d, n}]
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

(* Export .gos file *)
exportPacking[Phi_,filename_String]:=Export[filename,GoSfromSO[Phi],"List"]
exportPacking[Phi_,labels_List]:=Module[{d,n,numberTP,filename},
{d,n}=ToString/@Dimensions[Phi];
numberTP=ToString@numberTPfromSO@N[Phi];
filename=labels[[1]]<>"_"<>d<>"x"<>n<>"_"<>numberTP<>labels[[2]]<>".gos";
exportPacking[Phi,filename]
]/;MatchQ[labels,{_String,_String}]
(* Export .exa file *)
exportExactTP[Texact_,filename_]:=Export[filename,arrayPositionMap[Texact],"List"]
