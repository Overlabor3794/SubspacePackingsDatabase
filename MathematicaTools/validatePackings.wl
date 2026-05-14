(* ::Package:: *)

(* Tests validity of existing packings *)
(* Currently valid for ETFs only *)


Get[FileNameJoin[NotebookDirectory[], "init.wl"]];
Get[FileNameJoin[NotebookDirectory[], "convertFrameData.wl"]];
Get[FileNameJoin[NotebookDirectory[], "exactification.wl"]];
Get[FileNameJoin[NotebookDirectory[], "minimizationSetup.wl"]];
Get[FileNameJoin[NotebookDirectory[], "inputOutput.wl"]];
Get[FileNameJoin[NotebookDirectory[], "frameInvariants.wl"]];


(* Validates all ETF files with file name matching pattern and
   ending with .gos, .tp, or .exa *)
(* validatePackings["*"]     tests  all files *)
(* validatePackings["*.gos"] tests .gos files *)
(* validatePackings["*.tp"]  tests .tp  files *)
(* validatePackings["*.exa"] tests .exa files *)
validatePackings[pattern_String, opts:OptionsPattern[]] := 
 Module[{validators, files, validfiles, basenames},
  files = FileNameTake /@ FileNames[pattern, $packingsDirectory];
  validfiles = 
   FileNameTake /@ 
    FileNames[{"etf*.gos", "etf*.tp", "etf*.exa"}, $packingsDirectory];
  files = Intersection[files, validfiles];
  If[files == {},
   Message[validatePackings::Pattern, pattern];
   Return[]
   ];
  basenames = DeleteDuplicates[FileBaseName /@ files];
  validators = <|"gos" -> gosValidate, "tp" -> tpValidate, 
    "exa" -> exaValidate|>;
  Do[
   Print["=============== ", basename, " ==============="];
   validfiles = Select[files, FileBaseName[#] === basename &];
   Do[
     validators[FileExtension@file][file, opts],
    {file, validfiles}],
   {basename, basenames}
   ]
  ]
ResourceFunction["AddCodeCompletion"]["validatePackings"][
  "RelativeFileName"];
validatePackings::Pattern = "No valid files found matching \"`1`\".";


(* The functions below are internal functions and not meant
   to be used by the end user. Use validatePackings with an
   appropriate pattern. *)

(* .gos files *)
(* Validates a .gos ETF given its file name *)
(* Checks that the coherence is equal to the Welch bound,
   that the frame is unit-norm, that the number of distinct
   triple products is equal to the number in the file name *)
gosValidate[filename_, OptionsPattern[]] := Module[{d, n, Phi, pass},
  {d, n} = extractDimensions[filename];
  Phi = importPacking[filename];
  pass = True;
  If[Coherence[Phi] != N@Welch[d, n],
   Print[Style[filename, ": Coherence test failed", Red]];
   pass = False];
  If[normalizeSO[Phi] != Phi, 
   Print[Style[filename, ": Unit-norm test failed", Red]];
   pass = False];
  If[extractNumberTP[filename] != numberTPfromSO[Phi],
   Print[Style[filename, 
   ": Number of distinct triple products test failed", Red]];
   pass = False];
  If[pass, Print[filename <> ": Passed all tests"]]
  ]

(* .tp files *)
(* Validates a .tp ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF,
   that the number of distinct triple products is equal to the
   number in the file name, and that the contents match the
   contents of the corresponding .exa file *)
Options[tpValidate] = {WorkingPrecision -> MachinePrecision};
tpValidate[filename_, OptionsPattern[]] := 
 Module[{prec, d, n, TPPM, TP1, TP2, GM, pass},
  prec = OptionValue[WorkingPrecision];
  {d, n} = extractDimensions[filename];
  TPPM = importPacking[filename, "Position map"];
  TPPM[[All, 2]] = N[TPPM[[All, 2]], 2*prec];
  TP1 = arrayFromPositionMap[TPPM];
  GM = GMfromTP[TP1];
  pass = True;
  If[N[Abs[GM], prec] != 
    N[SparseArray[{{i_, i_} -> 1, {_, _} -> Welch[d, n]}, {n, n}], prec],
   Print[Style[filename, " : Gram matrix test failed", Red]];
   pass = False];
  If[Length[TPPM] != extractNumberTP[filename],
   Print[Style[filename, 
   " : Number of distinct triple products test failed", Red]];
   pass = False];
  If[FileExistsQ[FileBaseName[filename] <> ".exa"],
   TP2 = importPacking[FileBaseName[filename] <> ".exa", "TP slice"];
   If[N[TP1[[1]], prec] != N[TP2, prec],
    Print[Style[filename, 
   " : Match against corresponding .exa file test failed", Red]];
    pass = False];
   ];
  If[pass, Print[filename <> " : Passed all tests"]]
  ]

(* .exa files *)
(* Validates a .exa ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF *)
Options[exaValidate] = {WorkingPrecision -> MachinePrecision};
exaValidate[filename_, OptionsPattern[]] := 
 Module[{prec, d, n, TP, GM, pass},
  prec = OptionValue[WorkingPrecision];
  {d, n} = extractDimensions[filename];
  TP = N[importPacking[filename], 2*prec];
  GM = GMfromTP[TP];
  pass = True;
  If[N[Abs[GM], prec] != 
    N[SparseArray[{{i_, i_} -> 1, {_, _} -> Welch[d, n]}, {n, n}], 2*prec],
   Print[Style[filename, ": Gram matrix test failed", Red]];
   pass = False];
  If[pass, Print[filename <> ": Passed all tests"]]
  ]
