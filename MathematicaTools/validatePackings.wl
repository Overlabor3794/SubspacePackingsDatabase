(* ::Package:: *)

(* Tests validity of existing packings *)
(* Currently valid for ETFs only *)


Get[FileNameJoin[NotebookDirectory[],"convertFrameData.wl"]];
Get[FileNameJoin[NotebookDirectory[],"exactification.wl"]];
Get[FileNameJoin[NotebookDirectory[],"minimizationSetup.wl"]];
Get[FileNameJoin[NotebookDirectory[],"inputOutput.wl"]];
Get[FileNameJoin[NotebookDirectory[],"frameInvariants.wl"]];


packingsDir = 
  FileNameJoin[{ParentDirectory@NotebookDirectory[], "Packings"}];


(* Validates all ETF files with file name matching pattern and
   ending with .gos, .tp, or .exa *)
(* validatePackings["*"]     tests  all files *)
(* validatePackings["*.gos"] tests .gos files *)
(* validatePackings["*.tp"]  tests .tp  files *)
(* validatePackings["*.exa"] tests .exa files *)
validatePackings[pattern_String] := 
 Module[{validators, files, validfiles, basenames},
  files = FileNameTake /@ FileNames[pattern, packingsDir];
  validfiles = 
   FileNameTake /@ 
    FileNames[{"etf*.gos", "etf*.tp", "etf*.exa"}, packingsDir];
  files = Intersection[files, validfiles];
  If[files == {},
   Message[validatePackings::Pattern, pattern];
   Return[]
   ];
  basenames = DeleteDuplicates[FileBaseName /@ files];
  validators = <|"gos" -> validateGOS, "tp" -> validateTP, 
    "exa" -> validateEXA|>;
  Do[
   Print["=============== ", basename, " ==============="];
   Do[If[FileBaseName[file] === basename,
     validators[FileExtension@file]@file],
    {file, files}],
   {basename, basenames}
   ]
  ]
validatePackings::Pattern = 
  "No valid files found matching pattern \"`1`\". Expected patterns \
are \"*\" ,  \"*.gos\" ,  \"*.tp\" ,  or \"*.exa\" .";


(* The functions below are internal functions and not meant
   to be used by the end user. Use validatePackings with an
   appropriate pattern. *)

(* .gos files *)
(* Tests the validy of a .gos ETF given its file name *)
(* Checks that the coherence is equal to the Welch bound,
   that the frame is unit-norm, that the number of distinct
   triple products is equal tothe number in the file name *)
validateGOS[filename_] := Module[{d, n, Phi},
  {d, n} = extractDimensions[filename];
  Phi = importPacking[filename];
  If[RootApproximant@Coherence@N[Phi] != Welch[d, n],
   Print[filename, ": Coherence test failed"]];
  If[normalizeSO[Phi] != Phi, 
   Print[filename, ": Unit-norm test failed"]];
  If[extractNumberTP[filename] != numberTPfromSO[Phi],
   Print[filename, ": Number of distinct triple products test failed"]]
  ]

(* .tp files *)
(* Validates a .tp ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF
   and that the number of distinct triple products is equal
   to the number in the file name *)
validateTP[filename_] := Module[{d, n, TPPM, GM},
  {d, n} = extractDimensions[filename];
  TPPM = ToExpression /@ Import[filename, "List"];
  GM = GMfromTP@arrayFromPositionMap[TPPM];
  If[N@Abs[GM] != 
    N[ConstantArray[
       Welch[d, n], {n, n}] + (1 - Welch[d, n]) IdentityMatrix[n]],
   Print[filename, ":  Gram matrix test failed"]];
  If[Length[TPPM] != extractNumberTP[filename],
   Print[filename, ":  Number of distinct triple products test failed"]]
  ]

(* .exa files *)
(* Validates a .exa ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF
   and that the contents match the contents of the
   corresponding .tp file *)
validateEXA[filename_] := Module[{d, n, TP1, TP2, GM},
  {d, n} = extractDimensions[filename];
  TP1 = importExactTP[filename];
  GM = GMfromTP[TP1];
  If[N@Abs[GM] != 
    N[ConstantArray[
       Welch[d, n], {n, n}] + (1 - Welch[d, n]) IdentityMatrix[n]],
   Print[filename, ": Gram matrix test failed"]];
  If[FileExistsQ[FileBaseName[filename] <> ".tp"],
   TP2 = importExactTP[FileBaseName[filename] <> ".tp"];
   If[Chop@N[TP1 - TP2] != ConstantArray[0, {n, n, n}],
    Print[filename, 
     ": Match against corresponding .tp file test failed"]],
   Print[filename, ": Corresponding .tp file does not exist"]
   ];
  ]
