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
(* Available options are Tolerance and exaForceTest *)
(* Tolerance is used in tpValidate and exaValidate. Default is MachinePrecision *)
(* exaForceTest is used in exaValidate. Default is False *)
validatePackings[pattern_String, opts : OptionsPattern[]] := 
 Module[{files, basenames, validators, exaArg, validfiles, validator},
  files = FileNameTake /@ FileNames[pattern, $packingsDirectory];
  files = Select[files, StringMatchQ[#, {"etf*.gos", "etf*.tp", "etf*.exa"}] &];
  If[files == {},
   Message[validatePackings::Pattern, pattern];
   Return[]
   ];
  basenames = DeleteDuplicates[FileBaseName /@ files];
  validators = <|"gos" -> gosValidate, "tp" -> tpValidate, "exa" -> exaValidate|>;
  exaArg = StringMatchQ[pattern, "*.e*"];
  Do[
   Print["=============== ", basename, " ==============="];
   validfiles = Select[files, FileBaseName[#] === basename &];
   Do[
     validator = validators@FileExtension[file];
     validator[file, FilterRules[{opts, exaForceTest -> exaArg}, 
      Options[validator]]],
    {file, validfiles}],
   {basename, basenames}
   ]
  ]
ResourceFunction["AddCodeCompletion"]["validatePackings"]["RelativeFileName"];
validatePackings::Pattern = "No valid files found matching \"`1`\".";


(* The functions below are internal functions and not meant to be used by the end
   user. Use validatePackings with an appropriate pattern. *)

(* .gos files *)
(* Validates a .gos ETF given its file name *)
(* Checks that the coherence is equal to the Welch bound, that the frame is
   unit-norm, that the number of distinct triple products is equal to the number
   in the file name *)
gosValidate[filename_, OptionsPattern[]] := Module[{d, n, Phi, pass},
  {d, n} = extractDimensions[filename];
  Phi = importPacking[filename];
  pass = True;
  If[Coherence[Phi] != N@Welch[d, n],
   coherenceMessage[filename];
   pass = False];
  If[normalizeSO[Phi] != Phi, 
   unitNormMessage[filename];
   pass = False];
  If[extractNumberTP[filename] != numberTPfromSO[Phi],
   numberTPMessage[filename];
   pass = False];
  If[pass, passMessage[filename]]
  ]

(* .tp files *)
(* Validates a .tp ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF, that the number of
   distinct triple products is equal to the number in the file name, and that the
   contents match the contents of the corresponding .exa file *)
Options[tpValidate] = {Tolerance -> MachinePrecision};
tpValidate[filename_, OptionsPattern[]] := 
 Module[{tol, d, n, TPPM, TP1, TP2, GM, pass},
  tol = OptionValue[Tolerance];
  {d, n} = extractDimensions[filename];
  TPPM = importPacking[filename, "Position map", Precision -> 2*tol];
  TP1 = arrayFromPositionMap[TPPM];
  GM = GMfromTP[TP1];
  pass = True;
  If[N[Abs[GM], tol] != 
    N[SparseArray[{{i_, i_} -> 1, {_, _} -> Welch[d, n]}, {n, n}], tol],
   coherenceMessage[filename <> " "];
   pass = False];
  If[! HermitianMatrixQ[GM, Tolerance -> tol],
   GMHermitianMessage[filename <> " "];
   pass = False];
  If[N[GM . GM, tol] != N[n/d GM, tol],
   GMProjectionMessage[filename <> " "];
   pass = False];
  If[Length[TPPM] != extractNumberTP[filename],
   numberTPMessage[filename <> " "];
   pass = False];
  If[FileExistsQ[FileBaseName[filename] <> ".exa"],
   TP2 = importPacking[FileBaseName[filename] <> ".exa", "TP slice"];
   If[N[TP1[[1]], tol] != N[TP2, tol],
    exaMatchMessage[filename <> " "];
    pass = False];
   ];
  If[pass, passMessage[filename <> " "]]
  ]

(* .exa files *)
(* Validates a .exa ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF *)
(* If corresponding .tp file exists, then all checks are skipped because tpValidate
   compares with the existing .tp file *)
Options[exaValidate] = {Tolerance -> MachinePrecision, exaForceTest -> False};
exaValidate[filename_, OptionsPattern[]] := Module[{tol, d, n, TP, GM, pass},
  If[FileExistsQ[FileBaseName[filename] <> ".tp" && ! OptionValue[exaForceTest]],
   tpExistsMessage[filename];
   Return[]];
  tol = OptionValue[Tolerance];
  {d, n} = extractDimensions[filename];
  TP = importPacking[filename, Precision -> 2*tol];
  GM = GMfromTP[TP];
  pass = True;
  If[N[Abs[GM], tol] != 
    N[SparseArray[{{i_, i_} -> 1, {_, _} -> Welch[d, n]}, {n, n}], tol],
   coherenceMessage[filename];
   pass = False];
  If[! HermitianMatrixQ[GM, Tolerance -> tol],
   GMHermitianMessage[filename];
   pass = False];
  If[N[GM . GM, tol] != N[n/d GM, tol],
   GMProjectionMessage[filename];
   pass = False];
  If[pass, passMessage[filename]]
  ]

failMessage[message_] := Print[Style[message, Red]]
coherenceMessage[filename_] := failMessage[filename <> ": Coherence is not equal to Welch bound"]
unitNormMessage[filename_] := failMessage[filename <> ": Frame vectors are not of unit norm"]
numberTPMessage[filename_] := failMessage[filename <> ": Number of distinct triple products does not match file name"]
GMHermitianMessage[filename_] := failMessage[filename <> ": Gram matrix is not Hermitian"]
GMProjectionMessage[filename_] := failMessage[filename <> ": Gram matrix does not satisfy G^2 = n/d G"]
exaMatchMessage[filename_] := failMessage[filename <> ": Mismatch against corresponding .exa file"]
tpExistsMessage[filename_] := Print[filename <> ": Skipping tests; will compare with corresponding .tp file"]
passMessage[filename_] := Print[filename <> ": Passed all tests"]
