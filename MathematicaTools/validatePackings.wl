(* ::Package:: *)

(* Tests validity of existing packings *)
(* Currently valid for ETFs only *)


(* Validates all ETF files with file name matching pattern and ending with
   .gos, .tp, or .exa *)
(* validatePackings["*"]     tests  all files *)
(* validatePackings["*.gos"] tests .gos files *)
(* validatePackings["*.tp"]  tests .tp  files *)
(* validatePackings["*.exa"] tests .exa files *)
(* Available options are WorkingPrecision and "exaForceTest" *)
(* WorkingPrecision is used in tpValidate and exaValidate. Default is
   MachinePrecision *)
(* "exaForceTest" is used in exaValidate. Default is False unless a corresponding
   .tp file does not exist *)
SetAttributes[validatePackings, Listable];
validatePackings[pattern_String, opts : OptionsPattern[]] := 
 Module[{files, basenames, validators, exaArg, validfiles, validator},
  files = FileNames[pattern];
  files = Select[files, StringMatchQ[#, {"*etf*.gos", "*etf*.tp", "*etf*.exa"}] &];
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
     validator[file, FilterRules[{opts, "exaForceTest" -> exaArg}, 
      Options[validator]]],
    {file, validfiles}],
   {basename, basenames}
   ]
  ];
ResourceFunction["AddCodeCompletion"]["validatePackings"][
  "RelativeFileName", {"exaForceTest"}];
validatePackings::Pattern = "No valid files found matching \"`1`\".";


(* Internal functions to validate different file types *)

(* .gos files *)
(* Validates a .gos ETF given its file name *)
(* Checks that the coherence is equal to the Welch bound, that the frame is
   unit-norm, and that the number of distinct triple products is equal to the
   number in the file name *)
gosValidate[filename_, OptionsPattern[]] := Module[{Phi, d, n, pass},
  Phi = importPacking[filename];
  If[arrayType[Phi] =!= "SO",
   contentsMessage[filename];
   Return[]];
  {d, n} = extractDimensions[filename];
  pass = True;
  If[Coherence[Phi] != N@Welch[d, n],
   coherenceMessage[filename];
   pass = False];
  If[normalizeSO[Phi] != Phi, 
   unitNormMessage[filename];
   pass = False];
  If[extractNumberTP[filename] != numberTP[Phi],
   numberTPMessage[filename];
   pass = False];
  If[pass, passMessage[filename]]
  ]

(* .tp files *)
(* Validates a .tp ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF, that the number of
   distinct triple products is equal to the number in the file name, and that
   the contents match the contents of the corresponding .exa file *)
Options[tpValidate] = {WorkingPrecision -> MachinePrecision};
tpValidate[filename_, OptionsPattern[]] := 
 Module[{wprec, LUT, d, n, TP1, TP2, GM, extfile, pass},
  wprec = OptionValue[WorkingPrecision];
  LUT = importPacking[filename, "Lookup table", PrecisionGoal -> wprec + 10];
  If[arrayType[LUT] =!= "TP LUT",
   contentsMessage[filename];
   Return[]];
  {d, n} = extractDimensions[filename];
  TP1 = arrayfromLUT[LUT];
  GM = GMfromTP[TP1];
  pass = True;
  If[N[Abs[GM], wprec] != N[SparseArray[{i_, i_} -> 1, {n, n}, Welch[d, n]], wprec],
   coherenceMessage[filename <> " "];
   pass = False];
  If[! HermitianMatrixQ[GM, Tolerance -> 10^(-wprec)],
   GMHermitianMessage[filename <> " "];
   pass = False];
  If[N[GM . GM, wprec] != N[n/d GM, wprec],
   GMProjectionMessage[filename <> " "];
   pass = False];
  If[Length[LUT[[1]]] != extractNumberTP[filename],
   numberTPMessage[filename <> " "];
   pass = False];
  TP2 = importPacking[replaceExt[filename, "exa"], "TP slice", 
    PrecisionGoal -> wprec] // Quiet;
  If[TP2 =!= $Failed && N[TP1[[1]], wprec] != TP2,
   matchMessage[filename <> " ", ".exa"];
   pass = False];
  If[pass, passMessage[filename <> " "]]
  ]

(* .exa files *)
(* Validates a .exa ETF given its file name *)
(* Checks that the Gram matrix is the Gram matrix of an ETF, that the number of
   distinct triple products is equal to the number in the file name, and that
   the contents match the contents of the corresponding .tp file *)
(* If corresponding .tp file exists, then all checks are skipped (unless the
   option "exaForceTest" is set to True) and tpValidate compares with the
   existing .tp file *)
Options[exaValidate] = {WorkingPrecision -> MachinePrecision, 
   "exaForceTest" -> False};
exaValidate[filename_, OptionsPattern[]] :=
 Module[{wprec, TP1, TP2, d, n, GM, pass},
  If[FileExistsQ[replaceExt[filename, "tp"] && ! OptionValue["exaForceTest"]],
   tpExistsMessage[filename];
   Return[]];
  wprec = OptionValue[WorkingPrecision];
  TP1 = importPacking[filename, "Lookup table", PrecisionGoal -> wprec + 10];
  If[arrayType[TP1] =!= "TPS LUT",
   contentsMessage[filename];
   Return[]];
  TP1 = TPfromTPslice@arrayfromLUT[TP1];
  {d, n} = extractDimensions[filename];
  GM = GMfromTP[TP1];
  pass = True;
  If[N[Abs[GM], wprec] != N[SparseArray[{i_, i_} -> 1, {n, n}, Welch[d, n]], wprec],
   coherenceMessage[filename];
   pass = False];
  If[! HermitianMatrixQ[GM, Tolerance -> 10^(-wprec)],
   GMHermitianMessage[filename];
   pass = False];
  If[N[GM . GM, wprec] != N[n/d GM, wprec],
   GMProjectionMessage[filename];
   pass = False];
  TP1 = arraytoLUT[TP1];
  If[Length[TP1[[1]]] != extractNumberTP[filename],
   numberTPMessage[filename <> " "];
   pass = False];
  TP1[[1]] = N[TP1[[1]], wprec];
  TP2 = importPacking[replaceExt[filename, "tp"], "Lookup table", 
    PrecisionGoal -> wprec] // Quiet;
  If[TP2 =!= $Failed && TP1 != TP2,
   matchMessage[filename, ".tp"];
   pass = False];
  If[pass, passMessage[filename]]
  ]

failMessage[message_] := Print[Style[message, Red]]
contentsMessage[filename_] := failMessage[filename <> ": File contents do not match file extension"]
coherenceMessage[filename_] := failMessage[filename <> ": Coherence is not equal to Welch bound"]
unitNormMessage[filename_] := failMessage[filename <> ": Frame vectors are not of unit norm"]
numberTPMessage[filename_] := failMessage[filename <> ": Number of distinct triple products does not match file name"]
GMHermitianMessage[filename_] := failMessage[filename <> ": Gram matrix is not Hermitian"]
GMProjectionMessage[filename_] := failMessage[filename <> ": Gram matrix does not satisfy G^2 = n/d G"]
matchMessage[filename_, ext_] := failMessage[filename <> ": Mismatch against corresponding " <> ext <> " file"]
tpExistsMessage[filename_] := Print[filename <> ": Skipping tests; will compare with corresponding .tp file"]
passMessage[filename_] := Print[filename <> ": Passed all tests"]

replaceExt[filename_, ext_] := Module[{split},
  split = FileNameSplit[filename];
  split[[-1]] = FileBaseName[split[[-1]]] <> "." <> ext;
  FileNameJoin[split]
  ]
