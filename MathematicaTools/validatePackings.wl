(* ::Package:: *)

(* Tests validity of existing packings *)
(* Currently valid for ETFs only *)


packingsDir = 
  FileNameJoin[{ParentDirectory@NotebookDirectory[], "Packings"}];


(* .gos files *)
files = Most[FileNameTake /@ FileNames["*.gos", packingsDir]];
Do[
 Print["=============== ", file, " ==============="];
 {d, n} = extractDimensions[file];
 SO = importPacking[file];
 If[MatrixRank@SO != d, Print["Rank test fails"]];
 If[RootApproximant@Coherence@N[SO] != Welch[d, n], 
  Print["Coherence test fails"]];
 ,
 {file, files}
 ]


(* .tp files *)
files = FileNameTake /@ FileNames["*.tp", packingsDir];
Do[
 Print["=============== ", file, " ==============="];
 {d, n} = extractDimensions[file];
 GM = GMfromTP@importExactTP[file];
 If[N@Abs@GM != 
   N[ConstantArray[
      Welch[d, n], {n, n}] + (1 - Welch[d, n]) IdentityMatrix[n]], 
  Print["Gram matrix test fails"]]
 ,
 {file, files}
 ]


(* .exa files *)
files = FileNameTake /@ FileNames["*.exa", packingsDir];
Do[
 Print["=============== ", file, " ==============="];
 {d, n} = extractDimensions[file];
 GM = GMfromTP@importExactTP[file];
 If[N@Abs@GM != 
   N[ConstantArray[
      Welch[d, n], {n, n}] + (1 - Welch[d, n]) IdentityMatrix[n]], 
  Print["Gram matrix test fails"]]
 ,
 {file, files}
 ]
