(* ::Package:: *)

(* Standard shortcuts *)
mf = MatrixForm;
mp = MatrixPlot;

(* Directory containing Mathematica packages *)
$toolsDirectory = NotebookDirectory[];

(* Directory containing text files of packings *)
$packingsDirectory = FileNameJoin[ParentDirectory[NotebookDirectory[]], "Packings"];

(* Set as present working directory *)
SetDirectory[$packingsDirectory];

(* Global flag to enable or disable file name checks when using exportPacking *)
$exportPackingChecks = True;
