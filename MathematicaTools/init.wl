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


Get[FileNameJoin[$toolsDirectory, "toolbox.wl"]];
Get[FileNameJoin[$toolsDirectory, "convertFrameData.wl"]];
Get[FileNameJoin[$toolsDirectory, "exactification.wl"]];
Get[FileNameJoin[$toolsDirectory, "inputOutput.wl"]];
Get[FileNameJoin[$toolsDirectory, "frameInvariants.wl"]];
Get[FileNameJoin[$toolsDirectory, "frameEquiv.wl"]];
Get[FileNameJoin[$toolsDirectory, "minimizationSetup.wl"]];
Get[FileNameJoin[$toolsDirectory, "validatePackings.wl"]];
