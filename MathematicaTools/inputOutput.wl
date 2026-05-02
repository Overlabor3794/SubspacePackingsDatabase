(* ::Package:: *)

(* Standard shortcut *)
mf=MatrixForm;
(* Directory containing text files of packings *)
packingsDirectory=FileNameJoin[ParentDirectory[NotebookDirectory[]],"Packings"];
(* Set as present working directory *)
SetDirectory[packingsDirectory];
(* Function to import Game of Sloanes formatted (.gos) text file of packing *)
(* Imports at precision=15 unless precision is specified *)
(* Returns transpose of short, fat matrix *)
importPacking[d_,n_,filename_,precision_:15]:=Module[{plist},
plist=SetPrecision[Import[filename,"List"],precision];
SOfromGoS[plist,{d,n}]
]
(* Export .gos file *)
exportPacking[Phi_,filename_]:=Export[filename,GoSfromSO[Phi],"List"]
(* Import .tp or .exa file *)
importExactTPdirect[filename_]:=arrayFromPositionMap[ToExpression/@Import[filename,"List"]]
importExactTP[filename_]:=TPfromTPslice[arrayFromPositionMap[ToExpression/@Import[filename,"List"]]]
(* Export .exa file *)
exportExactTP[Texact_,filename_]:=Export[filename,arrayPositionMap[Texact],"List"]
