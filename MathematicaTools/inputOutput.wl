(* ::Package:: *)

(* Standard shortcut *)
mf=MatrixForm;
(* Directory containing text files of packings *)
packingsDirectory=FileNameJoin[ParentDirectory[NotebookDirectory[]],"Packings"];
(* Set as present working directory *)
SetDirectory[packingsDirectory];
(* Extract packing dimensions (d,n) from file name *)
extractDimensions[str_String]:=ToExpression[StringCases[str,RegularExpression["(\\d+)x(\\d+)"]->{"$1","$2"}][[-1]]]
(* Function to import Game of Sloanes formatted (.gos) text file of packing *)
(* Imports at MachinePrecision unless precision is specified *)
(* Returns transpose of short, fat matrix *)
importPacking[d_,n_,filename_String,precision_:MachinePrecision]:=Module[{plist},
plist=SetPrecision[Import[filename,"List"],precision];
SOfromGoS[plist,{d,n}]
]
importPacking[filename_String,precision_:MachinePrecision]:=Module[{d,n},
{d,n}=extractDimensions[filename];
importPacking[d,n,filename,precision]
]
(* Export .gos file *)
exportPacking[Phi_,filename_String]:=Export[filename,GoSfromSO[Phi],"List"]
exportPacking[Phi_,labels_List]:=Module[{d,n,numberTP,filename},
{d,n}=ToString/@Dimensions[Phi];
numberTP=ToString@numberTPfromSO@N[phi];
filename=labels[[1]]<>"_"<>d<>"x"<>n<>"_"<>numberTP<>labels[[2]]<>".gos";
exportPacking[Phi,filename]
]/;MatchQ[labels,{_String,_String}]
(* Import .tp or .exa file *)
importExactTPdirect[filename_]:=arrayFromPositionMap[ToExpression/@Import[filename,"List"]]
importExactTP[filename_]:=TPfromTPslice[arrayFromPositionMap[ToExpression/@Import[filename,"List"]]]
(* Export .exa file *)
exportExactTP[Texact_,filename_]:=Export[filename,arrayPositionMap[Texact],"List"]
