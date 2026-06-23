(* ::Package:: *)

(* Functions by Gene Kopp for weak and probabalistic equivalence of frames *)

(* Tests "moment equivalence" of Phi1_ and Phi2_ up to threshold, that is, whether
   the first few moments are equal *)
(* At threshold_ = n^3, moment equivalence is the same as equivalence of the set
   of triple products as multisets *)
(* This is strictly weaker than projective permutation unitary equivalence *)
Options[dirtyFrameEquiv] = {Threshold -> 10, Tolerance -> 10^(-6)};
dirtyFrameEquiv[Phi1_, Phi2_, OptionsPattern[]] :=
 Module[{thres, tol, moments1, moments2},
  thres = OptionValue[Threshold];
  tol = OptionValue[Tolerance];
  moments1 = Table[moment[Phi1, m], {m, 1, thres}];
  moments2 = Table[moment[Phi2, m], {m, 1, thres}];
  Norm[moments1 - moments2] < tol
  ]
ResourceFunction["AddCodeCompletion"]["dirtyFrameEquiv"][
  None, None, RepeatOptions[dirtyFrameEquiv]];

(* Tests equivalence of tests_ random basic S_n invariants of degree degree_ on
   an index set of size breadth_ *)
(* For sufficiently many tests in sufficiently large degree, this is equivalent
   to projective permutation unitary equivalence by Hilbert's basis theorem *)
(* However, rigorous bounds are not known here *)
Options[randomInvariantFrameEquiv] = {"Breadth" -> 3, Degree -> 12,
   "Tests" -> 10, Tolerance -> 10^(-6), Parallelize -> True};
randomInvariantFrameEquiv[Phi1_, Phi2_, OptionsPattern[]] :=
 Module[{brdth, deg, tests, tol, table},
  brdth = OptionValue["Breadth"];
  deg = OptionValue[Degree];
  tests = OptionValue["Tests"];
  tol = OptionValue[Tolerance];
  If[OptionValue[Parallelize], table = ParallelTable, table = Table];
  table = table[
   Abs[generalSnInvariantfromSO[Phi1, #] - generalSnInvariantfromSO[Phi2, #]] &
     [RandomInteger[{1, brdth}, deg]] < tol, {j, 1, tests}
   ];
  And @@ table
  ]
ResourceFunction["AddCodeCompletion"]["randomInvariantFrameEquiv"][
  None, None, RepeatOptions[randomInvariantFrameEquiv]];


(* Functions by David Agbolade for fully testing equivalence of ETFs *)
(* These currently fail tests! Do not use yet. *)

PermuteRows[etf_?MatrixQ,sigma_]:=Module[{n=Length[etf],permList},
(*Ensure sigma is always converted to a numerical permutation list.This handles both Cycles objects (like Cycles[{}]) and existing lists consistently.*)
permList=PermutationList[sigma,n];
etf[[InversePermutation[permList]]]
]

HermitianInnerProduct[v1_?VectorQ,v2_?VectorQ]:=Conjugate[v1] . v2;

ComputeHermitianTripleProducts[etf_?MatrixQ, precision_:40]:=Module[{n=Length[etf]},
If[n==0,Return[{}]];
If[Length[etf[[1]]]==0,Return[{N[0,40]}]];
Table[N[HermitianInnerProduct[etf[[i]],etf[[j]]]*HermitianInnerProduct[etf[[j]],etf[[k]]]*
HermitianInnerProduct[etf[[k]],etf[[i]]],precision],{i,n},{j,n},{k,n}]
]

(*Get you the optimal k and l value for the Small step-big step algorithm*)
FindOptimalKL[n_Integer]:=Module[{sqrtNFact,bestK=0,bestL=0,minDiff=Infinity,kMax,localHSize,currentDiff,k,l},
sqrtNFact=Sqrt[N[Factorial[n]]];
bestK=0;
bestL=0;
minDiff=Infinity;
If[n<=1,Return[{0,0}]];
(*kMax limits computation,as k! grows very fast.*)
kMax=Min[n,{n-2}];
(*Adjusted kMax for improved stability*)
While[kMax>0&&Factorial[kMax]>2*sqrtNFact&&kMax>1,kMax--];
If[kMax==0,kMax=1];
For[k=0,k<=kMax,k++,
For[l=0,l<=n-k,l++,
If[k==0&&l==0,Continue[]];
localHSize=If[l==0||l==1,N[Factorial[k]],N[l*Factorial[k]]];
If[localHSize==0,Continue[]];
currentDiff=Abs[localHSize-sqrtNFact];
If[currentDiff<minDiff,minDiff=currentDiff;
bestK=k;
bestL=l;
]
]
];
{bestK,bestL}
]

GenerateSubgroupH[n_,k_,l_]:=Block[{},
If[k+l>n||(k+l)==0,Return[{}]];
Reap[Which[
(*Case 1:k=0,only an l-cycle*)
k==0,If[l>=2,Sow[Cycles[{Range[1,l]}]]],
(*Case 2:k=1,only l-cycle on {2,...,l+1}*)
k==1,If[l>=2,Sow[Cycles[{Range[2,l+1]}]]],
(*Case 3:k>=2*)
k>=2,(Sow[Cycles[{{1,2}}]];
(*transposition*)
Sow[Cycles[{Range[1,k]}]];
(*k-cycle*)
If[l>=2,Sow[Cycles[{Range[k+1,k+l]}]]])
]][[2,1]]
]

GenerateTransversalH[n_,k_,l_]:=Block[{A,B,C,permsB,permsC,harvested,interleaveCompiled},
If[k+l>n||(k+l)==0,Return[{}]];
(*Force numeric lists early*)
A=Range[1,k];
B=Range[k+1,k+l];
C=Range[k+l+1,n];
(*Step 2:permutations of B that start with k+1*)
permsB=If[Length[B]<=1,{B},Select[Permutations[B],First[#]==First[B]&]];
(*Step 3:permutations of C (may be empty)*)
permsC=If[Length[C]==0,{{}},Permutations[C]];
(*Compiled helper now takes pos explicitly*)
interleaveCompiled=
Compile[{{a,_Integer,1},{b,_Integer,1},{c,_Integer,1},{pos,_Integer,1}},
Block[{lenA=Length[a],lenB=Length[b],lenC=Length[c],total,slots,ia=1,ib=1,ic=1},
total=lenA+lenB+lenC;
slots=ConstantArray[0,total];
Do[Which[
pos[[i]]==0,slots[[i]]=a[[ia]];ia++,
pos[[i]]==1,slots[[i]]=b[[ib]];ib++,
pos[[i]]==2,slots[[i]]=c[[ic]];ic++
],{i,1,total}];
slots
],
CompilationTarget->"WVM"
];
(*Enumerate combinations and use Sow/Reap outside Compile*)
harvested=Reap[Do[Do[Module[{lenA=Length[A],lenB=Length[b],lenC=Length[c],total,tuples},
total=lenA+lenB+lenC;
tuples=Select[Tuples[{0,1,2},total],Count[#,0]==lenA&&Count[#,1]==lenB&&Count[#,2]==lenC&];
Scan[Sow[interleaveCompiled[A,b,c,#]]&,tuples];
],{c,permsC}],{b,permsB}]
][[2,1]];
(*Return permutations as Cycles[] form*)
Map[PermutationCycles,harvested]
]

(*large prime modulus*)
(*Encode complex numbers with tolerance 10^-20*)
EncodeComplex[z_,tol_:10^(-20)]:=Module[{scale=1/tol},
Round[Re[z]*scale]*37+Round[Im[z]*scale]
]

(*Order-invariant hash:sum of encodings modulo prime*)
HashfunctionH[Tri_,tol_:10^(-20)]:=Module[{prime=104729,enc=EncodeComplex[#,tol]&/@Flatten[Tri]},
Mod[Total[enc],prime]
]

Matchlist[list1_,list2_,verbose_:False]:=Module[{tagged1,tagged2,sorted1,sorted2,i=1,j=1,matches={}},
(*Tag each element with its original position*)
tagged1=MapIndexed[{#1,First[#2]}&,list1];
tagged2=MapIndexed[{#1,First[#2]}&,list2];
(*Sort both lists by value*)
sorted1=SortBy[tagged1,First];
sorted2=SortBy[tagged2,First];
(*Two-pointer scan to find collisions*)
While[i<=Length[sorted1]&&j<=Length[sorted2],
With[{val1=First[sorted1[[i]]],pos1=Last[sorted1[[i]]],val2=First[sorted2[[j]]],pos2=Last[sorted2[[j]]]},
Which[val1==val2,AppendTo[matches,{val1,{pos1,pos2}}];i++;j++,
val1<val2,i++,True,j++
]
]
];
(*Return True/False and optional verbose output*)
If[matches==={},
False,
If[verbose,Column[{True,"Collisions found:",TableForm[matches,TableHeadings->{None,{"value","{pos in list1, pos in list2}"}}]}],True]]
]

OptimalKLCache[n_]:=OptimalKLCache[n]=FindOptimalKL[n];
hGeneratorsCyclesCache[n_,k_,l_]:=hGeneratorsCyclesCache[n,k,l]=GenerateSubgroupH[n,k,l];

GenerateTransversalHCache[n_,k_,l_]:=GenerateTransversalHCache[n,k,l]=GenerateTransversalH[n,k,l];

(*Function to Compare unitary permutation equivalence of etfs*)
ComPareETFs[etf1_?MatrixQ,etf2_?MatrixQ,tol_:10^(-20),verbose_:False]:=Module[{n,d,k,l,hGeneratorsCycles,hElements,
transversalElements,etf1TransformedHashes,etf2TransformedHashes,allhashlist1,allhashlist2,Result,vPrint},
vPrint[msg_]:=If[verbose,Print[msg]];
n=Length[etf1];
d=If[n>0,Length[etf1[[1]]],0];
If[Length[etf2]!=n||(n>0&&Length[etf2[[1]]]!=d),
vPrint["Error: Input ETFs must be n x d matrices with the same dimensions."];
Return[{False,{}}]
];
If[n==0,
vPrint["Warning: Empty ETFs. Considering them equivalent."];
Return[{True,{}}]
];
If[d==0,vPrint["Warning: ETFs with zero-length rows. Triple products will be trivial."]];
vPrint["Starting ETF equivalence comparison for n = "<>ToString[n]<>" and d = "<>ToString[d]<>"."];
{k,l}=OptimalKLCache[n];
vPrint["Determined optimal k = "<>ToString[k]<>", l = "<>ToString[l]];
vPrint["Computing subgroup H..."];
hGeneratorsCycles=hGeneratorsCyclesCache[n,k,l];
hElements=GroupElements[PermutationGroup[hGeneratorsCycles]];
vPrint["|H| = "<>ToString[Length[hElements]]];
vPrint["Computing S_"<>ToString[n]<>" and transversal..."];
transversalElements=GenerateTransversalHCache[n,k,l];
vPrint["|T| = "<>ToString[Length[transversalElements]]];
vPrint["Computing ETF1 hashes (via H)..."];
etf1TransformedHashes=ParallelTable[ComputeHermitianTripleProducts[PermuteRows[etf1,hElem]],{hElem,hElements}];
vPrint["ETF1 hash count: "<>ToString[Length[etf1TransformedHashes]]];
vPrint["Computing ETF2 hashes (via T)..."];
etf2TransformedHashes=ParallelTable[ComputeHermitianTripleProducts[PermuteRows[etf2,tElem]],{tElem,transversalElements}];
vPrint["ETF2 hash count: "<>ToString[Length[etf2TransformedHashes]]];
vPrint["Flattening hashes..."];
allhashlist1=HashfunctionH[#,tol]&/@etf1TransformedHashes;
allhashlist2=HashfunctionH[#,tol]&/@etf2TransformedHashes;
vPrint["Comparing all possible pairs of hashes..."];
Result=Matchlist[allhashlist1,allhashlist2];
If[Result,vPrint["The two ETFs are equivalent."];True,vPrint["No equivalence found."];False]];

(*Version to match notation elsewhere in this package*)
fullFrameEquiv[Phi1_?MatrixQ,Phi2_?MatrixQ,tol_:10^(-6),verbose_:False]:=ComPareETFs[Phi1\[ConjugateTranspose],Phi2\[ConjugateTranspose],tol,verbose];
