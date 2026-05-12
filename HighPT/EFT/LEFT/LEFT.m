(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`LEFT`*)


(* ::Subtitle:: *)
(*LEFT implementation*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["WCL"]


PackageExport["SanDiegoBasis"]


PackageExport["LEFTBasis"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["WCLS"]


PackageScope["GetAllWCL"]


PackageScope["LEFTTruncate"]


PackageScope["NonRedundantToSymmetricLEFT"]
PackageScope["SymmetricToNonRedundantLEFT"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Wilson Coefficients*)


WCL::usage=
"WCL[\"label\",{i,j,k,l}] LEFT Wilson coefficient associated to the four-fermion operator with flavor indices i,j,k,l.
WCL[\"label\",{i,j}] LEFT Wilson coefficient associated to the dimension-five operator with flavor indices i,j."


(* ::Subsection::Closed:: *)
(*Formatting*)


Format[WCL[label_,{indices__}],TraditionalForm]:=Module[
	{fields,type,chirality,superscript},
	If[
		MatchQ[StringTake[label,1],"g"],
		DisplayForm@SubscriptBox[
			RowBox[
				{
					"[",
					Subsuperscript[
						"g",
						StringTake[label,4],
						StringTake[label,{2,3}]
					],
					"]"
				}
			],
			StringJoin[ToString/@{indices}]
		],
		If[
			StringContainsQ[label,"V"|"S"|"T"],
			{fields,type,chirality}=StringSplit[label,{"V1"->"V1","V8"->"V8","V"->"V","S1"->"S1","S8"->"S8","S"->"S","T"->"T"}];superscript=type<>","<>chirality;,
			{fields,type,chirality}={label,"",""};superscript="";
		];
		DisplayForm@SubscriptBox[
			RowBox[
				{
					"[",
					Subsuperscript[
						"L",
						fields,
						superscript
					],
					"]"
				}
			],
			StringJoin[ToString/@{indices}]
		]
	]
]


(* ::Subsection:: *)
(*WCL classes and redundancies*)


class0WCL=Alternatives[
	"G", "Gt", "mW"
];


class2WCL=Alternatives[
	"gZeL", "gZeR",
	"gZ\[Nu]L",
	"gZdL", "gZdR",
	"gZuL", "gZuR"
];


class3WCL=Alternatives[
	"M\[Nu]"
];


class4WCL= Alternatives[
	"\[Nu]\[Gamma]"
];


class6WCL= Alternatives[
	"uuVLL","ddVLL","uuVRR","ddVRR"(*,
	"eeSRR","uuS1RR","uuS8RR","ddS1RR","ddS8RR"*)
];


class6pWCL= Alternatives[
	"eeSRR","uuS1RR","uuS8RR","ddS1RR","ddS8RR"
];


class7WCL= Alternatives[
	"\[Nu]eVLL","\[Nu]uVLL","\[Nu]dVLL","euVLL","edVLL","udV1LL","udV8LL",
	"euVRR","edVRR","udV1RR","udV8RR",
	"\[Nu]eVLR","eeVLR","\[Nu]uVLR","\[Nu]dVLR","euVLR","edVLR","ueVLR","deVLR",
	"uuV1LR","uuV8LR","udV1LR","udV8LR","duV1LR","duV8LR","ddV1LR","ddV8LR"
];


class8WCL= Alternatives[
	"\[Nu]\[Nu]VLL","eeVLL","eeVRR"
];


(* ::Subsubsection:: *)
(*Index relabeling redundancies*)


(* ::Text:: *)
(*2 fermion operators -class 2*)


WCL[lab:class2WCL,{p_Integer,r_Integer}]:= WCL[lab,{r,p}]\[Conjugate] /; p>r


(* ::Text:: *)
(*2 fermion operators -class 3*)


WCL[lab:class3WCL,{p_Integer,r_Integer}]:= WCL[lab,{r,p}] /; p>r


(* ::Text:: *)
(*2 fermion operators - class 4*)


WCL[lab:class4WCL,{a_Integer,b_Integer}]:= -WCL[lab,{b,a}] /; a>b


WCL[lab:class4WCL,{a_Integer,a_Integer}]:= 0


(* ::Text:: *)
(*4 fermion operators - class 6*)


(*WCL[lab:class6WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{i,j,a,b}] /; ((a<b && i<j && (a>i || b>j)) || (a==b && i<j && a>i) || (a>b && i<j && a>=j) || (a<b && i==j && a>=i && b>j) || (a==b && i==j && a>i))*)


(*WCL[lab:class6WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{b,a,j,i}]\[Conjugate] /; ((a>b && i<j && (a<j || b<i)) || (a>b && i==j && a<=i) || (a==b && i>j && a<i) || (a>b && i>j && a<=i))*)


(*WCL[lab:class6WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{j,i,b,a}]\[Conjugate] /; ((a>b && i==j && a>i) || (a<b && i>j && (b>i || a>j)) || (a==b && i>j && a>=i) || (a>b && i>j && a>i))*)


WCL[lab:class6WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= WCL[lab,{k,l,i,j}] /; 10i+j>10k+l


WCL[lab:class6WCL,{i_Integer,i_Integer,k_Integer,l_Integer}]:= WCL[lab,{i,i,l,k}]\[Conjugate] /; k>l


WCL[lab:class6WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= WCL[lab,{j,i,l,k}]\[Conjugate] /; 10l+k<10i+j


WCL[lab:class6WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:=WCL[lab,{j,i,l,k}]\[Conjugate] /; i>j


(* ::Text:: *)
(*4 fermion operators - class 6'*)


WCL[lab:class6pWCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= WCL[lab,{k,l,i,j}] /; 10i+j>10k+l


(* ::Text:: *)
(*4 fermion operators - class 7*)


WCL[lab:class7WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{b,a,j,i}]\[Conjugate] /; a>b


WCL[lab:class7WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{b,a,j,i}]\[Conjugate] /; (a==b && i>j)


(* ::Text:: *)
(*4 fermion operators - class 8*)


WCL[lab:class8WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= WCL[lab,{k,l,i,j}] /; 10i+j>10k+l


WCL[lab:class8WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= WCL[lab,{j,i,l,k}]\[Conjugate] /; i>j


WCL[lab:class8WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= WCL[lab,{i,l,k,j}] /; j>l


WCL[lab:class8WCL,{i_Integer,j_Integer,j_Integer,i_Integer}]:= WCL[lab,{i,i,j,j}] /; i!=j


WCL[lab:class8WCL,{i_Integer,i_Integer,k_Integer,l_Integer}]:= WCL[lab,{i,i,l,k}]\[Conjugate] /; k>l


WCL[lab:class8WCL,{i_Integer,j_Integer,k_Integer,i_Integer}]:= WCL[lab,{i,i,k,j}] /; (i<j && i<k)


(*WCL[lab:class8WCL,{1,1,2,1}]:=Conjugate[WCL[lab,{1,1,1,2}]]
WCL[lab:class8WCL,{1,1,3,1}]:=Conjugate[WCL[lab,{1,1,1,3}]]
WCL[lab:class8WCL,{1,1,3,2}]:=Conjugate[WCL[lab,{1,1,2,3}]]
WCL[lab:class8WCL,{1,2,1,1}]:=WCL[lab,{1,1,1,2}]
WCL[lab:class8WCL,{1,2,2,1}]:=WCL[lab,{1,1,2,2}]
WCL[lab:class8WCL,{1,2,3,1}]:=Conjugate[WCL[lab,{1,1,2,3}]]
WCL[lab:class8WCL,{1,3,1,1}]:=WCL[lab,{1,1,1,3}]
WCL[lab:class8WCL,{1,3,1,2}]:=WCL[lab,{1,2,1,3}]
WCL[lab:class8WCL,{1,3,2,1}]:=WCL[lab,{1,1,2,3}]
WCL[lab:class8WCL,{1,3,2,2}]:=WCL[lab,{1,2,2,3}]
WCL[lab:class8WCL,{1,3,3,1}]:=WCL[lab,{1,1,3,3}]
WCL[lab:class8WCL,{1,3,3,2}]:=WCL[lab,{1,2,3,3}]
WCL[lab:class8WCL,{2,1,1,1}]:=Conjugate[WCL[lab,{1,1,1,2}]]
WCL[lab:class8WCL,{2,1,1,2}]:=WCL[lab,{1,1,2,2}]
WCL[lab:class8WCL,{2,1,1,3}]:=WCL[lab,{1,1,2,3}]
WCL[lab:class8WCL,{2,1,2,1}]:=Conjugate[WCL[lab,{1,2,1,2}]]
WCL[lab:class8WCL,{2,1,2,2}]:=Conjugate[WCL[lab,{1,2,2,2}]]
WCL[lab:class8WCL,{2,1,2,3}]:=Conjugate[WCL[lab,{1,2,3,2}]]
WCL[lab:class8WCL,{2,1,3,1}]:=Conjugate[WCL[lab,{1,2,1,3}]]
WCL[lab:class8WCL,{2,1,3,2}]:=Conjugate[WCL[lab,{1,2,2,3}]]
WCL[lab:class8WCL,{2,1,3,3}]:=Conjugate[WCL[lab,{1,2,3,3}]]
WCL[lab:class8WCL,{2,2,1,1}]:=WCL[lab,{1,1,2,2}]
WCL[lab:class8WCL,{2,2,1,2}]:=WCL[lab,{1,2,2,2}]
WCL[lab:class8WCL,{2,2,1,3}]:=WCL[lab,{1,2,2,3}]
WCL[lab:class8WCL,{2,2,2,1}]:=Conjugate[WCL[lab,{1,2,2,2}]]
WCL[lab:class8WCL,{2,2,3,1}]:=Conjugate[WCL[lab,{1,2,2,3}]]
WCL[lab:class8WCL,{2,2,3,2}]:=Conjugate[WCL[lab,{2,2,2,3}]]
WCL[lab:class8WCL,{2,3,1,1}]:=WCL[lab,{1,1,2,3}]
WCL[lab:class8WCL,{2,3,1,2}]:=WCL[lab,{1,2,2,3}]
WCL[lab:class8WCL,{2,3,1,3}]:=WCL[lab,{1,3,2,3}]
WCL[lab:class8WCL,{2,3,2,1}]:=Conjugate[WCL[lab,{1,2,3,2}]]
WCL[lab:class8WCL,{2,3,2,2}]:=WCL[lab,{2,2,2,3}]
WCL[lab:class8WCL,{2,3,3,1}]:=Conjugate[WCL[lab,{1,2,3,3}]]
WCL[lab:class8WCL,{2,3,3,2}]:=WCL[lab,{2,2,3,3}]
WCL[lab:class8WCL,{3,1,1,1}]:=Conjugate[WCL[lab,{1,1,1,3}]]
WCL[lab:class8WCL,{3,1,1,2}]:=Conjugate[WCL[lab,{1,1,2,3}]]
WCL[lab:class8WCL,{3,1,1,3}]:=WCL[lab,{1,1,3,3}]
WCL[lab:class8WCL,{3,1,2,1}]:=Conjugate[WCL[lab,{1,2,1,3}]]
WCL[lab:class8WCL,{3,1,2,2}]:=Conjugate[WCL[lab,{1,2,2,3}]]
WCL[lab:class8WCL,{3,1,2,3}]:=Conjugate[WCL[lab,{1,2,3,3}]]
WCL[lab:class8WCL,{3,1,3,1}]:=Conjugate[WCL[lab,{1,3,1,3}]]
WCL[lab:class8WCL,{3,1,3,2}]:=Conjugate[WCL[lab,{1,3,2,3}]]
WCL[lab:class8WCL,{3,1,3,3}]:=Conjugate[WCL[lab,{1,3,3,3}]]
WCL[lab:class8WCL,{3,2,1,1}]:=Conjugate[WCL[lab,{1,1,2,3}]]
WCL[lab:class8WCL,{3,2,1,2}]:=WCL[lab,{1,2,3,2}]
WCL[lab:class8WCL,{3,2,1,3}]:=WCL[lab,{1,2,3,3}]
WCL[lab:class8WCL,{3,2,2,1}]:=Conjugate[WCL[lab,{1,2,2,3}]]
WCL[lab:class8WCL,{3,2,2,2}]:=Conjugate[WCL[lab,{2,2,2,3}]]
WCL[lab:class8WCL,{3,2,2,3}]:=WCL[lab,{2,2,3,3}]
WCL[lab:class8WCL,{3,2,3,1}]:=Conjugate[WCL[lab,{1,3,2,3}]]
WCL[lab:class8WCL,{3,2,3,2}]:=Conjugate[WCL[lab,{2,3,2,3}]]
WCL[lab:class8WCL,{3,2,3,3}]:=Conjugate[WCL[lab,{2,3,3,3}]]
WCL[lab:class8WCL,{3,3,1,1}]:=WCL[lab,{1,1,3,3}]
WCL[lab:class8WCL,{3,3,1,2}]:=WCL[lab,{1,2,3,3}]
WCL[lab:class8WCL,{3,3,1,3}]:=WCL[lab,{1,3,3,3}]
WCL[lab:class8WCL,{3,3,2,1}]:=Conjugate[WCL[lab,{1,2,3,3}]]
WCL[lab:class8WCL,{3,3,2,2}]:=WCL[lab,{2,2,3,3}]
WCL[lab:class8WCL,{3,3,2,3}]:=WCL[lab,{2,3,3,3}]
WCL[lab:class8WCL,{3,3,3,1}]:=Conjugate[WCL[lab,{1,3,3,3}]]
WCL[lab:class8WCL,{3,3,3,2}]:=Conjugate[WCL[lab,{2,3,3,3}]]*)


(* ::Text:: *)
(*4 fermion operators - class 9*)


WCL[lab:class9WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= WCL[lab,{j,i,l,k}]\[Conjugate]/; i>j


(* ::Text:: *)
(*4 fermion operators - class 10*)


WCL[lab:class10WCL,{i_Integer,j_Integer,k_Integer,l_Integer}]:= -WCL[lab,{j,i,l,k}]\[Conjugate] /; i>j


WCL[lab:class10WCL,{i_Integer,i_Integer,k_Integer,l_Integer}]:= 0


(* ::Subsubsection:: *)
(*Real coefficients*)


(* ::Text:: *)
(*Remove conjugates*)


(* 0 *)
WCL/:Conjugate[WCL[lab:class0WCL,{}]]:= WCL[lab,{}] 


(* 2 *)
WCL/:Conjugate[WCL[lab:class2WCL,{p_Integer,p_Integer}]]:= WCL[lab,{p,p}]


(* 6 *)
WCL/:Conjugate[WCL[lab:class6WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}] /; a<=i
WCL/:Conjugate[WCL[lab:class6WCL,{a_Integer,i_Integer,i_Integer,a_Integer}]]:= WCL[lab,{a,i,i,a}] /; a<i


(* 7 *)
WCL/:Conjugate[WCL[lab:class7WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}]


(* 8 *)
WCL/:Conjugate[WCL[lab:class8WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}]


(* ::Text:: *)
(*Remove real parts*)


(* 0 *)
WCL/:Re[WCL[lab:class0WCL,{}]]:= WCL[lab,{}] 


(* 2 *)
WCL/:Re[WCL[lab:class2WCL,{p_Integer,p_Integer}]]:= WCL[lab,{p,p}]


(* 6 *)
WCL/:Re[WCL[lab:class6WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}] /; a<=i
WCL/:Re[WCL[lab:class6WCL,{a_Integer,i_Integer,i_Integer,a_Integer}]]:= WCL[lab,{a,i,i,a}] /; a<i


(* 7 *)
WCL/:Re[WCL[lab:class7WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}]


(* 8 *)
WCL/:Re[WCL[lab:class8WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}]


(* ::Text:: *)
(*Set imaginary parts to zero*)


(* 0 *)
WCL/:Im[WCL[lab:class0WCL,{}]]:= 0 


(* 2 *)
WCL/:Im[WCL[lab:class2WCL,{p_Integer,p_Integer}]]:= 0


(* 6 *)
WCL/:Im[WCL[lab:class6WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= 0 /; a<=i
WCL/:Im[WCL[lab:class6WCL,{a_Integer,i_Integer,i_Integer,a_Integer}]]:= 0 /; a<i


(* 7 *)
WCL/:Im[WCL[lab:class7WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= 0


(* 8 *)
WCL/:Im[WCL[lab:class8WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= 0


(* ::Subsubsection:: *)
(*Set coefficients with top quark to zero*)


(* ::Text:: *)
(*uu - 2 \[Psi]*)


zeroWCLclass1= Alternatives[
	"u\[Gamma]","uG"
];


WCL[lab:zeroWCLclass1,{OrderlessPatternSequence[3,a_Integer]}] := 0


(* ::Text:: *)
(*\[Psi]\[Psi]\[Psi]u*)


zeroWCLclass2= Alternatives[
	"\[Nu]eduVLL","\[Nu]eduVLR","\[Nu]eduSRR","\[Nu]eduTRR","\[Nu]eduSRL"
];


WCL[lab:zeroWCLclass2,{a_Integer,b_Integer,i_Integer,3}] := 0


(* ::Text:: *)
(*\[Psi]\[Psi]uu*)


zeroWCLclass3= Alternatives[
	"\[Nu]uVLL","euVLL","euVRR","\[Nu]uVLR","euVLR","duV1LR","duV8LR","euSRR","euTRR","euSRL"
];


WCL[lab:zeroWCLclass3,{a_Integer,b_Integer,OrderlessPatternSequence[3,i_Integer]}] := 0


(* ::Text:: *)
(*u\[Psi]\[Psi]u*)


zeroWCLclass4= Alternatives[
	"udduV1LR","udduV8LR","udduS1RR","udduS8RR"
];


WCL[lab:zeroWCLclass4,{3,b_Integer,i_Integer,j_Integer}] := 0
WCL[lab:zeroWCLclass4,{a_Integer,b_Integer,i_Integer,3}] := 0


(* ::Text:: *)
(*uu\[Psi]\[Psi]*)


zeroWCLclass5= Alternatives[
	"udV1LL","udV8LL","udV1RR","udV8RR","ueVLR","udV1LR","udV8LR","udS1RR","udS8RR"
];


WCL[lab:zeroWCLclass5,{OrderlessPatternSequence[3,a_Integer],i_Integer,j_Integer}] := 0


(* ::Text:: *)
(*uuuu*)


zeroWCLclass6= Alternatives[
	"uuVLL","uuVRR","uuV1LR","uuV8LR","uuS1RR","uuS8RR"
];


WCL[lab:zeroWCLclass6,{OrderlessPatternSequence[3,b_Integer,i_Integer,j_Integer]}] := 0


(* ::Subsection:: *)
(*WCL argument check*)


WCL::unknownWCLlabel= "The label `1` is not an allowed label for LEFT Wilson coefficients (WCL)."


(* ::Subsubsection:: *)
(*d = 2*)


$WCLList2=List[
	"mW"
]


(* ::Subsubsection:: *)
(*d = 3*)


$WCLList3=List[
	"M\[Nu]","Me",
	"Mu","Md"
]


(* ::Subsubsection:: *)
(*d = 4*)


$WCLList4=List[
	"gZeL","gZeR",
	"gZ\[Nu]L",
	"gZdL","gZdR",
	"gZuL","gZuR",
	"gWqL","gWqR",
	"gWlL"
]


(* ::Subsubsection:: *)
(*d = 5*)


$WCLList5=List[
	(* (\[Nu]\[Nu])X *)
	"\[Nu]\[Gamma]",
	(* (LR)X *)
	"e\[Gamma]","eZ",
	"u\[Gamma]","d\[Gamma]","uZ","dZ",
	"uG","dG"
]


(* ::Subsubsection:: *)
(*d=6 X^3*)


$WCLList6X3=List[
	"G","Gt"
]


(* ::Subsubsection:: *)
(*d=6 (\[Psi]^4) *)


$WCLList6psi4=List[
	(* (LL)(LL) *)
	"\[Nu]\[Nu]VLL","eeVLL","\[Nu]eVLL",
	"\[Nu]uVLL","\[Nu]dVLL","euVLL","edVLL","\[Nu]eduVLL",
	"uuVLL","ddVLL","udV1LL","udV8LL",
	(* (RR)(RR) *)
	"eeVRR",
	"euVRR","edVRR",
	"uuVRR","ddVRR","udV1RR","udV8RR",
	(* (LL)(RR) *)
	"\[Nu]eVLR","eeVLR",
	"\[Nu]uVLR","\[Nu]dVLR","euVLR","edVLR","ueVLR","deVLR","\[Nu]eduVLR",
	"uuV1LR","uuV8LR",
	"udV1LR","udV8LR","duV1LR","duV8LR",
	"ddV1LR","ddV8LR",
	"udduV1LR","udduV8LR",
	(* (LR)(LR) *)
	"eeSRR",
	"euSRR","euTRR",
	"edSRR","edTRR",
	"\[Nu]eduSRR","\[Nu]eduTRR",
	"uuS1RR","uuS8RR",
	"udS1RR","udS8RR",
	"ddS1RR","ddS8RR",
	"udduS1RR","udduS8RR",
	(* (LR)(RL) *)
	"euSRL","edSRL",
	"\[Nu]eduSRL"
]


(* ::Subsubsection:: *)
(*d=6 (\[Psi]^2D^2V) *)


$WCLList6psi2 = List[

]


(* ::Subsubsection:: *)
(*d=7 (operators with gluons, for LFV) *)


(*$WCLList7=List[
	"eeGG","eeGGt"
]*)


(* ::Subsubsection:: *)
(*d=8 (\[Psi]^4D^2) *)


$WCLList8psi4D2 = List[

]


(* ::Subsubsection:: *)
(*Check WC label*)


WCL[l:Except[Alternatives@@Join[$WCLList2,$WCLList3, $WCLList4, $WCLList5, $WCLList6X3, $WCLList6psi4(*,$WCLList7*), {_Pattern, _Blank, _Except, _BlankNullSequence, _BlankSequence}]],___]:=(
	Message[WCL::unknownWCLlabel,l];
	Abort[]
)


GetAllWCL = Join[$WCLList2,$WCLList3, $WCLList4, $WCLList5, $WCLList6X3, $WCLList6psi4]


(* ::Subsection:: *)
(*San Diego Basis *)


NindLEFT[lab_] := If[
	MemberQ[$WCLList6psi4,lab],
	4,
	If[
		MemberQ[Join[$WCLList3,$WCLList5,$WCLList4],lab],
		2,
		If[
			MemberQ[Join[$WCLList6X3,$WCLList2],lab],
			0,
			Abort[]
		]
	]
]


(*SanDiegoBasis[] = Join[
	Table[WCL[lab,{i,j,k,l}],{lab,$WCLList6psi4},{i,3},{j,3},{k,3},{l,3}]/.Conjugate[x_]->x//Flatten//DeleteDuplicates,
	Table[WCL[lab,{i,j}],{lab,Join[$WCLList3,$WCLList5]},{i,3},{j,3}]/.Conjugate[x_]->x//Flatten//DeleteDuplicates,
	Table[WCL[lab,{}],{lab,$WCLList6X3}]/.Conjugate[x_]->x//DeleteDuplicates
]*)


SanDiegoBasis::WrongLabel = "The label `1` is not a San Diego label"


SanDiegoBasis[lab_] := Module[
	{tab},
	If[!MemberQ[Join[$WCLList3,Complement[$WCLList5,{"eZ","uZ","dZ"}],$WCLList6X3,$WCLList6psi4],lab],Message[SanDiegoBasis::WrongLabel,lab];Abort[]];
	Switch[NindLEFT[lab],
		0,
		tab = WCL[lab,{}],
		2,
		tab = Table[WCL[lab,{i,j}],{i,3},{j,3}],
		4,
		tab = Table[WCL[lab,{i,j,k,l}],{i,3},{j,3},{k,3},{l,3}],
		_,
		Abort[]
	];
	Return[Cases[tab,_WCL,All]//DeleteDuplicates]
]


SanDiegoBasis[] = Table[SanDiegoBasis[lab],{lab,Join[$WCLList6X3,$WCLList3,Complement[$WCLList5,{"eZ","uZ","dZ"}],$WCLList6psi4]}]//Flatten


(* ::Subsection:: *)
(*Full LEFT basis*)


LEFTBasis::WrongLabel = "The label `1` is not a LEFT label"


LEFTBasis[lab_] := Module[
	{tab},
	If[MemberQ[Join[$WCLList3,Complement[$WCLList5,{"eZ","uZ","dZ"}],$WCLList6X3,$WCLList6psi4],lab],Return[SanDiegoBasis[lab]]];
	If[!MemberQ[Join[$WCLList2,$WCLList4,{"eZ","uZ","dZ"}],lab],Message[LEFTBasis::WrongLabel,lab];Abort[]];
	Switch[NindLEFT[lab],
		0,
		tab = WCL[lab,{}],
		2,
		tab = Table[WCL[lab,{i,j}],{i,3},{j,3}],
		4,
		tab = Table[WCL[lab,{i,j,k,l}],{i,3},{j,3},{k,3},{l,3}],
		_,
		Abort[]
	];
	Return[Cases[tab,_WCL,All]//DeleteDuplicates]
]


(* ::Subsection:: *)
(*Map of redundant structures*)


indexlist4=Flatten[Table[{i,j,k,l},{i,3},{j,3},{k,3},{l,3}],3];
indexlist2=Flatten[Table[{i,j},{i,3},{j,3}],1];
indexlist0={{}};


RedundancyAssociation[lab_]:=Module[
	{
		tab,tabnozeros,
		tabconj,assconj,
		tabwcl,asswcl,
		tabminus,assminus,
		indexlist4,indexlist2,indexlist0
	}
	,
	indexlist4=Flatten[Table[{i,j,k,l},{i,3},{j,3},{k,3},{l,3}],3];
	indexlist2=Flatten[Table[{i,j},{i,3},{j,3}],1];
	indexlist0={{}};
	Switch[NindLEFT[lab],
		0,
			(*Return[
				Association[
					"redundant" -> <|WCL[lab,{}]->{{}}|>,
					"conjugate" -> <||>
				]
			]*)tab={{{},WCL[lab,{}]}},
		2,
			tab = Table[{i,WCL[lab,i]},{i,indexlist2}],
		4,
			tab = Table[{i,WCL[lab,i]},{i,indexlist4}],
		_,
			Abort[]
	];
	tabnozeros=DeleteCases[tab,{_,0}];
	(* Build Association with all indices related by redundancy *)
	tabwcl=Cases[tabnozeros,{_,_WCL}];
	asswcl=Association@Table[i->Cases[tabwcl,{_,i}][[;;,1]],{i,tabwcl[[;;,2]]//DeleteDuplicates}];
	(* Build Association with all indices related by conjugation *)
	tabconj=Cases[tabnozeros,{_,_Conjugate}]/.Conjugate[a_]:>a;
	assconj=Association@Table[i->Cases[tabconj,{_,i}][[;;,1]],{i,tabconj[[;;,2]]}];
	(* Build Association with all indices related by a minus sign *)
	tabminus=Cases[tabnozeros,{_,_Times}]/.Times[ii_Integer,a_WCL]:>a;
	assminus=Association@Table[i->Cases[tabminus,{_,i}][[;;,1]],{i,tabminus[[;;,2]]}];
	Return[
		Association[
			"redundant" -> asswcl,
			"conjugate" -> assconj,
			"minus"     -> assminus
		]
	]
]


NonRedundantToSymmetricAssociation = Association[
	Table[
		Table[
			i -> Sum[WCLS[lab,j],{j,RedundancyAssociation[lab]["redundant"][i]}]+Sum[Conjugate[WCLS[lab,j]],{j,RedundancyAssociation[lab]["conjugate"][i]/._Missing->0}]-Sum[WCLS[lab,j],{j,RedundancyAssociation[lab]["minus"][i]/._Missing->0}]
			,
			{i,LEFTBasis[lab]}
		]
		,
		{lab,Join[$WCLList6X3,$WCLList3,$WCLList5,$WCLList6psi4,$WCLList4,$WCLList2]}
	]//Flatten
]


NonRedundantToSymmetricLEFT[expr_] := expr/.NonRedundantToSymmetricAssociation


SymmetricToNonRedundantAssociation = Association[
	Table[
		Table[
			If[Length[Join[RedundancyAssociation[lab]["redundant"][i],RedundancyAssociation[lab]["conjugate"][i]/._Missing->{},RedundancyAssociation[lab]["minus"][i]/._Missing->{}]] == 1,
				Nothing[],
				i->1/Length[Join[RedundancyAssociation[lab]["redundant"][i],RedundancyAssociation[lab]["conjugate"][i]/._Missing->{},RedundancyAssociation[lab]["minus"][i]/._Missing->{}]] i
			]
			,
		{i,LEFTBasis[lab]}
		]
		,
		{lab,Join[$WCLList6X3,$WCLList3,$WCLList5,$WCLList6psi4]}
	]//Flatten
]


SymmetricToNonRedundantLEFT[expr_] := expr/.SymmetricToNonRedundantAssociation


(* ::Section:: *)
(*LEFT Truncation*)


MassDimension[Alternatives@@$WCLList4] := 4
MassDimension[Alternatives@@$WCLList5] := 5
MassDimension[Alternatives@@Join[$WCLList6X3,$WCLList6psi4]] := 6


DimensionCountingLEFT[expr_]:=expr/.WCL[lab_,ind_]:>WCL[lab,ind]*Power[eps,MassDimension[lab]-4]/.Conjugate[WC[lab_,ind_]]:>Conjugate[WC[lab,ind]]*Power[eps,MassDimension[lab]-4]


LEFTTruncate[expr_,lambdapower_Integer]:=(Series[DimensionCountingLEFT[expr],{eps,0,-lambdapower}]//Normal)/.eps->1
