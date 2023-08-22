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


(* ::Subsection:: *)
(*Internal*)


PackageScope["GetAllWCL"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Wilson Coefficients*)


WCL::usage=
"WCL[\"label\",{i,j,k,l}] LEFT Wilson coefficient associated to the four-fermion operator with flavor indices i,j,k,l.
WCL[\"label\",{i,j}] LEFT Wilson coefficient associated to the dimension-five operator with flavor indices i,j."


(* ::Subsection:: *)
(*Formatting*)


Format[WCL[label_,{indices__}],TraditionalForm]:=Module[
	{fields,type,chirality,superscript},
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


(* ::Subsection:: *)
(*WCL classes and redundancies*)


class4WCL= Alternatives[
	"\[Nu]\[Gamma]"
];


class6WCL= Alternatives[
	"uuVLL","ddVLL","uuVRR","ddVRR",
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
(*Index relabeling redundancies (to do)*)


(* ::Text:: *)
(*2 fermion operators - class 4*)


WCL[lab:class4WCL,{a_Integer,b_Integer}]:= -WCL[lab,{b,a}] /; a>b


WCL[lab:class4WCL,{a_Integer,a_Integer}]:= 0


(* ::Text:: *)
(*4 fermion operators - class 6*)


WCL[lab:class6WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{i,j,a,b}] /; ((a<b && i<j && (a>i || b>j)) || (a==b && i<j && a>i) || (a>b && i<j && a>=j) || (a<b && i==j && a>=i && b>j) || (a==b && i==j && a>i))


WCL[lab:class6WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{b,a,j,i}]\[Conjugate] /; ((a>b && i<j && (a<j || b<i)) || (a>b && i==j && a<=i) || (a==b && i>j && a<i) || (a>b && i>j && a<=i))


WCL[lab:class6WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{j,i,b,a}]\[Conjugate] /; ((a>b && i==j && a>i) || (a<b && i>j && (b>i || a>j)) || (a==b && i>j && a>=i) || (a>b && i>j && a>i))


(* ::Text:: *)
(*4 fermion operators - class 7*)


WCL[lab:class7WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{b,a,j,i}]\[Conjugate] /; a>b


WCL[lab:class7WCL,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WCL[lab,{b,a,j,i}]\[Conjugate] /; (a==b && i>j)


(* ::Text:: *)
(*4 fermion operators - class 8*)


WCL[lab:class8WCL,{1,1,2,1}]:=Conjugate[WCL[lab,{1,1,1,2}]]
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
WCL[lab:class8WCL,{3,3,3,2}]:=Conjugate[WCL[lab,{2,3,3,3}]]


(* ::Subsubsection:: *)
(*Real coefficients*)


WCL/:Conjugate[WCL[lab:class2WCL,{p_Integer,p_Integer}]]:= WCL[lab,{p,p}]


WCL/:Conjugate[WCL[lab:class6WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}] /; a<=i


WCL/:Conjugate[WCL[lab:class6WCL,{a_Integer,i_Integer,i_Integer,a_Integer}]]:= WCL[lab,{a,i,i,a}] /; a<i


WCL/:Conjugate[WCL[lab:class7WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}]


WCL/:Conjugate[WCL[lab:class8WCL,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WCL[lab,{a,a,i,i}]


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
(*d = 3*)


$WCLList3=List[
	(*"\[Nu]"*)
]


(* ::Subsubsection:: *)
(*d = 5*)


$WCLList5=List[
	(* (\[Nu]\[Nu])X *)
	"\[Nu]\[Gamma]",
	(* (LR)X *)
	"e\[Gamma]",
	"u\[Gamma]","d\[Gamma]",
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
(*Check WC label*)


WCL[l:Except[Alternatives@@Join[$WCLList3, $WCLList5, $WCLList6X3, $WCLList6psi4, {_Pattern, _Blank, _Except, _BlankNullSequence, _BlankSequence}]],___]:=(
	Message[WCL::unknownWCLlabel,l];
	Abort[]
)


GetAllWCL = Join[$WCLList3, $WCLList5, $WCLList6X3, $WCLList6psi4]
