(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SMEFT`*)


(* ::Subtitle:: *)
(*SMEFT implementation.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["WC"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["SetEFTorder"]
PackageScope["GetEFTorder"]


PackageScope["SetOperatorDimension"]
PackageScope["GetOperatorDimension"]


PackageScope["SetEFTscale"]
PackageScope["GetEFTscale"]


PackageScope["CanonizeFF"]


PackageScope["SubstitutionRulesSMEFT"]


PackageScope["GetAllWC"]


PackageScope["MassDimension"]
PackageScope["SMEFTTruncate"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*EFT dimension and power counting*)


(* ::Subsection:: *)
(*EFT operator dimensions*)


(* default mass dimension up to which operators are considered *)
$OperatorDimension= 6;


SetOperatorDimension::usage= "SetOperatorDimension[\[ScriptD]] speciefies the maximum mass dimension \[ScriptD] of effective operators that should be considered in all following computations. Allowed values are \[ScriptD]\[Element]{4,6,8}. The default value is \[ScriptD]=6.";


GetOperatorDimension::usage= "GetOperatorDimension[\[ScriptD]] returns the maximum mass dimension \[ScriptD] of effective operators that are considered in all computations by default at the moment. The value of \[ScriptD] can be modified using SetOperatorDimension[\[ScriptD]].";


SetOperatorDimension::dimensioninvalid= "The operator mass dimension specified \[ScriptD]=`1` is not valid. The allowed values are \[ScriptD] \[Epsilon] {4, 6, 8}.";


SetOperatorDimension[d:4|6|8] := ($OperatorDimension = d);


SetOperatorDimension[d:Except[4|6|8]] := Message[SetOperatorDimension::dimensioninvalid, d]


GetOperatorDimension[] := $OperatorDimension


(* ::Subsection:: *)
(*EFT power expansion*)


(* Powers of suppression factors that should be considered in cross sections *)
$EFTorder= 4;


SetEFTorder::usage= "SetEFTorder[\[ScriptN]] sets a global flag specifying that cross sections should be expanded up to and including terms of order (\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\)\!\(\*SuperscriptBox[\()\), \(-\[ScriptN]\)]\). Allowed values are \[ScriptN] \[Element] {0, 2, 4} and the default value is \[ScriptN]=4.";


GetEFTorder::usage= "GetEFTorder[] returns current the global value \[ScriptN] used for the EFT series truncation (\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\)\!\(\*SuperscriptBox[\()\), \(-\[ScriptN]\)]\). The returned value can be modified using SetEFTorder[\[ScriptN]].";


SetEFTorder::invalidEFTorder= "The given argument n=`1` is not a valid EFT order. The allowed values are n \[Element] {0, 2, 4}.";


SetEFTorder[n:(0|2|4|8)] := ($EFTorder = n);


SetEFTorder[n:Except[0|2|4|8]] := (Message[SetEFTorder::invalidEFTorder,n];Abort[])


GetEFTorder[] := $EFTorder


(* ::Subsection::Closed:: *)
(*EFTscale*)


(* EFT scale set to 1 TeV *)
$EFTscale = 1000;


SetEFTscale[\[CapitalLambda]_] := ($EFTscale = \[CapitalLambda]);


GetEFTscale[] := $EFTscale


(* ::Section:: *)
(*Wilson coefficients*)


WC::usage= 
"WC[\"label\",{\[Alpha],\[Beta],\[ScriptI],\[ScriptJ]}] Wilson coefficient associated to the four-fermion operator denoted by label with lepton (quark) flavor indices \[Alpha],\[Beta] (\[ScriptI],\[ScriptJ]).
WC[\"label\",{\[ScriptR],\[ScriptS]}] Wilson coefficient associated to the two-fermion operator denoted by label with flavor indices \[ScriptR],\[ScriptS] which can be either lepton or quark indices.";


(* ::Subsection:: *)
(*Formatting*)


Format[WC[label_,{indices___}],TraditionalForm]:= Module[
	{num=StringTake[ToString[label],-1]},
	If[NumericQ[ToExpression[num]],
		(* w/ exponents *)
		DisplayForm@SubscriptBox[
			RowBox[{"[", 
				Subsuperscript[
					"\[ScriptCapitalC]",
					StringReplace[StringDrop[ToString[label],-1],{\[Psi]_~~"2":>Superscript[\[Psi],"2"],\[Psi]_~~"3":>Superscript[\[Psi],"3"],\[Psi]_~~"4":>Superscript[\[Psi],"4"],\[Psi]_~~"5":>Superscript[\[Psi],"5"],\[Psi]_~~"6":>Superscript[\[Psi],"6"]}]/.StringExpression[a___]:>RowBox[{a}],
					StringJoin["(",num,")"]
				],
			"]"}],
			StringJoin[ToString/@{indices}]]
		,
		(* w/o exponents*)
		DisplayForm@SubscriptBox[
			RowBox[{"[", 
				Subscript[
					"\[ScriptCapitalC]",
					StringReplace[ToString[label],{\[Psi]_~~"2":>Superscript[\[Psi],"2"],\[Psi]_~~"3":>Superscript[\[Psi],"3"],\[Psi]_~~"4":>Superscript[\[Psi],"4"],\[Psi]_~~"5":>Superscript[\[Psi],"5"],\[Psi]_~~"6":>Superscript[\[Psi],"6"]}]/.StringExpression[a___]:>RowBox[{a}]
				], 
			"]"}], 
			StringJoin[ToString/@{indices}]
		]	
	]
]


(* ::Subsection:: *)
(*Flavor indices*)


(* remove unwanted heads *)
WC[x_,{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}] := WC[x,{a,b,i,j}]
WC[x_,{f1_[a_],f2_[b_]}] := WC[x,{a,b}]


(* ::Subsection:: *)
(*WC classes*)


class0WC= Alternatives[
	"H", "HD", "HBox",
	"HG", "HGt", "HW", "HWt", "HB", "HBt", "HWB", "HWtB",
	"G", "Gt", "W", "Wt"
];


class2WC= Alternatives[
	(* Psi^2 H^2 D *)
	"Hl1", "Hl3", "He", "Hq1", "Hq3", "Hu", "Hd",
	(* Psi^2 H^4 D *)
	"l2H4D1","l2H4D2","l2H4D3","l2H4D4",
	"q2H4D1","q2H4D2","q2H4D3","q2H4D4",
	"e2H4D","u2H4D","d2H4D",
	(* Psi^2 H^2 D^3 *)
	"l2H2D31","l2H2D32","l2H2D33","l2H2D34",
	"e2H2D31","e2H2D32",
	"q2H2D31","q2H2D32","q2H2D33","q2H2D34",
	"u2H2D31","u2H2D32",
	"d2H2D31","d2H2D32"
];


class6WC= Alternatives[
	"ll","qq1","qq3","uu","dd"
];


class7WC= Alternatives[
	(* Psi^4 *)
	"le",
	"lq1", "lq3", "eu", "ed", "lu", "ld", "eq",
	"ud1","ud8","qu1","qu8","qd1","qd8",
	(* Psi^4 H^2 *)
	"l2q2H21","l2q2H22","l2q2H23","l2q2H24","l2q2H25",
	"l2u2H21","l2u2H22","l2d2H21","l2d2H22",
	"e2q2H21","e2q2H22",
	"e2u2H2", "e2d2H2",
	(* Psi^4 D^2 *)
	"l2q2D21","l2q2D22","l2q2D23","l2q2D24",
	"l2u2D21","l2u2D22",
	"l2d2D21","l2d2D22",
	"e2q2D21","e2q2D22",
	"e2u2D21","e2u2D22",
	"e2d2D21","e2d2D22"
];


class8WC= Alternatives[
	"ee"
];


(* ::Subsubsection:: *)
(*Index relabeling redundancies*)


(* ::Text:: *)
(*2 fermion operators -class 2*)


WC[lab:class2WC,{p_Integer,r_Integer}]:= WC[lab,{r,p}]\[Conjugate] /; p>r


(* ::Text:: *)
(*4 fermion operators - class 6*)


WC[lab:class6WC,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WC[lab,{i,j,a,b}] /; ((a<b && i<j && (a>i || b>j)) || (a==b && i<j && a>i) || (a>b && i<j && a>=j) || (a<b && i==j && a>=i && b>j) || (a==b && i==j && a>i))


WC[lab:class6WC,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WC[lab,{b,a,j,i}]\[Conjugate] /; ((a>b && i<j && (a<j || b<i)) || (a>b && i==j && a<=i) || (a==b && i>j && a<i) || (a>b && i>j && a<=i))


WC[lab:class6WC,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WC[lab,{j,i,b,a}]\[Conjugate] /; ((a>b && i==j && a>i) || (a<b && i>j && (b>i || a>j)) || (a==b && i>j && a>=i) || (a>b && i>j && a>i))


(* ::Text:: *)
(*4 fermion operators - class 7*)


WC[lab:class7WC,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WC[lab,{b,a,j,i}]\[Conjugate] /; a>b


WC[lab:class7WC,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WC[lab,{b,a,j,i}]\[Conjugate] /; (a==b && i>j)


(* ::Text:: *)
(*4 fermion operators - class 8*)


WC[lab:class8WC,{1,1,2,1}]:=Conjugate[WC[lab,{1,1,1,2}]]
WC[lab:class8WC,{1,1,3,1}]:=Conjugate[WC[lab,{1,1,1,3}]]
WC[lab:class8WC,{1,1,3,2}]:=Conjugate[WC[lab,{1,1,2,3}]]
WC[lab:class8WC,{1,2,1,1}]:=WC[lab,{1,1,1,2}]
WC[lab:class8WC,{1,2,2,1}]:=WC[lab,{1,1,2,2}]
WC[lab:class8WC,{1,2,3,1}]:=Conjugate[WC[lab,{1,1,2,3}]]
WC[lab:class8WC,{1,3,1,1}]:=WC[lab,{1,1,1,3}]
WC[lab:class8WC,{1,3,1,2}]:=WC[lab,{1,2,1,3}]
WC[lab:class8WC,{1,3,2,1}]:=WC[lab,{1,1,2,3}]
WC[lab:class8WC,{1,3,2,2}]:=WC[lab,{1,2,2,3}]
WC[lab:class8WC,{1,3,3,1}]:=WC[lab,{1,1,3,3}]
WC[lab:class8WC,{1,3,3,2}]:=WC[lab,{1,2,3,3}]
WC[lab:class8WC,{2,1,1,1}]:=Conjugate[WC[lab,{1,1,1,2}]]
WC[lab:class8WC,{2,1,1,2}]:=WC[lab,{1,1,2,2}]
WC[lab:class8WC,{2,1,1,3}]:=WC[lab,{1,1,2,3}]
WC[lab:class8WC,{2,1,2,1}]:=Conjugate[WC[lab,{1,2,1,2}]]
WC[lab:class8WC,{2,1,2,2}]:=Conjugate[WC[lab,{1,2,2,2}]]
WC[lab:class8WC,{2,1,2,3}]:=Conjugate[WC[lab,{1,2,3,2}]]
WC[lab:class8WC,{2,1,3,1}]:=Conjugate[WC[lab,{1,2,1,3}]]
WC[lab:class8WC,{2,1,3,2}]:=Conjugate[WC[lab,{1,2,2,3}]]
WC[lab:class8WC,{2,1,3,3}]:=Conjugate[WC[lab,{1,2,3,3}]]
WC[lab:class8WC,{2,2,1,1}]:=WC[lab,{1,1,2,2}]
WC[lab:class8WC,{2,2,1,2}]:=WC[lab,{1,2,2,2}]
WC[lab:class8WC,{2,2,1,3}]:=WC[lab,{1,2,2,3}]
WC[lab:class8WC,{2,2,2,1}]:=Conjugate[WC[lab,{1,2,2,2}]]
WC[lab:class8WC,{2,2,3,1}]:=Conjugate[WC[lab,{1,2,2,3}]]
WC[lab:class8WC,{2,2,3,2}]:=Conjugate[WC[lab,{2,2,2,3}]]
WC[lab:class8WC,{2,3,1,1}]:=WC[lab,{1,1,2,3}]
WC[lab:class8WC,{2,3,1,2}]:=WC[lab,{1,2,2,3}]
WC[lab:class8WC,{2,3,1,3}]:=WC[lab,{1,3,2,3}]
WC[lab:class8WC,{2,3,2,1}]:=Conjugate[WC[lab,{1,2,3,2}]]
WC[lab:class8WC,{2,3,2,2}]:=WC[lab,{2,2,2,3}]
WC[lab:class8WC,{2,3,3,1}]:=Conjugate[WC[lab,{1,2,3,3}]]
WC[lab:class8WC,{2,3,3,2}]:=WC[lab,{2,2,3,3}]
WC[lab:class8WC,{3,1,1,1}]:=Conjugate[WC[lab,{1,1,1,3}]]
WC[lab:class8WC,{3,1,1,2}]:=Conjugate[WC[lab,{1,1,2,3}]]
WC[lab:class8WC,{3,1,1,3}]:=WC[lab,{1,1,3,3}]
WC[lab:class8WC,{3,1,2,1}]:=Conjugate[WC[lab,{1,2,1,3}]]
WC[lab:class8WC,{3,1,2,2}]:=Conjugate[WC[lab,{1,2,2,3}]]
WC[lab:class8WC,{3,1,2,3}]:=Conjugate[WC[lab,{1,2,3,3}]]
WC[lab:class8WC,{3,1,3,1}]:=Conjugate[WC[lab,{1,3,1,3}]]
WC[lab:class8WC,{3,1,3,2}]:=Conjugate[WC[lab,{1,3,2,3}]]
WC[lab:class8WC,{3,1,3,3}]:=Conjugate[WC[lab,{1,3,3,3}]]
WC[lab:class8WC,{3,2,1,1}]:=Conjugate[WC[lab,{1,1,2,3}]]
WC[lab:class8WC,{3,2,1,2}]:=WC[lab,{1,2,3,2}]
WC[lab:class8WC,{3,2,1,3}]:=WC[lab,{1,2,3,3}]
WC[lab:class8WC,{3,2,2,1}]:=Conjugate[WC[lab,{1,2,2,3}]]
WC[lab:class8WC,{3,2,2,2}]:=Conjugate[WC[lab,{2,2,2,3}]]
WC[lab:class8WC,{3,2,2,3}]:=WC[lab,{2,2,3,3}]
WC[lab:class8WC,{3,2,3,1}]:=Conjugate[WC[lab,{1,3,2,3}]]
WC[lab:class8WC,{3,2,3,2}]:=Conjugate[WC[lab,{2,3,2,3}]]
WC[lab:class8WC,{3,2,3,3}]:=Conjugate[WC[lab,{2,3,3,3}]]
WC[lab:class8WC,{3,3,1,1}]:=WC[lab,{1,1,3,3}]
WC[lab:class8WC,{3,3,1,2}]:=WC[lab,{1,2,3,3}]
WC[lab:class8WC,{3,3,1,3}]:=WC[lab,{1,3,3,3}]
WC[lab:class8WC,{3,3,2,1}]:=Conjugate[WC[lab,{1,2,3,3}]]
WC[lab:class8WC,{3,3,2,2}]:=WC[lab,{2,2,3,3}]
WC[lab:class8WC,{3,3,2,3}]:=WC[lab,{2,3,3,3}]
WC[lab:class8WC,{3,3,3,1}]:=Conjugate[WC[lab,{1,3,3,3}]]
WC[lab:class8WC,{3,3,3,2}]:=Conjugate[WC[lab,{2,3,3,3}]]


(* ::Subsubsection:: *)
(*Real coefficients*)


(* ::Text:: *)
(*Remove conjugates*)


WC/:Conjugate[WC[lab:class0WC,{}]]:= WC[lab,{}] 


WC/:Conjugate[WC[lab:class2WC,{p_Integer,p_Integer}]]:= WC[lab,{p,p}]


WC/:Conjugate[WC[lab:class6WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WC[lab,{a,a,i,i}] /; a<=i


WC/:Conjugate[WC[lab:class6WC,{a_Integer,i_Integer,i_Integer,a_Integer}]]:= WC[lab,{a,i,i,a}] /; a<i


WC/:Conjugate[WC[lab:class7WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WC[lab,{a,a,i,i}]


WC/:Conjugate[WC[lab:class8WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WC[lab,{a,a,i,i}]


(* ::Text:: *)
(*Remove real parts*)


WC/:Re[WC[lab:class0WC,{}]]:= WC[lab,{}] 


WC/:Re[WC[lab:class2WC,{p_Integer,p_Integer}]]:= WC[lab,{p,p}]


WC/:Re[WC[lab:class6WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WC[lab,{a,a,i,i}] /; a<=i


WC/:Re[WC[lab:class6WC,{a_Integer,i_Integer,i_Integer,a_Integer}]]:= WC[lab,{a,i,i,a}] /; a<i


WC/:Re[WC[lab:class7WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WC[lab,{a,a,i,i}]


WC/:Re[WC[lab:class8WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WC[lab,{a,a,i,i}]


(* ::Text:: *)
(*Set imaginary parts to zero*)


WC/:Im[WC[lab:class0WC,{}]]:= 0


WC/:Im[WC[lab:class2WC,{p_Integer,p_Integer}]]:= 0


WC/:Im[WC[lab:class6WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= 0 /; a<=i


WC/:Im[WC[lab:class6WC,{a_Integer,i_Integer,i_Integer,a_Integer}]]:= 0 /; a<i


WC/:Im[WC[lab:class7WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= 0


WC/:Im[WC[lab:class8WC,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= 0


(* ::Subsection:: *)
(*WC argument check*)


(* ::Subsubsection:: *)
(*\[Psi]^0*)


$WCList0d6=List[
	"H","HD","HBox",
	"HG","HGt","HW","HWt","HB","HBt","HWB","HWtB",
	"G","Gt","W","Wt"
]


$WCList0d8=List[
	"H61","H62",
	"G2H41","G2H42",
	"W2H41","W2H42","W2H43","W2H44",
	"WBH41","WBH42",
	"B2H41","B2H42"
]


(*$WCList0=List[
	"H","HD","HBox",
	"HG","HGt","HW","HWt","HB","HBt","HWB","HWtB",
	"G","Gt","W","Wt",
	"H61","H62",
	"G2H41","G2H42",
	"W2H41","W2H42","W2H43","W2H44",
	"WBH41","WBH42",
	"B2H41","B2H42"
]*)
$WCList0=Join[$WCList0d6,$WCList0d8]


(* ::Subsubsection:: *)
(*\[Psi]^2*)


$WCList2d6=List[
	(* Psi^2 H^2 D *)
	"Hl1", "Hl3", "He", "Hq1", "Hq3", "Hu", "Hd",
	
	(* non-hermitain *)
	"Hud", "eW", "eB", "uW", "uB", "dW", "dB", "uG", "dG",
	
	(* Psi^2 H^3 *)
	"uH","dH","eH"
]


$WCList2d8=List[
	(* Psi^2 H^4 D *)
	"l2H4D1","l2H4D2","l2H4D3","l2H4D4",
	"q2H4D1","q2H4D2","q2H4D3","q2H4D4",
	"e2H4D","u2H4D","d2H4D",
	(* Psi^2 H^2 D^3 *)
	"l2H2D31","l2H2D32","l2H2D33","l2H2D34",
	"e2H2D31","e2H2D32",
	"q2H2D31","q2H2D32","q2H2D33","q2H2D34",
	"u2H2D31","u2H2D32",
	"d2H2D31","d2H2D32",
	
	(* non-hermitain *)
	"udH4D"
]


(*$WCList2=List[
	(* Psi^2 H^2 D *)
	"Hl1", "Hl3", "He", "Hq1", "Hq3", "Hu", "Hd",
	(* Psi^2 H^4 D *)
	"l2H4D1","l2H4D2","l2H4D3","l2H4D4",
	"q2H4D1","q2H4D2","q2H4D3","q2H4D4",
	"e2H4D","u2H4D","d2H4D",
	(* Psi^2 H^2 D^3 *)
	"l2H2D31","l2H2D32","l2H2D33","l2H2D34",
	"e2H2D31","e2H2D32",
	"q2H2D31","q2H2D32","q2H2D33","q2H2D34",
	"u2H2D31","u2H2D32",
	"d2H2D31","d2H2D32",
	
	(* non-hermitain *)
	"Hud", "eW", "eB", "uW", "uB", "dW", "dB","uG","dG","udH4D",
	
	(* Psi^2 H^3 *)
	"uH","dH","eH"
]*)
$WCList2=Join[$WCList2d6,$WCList2d8]


(*$WCList2=List[
	(* Psi^2 H^2 D *)
	"Hl1", "Hl3", "He", "Hq1", "Hq3", "Hu", "Hd",
	(* Psi^2 H^4 D *)
	"l2H4D1","l2H4D2","l2H4D3","l2H4D4",
	"q2H4D1","q2H4D2","q2H4D3","q2H4D4",
	"e2H4D","u2H4D","d2H4D",
	(* Psi^2 H^2 D^3 *)
	"l2H2D31","l2H2D32","l2H2D33","l2H2D34",
	"e2H2D31","e2H2D32",
	"q2H2D31","q2H2D32","q2H2D33","q2H2D34",
	"u2H2D31","u2H2D32",
	"d2H2D31","d2H2D32",
	
	(* non-hermitain *)
	"Hud", "eW", "eB", "uW", "uB", "dW", "dB"
]*)


(* ::Subsubsection:: *)
(*\[Psi]^4*)


$WCList4d6=List[
	(* Psi^4 *)
	"lq1", "lq3", "eu", "ed", "lu", "ld", "eq",

	(* non-hermitian *)
	"ledq", "lequ1", "lequ3",
	
	(* 4-quarks *)
	"qq1","qq3",
	"uu","dd","ud1","ud8",
	"qu1","qu8","qd1","qd8",
	"quqd1","quqd8",
	
	(* 4-leptons *)
	"ll","ee","le"
];


$WCList4d8=List[
	(* Psi^4 H^2 *)
	"l2q2H21","l2q2H22","l2q2H23","l2q2H24","l2q2H25",
	"l2u2H21","l2u2H22","l2d2H21","l2d2H22",
	"e2q2H21","e2q2H22",
	"e2u2H2", "e2d2H2",
	"l4H21", "l4H22",
	"l2e2H21","l2e2H22",
	(* Psi^4 D^2 *)
	"l2q2D21","l2q2D22","l2q2D23","l2q2D24",
	"l2u2D21","l2u2D22",
	"l2d2D21","l2d2D22",
	"e2q2D21","e2q2D22",
	"e2u2D21","e2u2D22",
	"e2d2D21","e2d2D22"
];


(*$WCList4=List[
	(* Psi^4 *)
	"lq1", "lq3", "eu", "ed", "lu", "ld", "eq",
	(* Psi^4 H^2 *)
	"l2q2H21","l2q2H22","l2q2H23","l2q2H24","l2q2H25",
	"l2u2H21","l2u2H22","l2d2H21","l2d2H22",
	"e2q2H21","e2q2H22",
	"e2u2H2", "e2d2H2",
	"l4H21", "l4H22",
	(* Psi^4 D^2 *)
	"l2q2D21","l2q2D22","l2q2D23","l2q2D24",
	"l2u2D21","l2u2D22",
	"l2d2D21","l2d2D22",
	"e2q2D21","e2q2D22",
	"e2u2D21","e2u2D22",
	"e2d2D21","e2d2D22",
	
	(* non-hermitian *)
	"ledq", "lequ1", "lequ3",
	
	(* 4-quarks *)
	"qq1","qq3",
	"uu","dd","ud1","ud8",
	"qu1","qu8","qd1","qd8",
	"quqd1","quqd8",
	(* 4-leptons *)
	"ll","ee","le"
];*)
$WCList4=Join[$WCList4d6,$WCList4d8]


(*$WCList4=List[
	(* Psi^4 *)
	"lq1", "lq3", "eu", "ed", "lu", "ld", "eq",
	(* Psi^4 H^2 *)
	"l2q2H21","l2q2H22","l2q2H23","l2q2H24","l2q2H25",
	"l2u2H21","l2u2H22","l2d2H21","l2d2H22",
	"e2q2H21","e2q2H22",
	"e2u2H2", "e2d2H2",
	(* Psi^4 D^2 *)
	"l2q2D21","l2q2D22","l2q2D23","l2q2D24",
	"l2u2D21","l2u2D22",
	"l2d2D21","l2d2D22",
	"e2q2D21","e2q2D22",
	"e2u2D21","e2u2D22",
	"e2d2D21","e2d2D22",
	
	(* non-hermitian *)
	"ledq", "lequ1", "lequ3"
];*)


(* ::Subsubsection:: *)
(*Check WC label*)


WC[l:Except[Alternatives@@Join[$WCList0, $WCList2, $WCList4, {_Pattern, _Blank, _Except, _BlankNullSequence, _BlankSequence}]],___]:=(
	Message[WC::unknownWClabel,l];
	Abort[]
)


GetAllWC = Join[$WCList0, $WCList2, $WCList4]


(* ::Section:: *)
(*SMEFT Truncation*)


MassDimension[Alternatives@@Join[$WCList0d6,$WCList2d6,$WCList4d6]] := 6
MassDimension[Alternatives@@Join[$WCList0d8,$WCList2d8,$WCList4d8]] := 8


DimensionCountingSMEFT[expr_]:=expr/.WC[lab_,ind_]:>WC[lab,ind]*Power[eps,MassDimension[lab]-4]/.Conjugate[WC[lab_,ind_]]:>Conjugate[WC[lab,ind]]*Power[eps,MassDimension[lab]-4]


SMEFTTruncate[expr_,lambdapower_Integer]:=(Series[DimensionCountingSMEFT[expr],{eps,0,-lambdapower}]//Normal)/.eps->1


(* ::Section:: *)
(*Matching the form factors to the SMEFT*)


(* returns the replacement rules to match the form factors to the d\[LessEqual]8 SMEFT in the Warsaw / Murphy basis *)
SubstitutionRulesSMEFT[dim_, \[Epsilon]_] := Module[{list,f6,f8,$DelayedRule},
	(* there are some redundant rule below, since we now canonize the FF before substituting them *)
	list= {
		(* SCALAR *)
		(* NC *)
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Tensor *)
		(* NC *)
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Vector UP *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * (WC["lq1", {a,b,i,j}] - WC["lq3", {a,b,i,j}]) * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["l2q2H21", {a,b,i,j}]+WC["l2q2H22", {a,b,i,j}]-WC["l2q2H23", {a,b,i,j}]-WC["l2q2H24", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["q2H2D31", {i,j}]-WC["q2H2D32", {i,j}]-WC["q2H2D33", {i,j}]+WC["q2H2D34", {i,j}])+
		                                gZ[u,Left,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["eu", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["e2u2H2", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["u2H2D31", {i,j}]-WC["u2H2D32", {i,j}])+
		                                gZ[u,Right,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["lu", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["l2u2H21", {a,b,i,j}]+WC["l2u2H22", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["u2H2D31", {i,j}]-WC["u2H2D32", {i,j}])+
		                                gZ[u,Right,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["eq", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["e2q2H21", {a,b,i,j}]+WC["e2q2H22", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["q2H2D31", {i,j}]-WC["q2H2D32", {i,j}]-WC["q2H2D33", {i,j}]+WC["q2H2D34", {i,j}])+
		                                gZ[u,Left,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		
		(* Vector DOWN *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * (WC["lq1", {a,b,i,j}] + WC["lq3", {a,b,i,j}]) * f6 +
		f8 * \[Epsilon]^2 * 1/2 * (WC["l2q2H21", {a,b,i,j}]+WC["l2q2H22", {a,b,i,j}]+WC["l2q2H23", {a,b,i,j}]+WC["l2q2H24", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["q2H2D31", {j,i}]\[Conjugate]-WC["q2H2D32", {j,i}]\[Conjugate]+WC["q2H2D33", {j,i}]\[Conjugate]-WC["q2H2D34", {j,i}]\[Conjugate])+
                                        gZ[d,Left,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
        
        FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["ld", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * (WC["l2d2H21", {a,b,i,j}]+WC["l2d2H22", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["d2H2D31", {j,i}]\[Conjugate]-WC["d2H2D32", {j,i}]\[Conjugate])+
                                        gZ[d,Right,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["eq", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * (WC["e2q2H21", {a,b,i,j}]+WC["e2q2H22", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["q2H2D31", {j,i}]\[Conjugate]-WC["q2H2D32", {j,i}]\[Conjugate]+WC["q2H2D33", {j,i}]\[Conjugate]-WC["q2H2D34", {j,i}]\[Conjugate])+
                                        gZ[d,Left,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["ed", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * WC["e2d2H2", {a,b,i,j}]+
		f8 * \[Epsilon]^2 * 1/2 * Mass["ZBoson"]^2/Param["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["d2H2D31", {j,i}]\[Conjugate]-WC["d2H2D32", {j,i}]\[Conjugate])+
                                        gZ[d,Right,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		(* energy enhanced UP *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["l2q2D21", {a,b,i,j}]+WC["l2q2D22", {a,b,i,j}]-WC["l2q2D23", {a,b,i,j}]-WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D22", {a,b,i,j}]-WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{1,0}}, {Left,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["l2u2D21", {a,b,i,j}]+WC["l2u2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["l2u2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["e2q2D21", {a,b,i,j}]+WC["e2q2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["e2q2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["e2u2D21", {a,b,i,j}]+WC["e2u2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["e2u2D22", {a,b,i,j}],
		
		(* energy enhanced DOWN *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["l2q2D21", {a,b,i,j}]+WC["l2q2D22", {a,b,i,j}]+WC["l2q2D23", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D22", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{1,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["l2d2D21", {a,b,i,j}]+WC["l2d2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["l2d2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> f8 *  \[Epsilon]^2 * (WC["e2q2D21", {a,b,i,j}]+WC["e2q2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["e2q2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["e2d2D21", {a,b,i,j}]+WC["e2d2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["e2d2D22", {a,b,i,j}],
		
		(* CC *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> 
		\[Epsilon] * 2 * WC["lq3", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * (WC["l2q2H23",{a,b,i,j}]+I*WC["l2q2H25",{a,b,i,j}])+
		f8 * \[Epsilon]^2 * (-1) Mass["WBoson"]^2/Param["vev"]^2 * ((WC["l2H2D33",{a,b}]-WC["l2H2D34",{b,a}]\[Conjugate])*KroneckerDelta[i,j]+
		                               (WC["q2H2D33",{j,i}]\[Conjugate]+WC["q2H2D34",{i,j}])*KroneckerDelta[a,b]),
		                               
		FF[Vector, {"regular",{0,0}}, {OrderlessPatternSequence[Right,_]}, {a_,b_,i_u,j_d}]:> 0,
		
		(* energy enhanced CC *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D23", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_u,j_d}]:> f8 * \[Epsilon]^2 * 4 * (WC["l2q2D24", {a,b,i,j}]),
		
		(* Gauge coupling modifications *)
		(* NC *)
		FF[Vector, {"Photon",0}, ___]:> 0,
		
		(* Gauge coupling modifications UP*)
		FF[Vector, {"ZBoson",0}, {Left,Left}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hq1",{i,j}] - WC["Hq3",{i,j}])+gZ[u,Left,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["q2H4D1",{i,j}]-2*WC["q2H4D2",{i,j}])+gZ[u,Left,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]-WC["q2H2D33",{i,j}]+WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[u,Left,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {"ZBoson",0}, {Left,Right}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hu",{i,j}])+gZ[u,Right,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hu",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["u2H4D",{i,j}])+gZ[u,Right,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["u2H2D31",{i,j}]-WC["u2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[u,Right,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {"ZBoson",0}, {Right,Left}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hq1",{i,j}] - WC["Hq3",{i,j}])+gZ[u,Left,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])*(WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*(WC["q2H4D1",{i,j}]-2*WC["q2H4D2",{i,j}])+gZ[u,Left,{i,j}]*(WC["e2H4D",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]-WC["q2H2D33",{i,j}]+WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[u,Left,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		FF[Vector, {"ZBoson",0}, {Right,Right}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hu",{i,j}])+gZ[u,Right,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hu",{i,j}]*WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*WC["u2H4D",{i,j}]+gZ[u,Right,{i,j}]*WC["e2H4D",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["u2H2D31",{i,j}]-WC["u2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[u,Right,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		
		(* Gauge coupling modifications DOWN*)
		FF[Vector, {"ZBoson",0}, {Left,Left}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hq1",{i,j}] + WC["Hq3",{i,j}])+gZ[d,Left,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["q2H4D1",{i,j}]+2*WC["q2H4D2",{i,j}])+gZ[d,Left,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]+WC["q2H2D33",{i,j}]-WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[d,Left,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {"ZBoson",0}, {Left,Right}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hd",{i,j}])+gZ[d,Right,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hd",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["d2H4D",{i,j}])+gZ[d,Right,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["d2H2D31",{i,j}]-WC["d2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[d,Right,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {"ZBoson",0}, {Right,Left}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hq1",{i,j}] + WC["Hq3",{i,j}])+gZ[d,Left,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])*(WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*(WC["q2H4D1",{i,j}]+2*WC["q2H4D2",{i,j}])+gZ[d,Left,{i,j}]*(WC["e2H4D",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]+WC["q2H2D33",{i,j}]-WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[d,Left,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		FF[Vector, {"ZBoson",0}, {Right,Right}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass["ZBoson"]^2/Param["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hd",{i,j}])+gZ[d,Right,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (WC["Hd",{i,j}]*WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^2/Param["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*WC["d2H4D",{i,j}]+gZ[d,Right,{i,j}]*WC["e2H4D",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["d2H2D31",{i,j}]-WC["d2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass["ZBoson"]^4/Param["vev"]^4 * 1/2 * gZ[d,Right,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		
		(* CC *)
		
		(* Gauge coupling modifications CC*)
		FF[Vector, {"WBoson",0}, {Left,Left}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:>
		f6 * \[Epsilon] * Mass["WBoson"]^2/Param["vev"]^2 * 2 *(WC["Hl3",{b,a}]\[Conjugate]*KroneckerDelta[i,j]+WC["Hq3",{i,j}]*KroneckerDelta[a,b])+
		f6 * \[Epsilon]^2 * Mass["WBoson"]^2/Param["vev"]^2 * 2 * WC["Hl3",{b,a}]\[Conjugate] * WC["Hq3",{i,j}]+
		f8 * \[Epsilon]^2 * Mass["WBoson"]^2/Param["vev"]^2 * KroneckerDelta[i,j] * (WC["l2H4D2",{b,a}]\[Conjugate]-WC["l2H4D3",{b,a}]\[Conjugate]+WC["l2H4D4",{a,b}])+
		f8 * \[Epsilon]^2 * Mass["WBoson"]^2/Param["vev"]^2 * KroneckerDelta[a,b] * (WC["q2H4D2",{i,j}]-WC["q2H4D3",{i,j}]+WC["q2H4D4",{j,i}]\[Conjugate])+
		f8 * \[Epsilon]^2 * Mass["WBoson"]^2/Param["vev"]^2 * (-1) * KroneckerDelta[i,j] * (WC["l2H2D33",{a,b}]-WC["l2H2D34",{b,a}]\[Conjugate])+
		f8 * \[Epsilon]^2 * Mass["WBoson"]^2/Param["vev"]^2 * (-1) * KroneckerDelta[a,b] * (WC["q2H2D34",{i,j}]-WC["q2H2D33",{j,i}]\[Conjugate]),
		
		FF[Vector, {"WBoson",0}, {Left,Right}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:>
		f6 * \[Epsilon] Mass["WBoson"]^2/Param["vev"]^2 * KroneckerDelta[a,b] * WC["Hud",{i,j}],
				
		(* Lepton Dipoles *)
		(* NC *)
		FF[DipoleL, {"Photon",0}, {Left,_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gA[q,{i,j}] * \[Epsilon] * (Param["sW"]*WC["eW",{b,a}]\[Conjugate] - Param["cW"]*WC["eB",{b,a}]\[Conjugate]) * f6,
		FF[DipoleL, {"ZBoson",0}, {Left,\[Chi]q_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gZ[q,\[Chi]q,{i,j}] * \[Epsilon] * (Param["cW"]*WC["eW",{b,a}]\[Conjugate] + Param["sW"]*WC["eB",{b,a}]\[Conjugate]) * f6,
		(* CC *)
		FF[DipoleL, {"WBoson",0}, {Right,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/Param["sW"] * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{a,b}] * f6,
		FF[DipoleL, {"WBoson",0}, {Left,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> -Sqrt[2]* Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/Param["sW"] * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{b,a}]\[Conjugate] * f6,
		
		(* Quark Dipoles *)
		(* NC *)
		FF[DipoleQ, {"Photon",0}, {_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (Param["sW"]*WC["dW",{j,i}]\[Conjugate] - Param["cW"]*WC["dB",{j,i}]\[Conjugate]) * f6,
		FF[DipoleQ, {"ZBoson",0}, {\[Chi]l_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (Param["cW"]*WC["dW",{j,i}]\[Conjugate] + Param["sW"]*WC["dB",{j,i}]\[Conjugate]) * f6,
		
		FF[DipoleQ, {"Photon",0}, {_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (Param["sW"]*WC["uW",{j,i}]\[Conjugate] + Param["cW"]*WC["uB",{j,i}]\[Conjugate]) * f6,
		FF[DipoleQ, {"ZBoson",0}, {\[Chi]l_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (Param["cW"]*WC["uW",{j,i}]\[Conjugate] - Param["sW"]*WC["uB",{j,i}]\[Conjugate]) * f6,
		
		(* CC *)
		FF[DipoleQ, {"WBoson",0}, {Left,Right},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> - Sqrt[2]* Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/Param["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["dW",{i,j}] * f6,
		FF[DipoleQ, {"WBoson",0}, {Left,Left},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/Param["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["uW",{j,i}]\[Conjugate] * f6
	};
	
	list = list /. RuleDelayed->$DelayedRule;
	list = list /. {f6->If[dim<=4,0,1], f8->If[dim<=6,0,1]};
	list = list /. $DelayedRule -> RuleDelayed;
	Return[list]
]


(* ::Subsection:: *)
(*Gauge boson couplings*)


(* ::Subsubsection::Closed:: *)
(*Photon*)


(* SM coupling of the photon *)
gA[particle_, {p_,r_}]:= Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] * Charge[particle] * KroneckerDelta[p,r]


(* ::Subsubsection::Closed:: *)
(*Z boson*)


(* SM coupling of the Z boson *)
gZ[particle_, chirality_,{p_,r_}]:= Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"])*(WeakIsospin3[particle, chirality] - Param["sW"]^2*Charge[particle])*KroneckerDelta[p,r]


(* ::Text:: *)
(*NP modifications*)


\[CapitalDelta]gZe[Left, \[Epsilon]_, {a_,b_}]:= - Sqrt[\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"]) * \[Epsilon] * (WC["Hl1",{a,b}] + WC["Hl3",{a,b}])
\[CapitalDelta]gZe[Right, \[Epsilon]_, {a_,b_}]:= - Sqrt[\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"]) * \[Epsilon] * WC["He",{a,b}]


\[CapitalDelta]gZd[Left, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"]) * \[Epsilon] * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])
\[CapitalDelta]gZd[Right, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"]) * \[Epsilon] * WC["Hd",{i,j}]


\[CapitalDelta]gZu[Left, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"]) * \[Epsilon] * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])
\[CapitalDelta]gZu[Right, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"]) * \[Epsilon] * WC["Hu",{i,j}]


(* ::Subsubsection:: *)
(*W boson*)


(* SM coupling of the W boson in weak eigenbasis -> flavor diagonal *)
gW[{p_,r_}]:= Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/(Sqrt[2]*Param["sW"]) * KroneckerDelta[p,r]


(* ::Section::Closed:: *)
(*Canonize FF*)


(* Rule to remove all redundancies in the form factors due to Hermiticity *)
CanonizeFF = Dispatch[
	{
	(* CC *)
	FF[Vector, type_, {X_,Y_}, {a_,b_,i_d,j_u}]                        :> FF[Vector, type, {X,Y}, {b,a,j,i}]\[Conjugate],
	FF[lorentz:(Scalar|Tensor), type_, {Right,Right}, {a_,b_,i_d,j_u}] :> FF[lorentz, type, {Left,Left}, {b,a,j,i}]\[Conjugate],
	FF[lorentz:(Scalar|Tensor), type_, {Left,Left}, {a_,b_,i_d,j_u}]   :> FF[lorentz, type, {Right,Right}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Left,Right}, {a_,b_,i_d,j_u}]                   :> FF[Scalar, type, {Right,Left}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Right,Left}, {a_,b_,i_d,j_u}]                   :> FF[Scalar, type, {Left,Right}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Left,X_}, {a_,b_,i_d,j_u}]                     :> - FF[DipoleL, type, {Right,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Right,X_}, {a_,b_,i_d,j_u}]                    :> - FF[DipoleL, type, {Left,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Left}, {a_,b_,i_d,j_u}]                     :> - FF[DipoleQ, type, {X,Right}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Right}, {a_,b_,i_d,j_u}]                    :> - FF[DipoleQ, type, {X,Left}, {b,a,j,i}]\[Conjugate],
	(* NC *)
	FF[lorentz:(Scalar|Tensor), type_, {Right,Right}, {a_,b_,i_u,j_u}] :> FF[lorentz, type, {Left,Left}, {b,a,j,i}]\[Conjugate],
	FF[lorentz:(Scalar|Tensor), type_, {Right,Right}, {a_,b_,i_d,j_d}] :> FF[lorentz, type, {Left,Left}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Right,Left}, {a_,b_,i_u,j_u}]                    :> FF[Scalar, type, {Left,Right}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Right,Left}, {a_,b_,i_d,j_d}]                    :> FF[Scalar, type, {Left,Right}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Right,X_}, {a_,b_,i_u,j_u}]                     :> - FF[DipoleL, type, {Left,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Right,X_}, {a_,b_,i_d,j_d}]                     :> - FF[DipoleL, type, {Left,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Right}, {a_,b_,i_u,j_u}]                     :> - FF[DipoleQ, type, {X,Left}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Right}, {a_,b_,i_d,j_d}]                     :> - FF[DipoleQ, type, {X,Left}, {b,a,j,i}]\[Conjugate]
	}
];
