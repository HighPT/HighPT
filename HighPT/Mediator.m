(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`SMEFT`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["Coupling"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Wilson coefficients*)


(*WC::usage= 
"WC[\"label\",{\[Alpha],\[Beta],i,j}]
	Wilson coefficient associated to the four-fermion operator denoted by label with lepton (quark) flavor indices \[Alpha],\[Beta] (i,j).
	Possible values for \"label\" are:
	\"lq1\", \"lq3\", \"eu\", \"ed\", \"lu\", \"ld\", \"eq\" (NOTICE: in the Warsaw basis this is the operator \"qe\"), \"ledq\", \"lequ1\", \"lequ3\".

WC[\"label\",{r,s}]
	Wilson coefficient associated to the two-fermion operator denoted by label with flavor indices r,s which can be either lepton or quark indices.
	Possible values for \"label\" are:
	\"eW\", \"eB\", \"uW\", \"uB\", \"dW\", \"dB\", \"Hl1\", \"Hl3\", \"Hq1\", \"Hq3\", \"He\", \"Hu\", \"Hd\", \"Hud\".";*)


(* ::Subsection:: *)
(*Formatting*)


(*Format[WC[label_,{indices__}],TraditionalForm]:= Module[
	{num=StringTake[ToString[label],-1]},
	If[NumericQ[ToExpression[num]],
		Subsuperscript["\[ScriptCapitalC]",Underscript[StringDrop[ToString[label],-1],StringJoin[ToString/@{indices}]],StringJoin["(",num,")"]],
		Subscript["\[ScriptCapitalC]",Underscript[ToString[label],StringJoin[ToString/@{indices}]]]
	]
]*)


(* ::Subsection:: *)
(*Flavor indices*)


(*(* remove unwanted heads *)
WC[x_,{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:= WC[x,{a,b,i,j}]
WC[x_,{f1_[a_],f2_[b_]}]:= WC[x,{a,b}]*)


(* ::Section:: *)
(*Couplings*)


Coupling["x1L",{a,b}]


(* ::Section:: *)
(*Mediators*)


$MediatorCouplings= <|
	"U1" -> {Coupling["x1L",{a,b}], Coupling["x2L",{a,b}]},
	"R2" -> {},
	"S1" -> {}
|>


$MediatorLagrangians= <|
	"U1" -> L
|>
