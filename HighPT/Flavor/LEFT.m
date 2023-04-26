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
(*Formatting (to do)*)


(* ::Subsection:: *)
(*Index relabeling redundancies (to do)*)


(* ::Subsection:: *)
(*WC argument check*)


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
