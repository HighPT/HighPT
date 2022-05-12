(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`LeftRun`*)


(* ::Subtitle:: *)
(*Running of the Low Energy theory*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["LEFTRun"]


PackageExport["LEFTLabels"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["$LEFTSectors"]


PackageScope["$LowScales"]


(* ::Chapter:: *)
(*Private:*)


$LowScales = {"\[Mu]b","\[Mu]had"}


(* ::Section:: *)
(*LEFT Basis*)


$LEFTSectors = {
	"cc",
	"\[CapitalDelta]F=1", 
	"\[CapitalDelta]F=1,ddll",
	"\[CapitalDelta]F=1,dd\[Nu]\[Nu]",
	"\[CapitalDelta]F=1,uullL",
	"\[CapitalDelta]F=1,uullR",
	"\[CapitalDelta]F=2",
	"\[CapitalDelta]F=2,d",
	"\[CapitalDelta]F=2,u",
	"ll\[Nu]\[Nu]",
	"ll\[Gamma]",
	"llll"
};


(* ::Subsection:: *)
(*cc*)


LEFTLabels["cc"]={
	"VLudl\[Nu]",
	"VRudl\[Nu]",
	"SLudl\[Nu]",
	"SRudl\[Nu]",
	"Tudl\[Nu]"
};


(* ::Subsection:: *)
(*\[CapitalDelta]F=1*)


LEFTLabels["\[CapitalDelta]F=1,ddll"]={"9","9'","10","10'","S","S'","P","P'"}
LEFTLabels["\[CapitalDelta]F=1,dd\[Nu]\[Nu]"]={"VLLd\[Nu]","VRLd\[Nu]"}
LEFTLabels["\[CapitalDelta]F=1,uullL"]={"VLLul","VLRul","SLLul","SLRul","TLLul"}
LEFTLabels["\[CapitalDelta]F=1,uullR"]={"VRRul","VRLul","SRRul","SRLul","TRRul"}


LEFTLabels["\[CapitalDelta]F=1"]=Join[
	LEFTLabels["\[CapitalDelta]F=1,ddll"],
	LEFTLabels["\[CapitalDelta]F=1,dd\[Nu]\[Nu]"],
	LEFTLabels["\[CapitalDelta]F=1,uullL"],
	LEFTLabels["\[CapitalDelta]F=1,uullR"]
]


(* ::Subsection:: *)
(*\[CapitalDelta]F=2*)


LEFTLabels["\[CapitalDelta]F=2,d"]={"dd1","dd2","dd3","dd4","dd5","dd1'","dd2'","dd3'"}
LEFTLabels["\[CapitalDelta]F=2,u"]={"uu1","uu2","uu3","uu4","uu5","uu1'","uu2'","uu3'"}


LEFTLabels["\[CapitalDelta]F=2"]=Join[LEFTLabels["\[CapitalDelta]F=2,d"],LEFTLabels["\[CapitalDelta]F=2,u"]]


(* ::Subsection:: *)
(*ll\[Nu]\[Nu]*)


LEFTLabels["ll\[Nu]\[Nu]"]={"VLLl\[Nu]","VRLl\[Nu]"}


(* ::Subsection:: *)
(*ll\[Gamma]*)


LEFTLabels["ll\[Gamma]"]={"e\[Gamma]"};


(* ::Subsection:: *)
(*llll*)


LEFTLabels["llll"]={"VLLll","VLRll","SLLll","VRRll","VRLll","SRRll"}


(* ::Section:: *)
(*LEFTRun*)


LEFTRun::norunning="LEFTU[`1`,`2`,`3`] is not implemented. WCs of sector `1` will not be evolved up to mZ."
LEFTRun::nosector="LEFT sector `1` does not exist. Aborting."


LEFTRun[sec_, lowscale_, highscale_] := Module[
	{
		rules,
		evolutionmatrix
	}
	,
	If[MemberQ[$LEFTSectors,sec],
		If[Dimensions@EvolutionMatrix[sec,lowscale,highscale]=={LEFTLabels[sec]//Length,LEFTLabels[sec]//Length},
			evolutionmatrix = EvolutionMatrix[sec,lowscale,highscale];,
			evolutionmatrix = IdentityMatrix[LEFTLabels[sec]//Length];Message[LEFTRun::norunning, sec, lowscale, highscale];
		];
		rules = Table[WCL[LEFTLabels[sec][[a]],flav_]->Sum[evolutionmatrix[[a,b]]*WCL[LEFTLabels[sec][[b]],flav],
{b,LEFTLabels[sec]//Length}],{a,LEFTLabels[sec]//Length}];
		,
		If[lowscale==highscale,
			rules = {},
			Message[LEFTRun::nosector, sec];Abort[]
		];
	];

	Return@rules
]
