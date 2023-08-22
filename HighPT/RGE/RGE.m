(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`RGE`*)


(* ::Subtitle:: *)
(*Generic things about RGEs in the EFT*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(*PackageExport["RunRGE"]*)


PackageExport["SetRGEMode"]


PackageExport["GetRGEMode"]


PackageExport["LEFT"]


(* ::Subsection:: *)
(*Internal*)


(*PackageScope["SMEFTRGEMode"]*)


(*PackageScope["LEFTRGEMode"]*)


PackageScope["GetSMEFTRGEMode"]
PackageScope["GetLEFTRGEMode"]


(* ::Chapter:: *)
(*Private:*)


checkDsixTools=Check[Needs["DsixTools`"],"noDsixTools"]
If[
	checkDsixTools==Null,
	SMEFTRGEMode$default="DsixTools";LEFTRGEMode$default="DsixTools";$AllowedRGEModes={"DsixTools","LL","Off"};DsixTools`SetMatchingLoopOrder[0];Print["RGE running is performed with DsixTools."],
	SMEFTRGEMode="LL";LEFTRGEMode="LL";$AllowedRGEModes={"LL","Off"};Print["DsixTools was not found. SMEFT and LEFT Running will be performed at LL."]
];


$RGEOptionsAss= <|
	"LEFT"  -> Alternatives@@$AllowedRGEModes,
	"SMEFT" -> Alternatives@@$AllowedRGEModes
|>;


RGEOptionCheck::optionvalue= "Invalid RGEOptionValue specified: `1`\[Rule]`2`, the allowed values for `1` must match `3`.";


RGEOptionCheck[opt_, optVal_]:= If[!MatchQ[optVal, $RGEOptionsAss[opt]],
	Message[RGEOptionCheck::optionvalue, opt, optVal, $RGEOptionsAss[opt]];
	Abort[]
];


SMEFTRGEMode$current = SMEFTRGEMode$default;
LEFTRGEMode$current = LEFTRGEMode$default;


GetSMEFTRGEMode[] := SMEFTRGEMode$current


GetLEFTRGEMode[] := LEFTRGEMode$current


GetRGEMode[]:=(Print["RGE settings:"];Print["SMEFT: ", GetSMEFTRGEMode[]];Print["LEFT: ", GetLEFTRGEMode[]];);


Options[SetRGEMode]={
	"SMEFT":>SMEFTRGEMode$current,
	"LEFT":>LEFTRGEMode$current
};


SetRGEMode[Default] := SetRGEMode["SMEFT" -> SMEFTRGEMode$default, "LEFT" -> LEFTRGEMode$default]


SetRGEMode[OptionsPattern[]] := Module[{smeft=OptionValue["SMEFT"],left=OptionValue["LEFT"]},
	RGEOptionCheck[#,OptionValue[#]]&/@{"SMEFT","LEFT"};
	SMEFTRGEMode$current = If[MatchQ[smeft,Default], smeft = SMEFTRGEMode$default, smeft];
	LEFTRGEMode$current = If[MatchQ[left,Default], left = LEFTRGEMode$default, left];
]


(*SetRGEMode["SMEFT",x_]:=Switch[x,"LL",SMEFTRGEMode="LL","DsixTools",SMEFTRGEMode="DsixTools","Off",SMEFTRGEMode="Off",_,Message[SMEFTRun::undefinedrunningmode,x];Abort[]];*)


(*SetRGEMode["LEFT",x_]:=Switch[x,"LL",LEFTRGEMode="LL","DsixTools",LEFTRGEMode="DsixTools","Off",LEFTRGEMode="Off",_,Message[LEFTRun::undefinedrunningmode,x];Abort[]];*)


(*RunRGE[expr_,lowscale_,highscale_]:=SMEFTRun[LEFTRun[expr,lowscale,DsixTools`EWSCALE],DsixTools`EWSCALE,highscale]*)
