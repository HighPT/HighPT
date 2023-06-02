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


PackageExport["RunRGE"]


PackageExport["SetRGEMode"]


PackageExport["GetRGEMode"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["SMEFTRGEMode"]


PackageScope["LEFTRGEMode"]


PackageScope["GetSMEFTRGEMode"]
PackageScope["GetLEFTRGEMode"]


(* ::Chapter:: *)
(*Private:*)


checkDsixTools=Check[Needs["DsixTools`"],"noDsixTools"]
If[
	checkDsixTools==Null,
	SMEFTRGEMode="DsixTools";LEFTRGEMode="DsixTools";DsixTools`SetMatchingLoopOrder[0];Print["RGE running is performed with DsixTools."],
	SMEFTRGEMode="LL";LEFTRGEMode="LL";Print["DsixTools was not found. SMEFT and LEFT Running will be performed at LL."]
];


GetSMEFTRGEMode[]:=SMEFTRGEMode


GetLEFTRGEMode[]:=LEFTRGEMode


GetRGEMode[]:=(Print["RGE settings:"];Print["SMEFT: ", GetSMEFTRGEMode[]];Print["LEFT: ", GetLEFTRGEMode[]];);


SetRGEMode["SMEFT",x_]:=Switch[x,"LL",SMEFTRGEMode="LL","DsixTools",SMEFTRGEMode="DsixTools",_,Message[SMEFTRun::undefinedrunningmode,x];Abort[]];


SetRGEMode["LEFT",x_]:=Switch[x,"LL",LEFTRGEMode="LL","DsixTools",LEFTRGEMode="DsixTools",_,Message[LEFTRun::undefinedrunningmode,x];Abort[]];


RunRGE[expr_,lowscale_,highscale_]:=SMEFTRun[LEFTRun[expr,lowscale,DsixTools`EWSCALE],DsixTools`EWSCALE,highscale]
