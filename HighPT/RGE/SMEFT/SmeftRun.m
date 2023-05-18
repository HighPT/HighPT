(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SmeftRun`*)


(* ::Subtitle:: *)
(*SMEFT running*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["SMEFTRun"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["SMEFTAD"]


(*<<FileNameJoin[{Global`$DirectoryHighPT,"RGE","SMEFT","SMEFTAD"}]*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*SMEFTRun*)


<<"/Users/allwicher/Documents/Uni/PhD/Research/Flavour@LHC/Mathematica/SMEFTAD.m"


SMEFTSimplify=<<"/Users/allwicher/Documents/Uni/PhD/Research/Flavour@LHC/Mathematica/SMEFTSimplify.m";


SMEFTRun::undefinedrunningmode= "The mode `1` is not defined for SMEFT Running.";


SMEFTRun[expr_,lowscale_, highscale_]:=Module[
	{
		params,
		temp
	}
	,
	Switch[
		SMEFTRGEMode,
		"LL",
		Return[expr/.wc_WC->(wc+1/(16\[Pi]^2)Log[lowscale/highscale]SMEFTAD[wc])],
		"DsixTools",
		temp=HighPTToDsixToolsSMEFT[expr]//DsixTools`D6Simplify;
		params=Select[Variables[temp/.Conjugate[a_]->a],MemberQ[DsixTools`SMEFTParameterList[],#] &];
		temp=(temp/.Dispatch[(#1->DsixTools`SMEFTEvolve[#1,lowscale,highscale]&)/@params])//DsixTools`D6Simplify;
		Return[DsixToolsToHighPTSMEFT[temp]],
		_,
		Message[SMEFTRun::undefinedrunningmode,SMEFTRGEMode];Abort[];
	];
];
