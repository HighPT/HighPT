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


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*SMEFTRun*)


SMEFTRun::undefinedrunningmode= "The mode `1` is not defined for SMEFT Running.";


SMEFTRun[expr_,lowscale_, highscale_]:=Module[
	{
		params,
		temp
	}
	,
	Switch[
		RunningMode,
		"HighPT",
		Return[expr/.wc_WC->(wc+1/(16\[Pi]^2)Log[lowscale/highscale]SMEFTAnomalousDimension[wc])],
		"DsixTools",
		temp=HighPTToDsixToolsSMEFT[expr]//DsixTools`D6Simplify;
		params=Select[Variables[temp/.Conjugate[a_]->a],MemberQ[DsixTools`SMEFTParameterList[],#] &];
		temp=(temp/.Dispatch[(#1->DsixTools`SMEFTEvolve[#1,lowscale,highscale]&)/@params])//DsixTools`D6Simplify;
		Return[DsixToolsToHighPTSMEFT[temp]],
		_,
		Message[SMEFTRun::undefinedrunningmode,RunningMode];Abort[];
	];
];
