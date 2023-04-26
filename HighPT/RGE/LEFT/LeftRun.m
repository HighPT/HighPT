(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`LeftRun`*)


(* ::Subtitle:: *)
(*Running in the Low Energy Effective Theory*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["LEFTRun"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


SMEFTInput = {
	DsixTools`\[Lambda]->0.2813,
	DsixTools`m2->8528,
	DsixTools`g->0.6515,
	DsixTools`gp->0.3576,
	DsixTools`gs->1.220
};


LEFTRun::undefinedrunningmode= "The mode `1` is not defined for LEFT Running.";


LEFTRun[expr_,lowscale_,highscale_]:=Module[
	{
		params,
		temp
	}
	,
	Switch[
		RunningMode,
		"HighPT",
		Message[SMEFTRun::undefinedrunningmode,RunningMode];Abort[];,
		"DsixTools",
		temp=(HighPTToDsixToolsLEFT[expr])//DsixTools`D6Simplify;
		params=Select[Variables[temp/.Conjugate[a_]->a],MemberQ[DsixTools`LEFTParameterList[],#] &];
		temp=temp/.Dispatch[(#1->DsixTools`LEFTEvolve[#1,lowscale]&)/@params];
		temp=temp/.DsixTools`MatchAnalytical/.DsixTools`LoopParameter->DsixTools`MatchingLoopOrder/.SMEFTInput;
		Return[DsixToolsToHighPTSMEFT[temp//DsixTools`D6Simplify]];,
		_,
		Message[SMEFTRun::undefinedrunningmode,RunningMode];Abort[];
	];
];
