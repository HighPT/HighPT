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


PackageScope["LEFTAD"]


PackageScope["LEFTSimplify"]


(* ::Chapter:: *)
(*Private:*)


(*SMEFTInput = {
	DsixTools`\[Lambda]->0.2813,
	DsixTools`m2->8528,
	DsixTools`g->0.6515,
	DsixTools`gp->0.3576,
	DsixTools`gs->1.220
};*)


Get@FileNameJoin[{Global`$DirectoryHighPT,"RGE","LEFT","LEFTAD.dat"}];


LEFTSimplify=Get@FileNameJoin[{Global`$DirectoryHighPT,"RGE","Simplifications","LEFTSimplify.dat"}];


LEFTRun::undefinedrunningmode= "The mode `1` is not defined for LEFT Running.";


LEFTRun::nonnumericlowscale = "In \"DsixTools\" running mode lowscale must be a number"


LEFTRun[expr_,lowscale_,highscale_]:=Module[
	{
		params,
		temp
	}
	,
	Switch[
		LEFTRGEMode,
		"LL",
		Return[expr/.wc_WCL->(wc+1/(16\[Pi]^2)Log[lowscale/highscale]LEFTAD[wc])],
		"DsixTools",
		If[NumericQ[lowscale],
			temp=(HighPTToDsixToolsLEFT[expr])//DsixTools`D6Simplify;
			params=Select[Variables[temp/.Conjugate[a_]->a/.Re->Identity/.Abs->Identity],MemberQ[DsixTools`LEFTParameterList[],#] &];
			temp=temp/.Dispatch[(#1->DsixTools`LEFTEvolve[#1,lowscale]&)/@params];
			(*temp=temp/.DsixTools`MatchAnalytical/.DsixTools`LoopParameter->DsixTools`MatchingLoopOrder/.SMEFTInput;*)
			(*Return[DsixToolsToHighPTSMEFT[temp//DsixTools`D6Simplify]];*)
			Return[DsixToolsToHighPTLEFT[temp]],
			Message[LEFTRun::nonnumericlowscale];Abort[]
		];,
		_,
		Message[LEFTRun::undefinedrunningmode,LEFTRGEMode];Abort[];
	];
];
