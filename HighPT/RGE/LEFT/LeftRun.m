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


(*PackageScope["LEFTSimplify"]*)


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


(*LEFTSimplify=Get@FileNameJoin[{Global`$DirectoryHighPT,"RGE","Simplifications","LEFTSimplify.dat"}];*)


LEFTRun::undefinedrunningmode= "The mode `1` is not defined for LEFT Running.";


LEFTRun::nonnumericlowscale = "In \"DsixTools\" running mode lowscale must be a number"


LEFTRun::nocoefficients="No LEFT coefficients found"


LEFTRun[expr_,lowscale_,highscale_]:=Module[
	{
		params,
		evolution,
		mode
	}
	,
	If[lowscale>=DsixTools`EWSCALE,Return@expr];
	mode=GetLEFTRGEMode[];
	Switch[
		mode,
		"LL",
		Return[expr/.wc_WCL->(wc+1/(16\[Pi]^2)Log[lowscale/highscale]LEFTAD[wc])],
		"DsixTools",
		If[NumericQ[lowscale],
			(*temp=(HighPTToDsixToolsLEFT[expr])//DsixTools`D6Simplify;*)
			params=DeleteDuplicates@Cases[expr, _WCL, \[Infinity]];
			(* Deal with the case of a single WCL being evolved *)
			If[MatchQ[params,{}] && MatchQ[Head@expr,WCL],params={expr}];
			If[MatchQ[params,{}],Message[LEFTRun::nocoefficients]];
			evolution=Dispatch[(#1->DsixToolsToHighPTLEFT[DsixTools`LEFTEvolve[HighPTToDsixToolsLEFT[#1],lowscale]]&)/@params];
			(*params=Select[Variables[temp/.Conjugate[a_]->a/.Re->Identity/.Abs->Identity],MemberQ[DsixTools`LEFTParameterList[],#] &];
			temp=temp/.Dispatch[(#1->DsixTools`LEFTEvolve[#1,lowscale]&)/@params];*)
			(*Return[DsixToolsToHighPTLEFT[temp]]*)
			Return[expr/.evolution],
			Message[LEFTRun::nonnumericlowscale];Abort[]
		];,
		"Off",
		Return[expr],
		_,
		Message[LEFTRun::undefinedrunningmode,LEFTRGEMode];Abort[];
	];
];



