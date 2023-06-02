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


SMEFTRun::nonnumericscale= "In \"DsixTools\" running mode both lowscale and highscale must be a number"


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
		If[NumericQ[lowscale]&&NumericQ[highscale],
			(*Print["Matching to DsixTools:"];*)
			temp=HighPTToDsixToolsSMEFT[expr/.SMEFTSimplify];
			(*Print[temp];*)
			(*Print["Coefficients to run:"];*)
			params=Select[Variables[temp/.Conjugate[a_]->a/.Re->Identity/.Abs->Identity],MemberQ[DsixTools`SMEFTParameterList[],#] &];
			(*Print[params];*)
			(*Table[DsixTools`SMEFTEvolve[i,lowscale,highscale],{i,params}]//Print;*)
			(*Dispatch[(#1->DsixTools`SMEFTEvolve[#1,lowscale,highscale]&)/@params]//Normal//Print;*)
			temp=(temp/.Dispatch[(#1->DsixTools`SMEFTEvolve[#1,lowscale,highscale]&)/@params]);
			Return[DsixToolsToHighPTSMEFT[temp]/.SMEFTSimplify],
			Message[SMEFTRun::nonnumericscale];Abort[];
		];,
		_,
		Message[SMEFTRun::undefinedrunningmode,SMEFTRGEMode];Abort[];
	];
];
