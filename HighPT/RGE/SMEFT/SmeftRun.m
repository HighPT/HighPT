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


PackageExport["Basis"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["SMEFTAD"]


(*PackageScope["SMEFTSimplify"]*)


(*<<FileNameJoin[{Global`$DirectoryHighPT,"RGE","SMEFT","SMEFTAD"}]*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*SMEFTRun*)


Get@FileNameJoin[{Global`$DirectoryHighPT,"RGE","SMEFT","SMEFTAD.dat"}];


SMEFTAD[Conjugate[WC[lab_,ind___]]] := Conjugate[SMEFTAD[WC[lab,ind]]]


(*SMEFTSimplify=Get@FileNameJoin[{Global`$DirectoryHighPT,"RGE","Simplifications","SMEFTSimplify.dat"}];*)


SMEFTRun::undefinedrunningmode= "The mode `1` is not defined for SMEFT Running.";


SMEFTRun::nonnumericscale= "In \"DsixTools\" running mode both lowscale and highscale must be a number"


SMEFTRun::custombasisdsixtools="Running in a custom basis with dsixtools is not implemented yet. Please provide an expression in either up or down basis."


SMEFTRun::nocoefficients="No SMEFT coefficients found"


(*Options[SMEFTRun]={
	Basis->"up"
}*)


SMEFTRun[expr_,lowscale_, highscale_,OptionsPattern[]]:=Module[
	{
		params,
		evolution,
		mode,
		eqcoeff,
		eqcoeffd6,
		eqcoeffevolved,
		eqevolution = {}
	}
	,
	(*OptionCheck[Basis,OptionValue[Basis]];*)
	mode=GetSMEFTRGEMode[];
	Switch[
		mode,
		"LL",
		Return[expr/.wc_WC->(wc+1/(16\[Pi]^2)Log[lowscale/highscale]SMEFTAD[wc])],
		"DsixTools",
		(*If[MatchQ[OptionValue[Basis],"custom"],
			Message[SMEFTRun::custombasisdsixtools];Abort[]];*)
		(*If[MatchQ[OptionValue[Basis],"up"],temp=expr(*ToDownBasis[expr]*),temp=expr];*)
		If[NumericQ[lowscale]&&NumericQ[highscale],
			params=DeleteDuplicates@Cases[expr, _WC, \[Infinity]];
			(* Deal with the case of a single WC being evolved *)
			If[MatchQ[params,{}] && MatchQ[Head@expr,WC],params={expr}];
			If[MatchQ[params,{}],Message[SMEFTRun::nocoefficients]];
			(* Deal with WC["eq",_] *)
			If[!MatchQ[eqcoeff=Cases[params,WC["eq",_]],{}],
				params=DeleteCases[params/.WC["eq",_]->0,0];
				(*Print["eq detected"];
				Print[eqcoeff];*)
				eqcoeffd6=DsixTools`D6Simplify[HighPTToDsixToolsSMEFT[#]]&/@eqcoeff;
				eqcoeffevolved=If[MatchQ[Head@#,Conjugate],
					Conjugate[DsixTools`SMEFTEvolve[#[[1]],lowscale,highscale]],
					DsixTools`SMEFTEvolve[#,lowscale,highscale]
				]&/@eqcoeffd6;
				eqevolution=Table[eqcoeff[[i]]->DsixToolsToHighPTSMEFT[eqcoeffevolved[[i]]],{i,eqcoeffd6//Length}];
			];
			evolution=Dispatch[Join[(#1->DsixToolsToHighPTSMEFT[DsixTools`SMEFTEvolve[HighPTToDsixToolsSMEFT[#1],lowscale,highscale]]&)/@params,eqevolution]];
			(*Print[evolution//Normal];*)
			Return[expr/.evolution],
			Message[SMEFTRun::nonnumericscale];Abort[];
		];,
		"Off",
		Return[expr],
		_,
		Message[SMEFTRun::undefinedrunningmode,SMEFTRGEMode];Abort[];
	];
];
