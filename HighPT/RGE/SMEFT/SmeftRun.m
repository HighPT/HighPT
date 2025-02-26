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


PackageScope["ReplaceRedundant"]


(*PackageScope["SMEFTSimplify"]*)


(*<<FileNameJoin[{Global`$DirectoryHighPT,"RGE","SMEFT","SMEFTAD"}]*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Change to redundant basis*)


ReplaceRedundant = <|
(* ee *)
WC["ee",{1,1,1,2}]:>WC["ee",{1,1,1,2}]+WC["ee",{1,2,1,1}],
WC["ee",{1,1,1,3}]:>WC["ee",{1,1,1,3}]+WC["ee",{1,3,1,1}],
WC["ee",{1,2,1,3}]:>WC["ee",{1,2,1,3}]+WC["ee",{1,3,1,2}],
WC["ee",{1,1,2,2}]:>WC["ee",{1,1,2,2}]+WC["ee",{2,2,1,1}],
WC["ee",{1,2,2,2}]:>WC["ee",{1,2,2,2}]+WC["ee",{2,2,1,2}],
WC["ee",{1,1,2,3}]:>WC["ee",{1,1,2,3}]+WC["ee",{2,3,1,1}],
WC["ee",{1,2,2,3}]:>WC["ee",{1,2,2,3}]+WC["ee",{2,3,1,2}],
WC["ee",{1,3,2,3}]:>WC["ee",{1,3,2,3}]+WC["ee",{2,3,1,3}],
WC["ee",{2,2,2,3}]:>WC["ee",{2,2,2,3}]+WC["ee",{2,3,2,2}],
WC["ee",{1,2,3,2}]:>WC["ee",{1,2,3,2}]+WC["ee",{3,2,1,2}],
WC["ee",{1,1,3,3}]:>WC["ee",{1,1,3,3}]+WC["ee",{3,3,1,1}],
WC["ee",{1,2,3,3}]:>WC["ee",{1,2,3,3}]+WC["ee",{3,3,1,2}],
WC["ee",{1,3,3,3}]:>WC["ee",{1,3,3,3}]+WC["ee",{3,3,1,3}],
WC["ee",{2,2,3,3}]:>WC["ee",{2,2,3,3}]+WC["ee",{3,3,2,2}],
WC["ee",{2,3,3,3}]:>WC["ee",{2,3,3,3}]+WC["ee",{3,3,2,3}],
(* ll *)
WC["ll",{1,1,1,2}]:>WC["ll",{1,1,1,2}]+WC["ll",{1,2,1,1}],
WC["ll",{1,1,1,3}]:>WC["ll",{1,1,1,3}]+WC["ll",{1,3,1,1}],
WC["ll",{1,2,1,3}]:>WC["ll",{1,2,1,3}]+WC["ll",{1,3,1,2}],
WC["ll",{1,1,2,2}]:>WC["ll",{1,1,2,2}]+WC["ll",{2,2,1,1}],
WC["ll",{1,2,2,2}]:>WC["ll",{1,2,2,2}]+WC["ll",{2,2,1,2}],
WC["ll",{1,1,2,3}]:>WC["ll",{1,1,2,3}]+WC["ll",{2,3,1,1}],
WC["ll",{1,2,2,3}]:>WC["ll",{1,2,2,3}]+WC["ll",{2,3,1,2}],
WC["ll",{1,3,2,3}]:>WC["ll",{1,3,2,3}]+WC["ll",{2,3,1,3}],
WC["ll",{2,2,2,3}]:>WC["ll",{2,2,2,3}]+WC["ll",{2,3,2,2}],
WC["ll",{1,2,3,2}]:>WC["ll",{1,2,3,2}]+WC["ll",{3,2,1,2}],
WC["ll",{1,1,3,3}]:>WC["ll",{1,1,3,3}]+WC["ll",{3,3,1,1}],
WC["ll",{1,2,3,3}]:>WC["ll",{1,2,3,3}]+WC["ll",{3,3,1,2}],
WC["ll",{1,3,3,3}]:>WC["ll",{1,3,3,3}]+WC["ll",{3,3,1,3}],
WC["ll",{2,2,3,3}]:>WC["ll",{2,2,3,3}]+WC["ll",{3,3,2,2}],
WC["ll",{2,3,3,3}]:>WC["ll",{2,3,3,3}]+WC["ll",{3,3,2,3}],
(* uu *)
WC["uu",{1,1,1,2}]:>WC["uu",{1,1,1,2}]+WC["uu",{1,2,1,1}],
WC["uu",{1,1,1,3}]:>WC["uu",{1,1,1,3}]+WC["uu",{1,3,1,1}],
WC["uu",{1,2,1,3}]:>WC["uu",{1,2,1,3}]+WC["uu",{1,3,1,2}],
WC["uu",{1,1,2,2}]:>WC["uu",{1,1,2,2}]+WC["uu",{2,2,1,1}],
WC["uu",{1,2,2,2}]:>WC["uu",{1,2,2,2}]+WC["uu",{2,2,1,2}],
WC["uu",{1,1,2,3}]:>WC["uu",{1,1,2,3}]+WC["uu",{2,3,1,1}],
WC["uu",{1,2,2,3}]:>WC["uu",{1,2,2,3}]+WC["uu",{2,3,1,2}],
WC["uu",{1,3,2,3}]:>WC["uu",{1,3,2,3}]+WC["uu",{2,3,1,3}],
WC["uu",{2,2,2,3}]:>WC["uu",{2,2,2,3}]+WC["uu",{2,3,2,2}],
WC["uu",{1,2,3,2}]:>WC["uu",{1,2,3,2}]+WC["uu",{3,2,1,2}],
WC["uu",{1,1,3,3}]:>WC["uu",{1,1,3,3}]+WC["uu",{3,3,1,1}],
WC["uu",{1,2,3,3}]:>WC["uu",{1,2,3,3}]+WC["uu",{3,3,1,2}],
WC["uu",{1,3,3,3}]:>WC["uu",{1,3,3,3}]+WC["uu",{3,3,1,3}],
WC["uu",{2,2,3,3}]:>WC["uu",{2,2,3,3}]+WC["uu",{3,3,2,2}],
WC["uu",{2,3,3,3}]:>WC["uu",{2,3,3,3}]+WC["uu",{3,3,2,3}],
(* dd *)
WC["dd",{1,1,1,2}]:>WC["dd",{1,1,1,2}]+WC["dd",{1,2,1,1}],
WC["dd",{1,1,1,3}]:>WC["dd",{1,1,1,3}]+WC["dd",{1,3,1,1}],
WC["dd",{1,2,1,3}]:>WC["dd",{1,2,1,3}]+WC["dd",{1,3,1,2}],
WC["dd",{1,1,2,2}]:>WC["dd",{1,1,2,2}]+WC["dd",{2,2,1,1}],
WC["dd",{1,2,2,2}]:>WC["dd",{1,2,2,2}]+WC["dd",{2,2,1,2}],
WC["dd",{1,1,2,3}]:>WC["dd",{1,1,2,3}]+WC["dd",{2,3,1,1}],
WC["dd",{1,2,2,3}]:>WC["dd",{1,2,2,3}]+WC["dd",{2,3,1,2}],
WC["dd",{1,3,2,3}]:>WC["dd",{1,3,2,3}]+WC["dd",{2,3,1,3}],
WC["dd",{2,2,2,3}]:>WC["dd",{2,2,2,3}]+WC["dd",{2,3,2,2}],
WC["dd",{1,2,3,2}]:>WC["dd",{1,2,3,2}]+WC["dd",{3,2,1,2}],
WC["dd",{1,1,3,3}]:>WC["dd",{1,1,3,3}]+WC["dd",{3,3,1,1}],
WC["dd",{1,2,3,3}]:>WC["dd",{1,2,3,3}]+WC["dd",{3,3,1,2}],
WC["dd",{1,3,3,3}]:>WC["dd",{1,3,3,3}]+WC["dd",{3,3,1,3}],
WC["dd",{2,2,3,3}]:>WC["dd",{2,2,3,3}]+WC["dd",{3,3,2,2}],
WC["dd",{2,3,3,3}]:>WC["dd",{2,3,3,3}]+WC["dd",{3,3,2,3}],
(* qq1 *)
WC["qq1",{1,1,1,2}]:>WC["qq1",{1,1,1,2}]+WC["qq1",{1,2,1,1}],
WC["qq1",{1,1,1,3}]:>WC["qq1",{1,1,1,3}]+WC["qq1",{1,3,1,1}],
WC["qq1",{1,2,1,3}]:>WC["qq1",{1,2,1,3}]+WC["qq1",{1,3,1,2}],
WC["qq1",{1,1,2,2}]:>WC["qq1",{1,1,2,2}]+WC["qq1",{2,2,1,1}],
WC["qq1",{1,2,2,2}]:>WC["qq1",{1,2,2,2}]+WC["qq1",{2,2,1,2}],
WC["qq1",{1,1,2,3}]:>WC["qq1",{1,1,2,3}]+WC["qq1",{2,3,1,1}],
WC["qq1",{1,2,2,3}]:>WC["qq1",{1,2,2,3}]+WC["qq1",{2,3,1,2}],
WC["qq1",{1,3,2,3}]:>WC["qq1",{1,3,2,3}]+WC["qq1",{2,3,1,3}],
WC["qq1",{2,2,2,3}]:>WC["qq1",{2,2,2,3}]+WC["qq1",{2,3,2,2}],
WC["qq1",{1,2,3,2}]:>WC["qq1",{1,2,3,2}]+WC["qq1",{3,2,1,2}],
WC["qq1",{1,1,3,3}]:>WC["qq1",{1,1,3,3}]+WC["qq1",{3,3,1,1}],
WC["qq1",{1,2,3,3}]:>WC["qq1",{1,2,3,3}]+WC["qq1",{3,3,1,2}],
WC["qq1",{1,3,3,3}]:>WC["qq1",{1,3,3,3}]+WC["qq1",{3,3,1,3}],
WC["qq1",{2,2,3,3}]:>WC["qq1",{2,2,3,3}]+WC["qq1",{3,3,2,2}],
WC["qq1",{2,3,3,3}]:>WC["qq1",{2,3,3,3}]+WC["qq1",{3,3,2,3}],
(* qq3 *)
WC["qq3",{1,1,1,2}]:>WC["qq3",{1,1,1,2}]+WC["qq3",{1,2,1,1}],
WC["qq3",{1,1,1,3}]:>WC["qq3",{1,1,1,3}]+WC["qq3",{1,3,1,1}],
WC["qq3",{1,2,1,3}]:>WC["qq3",{1,2,1,3}]+WC["qq3",{1,3,1,2}],
WC["qq3",{1,1,2,2}]:>WC["qq3",{1,1,2,2}]+WC["qq3",{2,2,1,1}],
WC["qq3",{1,2,2,2}]:>WC["qq3",{1,2,2,2}]+WC["qq3",{2,2,1,2}],
WC["qq3",{1,1,2,3}]:>WC["qq3",{1,1,2,3}]+WC["qq3",{2,3,1,1}],
WC["qq3",{1,2,2,3}]:>WC["qq3",{1,2,2,3}]+WC["qq3",{2,3,1,2}],
WC["qq3",{1,3,2,3}]:>WC["qq3",{1,3,2,3}]+WC["qq3",{2,3,1,3}],
WC["qq3",{2,2,2,3}]:>WC["qq3",{2,2,2,3}]+WC["qq3",{2,3,2,2}],
WC["qq3",{1,2,3,2}]:>WC["qq3",{1,2,3,2}]+WC["qq3",{3,2,1,2}],
WC["qq3",{1,1,3,3}]:>WC["qq3",{1,1,3,3}]+WC["qq3",{3,3,1,1}],
WC["qq3",{1,2,3,3}]:>WC["qq3",{1,2,3,3}]+WC["qq3",{3,3,1,2}],
WC["qq3",{1,3,3,3}]:>WC["qq3",{1,3,3,3}]+WC["qq3",{3,3,1,3}],
WC["qq3",{2,2,3,3}]:>WC["qq3",{2,2,3,3}]+WC["qq3",{3,3,2,2}],
WC["qq3",{2,3,3,3}]:>WC["qq3",{2,3,3,3}]+WC["qq3",{3,3,2,3}]
|>;


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
		Return[expr/.ReplaceRedundant/.wc_WC->(wc+1/(16\[Pi]^2)Log[lowscale/highscale]SMEFTAD[wc])],
		"NLL",
		params=DeleteDuplicates@Cases[expr, _WC, All];
		evolution=Dispatch@Table[wc->Expand[(wc+1/2 1/(16\[Pi]^2)Log[lowscale/highscale]SMEFTAD[wc])/.a_WC->(a+1/2 1/(16\[Pi]^2)Log[lowscale/highscale]SMEFTAD[a])]/.Log[b_]^2->2*Log[b]^2,{wc,params}];
		(*Print[evolution];*)
		Return[expr/.evolution],
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
			Return[expr/.ReplaceRedundant/.evolution],
			Message[SMEFTRun::nonnumericscale];Abort[];
		];,
		"Off",
		Return[expr],
		_,
		Message[SMEFTRun::undefinedrunningmode,SMEFTRGEMode];Abort[];
	];
];
