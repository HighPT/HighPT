(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Flavor`*)


(* ::Subtitle:: *)
(*Implementation of the flavor observables.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["FlavorObservables"]


PackageExport["RestoreFlavorObservables"]


PackageExport["Obs"]


PackageExport["ChangeFlavorObservable"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["SMPrediction"]
PackageScope["SMPrediction$default"]


PackageScope["SMInfo"]


PackageScope["ExpValue"]
PackageScope["ExpValue$default"]


PackageScope["ExpInfo"]


PackageScope["NPContribution"]
PackageScope["NPContribution$default"]
PackageScope["NPContributionError"]


PackageScope["NPInfo"]


PackageScope["ExpCorrelation"]


PackageScope["LowScale"]


PackageScope["FlavorOptionCheck"]


(* ::Chapter:: *)
(*Private:*)


$FlavorSectors = {"ChargedCurrents"};


FlavorObservables[] = FlavorObservables/@$FlavorSectors


(* ::Section:: *)
(*Observables*)


Default[ExpCorrelation,0];


ExpInfo[_]="";
SMInfo[_]="";
NPInfo[_]="";


Obs[label_] := <|
	"Exp"->ExpValue[label],
	"SM"->SMPrediction[label],
	"NP"->NPContribution[label],
	"\[Sigma]NP"->NPContributionError[label],
	"info"->"Exp: "<>ExpInfo[label]<>"
SM: "<>SMInfo[label]<>"
NP: "<>NPInfo[label]
	|>


(* ::Section:: *)
(*Check Observable implementation*)


FlavorOptionCheck::optionvalue= "Invalid OptionValue specified: `1`\[Rule]`2`, the allowed values for `1` must match `3`.";


FlavorOptionCheck[opt_,optVal_]:=If[!MatchQ[optVal,$FlavorOptionValueAssociation[opt]],
	Message[FlavorOptionCheck::optionvalue, opt, optVal, $FlavorOptionValueAssociation[opt]];
	Abort[],
	True
];


$FlavorOptionValueAssociation= <|
	"Exp" -> {_?NumericQ,_?NumericQ},
	"SM" -> {_?NumericQ,_?NumericQ}
|>;


(* ::Section:: *)
(*Change Observables*)


ChangeFlavorObservable::invalidNP= "Invalid NP contribution. It must be an expressions of LEFT coefficients (WCL) only."
ChangeFlavorObservable::wrongobservable= "The observable `1` doesn't exist."


Options[ChangeFlavorObservable]={
	"Exp"->"current",
	"SM"->"current",
	"NP"->"current"
	};


ChangeFlavorObservable[obs_,Default] := ChangeFlavorObservable[
	obs,
	"Exp"->ExpValue$default[obs],
	"SM"->SMPrediction$default[obs],
	"NP"->NPContribution$default[obs]
]


ChangeFlavorObservable[obs_,OptionsPattern[]] := Module[
	{
		exp = OptionValue["Exp"], 
		SM = OptionValue["SM"], 
		NP = OptionValue["NP"],
		var
	}
	,
	If[!MemberQ[FlavorObservables[]//Flatten,obs],
		Message[ChangeFlavorObservable::wrongobservable,obs];Abort[];
	];
	If[(!MatchQ[exp,"current"])&&(FlavorOptionCheck["Exp",exp]),
		ExpValue[obs] = exp
	];
	If[(!MatchQ[SM,"current"])&&(FlavorOptionCheck["SM",SM]),
		SMPrediction[obs] = SM
	];
	If[!MatchQ[NP,"current"],
		var=Variables[NP/.Re->Identity/.Abs->Identity/.LEFTSimplify/.Conjugate[a_]->a];
		(*Print[var];
		Print[(Head/@var)//DeleteDuplicates];*)
		If[MatchQ[(Head/@var)//DeleteDuplicates,{WCL}],
			NPContribution[obs] = NP,
			Message[ChangeFlavorObservable::invalidNP];Abort[]
		];
	];
]


RestoreFlavorObservables[]:=ChangeFlavorObservable[#,Default]& /@ (FlavorObservables[]//Flatten);
