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


PackageScope["InputDependence"]
PackageScope["NumericalInput"]


(* ::Chapter:: *)
(*Private:*)


$FlavorSectors = {"ChargedCurrents","\[CapitalDelta]F=1"};


FlavorObservables::usage = "FlavorObservables[] returns a nested list of all the flavor observables implemented in HighPT. FlavorObservables[\"sector\"] gives a list of all flavor observables in the sector \"sector\""


FlavorObservables[] = FlavorObservables/@$FlavorSectors


(* ::Section:: *)
(*Observables*)


Default[ExpCorrelation,0];


ExpInfo[_]="";
SMInfo[_]="";
NPInfo[_]="";


Obs::usage = "Obs[\"obs\"] returns an association with all the information regarding observable \"obs\""


Obs[label_] := <|
	"Exp"->ExpValue[label]/.null->0,
	"SM"->SMPrediction[label]/.null->0,
	"NP"->NPContribution[label],
	(*"\[Sigma]NP"->NPContributionError[label],*)
	"info"->"Exp: "<>ExpInfo[label]<>"
SM: "<>SMInfo[label]<>"
NP: "<>NPInfo[label]
	|>


SMPrediction$default[obs_]:=InputDependence[obs]*NumericalInput[obs]/.GetParameters[Errors->True]


(* ::Section:: *)
(*Check Observable implementation*)


FlavorOptionCheck::optionvalue= "Invalid OptionValue specified: `1`\[Rule]`2`, the allowed values for `1` must match `3`.";


FlavorOptionCheck[opt_,optVal_]:=If[!MatchQ[optVal,$FlavorOptionValueAssociation[opt]],
	Message[FlavorOptionCheck::optionvalue, opt, optVal, $FlavorOptionValueAssociation[opt]];
	Abort[],
	True
];


$FlavorOptionValueAssociation= <|
	"Exp" -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ}] | _?((NumericQ[#]&&NonNegative[#])&),
	"SM" -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ}] | _?((NumericQ[#]&&NonNegative[#])&)
|>;


(* ::Section:: *)
(*Change Observables*)


ChangeFlavorObservable::invalidNP= "Invalid NP contribution for `1`. It must be an expressions of LEFT coefficients (WCL) only. Expression given: `2`"
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
		exp = OptionValue["Exp"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]], 
		SM = OptionValue["SM"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]], 
		NP = OptionValue["NP"],
		var
	}
	,
	If[!MemberQ[FlavorObservables[]//Flatten,obs],
		Message[ChangeFlavorObservable::wrongobservable,obs];Abort[];
	];
	If[(!MatchQ[exp,"current"])&&(FlavorOptionCheck["Exp",exp]),
		If[NumericQ[exp],
			ExpValue[obs] = Around[exp,null],
			ExpValue[obs] = exp
		];
	];
	If[(!MatchQ[SM,"current"])&&(FlavorOptionCheck["SM",SM]),
		If[NumericQ[SM],
			SMPrediction[obs] = Around[SM,null],
			SMPrediction[obs] = SM
		];
	];
	If[!MatchQ[NP,"current"],
		var=Variables[NP/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a];
		(*Print[var];
		Print[(Head/@var)//DeleteDuplicates];*)
		If[MatchQ[(Head/@var)//DeleteDuplicates,{WCL}],
			NPContribution[obs] = NP,
			Message[ChangeFlavorObservable::invalidNP,obs,NP];AboOptinrt[]
		];
	];
]


RestoreFlavorObservables[]:=ChangeFlavorObservable[#,Default]& /@ (FlavorObservables[]//Flatten);
