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


PackageExport["ObsTable"]
PackageExport["ObsGrid"]


PackageExport["RestoreFlavorObservables"]


PackageExport["Obs"]


PackageExport["ChangeFlavorObservable"]


PackageExport["AddFlavorObservable"]
PackageExport["RemoveFlavorObservable"]
PackageExport["ClearCustomObservables"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["TheoryExpression"]
PackageScope["NPFromTheoryExpression"]


PackageScope["SMPrediction"]
PackageScope["SMPrediction$default"]


PackageScope["SMInfo"]


PackageScope["ExpValue"]
PackageScope["ExpValue$default"]


PackageScope["ExpInfo"]


PackageScope["NPContribution"]
PackageScope["NPContribution$default"]
PackageScope["NPContribution$current"]
PackageScope["NPContributionError"]


PackageScope["NPInfo"]


PackageScope["ExpCorrelation"]


PackageScope["LowScale"]


PackageScope["FlavorOptionCheck"]


PackageScope["InputDependence"]
PackageScope["NumericalInput"]


(* ::Chapter:: *)
(*Private:*)


$FlavorSectors = {"ChargedCurrents","\[CapitalDelta]F=1","\[CapitalDelta]F=2","LFV","\[Tau]LFU","custom"};


FlavorObservables::usage = "FlavorObservables[] returns a nested list of all the flavor observables implemented in HighPT. FlavorObservables[\"sector\"] gives a list of all flavor observables in the sector \"sector\""
ObsTable::usage = "TEST!!!! ObsTable[\"group\"] returns a nice table with all the observables belonging to \"group\""


FlavorObservables[] := FlavorObservables/@$FlavorSectors


ObsTable[] := Column[ObsTable/@$FlavorSectors,Frame->True]


FlavorObservables["custom"] := $CustomObservables;
$CustomObservables = {};


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


ObsGrid[obs_]:=Grid[Join[{{"label",obs}},Table[{i,Obs[obs][i]},{i,Keys@Obs[i]}]],Dividers->{All,True}]


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
	"SM" -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ}] | _?((NumericQ[#]&&NonNegative[#])&),
	"Scale" -> _?NumericQ
|>;


(* ::Section:: *)
(*Change Observables*)


ChangeFlavorObservable::invalidNP= "Invalid NP contribution for `1`. It must be an expressions of LEFT coefficients (WCL) and/or SMEFT coefficients (WC) only. Spurious dependencies: `2`. Expression given: `3`"
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
		If[SubsetQ[{WCL,WC},(Head/@var)//DeleteDuplicates],
		    NPContribution$current[obs] = NP(*NPContribution[obs] = ApplyRedefinitions@NP*),
			Message[ChangeFlavorObservable::invalidNP,obs,DeleteCases[DeleteCases[var,_WC],_WCL],NP];Abort[]
		];
		,
		ApplyRedefinitions
	];
	NPContribution[obs] = ApplyRedefinitions[NPContribution$current[obs]];
]


RestoreFlavorObservables[]:=ChangeFlavorObservable[#,Default]& /@ (FlavorObservables[]//Flatten);


(* ::Section:: *)
(*Add a custom - defined observable*)


Options[AddFlavorObservable] = {
	"Exp"->"TBD",
	"SM"->"TBD",
	"NP"->"TBD",
	"Scale"->"TBD"
	};


AddFlavorObservable::usage = "AddFlavorObservable[\"name\"] defines a new observable with label \"name\".
The observable is added to the FlavorObservables list under the sector \"custom\".
Options are (all compulsory): \"Exp\", \"SM\", \"NP\", and \"Scale\".
\"Exp\" and \"SM\" need to be Around[] objects,
\"NP\" a function of WC and WCL only,
and \"Scale\" a number."


AddFlavorObservable::invalidinput = "Input not valid. \"Exp\" and \"SM\" must be Around[] objects, \"NP\" function of WC and WCL only, \"Scale\" a number"


AddFlavorObservable::existing = "The observable `1` already exists in a default implementation. Please choose another name."


AddFlavorObservable[name_String,OptionsPattern[]]:=Module[
	{
	exp = OptionValue["Exp"],
	sm = OptionValue["SM"],
	np = OptionValue["NP"],
	scale = OptionValue["Scale"],
	var
	}
	,
	(* check if some input has been left out *)
	If[MemberQ[Complement[FlavorObservables[]//Flatten,FlavorObservables["custom"]],name],Message[AddFlavorObservable::existing,name];Abort[]];
	Table[If[i == "TBD",Message[AddFlavorObservable::invalidinput];Abort[]],{i,{exp,sm,np,scale}}];
	
	(* check all the inputs separately now (NP check below) *)
	FlavorOptionCheck["Exp",exp];
	FlavorOptionCheck["SM",sm];
	FlavorOptionCheck["Scale",scale];
	
	(* Define ExpValue, SMPrediction, NPContribution and LowScale *)
	If[NumericQ[exp],
			ExpValue[name] = Around[exp,null],
			ExpValue[name] = exp
		];
	If[NumericQ[sm],
			SMPrediction[name] = Around[sm,null],
			SMPrediction[name] = sm
		];
	var=Variables[np/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a];
		(*Print[var];
		Print[(Head/@var)//DeleteDuplicates];*)
		If[SubsetQ[{WCL,WC},(Head/@var)//DeleteDuplicates],
			NPContribution[name] = np,
			Message[AddFlavorObservable::invalidinput];Abort[]
		];
	LowScale[name] := scale;
	(* Add observable to the list *)
	If[MemberQ[FlavorObservables["custom"],name]
	,
	Nothing[]
	,
	AppendTo[$CustomObservables,name];
	ObsTable["custom"] := Grid[{{"custom",Column[FlavorObservables["custom"]]}},Dividers->All];
	];
	Print["Success! Check by running Obs[\""<>name<>"\"], or ObsTable[]."]
]


(* ::Section:: *)
(*Remove a custom - defined observable*)


RemoveFlavorObservable[name_String] := Module[
	{
	a
	}
	,
	If[!MemberQ[FlavorObservables["custom"],name],
	Print["Observable " <> name <> " is not a custom-defined observable."];Abort[]];
	ExpValue[name]=.;
	SMPrediction[name]=.;
	NPContribution[name]=.;
	LowScale[name]=.;
	$CustomObservables = DeleteCases[$CustomObservables,name];
];


ClearCustomObservables[] := RemoveFlavorObservable/@FlavorObservables["custom"];


(* ::Section:: *)
(*Compute NP from the theor. expression*)


NPFromTheoryExpression[obs_] := 1/SMPrediction$default[obs]["Value"] ((TheoryExpression[obs]/.a_WCL->(SMValue[a]+a))-(TheoryExpression[obs]/.a_WCL->SMValue[a]))/.GetParameters[]
