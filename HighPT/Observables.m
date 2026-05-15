(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Observables`*)


(* ::Subtitle:: *)
(*General implementation of observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ObservableList"]
PackageExport["ObservableSectors"]


PackageExport["Obs"]
PackageExport["ObsTable"]
PackageExport["ObsGrid"]


PackageExport["SelectObservables"]


PackageExport["ChangeObservable"]
PackageExport["RestoreObservables"]


PackageExport["NewObservable"]
PackageExport["RemoveObservable"]


PackageExport["Info"]


PackageExport["TheoryExpression"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["THCorrelation"]
PackageScope["ExpCorrelation"]


PackageScope["ExpValue"]
PackageScope["ExpValue$default"]


PackageScope["SMPrediction"]
PackageScope["SMPrediction$default"]
PackageScope["SMExpression"] (* Probably outdated, check *)


PackageScope["NPFromTheoryExpression"]
PackageScope["CheckTheoryExpression"]


PackageScope["NPContribution"]
PackageScope["NPContribution$default"]
PackageScope["NPContribution$current"]
PackageScope["NPContributionError"]


PackageScope["SMInfo"]
PackageScope["ExpInfo"]
PackageScope["NPInfo"]


PackageScope["LowScale"]
PackageScope["LowScale$default"]


PackageScope["ObsOptionCheck"]


PackageScope["SelectedObservables"]
PackageScope["CheckboxBarSelectAll"]
PackageScope["AllSelector"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Define observable sectors*)


ObservableSectors[] := {"Flavor","Drell-Yan","Higgs","EW","LEP2","FCC","custom"};


(* Remove EWObservables from here eventually *)
ObservableList[] := Join[ObservableList/@ObservableSectors[],EWObservables[]]


(*ObservableList["Flavor"] := FlavorObservables[];*)
ObservableList["Drell-Yan"] := Keys@LHCSearch[];
(*ObservableList["EW"] := EWObservables[];*)
(*ObservableList["Higgs"] := HiggsObservables[];*)
(*ObservableList["LEP2"] := LEP2Observables[];*)
(*ObservableList["FCC"] := FCCObservables[];*)


ObservableList[_] := {}


ObservableList["custom"] := $CustomObservables;
$CustomObservables = {};


(* ::Section:: *)
(*Define Obs*)


ExpInfo[_]="";
SMInfo[_]="";
NPInfo[_]="";


Obs::usage = "Obs[\"obs\"] returns an association with all the information regarding observable \"obs\""


Obs[label_] := If[MemberQ[Flatten[ObservableList[]],label],
				<|
					"Exp"->ExpValue[label](*/.null->0*),
					"SM"->SMPrediction[label](*/.null->0*),
					"NP"->NPContribution[label],
					(*"\[Sigma]NP"->NPContributionError[label],*)
					"Scale"->LowScale[label],
					"info"->Grid[{
						{"Exp:",ExpInfo[label]},
						{"SM:",SMInfo[label]},
						{"NP:",NPInfo[label]}},
						Dividers->True],
					"label"->label
				|>,
				Print["The observable "<>label<>" is not implemented. Run ObservableList[] to see all observables."]
]


(* Obsolete, remove eventually *)
SMPrediction$default[obs_]:=InputDependence[obs]*NumericalInput[obs]/.GetParameters[Errors->True]


ObsGrid[obs_]:=Grid[Join[{{"label",obs}},Table[{i,Obs[obs][i]},{i,Keys@Obs[i]}]],Dividers->{All,True}]


(* ::Section:: *)
(*Print information*)


Info[x_Association] := If[MemberQ[Keys@x,"info"],x["info"],Print["No information available"]]


(* ::Section:: *)
(*Properties of correlation matrices*)


ExpCorrelation[i_,i_] := 1
ExpCorrelation[i_,j_] := 0
SetAttributes[ExpCorrelation,Orderless]


THCorrelation[i_,i_] := 1
THCorrelation[i_,j_] := 0
SetAttributes[THCorrelation,Orderless]


(* ::Section:: *)
(*Check observable implementation*)


ObsOptionCheck::optionvalue= "Invalid OptionValue specified: `1`\[Rule]`2`, the allowed values for `1` must match `3`.";


ObsOptionCheck[opt_,optVal_]:=If[!MatchQ[optVal,$ObsOptionValueAssociation[opt]],
	Message[ObsOptionCheck::optionvalue, opt, optVal, $ObsOptionValueAssociation[opt]];
	Abort[],
	True
];


$ObsOptionValueAssociation= <|
	"Exp"   -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ} | null] | _?((NumericQ[#]&&NonNegative[#])&),
	"SM"    -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ} | null] | _?((NumericQ[#]&&NonNegative[#])&),
	"Scale" -> _?NumericQ
|>;


CheckTheoryExpression::undefinedparameters = "The following parameters are unknown: `1`"
CheckTheoryExpression::undefinedsymbols = "The following symbols are undefined: `1`"


CheckTheoryExpression[expr_] := Module[
	{
	params, coeffs
	}
	,
	params = Cases[expr, _Param | _Mass | _Width | _DecayConstant | _Lifetime | _Vckm | _Yukawa, All]//DeleteDuplicates;
	If[!MatchQ[params,{}] && !SubsetQ[Keys@GetParameters[],params],Message[CheckTheoryExpression::undefinedparameters, Complement[params,Keys@GetParameters[]]];Abort[]];
	coeffs = Variables[expr/.GetParameters[]/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a];
	If[!MatchQ[coeffs,{}] && !SubsetQ[{WCL,WC},(Head/@coeffs)//DeleteDuplicates],Message[CheckTheoryExpression::undefinedsymbols, DeleteCases[coeffs, _WC | _WCL]];Abort[]];
	Return[True]
]


(* ::Section:: *)
(*Change an observable (also used to initialize)*)


ChangeObservable::wrongobservable= "The observable `1` doesn't exist."
ChangeObservable::drellyanobservable= "Drell-Yan observables cannot be changed at the moment."
ChangeObservable::invalidNP= "Invalid NP contribution for `1`. It must be an expressions of LEFT coefficients (WCL) and/or SMEFT coefficients (WC) only. Spurious dependencies: `2`. Expression given: `3`"


Options[ChangeObservable] = {
	"Exp"   -> "current",
	"SM"    -> "current",
	"NP"    -> "current",
	"Scale" -> "current"
};


(* Used to initialize observables *)
ChangeObservable[obs_String,Default] := ChangeObservable[
	obs,
	"Exp"   -> ExpValue$default[obs],
	"SM"    -> SMPrediction$default[obs],
	"NP"    -> NPContribution$default[obs],
	"Scale" -> LowScale$default[obs]
]


ChangeObservable[obs_String,OptionsPattern[]] := Module[
	{
		exp = OptionValue["Exp"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		SM = OptionValue["SM"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		NP = OptionValue["NP"],
		scale = OptionValue["Scale"],
		var
	}
	,
	(*Print["Modifying observable "<>obs];*)
	If[!MemberQ[ObservableList[]//Flatten,obs],
		Message[ChangeObservable::wrongobservable,obs];Abort[];
	];
	If[MemberQ[ObservableList["Drell-Yan"],obs],Message[ChangeObservable::drellyanobservable];Abort[]];
	If[(!MatchQ[exp,"current"])&&(ObsOptionCheck["Exp",exp]),
		If[NumericQ[exp],
			ExpValue[obs] = Around[exp,null],
			ExpValue[obs] = exp
		];
	];
	If[(!MatchQ[SM,"current"])&&(ObsOptionCheck["SM",SM]),
		If[NumericQ[SM],
			SMPrediction[obs] = Around[SM,null],
			SMPrediction[obs] = SM
		];
	];
	If[!MatchQ[NP,"current"],
		var = Variables[NP/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a];
		If[SubsetQ[{WCL,WC},(Head/@var)//DeleteDuplicates],
		    NPContribution$current[obs] = NP,
			Message[ChangeObservable::invalidNP,obs,DeleteCases[DeleteCases[var,_WC],_WCL],NP];Abort[]
		];
	];
	NPContribution[obs] = ApplyRedefinitions[NPContribution$current[obs]];
	If[(!MatchQ[scale,"current"])&&(ObsOptionCheck["Scale",scale]),
		LowScale[obs] = scale
	];
];


(* ::Section:: *)
(*Restore observables to default values*)


RestoreObservables::invalidobs="The observables `1` are not implemented or don't exist"


RestoreObservables[] := RestoreObservables[Flatten[ObservableList[]]]
RestoreObservables[obs_List] := RestoreObservables/@obs
RestoreObservables[obs_String] := ChangeObservable[obs,Default]


(* ::Section:: *)
(*Custom defined observables*)


(* ::Subsection:: *)
(*Add*)


Options[NewObservable] = {
	"Exp"   -> "TBD",
	"SM"    -> "TBD",
	"NP"    -> "TBD",
	"TheoryExpression" -> "TBD",
	"Scale" -> "TBD"
	};


NewObservable::usage = "NewObservable[\"name\"] defines a new observable with label \"name\".
The observable is added to the ObservableList list under the sector \"custom\".
Options are (all compulsory): \"Exp\", \"SM\", \"NP\", and \"Scale\".
\"Exp\" and \"SM\" need to be Around[] objects,
\"NP\" a function of WC and WCL, and HighPT-defined parameters only,
and \"Scale\" a number."


NewObservable::invalidinput = "Input not valid. \"Exp\" and \"SM\" must be Around[] objects, \"NP\" function of WC and WCL only, \"Scale\" a number"
NewObservable::incompatibleinput = "New Physics contibution and theory expression are incompatible."


NewObservable::existing = "The observable `1` already exists in a default implementation. Please choose another name."


NewObservable[name_String,OptionsPattern[]] := Module[
	{
	exp = OptionValue["Exp"],
	sm = OptionValue["SM"],
	np = OptionValue["NP"],
	thexp = OptionValue["TheoryExpression"],
	scale = OptionValue["Scale"],
	var
	}
	,
	
	(* check if observable is already existing (in the custom list) *)
	If[MemberQ[Complement[ObservableList[]//Flatten,ObservableList["custom"]],name],
		Message[NewObservable::existing,name];Abort[]
	];
	
	(* check if some required input is missing *)
	Table[If[i == "TBD",Message[NewObservable::invalidinput];Abort[]],{i,{exp,sm,scale}}];
	If[MatchQ[np,"TBD"] && MatchQ[thexp,"TBD"],Message[NewObservable::invalidinput];Abort[]];
	
	(* check if inputs are in correct form *)
	ObsOptionCheck["Exp",exp];
	ObsOptionCheck["SM",sm];
	ObsOptionCheck["Scale",scale];
	
	Switch[np,
		"TBD",
			CheckTheoryExpression[thexp];
			TheoryExpression[name]=thexp;
			np=NPFromTheoryExpression[name];,
		_,
		Switch[thexp,
			"TBD",
			Null,
			_,
			TheoryExpression[name]=thexp;
			If[Simplify[NPFromTheoryExpression[name]-np]!=0,Message[NewObservable::incompatibleinput];TheoryExpression[name]=.;Abort[]]
		]
	];
	
	(*var = Variables[np/.GetParameters[]/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a];
	If[SubsetQ[{WCL,WC},(Head/@var)//DeleteDuplicates],
		NPContribution[name] = np,
		Message[AddFlavorObservable::invalidinput];Abort[]
	];*)
	
	(* set everything *)
	If[NumericQ[exp],
		ExpValue[name] = Around[exp,null],
		ExpValue[name] = exp
	];
	If[NumericQ[sm],
		SMPrediction[name] = Around[sm,null],
		SMPrediction[name] = sm
	];
	NPContribution[name] = ApplyRedefinitions[np];
	LowScale[name] = scale;
	
	(* add the new observable to the list *)
	If[MemberQ[ObservableList["custom"],name],
		Nothing[],
		AppendTo[$CustomObservables,name];
	];
	Print["Success! Check by running Obs[\""<>name<>"\"]"]
]


(* ::Subsection:: *)
(*Remove*)


RemoveObservable[name_String] := Module[
	{}
	,
	If[!MemberQ[ObservableList["custom"],name],
	Print["Observable " <> name <> " is not a custom-defined observable."];Abort[]];
	ExpValue[name]=.;
	SMPrediction[name]=.;
	NPContribution[name]=.;
	LowScale[name]=.;
	$CustomObservables = DeleteCases[$CustomObservables,name];
];


(* ::Section:: *)
(*Computing NP part from theory expressions*)


(*NPFromTheoryExpression[obs_] := 1/SMPrediction$default[obs]["Value"] ((TheoryExpression[obs](*/.PhiPPpl\[Nu]Rep[obs]*)/.Around[a_,b_]:>a/._Phi:>0/.a_WCL:>(SMValue[a,TreeOnly->True]+a))-(TheoryExpression[obs](*/.PhiPPpl\[Nu]Rep[obs]*)/.Around[a_,b_]:>a/._Phi:>0/.a_WCL:>SMValue[a,TreeOnly->True]/._WC->0))/.GetParameters[]*)


NPFromTheoryExpression[obs_] := Module[
	{
	expr,
	withsm, smonly
	}
	,
	If[FreeQ[TheoryExpression[obs],Phi],
		expr = TheoryExpression[obs],
		expr = TheoryExpression[obs]/.PhiPPpl\[Nu]Rep[obs]/.SubstitutePsi/.Around[a_,b_]:>a/._Phi->0
	];
	If[MatchQ[SMPrediction$default[obs]["Value"],0],
		Return[expr/.GetParameters[]],
		withsm = (expr(*/.PhiPPpl\[Nu]Rep[obs]*)/.Around[a_,b_]:>a/._Phi:>0/.a_WCL:>(SMValue[a,TreeOnly->True]+a));
		smonly = (expr(*/.PhiPPpl\[Nu]Rep[obs]*)/.Around[a_,b_]:>a/._Phi:>0/.a_WCL:>SMValue[a,TreeOnly->True]/._WC->0);
		Return[1/SMPrediction$default[obs]["Value"] (withsm - smonly)/.GetParameters[]]
	]
]


(* ::Section:: *)
(*Interface for selecting observables*)


(* ::Subsection:: *)
(*Basic elements (build synced checkboxbar)*)


Options[AllSelector] = {"Label"->""};


AllSelector[Dynamic[var_],choices_,OptionsPattern[]] := DynamicModule[
	{state=False},
	DynamicWrapper[
		Row[{
			Checkbox[
				Dynamic[
					state,
					Function[
						{lval,lvar},
						If[state===True,
							state=False;var={},
							state=True;var=Union[ReplaceAll[choices,HoldPattern[a_->b_]:>a]]
						],
						{HoldAll}
					]
				],
				{False,True,"Partial"}
			],
			""<>OptionValue["Label"]<>" "
		}],
		Which[
			Not[ValueQ[var]],var={};state=False,
			Union[var]===Union[ReplaceAll[choices,HoldPattern[a_->b_]->a]],state=True,
			var==={},state=False,
			True,state="Partial"
		]
	]
];


Options[CheckboxBarSelectAll] = Join[
	Options[CheckboxBar],
	Options[Grid],
	{ControlPlacement->Top,"Label"->"Select all"}
];


CheckboxBarSelectAll[Dynamic[var_],choices_List,opts:OptionsPattern[]] := Block[
	{
	cts={{
		Row[{
			If[OptionValue[Appearance]==="Vertical","","\[ThinSpace]"],
			AllSelector[
				Dynamic[var],
				choices
			],
			"\[ThinSpace]",
			OptionValue["Label"]
			}],
		CheckboxBar[
			Dynamic[var],
			choices,
			Sequence@@FilterRules[{opts},Options[CheckboxBar]]
		]
		}}
	},
	Switch[OptionValue[ControlPlacement],
		Left,
			Grid[
				cts,
				Sequence@@FilterRules[{opts},Options[Grid]]
			],
		Right,
			Grid[
				Reverse/@cts,
				Sequence@@FilterRules[{opts},Options[Grid]]
			],
		Bottom,
			Grid[
				Reverse@Transpose[cts],
				Alignment->Left,
				Sequence@@FilterRules[{opts},Options[Grid]]
			],
		_,
			Grid[
				Transpose[cts],
				Alignment->Left,
				Sequence@@FilterRules[{opts},Options[Grid]]
			]
	]
];



(* ::Subsection:: *)
(*Function to select observables*)


SelectObservables[sec_] := Switch[Head[ObservableSectors[sec]],
	ObservableSectors,
		If[!MatchQ[ObservableList[sec],{}],
			CheckboxBarSelectAll[
				Dynamic[SelectedObservables[sec]],
				ObservableList[sec],
				ControlPlacement -> Left,
				Appearance -> "Vertical",
				"Label" -> sec,
				Dividers -> All
			],
			Nothing[]
		],
	List,
		Row[{
			AllSelector[
				Dynamic[SelectedObservables[sec]],
				Dynamic/@(SelectedObservables/@(ObservableSectors[sec])),
				"Label" -> sec
			],
			Column[
				SelectObservables/@(ObservableSectors[sec]),
				Frame -> True
			]
			},
			Frame->True
		]
]


(* ::Subsection:: *)
(*Loop over sectors*)


SelectObservables[] := Row[SelectObservables/@ObservableSectors[],Frame->False]


SelectedObservables[] := Join[SelectedObservables/@ObservableSectors[]]
