(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`EW`*)


(* ::Subtitle:: *)
(*General implementation of EW pole observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["EWObservables"]


PackageExport["RestoreEWObservables"]


PackageExport["ChangeEWObservable"]


PackageExport["\[Delta]gZ"]
PackageExport["\[Delta]gW"]
PackageExport["\[Delta]mW"]


PackageExport["FCCee"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["gZSM"]


PackageScope["Replace\[Delta]g"]


(*PackageScope["ExpValue$FCCee"]*)


PackageScope["null"]


(* ::Chapter:: *)
(*Private:*)


$EWSectors = {"Zpole","Wpole"};


EWObservables::usage = "EWObservables[] returns a nested list of all the pole observables implemented in HighPT. EWObservables[\"sector\"] gives a list of all pole observables in the sector \"sector\". \"sector\" can be \"Zpole\" or \"Wpole\""


EWObservables[] = EWObservables/@$EWSectors


(* ::Section:: *)
(*Check Observable implementation*)


EWOptionCheck::optionvalue= "Invalid OptionValue specified: `1`\[Rule]`2`, the allowed values for `1` must match `3`.";


EWOptionCheck[opt_,optVal_]:=If[!MatchQ[optVal,$EWOptionValueAssociation[opt]],
	Message[EWOptionCheck::optionvalue, opt, optVal, $EWOptionValueAssociation[opt]];
	Abort[],
	True
];


$EWOptionValueAssociation= <|
	"Exp" -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ}] | _?((NumericQ[#]&&NonNegative[#])&),
	"SM" -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ}]  | _?((NumericQ[#]&&NonNegative[#])&)
|>;


(* ::Section:: *)
(*Redefine an EW observable*)


ChangeEWObservable::invalidNP= "Invalid NP contribution for `1`. It must be an expressions of SMEFT coefficients (WC) or \[Delta]gs (\[Delta]gW,\[Delta]gZ,\[Delta]mW) only."
ChangeEWObservable::wrongobservable= "The observable `1` doesn't exist."
ChangeEWObservable::rescaleerror= "The rescaling factor must be a positive number"


Options[ChangeEWObservable]={
	"Exp"->"current",
	"SM"->"current",
	"NP"->"current",
	RescaleError -> 1
	};


ChangeEWObservable[obs_,Default] := ChangeEWObservable[
	obs,
	"Exp"->ExpValue$default[obs],
	"SM"->SMPrediction$default[obs],
	"NP"->NPContribution$default[obs]
]


FCCProjections = Join[FCCZpoleProjections,FCCWpoleProjections];


ChangeEWObservable[obs_,FCCee] := ChangeEWObservable[
	obs,
	"Exp"->Around[SMPrediction[obs]["Value"],FCCProjections[obs]],
	"SM"->"current",
	"NP"->"current"
]


ChangeEWObservable[obs_,OptionsPattern[]] := Module[
	{
		exp = OptionValue["Exp"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]], 
		SM = OptionValue["SM"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]], 
		NP = OptionValue["NP"],
		var
	}
	,
	If[!MemberQ[EWObservables[]//Flatten,obs],
		Message[ChangeEWObservable::wrongobservable,obs];Abort[];
	];
	If[(!MatchQ[exp,"current"])&&(EWOptionCheck["Exp",exp]),
		If[NumericQ[exp],
			ExpValue[obs] = Around[exp,null],
			ExpValue[obs] = exp
		];
	];
	If[(!MatchQ[SM,"current"])&&(EWOptionCheck["SM",SM]),
		If[NumericQ[SM],
			SMPrediction[obs] = Around[SM,null],
			SMPrediction[obs] = SM
		];
	];
	If[!MatchQ[NP,"current"],
		var=Variables[NP/.Re->Identity/.Abs->Identity/.SMEFTSimplify/.Conjugate[a_]->a];
		(*Print[var];
		Print[(Head/@var)//DeleteDuplicates];*)
		If[SubsetQ[{\[Delta]gZ,\[Delta]gW,\[Delta]mW(*,WC*)},(Head/@var)//DeleteDuplicates]||MatchQ[(Head/@var)//DeleteDuplicates,{WC}],
			NPContribution[obs] = NP,
			Message[ChangeEWObservable::invalidNP,obs];Abort[]
		];
	];
	If[!MatchQ[OptionValue[RescaleError],1],
		If[NumericQ[OptionValue[RescaleError]]&&Positive[OptionValue[RescaleError]],
			ExpValue[obs] = Around[SMPrediction[obs]["Value"],ExpValue[obs]["Uncertainty"]/OptionValue[RescaleError]];
			(*SMPrediction[obs] = SMPrediction$default[obs];
			NPContribution[obs]= NPContribution$default[obs];*),
			Message[ChangeEWObservable::rescaleerror];Abort[]
		];
	];
]


RestoreEWObservables[]:=ChangeEWObservable[#,Default]& /@ (EWObservables[]//Flatten);


RestoreEWObservables[FCCee]:=ChangeEWObservable[#,FCCee]& /@ (EWObservables[]//Flatten);


(* ::Section:: *)
(*Replace \[Delta]gs*)


\[Delta]U[f_,chir_]:=-Param["vev"]^2(WeakIsospin3[f,chir]+Charge[f] Param["g1"]^2/(Param["g2"]^2-Param["g1"]^2))(1/4 WC["HD",{}]+1/2 WC["Hl3",{2,2}]+1/2 WC["Hl3",{1,1}]-1/2 WC["ll",{1,2,2,1}])-Param["vev"]^2 Charge[f] (Param["g1"]Param["g2"])/(Param["g2"]^2-Param["g1"]^2) WC["HWB",{}]


Replace\[Delta]g={
	\[Delta]gZ[\[Nu],Left,{\[Alpha]_,\[Beta]_}]:>-(Param["vev"]^2/2)(WC["Hl1",{\[Alpha],\[Beta]}]-WC["Hl3",{\[Alpha],\[Beta]}])+\[Delta]U[\[Nu],Left] KroneckerDelta[\[Alpha],\[Beta]],
	\[Delta]gZ[e,Left,{\[Alpha]_,\[Beta]_}]:>-(Param["vev"]^2/2)(WC["Hl1",{\[Alpha],\[Beta]}]+WC["Hl3",{\[Alpha],\[Beta]}])+\[Delta]U[e,Left] KroneckerDelta[\[Alpha],\[Beta]],
	\[Delta]gZ[e,Right,{\[Alpha]_,\[Beta]_}]:>-(Param["vev"]^2/2)WC["He",{\[Alpha],\[Beta]}]+\[Delta]U[e,Right] KroneckerDelta[\[Alpha],\[Beta]],
	\[Delta]gZ[u,Left,{i_,j_}]:>-(Param["vev"]^2/2)(MassRotate[WC["Hq1",{i,j}],"uu"]-MassRotate[WC["Hq3",{i,j}],"uu"])+\[Delta]U[u,Left] KroneckerDelta[i,j],
	\[Delta]gZ[u,Right,{i_,j_}]:>-(Param["vev"]^2/2)WC["Hu",{i,j}]+\[Delta]U[u,Right] KroneckerDelta[i,j],
	\[Delta]gZ[d,Left,{i_,j_}]:>-(Param["vev"]^2/2)(MassRotate[WC["Hq1",{i,j}],"dd"]+MassRotate[WC["Hq3",{i,j}],"dd"])+\[Delta]U[d,Left] KroneckerDelta[i,j],
	\[Delta]gZ[d,Right,{i_,j_}]:>-(Param["vev"]^2/2)WC["Hd",{i,j}]+\[Delta]U[d,Right] KroneckerDelta[i,j],
	\[Delta]gW["q",Left,{i_,j_}]:>Param["vev"]^2 MassRotate[WC["Hq1",{i,j}],"ud"]+\[Delta]U[u,Left] KroneckerDelta[i,j]-\[Delta]U[d,Left] KroneckerDelta[i,j],
	\[Delta]gW["q",Right,{i_,j_}]:>-(Param["vev"]^2/2)WC["Hud",{i,j}]+\[Delta]U[u,Right] KroneckerDelta[i,j]-\[Delta]U[d,Right] KroneckerDelta[i,j],
	\[Delta]gW["l",Left,{\[Alpha]_,\[Beta]_}]:>Param["vev"]^2 WC["Hl3",{\[Alpha],\[Beta]}]+\[Delta]U[\[Nu],Left] KroneckerDelta[\[Alpha],\[Beta]]-\[Delta]U[e,Left] KroneckerDelta[\[Alpha],\[Beta]],
	\[Delta]mW[]:>-((Param["vev"]^2 Param["g2"]^2)/(4(Param["g2"]^2-Param["g1"]^2)))WC["HD",{}]-(Param["vev"]^2 Param["g2"]Param["g1"])/(Param["g2"]^2-Param["g1"]^2) WC["HWB",{}]+(Param["vev"]^2 Param["g1"]^2)/(4(Param["g2"]^2-Param["g1"]^2)) (WC["ll",{1,2,2,1}]-2 WC["Hl3",{2,2}]-2 WC["Hl3",{1,1}])
};
