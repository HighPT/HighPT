(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Higgs`*)


(* ::Subtitle:: *)
(*Implementation of Higgs observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["HiggsObservables"]


PackageExport["RestoreHiggsObservables"]


PackageExport["ChangeHiggsObservable"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Check Observable implementation*)


HiggsOptionCheck::optionvalue= "Invalid OptionValue specified: `1`\[Rule]`2`, the allowed values for `1` must match `3`.";


HiggsOptionCheck[opt_,optVal_]:=If[!MatchQ[optVal,$HiggsOptionValueAssociation[opt]],
	Message[HiggsOptionCheck::optionvalue, opt, optVal, $HiggsOptionValueAssociation[opt]];
	Abort[],
	True
];


$HiggsOptionValueAssociation= <|
	"Exp" -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ}] | _?((NumericQ[#]&&NonNegative[#])&),
	"SM" -> Around[_?NumericQ,_?NumericQ | {_?NumericQ,_?NumericQ}]  | _?((NumericQ[#]&&NonNegative[#])&)
|>;


(* ::Section:: *)
(*Redefine a Higgs observable*)


ChangeHiggsObservable::invalidNP= "Invalid NP contribution for `1`. It must be an expressions of SMEFT coefficients (WC) only."
ChangeHiggsObservable::wrongobservable= "The observable `1` doesn't exist."
ChangeHiggsObservable::rescaleerror= "The rescaling factor must be a positive number"


Options[ChangeHiggsObservable]={
	"Exp"->"current",
	"SM"->"current",
	"NP"->"current",
	RescaleError -> 1
	};


ChangeHiggsObservable[obs_,Default] := ChangeHiggsObservable[
	obs,
	"Exp"->ExpValue$default[obs],
	"SM"->SMPrediction$default[obs],
	"NP"->NPContribution$default[obs]
]


ChangeHiggsObservable[obs_,OptionsPattern[]] := Module[
	{
		exp = OptionValue["Exp"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]], 
		SM = OptionValue["SM"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]], 
		NP = OptionValue["NP"],
		var
	}
	,
	If[!MemberQ[HiggsObservables[]//Flatten,obs],
		Message[ChangeHiggsObservable::wrongobservable,obs];Abort[];
	];
	If[(!MatchQ[exp,"current"])&&(HiggsOptionCheck["Exp",exp]),
		If[NumericQ[exp],
			ExpValue[obs] = Around[exp,null],
			ExpValue[obs] = exp
		];
	];
	If[(!MatchQ[SM,"current"])&&(HiggsOptionCheck["SM",SM]),
		If[NumericQ[SM],
			SMPrediction[obs] = Around[SM,null],
			SMPrediction[obs] = SM
		];
	];
	If[!MatchQ[NP,"current"],
		var=Variables[NP/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a];
		(*Print[var];
		Print[(Head/@var)//DeleteDuplicates];*)
		If[MatchQ[(Head/@var)//DeleteDuplicates,{WC}],
			NPContribution[obs] = NP,
			Message[ChangeHiggsObservable::invalidNP,obs];Abort[]
		];
	];
	If[!MatchQ[OptionValue[RescaleError],1],
		If[NumericQ[OptionValue[RescaleError]]&&Positive[OptionValue[RescaleError]],
			ExpValue[obs] = Around[SMPrediction[obs]["Value"],ExpValue[obs]["Uncertainty"]/OptionValue[RescaleError]];
			(*SMPrediction[obs] = SMPrediction$default[obs];
			NPContribution[obs]= NPContribution$default[obs];*),
			Message[ChangeHiggsObservable::rescaleerror];Abort[]
		];
	];
]


RestoreHiggsObservables[]:=ChangeHiggsObservable[#,Default]& /@ (HiggsObservables[]//Flatten);


(* ::Section:: *)
(*Observables*)


HiggsObservables[] = {"H->bb","H->cc","H->\[Tau]\[Tau]","H->\[Mu]\[Mu]"};


(* ::Subsection:: *)
(*H -> bb*)


ExpValue$default["H->bb"] := Around[0.99,0.12];


SMPrediction$default["H->bb"] := Around[1,0];


NPContribution$default["H->bb"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["b"]) MassRotate[WC["dH",{3,3}],"d"]]^2/.GetParameters[];


(* ::Subsection:: *)
(*H -> cc*)


ExpValue$default["H->cc"] := Around[8,22];


SMPrediction$default["H->cc"] := Around[1,0];


NPContribution$default["H->cc"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["c"]) MassRotate[WC["uH",{2,2}],"u"]]^2/.GetParameters[];


(* ::Subsection:: *)
(*H -> \[Tau]\[Tau]*)


ExpValue$default["H->\[Tau]\[Tau]"] := Around[0.91,0.09];


SMPrediction$default["H->\[Tau]\[Tau]"] := Around[1,0];


NPContribution$default["H->\[Tau]\[Tau]"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["\[Tau]"]) WC["eH",{3,3}]]^2/.GetParameters[];


(* ::Subsection:: *)
(*H -> \[Mu]\[Mu]*)


ExpValue$default["H->\[Mu]\[Mu]"] := Around[1.21,0.35];


SMPrediction$default["H->\[Mu]\[Mu]"] := Around[1,0];


NPContribution$default["H->\[Mu]\[Mu]"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["\[Mu]"]) WC["eH",{2,2}]]^2/.GetParameters[];
