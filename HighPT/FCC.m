(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`FCC`*)


(* ::Subtitle:: *)
(*FCC projections for observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["FCCObservables"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["FCCRelErr"]


(* ::Chapter:: *)
(*Private:*)


(*FCCSectors = {"FCCZpole","FCCWW"};*)


ObservableSectors["FCC"] := {"FCCZpole","FCCWW"};


(*FCCObservables[] := FCCObservables/@FCCSectors*)


ObservableList["FCC"] := ObservableList/@ObservableSectors["FCC"]


(*SelectObservables["FCC"] := Row[{
	AllSelector[
		Dynamic[SelectedObservables["FCC"]],
		Dynamic/@(SelectedObservables/@ObservableSectors["FCC"]),
		"Label"->"FCC"
	],
	Column[
		SelectObservables/@ObservableSectors["FCC"],
		Frame\[Rule]True
	]
	},
	Frame\[Rule]True
]*)


(* ::Section:: *)
(*Z (91 GeV)*)


(*FCCObservables["FCCZpole"] := {"\[CapitalGamma]ZFCC","\[Sigma]hadFCC","RbFCC","RcFCC","ReFCC","R\[Mu]FCC","R\[Tau]FCC","AbFCC","AcFCC","AeFCC","A\[Mu]FCC","A\[Tau]FCC"};*)


ObservableList["FCCZpole"] := {"\[CapitalGamma]ZFCC","\[Sigma]hadFCC","RbFCC","RcFCC","ReFCC","R\[Mu]FCC","R\[Tau]FCC","AbFCC","AcFCC","AeFCC","A\[Mu]FCC","A\[Tau]FCC"};


(*SelectedObservables["FCCZpole"] = FCCObservables["FCCZpole"]*)


LowScale$default[Alternatives@@ObservableList["FCCZpole"]] := \[Mu]EW;


(*SelectObservables["FCCZpole"] := CheckboxBarSelectAll[
	Dynamic[SelectedObservables["FCCZpole"]],
	ObservableList["FCCZpole"],
	ControlPlacement -> Left,
	Appearance -> "Vertical",
	"Label" -> "FCCZpole",
	Dividers -> All
]*)


(* ::Subsection:: *)
(*Projections*)


FCCRelErr["\[CapitalGamma]ZFCC"] := 1*10^-5
FCCRelErr["\[Sigma]hadFCC"] := 9.6*10^-5
FCCRelErr["RbFCC"] := 3*10^-4
FCCRelErr["RcFCC"] := 1.5*10^-3
FCCRelErr["ReFCC"] := 3*10^-4
FCCRelErr["R\[Mu]FCC"] := 5*10^-5
FCCRelErr["R\[Tau]FCC"] := 1*10^-4
FCCRelErr["AbFCC"] := 2.3*10^-4
FCCRelErr["AcFCC"] := 3.4*10^-4
FCCRelErr["AeFCC"] := 1.4*10^-4
FCCRelErr["A\[Mu]FCC"] := 2*10^-4
FCCRelErr["A\[Tau]FCC"] := 1.4*10^-3


(* ::Subsection:: *)
(*Construct observables*)


Table[
	ExpValue$default[i] := Evaluate[SMPrediction$default[StringReplace[i,"FCC"->"NEW"]]["Value"]*Around[1,FCCRelErr[i]]];
	SMPrediction$default[i] := Evaluate[SMPrediction$default[StringReplace[i,"FCC"->"NEW"]]];
	NPContribution$default[i] := Evaluate[NPContribution$default[StringReplace[i,"FCC"->"NEW"]]];
	,
	{i,ObservableList["FCCZpole"]}
];


(* ::Section:: *)
(*WW (163 GeV)*)


(*FCCObservables["FCCWW"] := {"mWFCC","\[CapitalGamma]WFCC","W->e\[Nu]FCC","W->\[Mu]\[Nu]FCC","W->\[Tau]\[Nu]FCC"};*)


ObservableList["FCCWW"] := {"mWFCC","\[CapitalGamma]WFCC","W->e\[Nu]FCC","W->\[Mu]\[Nu]FCC","W->\[Tau]\[Nu]FCC"};


(*SelectedObservables["FCCWW"] = FCCObservables["FCCWW"]*)


LowScale$default[Alternatives@@ObservableList["FCCWW"]] := \[Mu]EW;


(*SelectObservables["FCCWW"] := CheckboxBarSelectAll[
	Dynamic[SelectedObservables["FCCWW"]],
	ObservableList["FCCWW"],
	ControlPlacement -> Left,
	Appearance -> "Vertical",
	"Label" -> "FCCWW",
	Dividers -> All
]*)


(* ::Subsection:: *)
(*Projections*)


FCCRelErr["mWFCC"] := 4.6*10^-6
FCCRelErr["\[CapitalGamma]WFCC"] := 5.1*10^-4
FCCRelErr["W->e\[Nu]FCC"] := 3*10^-4
FCCRelErr["W->\[Mu]\[Nu]FCC"] := 3*10^-4
FCCRelErr["W->\[Tau]\[Nu]FCC"] := 3*10^-4


(* ::Subsection:: *)
(*Construct observables*)


Table[
	ExpValue$default[i] := Evaluate[SMPrediction$default[StringReplace[i,"FCC"->"NEW"]]["Value"]*Around[1,FCCRelErr[i]]];
	SMPrediction$default[i] := Evaluate[SMPrediction$default[StringReplace[i,"FCC"->"NEW"]]];
	NPContribution$default[i] := Evaluate[NPContribution$default[StringReplace[i,"FCC"->"NEW"]]];
	,
	{i,ObservableList["FCCWW"]}
];


(* ::Section:: *)
(*ZH (240 GeV)*)


(* ::Section:: *)
(*tt (365 GeV)*)
