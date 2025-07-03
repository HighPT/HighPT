(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ZZObservableInitialize`*)


(* ::Subtitle:: *)
(*Initialize observables to their default values*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


DefineRedefinitions[Default];


paramlist = Keys[GetParameters[]];
Table[
	If[!MatchQ[Head[Info$default[i]],Info$default],
		Info[i] := Info$default[i]//Evaluate
	],
	{i,paramlist}
];


(*RestoreFlavorObservables[];*)


(*RestoreEWObservables[];*)


(*RestoreHiggsObservables[];*)


(*RestoreObservables[Flatten[Join[
	(*ObservableList["FCC"],*)
	ObservableList["EW"],
	ObservableList["Flavor"]
	]]
];*)


Print["Initializing EW observables..."]
RestoreObservables[ObservableList["EW"]]
Print["Initializing Flavor observables..."]
RestoreObservables[ObservableList["Flavor"]]


selector = SelectObservables[];


Table[SelectedObservables[sec] = ObservableList[sec], {sec,ObservableSectors[]}]
