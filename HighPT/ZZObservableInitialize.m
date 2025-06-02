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


(*RestoreFlavorObservables[];*)


(*RestoreEWObservables[];*)


(*RestoreHiggsObservables[];*)


RestoreObservables[Flatten[Join[
	(*ObservableList["FCC"],
	ObservableList["EW"],*)
	ObservableList["Flavor"]
	]]
];


selector = SelectObservables[];


Table[SelectedObservables[sec] = ObservableList[sec], {sec,ObservableSectors[]}]


(*DefineRedefinitions[Default];*)
