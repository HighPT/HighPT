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


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


ObservableSectors["EW"] := {"Zpole","Wpole"}
ObservableList["EW"] := ObservableList/@ObservableSectors["EW"]
