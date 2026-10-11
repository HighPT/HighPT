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


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


ObservableSectors["Flavor"] := {"ChargedCurrents","\[CapitalDelta]F=1","LFV"(*,"\[CapitalDelta]F=2","\[Tau]LFU"*)};
ObservableList["Flavor"] := ObservableList/@ObservableSectors["Flavor"]
