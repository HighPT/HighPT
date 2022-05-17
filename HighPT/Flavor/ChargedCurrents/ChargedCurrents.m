(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChargedCurrents`*)


(* ::Subtitle:: *)
(*Generalities about charged current flavor observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["coeffPPplnu"]
PackageScope["coeffPVlnu"]


PackageScope["RepSemilep"]
PackageScope["ind"]


PackageScope["gVL"]
PackageScope["gVR"]
PackageScope["gV"]
PackageScope["gA"]
PackageScope["gS"]
PackageScope["gP"]
PackageScope["gT"]


(* ::Chapter:: *)
(*Private:*)


$ChargedCurrentSectors={"b->c"};


FlavorObservables["ChargedCurrents"] = FlavorObservables/@$ChargedCurrentSectors


(* ::Section:: *)
(*LEFT Sector*)


LEFTSector[Alternatives@@(FlavorObservables["ChargedCurrents"]//Flatten)] = "cc"
