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


PackageExport["Obs"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["SMPrediction"]


PackageScope["SMInfo"]


PackageScope["ExpValue"]


PackageScope["ExpInfo"]


PackageScope["NPContribution"]
PackageScope["NPContributionError"]


PackageScope["NPInfo"]


PackageScope["ExpCorrelation"]


PackageScope["LowScale"]


(* ::Chapter:: *)
(*Private:*)


$FlavorSectors = {"ChargedCurrents"};


FlavorObservables[] = FlavorObservables/@$FlavorSectors


(* ::Section:: *)
(*Observables*)


Default[ExpCorrelation,0];


ExpInfo[_]="";
SMInfo[_]="";
NPInfo[_]="";


Obs[label_] := <|
	"Exp"->ExpValue[label],
	"SM"->SMPrediction[label],
	"NP"->NPContribution[label],
	"\[Sigma]NP"->NPContributionError[label],
	"info"->"Exp: "<>ExpInfo[label]<>"
SM: "<>SMInfo[label]<>"
NP: "<>NPInfo[label]
	|>
