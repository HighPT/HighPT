(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`bc`*)


(* ::Subtitle:: *)
(*Generalities about b->c observables*)


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


$btocSectors={"b->c,leptonic","b->c,semileptonic"}


FlavorObservables["b->c"] = FlavorObservables/@$btocSectors


(* ::Section:: *)
(*Leptonic*)


FlavorObservables["b->c,leptonic"] = {"B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"}


(* ::Section:: *)
(*Semileptonic*)


FlavorObservables["b->c,semileptonic"] = {
	"\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)",
	"\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)",
	"\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)",
	"\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"
	};


(* ::Section:: *)
(*Scale*)


LowScale[Alternatives@@(FlavorObservables["b->c"]//Flatten)] = "\[Mu]b"
