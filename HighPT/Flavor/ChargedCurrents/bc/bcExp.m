(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`bcExp`*)


(* ::Subtitle:: *)
(*Experimental information about b->c observables*)


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


(* ::Section:: *)
(*Leptonic*)


(* ::Subsection:: *)
(*B(Subscript[B, c]->\[Tau]\[Nu])*)


ExpValue["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = {0,0.3};


ExpInfo["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = "";


(* ::Section:: *)
(*Semileptonic*)


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Tau]/l)*)


ExpValue["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] = {0.340,Sqrt[0.027^2+0.013^2]};


ExpInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Tau]/l)*)


ExpValue["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] = {0.295,Sqrt[0.011^2+0.008^2]};


ExpInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Mu]/e)*)


ExpValue["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] = {1.005,0.045};


ExpInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Mu]/e)*)


ExpValue["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = {0.962,0.047};


ExpInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = "";
