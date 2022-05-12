(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`bcSM`*)


(* ::Subtitle:: *)
(*Standard Model predictions for b->c observables*)


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


SMPrediction["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = {0.0208,0.0007};


SMInfo["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = "";


(* ::Section:: *)
(*Semileptonic*)


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Tau]/l)*)


SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] = {0.2938,0.0040};


SMInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Tau]/l)*)


SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] = {0.246,0.009};


SMInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Mu]/e)*)


SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] = {0.99595,0.00011};


SMInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Mu]/e)*)


SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = {0.9953,0.0003};


SMInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = "";
