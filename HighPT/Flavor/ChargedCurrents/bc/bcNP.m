(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`bcNP`*)


(* ::Subtitle:: *)
(*New Physics contributions to b->c observables*)


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


NPContribution["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = Abs[1-(WCL["VRudl\[Nu]",ind]-WCL["VLudl\[Nu]",ind])+(WCL["SRudl\[Nu]",ind]-WCL["SLudl\[Nu]",ind]) Mass["\!\(\*SubscriptBox[\(B\), \(c\)]\)"]^2/(Mass["\[Tau]"] (Mass["c"]+Mass["b"]))]^2-1/.ind->{2,3,3,3};


NPInfo["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = "";


(* ::Section:: *)
(*Semileptonic*)


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Tau]/l)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] = 0;


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Tau]/l)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] = 0;


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Mu]/e)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] = 0;


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Mu]/e)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = 0;


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = "";
