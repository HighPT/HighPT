(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`DF1`*)


(* ::Subtitle:: *)
(*\[CapitalDelta]F=1 observables*)


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


$\[CapitalDelta]F1Sectors={(*"b->sll"*)(*,"b->s\[Nu]\[Nu]","leptonic"*)};


(*FlavorObservables["\[CapitalDelta]F=1"] = FlavorObservables/@$\[CapitalDelta]F1Sectors*)


(* ::Section:: *)
(*b -> sll (')*)


FlavorObservables["b->sll"] = {"B->K\[Tau]\[Tau]"};


(* ::Subsection:: *)
(*B -> K\[Mu]\[Mu]*)


(* ::Subsection:: *)
(*B -> K\[Tau]\[Tau]*)


ExpValue$default["B->K\[Tau]\[Tau]"] = {0,2.25}*10^-3


{0.0001003548981731836`,2.140854596647234`*^-6};
SMPrediction["B\[Rule]K\[Tau]\[Tau]"]=(Vckm[3,3]Vckm[3,2])^2 %[[1]]{1,Sqrt[(*(((2VtbVts0[[2]])/VtbVts0[[1]])^2)+*)(%[[2]]/%[[1]])^2]}


(* ::Subsection:: *)
(*B -> K*\[Tau]\[Tau]*)


(* ::Subsection:: *)
(*B -> K\[Mu]\[Tau]*)


(* ::Subsection:: *)
(*B -> K*\[Mu]-\[Tau]+*)


(* ::Subsection:: *)
(*B -> K*\[Mu]+\[Tau]-*)


(* ::Section:: *)
(*Subscript[B, s]->ll*)


(* ::Subsection:: *)
(*Subscript[B, s]->\[Mu]\[Mu]*)


(* ::Subsection:: *)
(*Subscript[B, s]->\[Tau]\[Tau]*)


(* ::Section:: *)
(*Subscript[B, d]->ll*)


(* ::Subsection:: *)
(*Subscript[B, d]->\[Mu]\[Mu]*)


(* ::Section:: *)
(*b -> s\[Nu]\[Nu]*)


(* ::Subsection:: *)
(*B -> K\[Nu]\[Nu]*)


(* ::Subsection:: *)
(*B -> K*\[Nu]\[Nu]*)
