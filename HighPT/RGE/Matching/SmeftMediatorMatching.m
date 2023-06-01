(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SmeftMediatorMatching`*)


(* ::Subtitle:: *)
(*Match the NP mediators to the SMEFT at tree-level and dimension 6*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["MatchMediator"]


(* ::Chapter:: *)
(*Private:*)


MediatorMatchingConditions= <|
	"S1"->{},
	"S1t"->{},
	"U1"->{	
		WC["lq1",{\[Alpha]_,\[Beta]_,i_,j_}]->-(1/2)Coupling["x1L",{i,\[Beta]}]Coupling["x1L",{j,\[Alpha]}]\[Conjugate],
		WC["lq3",{\[Alpha]_,\[Beta]_,i_,j_}]->-(1/2)Coupling["x1L",{i,\[Beta]}]Coupling["x1L",{j,\[Alpha]}]\[Conjugate],
		WC["ed",{\[Alpha]_,\[Beta]_,i_,j_}]->-Coupling["x1R",{i,\[Beta]}]Coupling["x1R",{j,\[Alpha]}]\[Conjugate],
		WC["ledq",{\[Alpha]_,\[Beta]_,i_,j_}]->2Coupling["x1R",{j,\[Beta]}]Coupling["x1L",{i,\[Alpha]}]\[Conjugate]
		},
	"U1t"->{},
	"R2"->{},
	"R2t"->{},
	"V2"->{},
	"V2t"->{},
	"S3"->{},
	"U3"->{}
|>


MatchMediator[mediator_]:=MediatorMatchingConditions[mediator]
