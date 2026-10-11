(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Higgs`*)


(* ::Subtitle:: *)
(*Implementation of Higgs observables*)


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
(*Signal strengths*)


ObservableList["Higgs"] = {(*"H->bb","H->cc","H->\[Tau]\[Tau]","H->\[Mu]\[Mu]"*)};


(* ::Subsection:: *)
(*H -> bb - TO DO*)


ExpValue$default["H->bb"] := Around[0.99,0.12];


SMPrediction$default["H->bb"] := Around[1,0];


(*NPContribution$default["H->bb"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["b"]) MassRotate[WC["dH",{3,3}],"d"]]^2/.GetParameters[];*)


(* ::Subsection:: *)
(*H -> cc -  TO DO*)


ExpValue$default["H->cc"] := Around[8,22];


SMPrediction$default["H->cc"] := Around[1,0];


(*NPContribution$default["H->cc"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["c"]) MassRotate[WC["uH",{2,2}],"u"]]^2/.GetParameters[];*)


(* ::Subsection:: *)
(*H -> \[Tau]\[Tau] - TO DO*)


ExpValue$default["H->\[Tau]\[Tau]"] := Around[0.91,0.09];


SMPrediction$default["H->\[Tau]\[Tau]"] := Around[1,0];


(*NPContribution$default["H->\[Tau]\[Tau]"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["\[Tau]"]) WC["eH",{3,3}]]^2/.GetParameters[];*)


(* ::Subsection:: *)
(*H -> \[Mu]\[Mu] -  TO DO*)


ExpValue$default["H->\[Mu]\[Mu]"] := Around[1.21,0.35];


SMPrediction$default["H->\[Mu]\[Mu]"] := Around[1,0];


(*NPContribution$default["H->\[Mu]\[Mu]"] := -1 + Abs[1 - Param["vev"]^3/(Sqrt[2]Mass["\[Mu]"]) WC["eH",{2,2}]]^2/.GetParameters[];*)
