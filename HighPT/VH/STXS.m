(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`STXS*)


(* ::Subtitle:: *)
(*STXS observables for VH production*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["BrHtoBB"]
PackageScope["BrZtoLep"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Makes STXS observables visible to ObservableList*)


ObservableList["STXS-ZH"] := "ATLAS-STXS-ZH-bin-" <> ToString[#] & /@ Range[1, 5]
(*ObservableList["STXS-ZH"] := "ATLAS-STXS-WH-bin-" <> ToString[#] & /@ Range[1, 5]*)
ObservableSectors["STXS-VH"] := {"STXS-ZH"}
ObservableList["STXS-VH"] := ObservableList /@ ObservableSectors["STXS-VH"]


(* ::Section:: *)
(*Branching ratios needed*)


(* Higgs Br to bbbar *)
BrHtoBB = 0.58
(* Z Br to leptons including taus and invisibles *)
BrZtoLep = (3 * 3.36 + 20) * 0.01
(* pb to fb *)
pbtofb = 1000
(* min value for integration *)
minZH = 0.1 + (Mass["ZBoson"] + Mass["H"]) /. GetParameters[]


(* ::Section:: *)
(*STXS ATLAS ZH*)


(* ::Subsubsection:: *)
(*bin 1 ( Subscript[p, T] : 75 GeV - 150 GeV)*)


ExpValue$default["ATLAS-STXS-ZH-bin-1"]       := Around[51, 32] (* in fb *)
SMPrediction$default["ATLAS-STXS-ZH-bin-1"]   := Around[50.7, 3.9] (* in fb *)
TheoryExpression["ATLAS-STXS-ZH-bin-1"]       := pbtofb * BrHtoBB * BrZtoLep * CrossSectionVH[MVHcuts -> {minZH, 13000}, PTcuts-> {75, 150}] (* in fb *)
NPContribution$default["ATLAS-STXS-ZH-bin-1"] := NPFromTheoryExpression["ATLAS-STXS-ZH-bin-1"]
LowScale$default["ATLAS-STXS-ZH-bin-1"]       := 4 * 75 (* mVH APRROX 4 pTV*)


(* ::Subsubsection:: *)
(*bin 2 ( Subscript[p, T] : 150 GeV - 250 GeV)*)


ExpValue$default["ATLAS-STXS-ZH-bin-2"]       := Around[17.7, 5.8] (* in fb *)
SMPrediction$default["ATLAS-STXS-ZH-bin-2"]   := Around[18.7, 3.5] (* in fb *)
TheoryExpression["ATLAS-STXS-ZH-bin-2"]       := pbtofb * BrHtoBB * BrZtoLep * CrossSectionVH[MVHcuts -> {minZH, 13000}, PTcuts-> {150, 250}] (* in fb *)
NPContribution$default["ATLAS-STXS-ZH-bin-2"] := NPFromTheoryExpression["ATLAS-STXS-ZH-bin-2"]
LowScale$default["ATLAS-STXS-ZH-bin-2"]       := 4 * 150 (* mVH APRROX 4 pTV*)


(* ::Subsubsection:: *)
(*bin 3 ( Subscript[p, T] : 250 GeV - 400 GeV)*)


ExpValue$default["ATLAS-STXS-ZH-bin-3"]       := Around[3.5, 1.5] (* in fb *)
SMPrediction$default["ATLAS-STXS-ZH-bin-3"]   := Around[4.15, 0.45] (* in fb *)
TheoryExpression["ATLAS-STXS-ZH-bin-3"]       := pbtofb * BrHtoBB * BrZtoLep * CrossSectionVH[MVHcuts -> {minZH, 13000}, PTcuts-> {250, 400}] (* in fb *)
NPContribution$default["ATLAS-STXS-ZH-bin-3"] := NPFromTheoryExpression["ATLAS-STXS-ZH-bin-3"]
LowScale$default["ATLAS-STXS-ZH-bin-3"]       := 4 * 250 (* mVH APRROX 4 pTV*)


(* ::Subsubsection:: *)
(*bin 4 ( Subscript[p, T] : 400 GeV - 600 GeV)*)


ExpValue$default["ATLAS-STXS-ZH-bin-4"]       := Around[0.61, 0.4] (* in fb *)
SMPrediction$default["ATLAS-STXS-ZH-bin-4"]   := Around[0.62, 0.05] (* in fb *)
TheoryExpression["ATLAS-STXS-ZH-bin-4"]       := pbtofb * BrHtoBB * BrZtoLep * CrossSectionVH[MVHcuts -> {minZH, 13000}, PTcuts-> {400, 600}] (* in fb *)
NPContribution$default["ATLAS-STXS-ZH-bin-4"] := NPFromTheoryExpression["ATLAS-STXS-ZH-bin-4"]
LowScale$default["ATLAS-STXS-ZH-bin-4"]       := 4 * 400 (* mVH APRROX 4 pTV*)


(* ::Subsubsection:: *)
(*bin 5 ( Subscript[p, T] : > 600 GeV)*)


ExpValue$default["ATLAS-STXS-ZH-bin-5"]       := Around[-0.1, 0.12] (* in fb *)
SMPrediction$default["ATLAS-STXS-ZH-bin-5"]   := Around[0.11, 0.01] (* in fb *)
TheoryExpression["ATLAS-STXS-ZH-bin-5"]       := pbtofb * BrHtoBB * BrZtoLep * CrossSectionVH[MVHcuts -> {minZH, 13000}, PTcuts-> {600, \[Infinity]}] (* in fb *)
NPContribution$default["ATLAS-STXS-ZH-bin-5"] := NPFromTheoryExpression["ATLAS-STXS-ZH-bin-5"]
LowScale$default["ATLAS-STXS-ZH-bin-5"]       := 4 * 600 (* mVH APRROX 4 pTV *)
