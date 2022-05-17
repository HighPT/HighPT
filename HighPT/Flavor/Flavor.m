(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Flavor`*)


(* ::Subtitle:: *)
(*Generic definitions for the low-energy part.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["FlavorObservables"]


(*Maybe PackageScope these in the end? *)


PackageExport["SMPrediction"]


PackageExport["SMInfo"]


PackageExport["ExpValue"]


PackageExport["ExpInfo"]


PackageExport["NPContribution"]
PackageExport["NPContributionError"]


PackageExport["NPInfo"]


PackageExport["LowScale"]


PackageExport["LEFTSector"]


PackageExport["WCL"]


PackageExport["ReplaceYukawas"]


PackageExport["ReplaceGaugeCouplings"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["Hypercharge"]


PackageScope["YukawaMatrix"]
PackageScope["YukawaCoupling"]


PackageScope["Nc"]


PackageScope["GaugeCoupling"]


(* ::Chapter:: *)
(*Private:*)


$FlavorSectors = {"ChargedCurrents"};


FlavorObservables[] = FlavorObservables/@$FlavorSectors


(* ::Section:: *)
(*Masses*)


(* ::Subsection:: *)
(*Leptons*)


Mass["e"] = 0.510998928 10^-3
Mass["\[Mu]"] = .105658357
Mass["\[Tau]"] = 1.77682


(* ::Subsection:: *)
(*Quarks*)


(* MSbar, PDG *)


(* at 2 GeV *)
Mass["d"] = 0.00467
Mass["s"] = 0.093
(* at mb *)
Mass["b"] = 4.162


(* at 2 GeV *)
Mass["u"] = 0.00216
(* at mc *)
Mass["c"] = 1.27
(* at mt *)
Mass["t"] = 162.5


(* ::Subsection:: *)
(*Mesons*)


(* from PDG *)


Mass["\!\(\*SuperscriptBox[\(\[Pi]\), \(+\)]\)"] = 0.13957018
Mass["\!\(\*SuperscriptBox[\(\[Pi]\), \(0\)]\)"] = 0.1349766


Mass["\!\(\*SuperscriptBox[\(K\), \(+\)]\)"] = 0.493677
Mass["\!\(\*SuperscriptBox[\(K\), \(0\)]\)"] = 0.497611


Mass["\!\(\*SuperscriptBox[\(D\), \(+\)]\)"] = 1.86962
Mass["\!\(\*SuperscriptBox[\(D\), \(0\)]\)"] = 1.86483
Mass["\!\(\*SubscriptBox[\(D\), \(s\)]\)"] = 1.96849


Mass["\!\(\*SubscriptBox[\(B\), \(d\)]\)"] = 5.27965
Mass["\!\(\*SubscriptBox[\(B\), \(s\)]\)"] = 5.36688
Mass["\!\(\*SubscriptBox[\(B\), \(c\)]\)"] = 6.27447


(* ::Subsection:: *)
(*Higgs*)


Mass["h"] = 125.25


(* ::Section:: *)
(*Replacements*)


ReplaceYukawas = <|
	YukawaCoupling["u"]->0,
	YukawaCoupling["d"]->0,
	YukawaCoupling["s"]->0,
	YukawaCoupling["c"]->Sqrt[2]Mass["c"]/ConstantInput["vev"],
	YukawaCoupling["b"]->Sqrt[2]Mass["b"]/ConstantInput["vev"],
	YukawaCoupling["t"]->Sqrt[2]Mass["t"]/ConstantInput["vev"]
	|>


ReplaceGaugeCouplings = <|
	GaugeCoupling["g1"]->Sqrt[4\[Pi] ConstantInput["\[Alpha]EM"]]/Sqrt[1/2 (1+(1-(4\[Pi] ConstantInput["\[Alpha]EM"])/(Sqrt[2]GF Mass[ZBoson]^2))^(1/2))]/.{GF->1/(Sqrt[2]ConstantInput["vev"]^2)},
	GaugeCoupling["g2"]->2 Sqrt[Sqrt[2]GF] Mass[ZBoson] Sqrt[1/2 (1+(1-(4\[Pi] ConstantInput["\[Alpha]EM"])/(Sqrt[2]GF Mass[ZBoson]^2))^(1/2))]/.{GF->1/(Sqrt[2]ConstantInput["vev"]^2)},
	GaugeCoupling["g3"]->Sqrt[4\[Pi] \[Alpha]sMz]/.\[Alpha]sMz->0.1179
	|>


Nc=3
