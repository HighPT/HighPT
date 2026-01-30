(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`TauLFU`*)


(* ::Subtitle:: *)
(*Template .m file*)


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


FlavorObservables["\[Tau]LFU"] = {"g\[Tau]/g\[Mu]","g\[Tau]/ge","g\[Tau]/g\[Mu]|\[Pi]","g\[Tau]/g\[Mu]|K"}


ObsTable["\[Tau]LFU"] := Grid[{{"\[Tau]LFU",Column[FlavorObservables["\[Tau]LFU"]]}},Dividers->All];


LowScale[Alternatives@@(FlavorObservables["\[Tau]LFU"]//Flatten)] := Mass["\[Tau]"]/.GetParameters[];


(* ::Section:: *)
(*g\[Tau]/g\[Mu]*)


ExpValue$default["g\[Tau]/g\[Mu]"] := Around[1.0009,0.0014]


SMPrediction$default["g\[Tau]/g\[Mu]"] := 1


NPContribution$default["g\[Tau]/g\[Mu]"] := -(Param["vev"]^2/2)Re[WCL["\[Nu]eVLL",{3,1,1,3}]-WCL["\[Nu]eVLL",{2,1,1,2}]]/.GetParameters[]


(* ::Section:: *)
(*g\[Tau]/ge*)


ExpValue$default["g\[Tau]/ge"] := Around[1.0027,0.0014]


SMPrediction$default["g\[Tau]/ge"] := 1


NPContribution$default["g\[Tau]/ge"] := -(Param["vev"]^2/2)Re[WCL["\[Nu]eVLL",{3,2,2,3}]-WCL["\[Nu]eVLL",{2,1,1,2}]]/.GetParameters[]


(* ::Section:: *)
(*g\[Tau]/g\[Mu] | \[Pi]*)


ExpValue$default["g\[Tau]/g\[Mu]|\[Pi]"] := Around[0.9959,0.0038]


SMPrediction$default["g\[Tau]/g\[Mu]|\[Pi]"] := 1


NPContribution$default["g\[Tau]/g\[Mu]|\[Pi]"] := -(Param["vev"]^2/2)Re[1/Vckm[1,1]\[Conjugate] (WCL["\[Nu]eduVLL",{3,3,1,1}]-WCL["\[Nu]eduVLL",{2,2,1,1}]+Mass["\[Pi]0"]^2/(Mass["\[Tau]"](Mass["d"]+Mass["u"])) WCL["\[Nu]eduSRL",{3,3,1,1}]-Mass["\[Pi]+"]^2/(Mass["\[Mu]"](Mass["d"]+Mass["u"])) WCL["\[Nu]eduSRL",{2,2,1,1}])]/.GetParameters[]


(* ::Section:: *)
(*g\[Tau]/g\[Mu] | K*)


ExpValue$default["g\[Tau]/g\[Mu]|K"] := Around[0.9855,0.0075]


SMPrediction$default["g\[Tau]/g\[Mu]|K"] := 1


NPContribution$default["g\[Tau]/g\[Mu]|K"] := -(Param["vev"]^2/2)Re[1/Vckm[1,2]\[Conjugate] (WCL["\[Nu]eduVLL",{3,3,2,1}]-WCL["\[Nu]eduVLL",{2,2,2,1}]+Mass["K0"]^2/(Mass["\[Tau]"](Mass["s"]+Mass["u"])) WCL["\[Nu]eduSRL",{3,3,2,1}]-Mass["K+"]^2/(Mass["\[Mu]"](Mass["s"]+Mass["u"])) WCL["\[Nu]eduSRL",{2,2,2,1}])]/.GetParameters[]


(* ::Section:: *)
(*Exp . correlations*)


ExpCorrelation["g\[Tau]/g\[Mu]","g\[Tau]/ge"] := 0.51


ExpCorrelation["g\[Tau]/g\[Mu]","g\[Tau]/g\[Mu]|\[Pi]"] := 0.16


ExpCorrelation["g\[Tau]/g\[Mu]","g\[Tau]/g\[Mu]|K"] := 0.12


ExpCorrelation["g\[Tau]/ge","g\[Tau]/g\[Mu]|\[Pi]"] := 0.18


ExpCorrelation["g\[Tau]/ge","g\[Tau]/g\[Mu]|K"] := 0.11


ExpCorrelation["g\[Tau]/g\[Mu]|\[Pi]","g\[Tau]/g\[Mu]|K"] := 0.07
