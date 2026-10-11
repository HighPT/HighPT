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


CL90to95=3.09/2.3;


ObservableSectors["\[CapitalDelta]F=1"] := {(*"b->sll", "b->dll"*)}
ObservableList["\[CapitalDelta]F=1"] := ObservableList/@ObservableSectors["\[CapitalDelta]F=1"]


(* ::Section:: *)
(*b -> sll (') - TO DO*)


(* ::Code::Initialization:: *)
ObservableList["b->sll"] := {(*"Bs->ee","Bs->\[Tau]\[Tau]","Bs->\[Mu]\[Mu]"*)}


LowScale$default[Alternatives@@(ObservableList["b->sll"]//Flatten)] := Mass["b"]/.GetParameters[];


(* ::Subsection:: *)
(*B -> K( * ) ll*)


(* ::Subsubsection:: *)
(*B -> K\[Mu]\[Mu] - TO DO*)


(* ::Subsubsection:: *)
(*B -> K*\[Mu]\[Mu] -  TO DO*)


(* ::Subsubsection:: *)
(*B -> K\[Tau]\[Tau] - TO DO*)


(* ::Subsubsection:: *)
(*B -> K*\[Tau]\[Tau] - TO DO*)


(* ::Subsubsection:: *)
(*B -> K\[Mu]\[Tau] - TO DO*)


(* ::Subsubsection:: *)
(*B -> K*\[Mu]-\[Tau]+ - TO DO*)


(* ::Subsubsection:: *)
(*B -> K*\[Mu]+\[Tau]- - TO DO*)


(* ::Subsection:: *)
(*Bs->ll - TO FINISH*)


me={Mass["e"],Mass["\[Mu]"],Mass["\[Tau]"]};


BsToll[l_]:=Lifetime["Bs"]/(128\[Pi]) DecayConstant["Bs"]^2 Mass["Bs"] Sqrt[1-(4me[[l]]^2)/Mass["Bs"]^2]((1-(4me[[l]]^2)/Mass["Bs"]^2)Abs[(WCL["edSRR",{l,l,3,2}]+Conjugate[WCL["edSRL",{l,l,2,3}]]-WCL["edSRL",{l,l,3,2}]-Conjugate[WCL["edSRR",{l,l,2,3}]]) Mass["Bs"]^2/(Mass["b"]+Mass["s"])]^2+Abs[2me[[l]](WCL["deVLR",{2,3,l,l}]\[Conjugate]-WCL["edVLL",{l,l,2,3}]\[Conjugate]-WCL["edVRR",{l,l,2,3}]\[Conjugate]+WCL["edVLR",{l,l,2,3}]\[Conjugate])+Mass["Bs"]^2/(Mass["b"]+Mass["s"]) (WCL["edSRR",{l,l,3,2}]-Conjugate[WCL["edSRL",{l,l,2,3}]]-WCL["edSRL",{l,l,3,2}]+Conjugate[WCL["edSRR",{l,l,2,3}]])]^2)


(* RBs = B((Bs\[Rule]\[Mu]\[Mu])^SM)/|Subscript[\[Lambda], t]|^2 taken from [1908.07011] *)
RBs := Around[2.1516,0.0455]*10^-6;


(* ::Subsubsection::Closed:: *)
(*Bs -> ee - TO FINISH*)


TheoryExpression["Bs->ee"] := BsToll[1];


ExpValue$default["Bs->ee"] := Around[0,11.2]*10^-9/2;
ExpInfo["Bs->ee"]:=Row[{"From PDG: ", Ref["PDGK"]}]


(*SMPrediction$default["Bs->ee"] :=*)
SMInfo["Bs->ee"] := "f_Bs (with 2+1+1) taken from [2411.04268]; theory prediction from [1908.07011] rescaled by ratio of lepton masses"


NPInfo["Bs->ee"]:="[1303.3820]" (* Make this better! *)


(* ::Subsubsection::Closed:: *)
(*Bs->\[Mu]\[Mu] - TO FINISH*)


TheoryExpression["Bs->\[Mu]\[Mu]"] := BsToll[2];


ExpValue$default["Bs->\[Mu]\[Mu]"] := Around[3.35,0.27]*10^-9;
ExpInfo["Bs->\[Mu]\[Mu]"]:="Experimental average of [1812.03017], [2108.09283] and [2212.10311], following the approach from [CMS-PAS-BPH-20-003]."


(*SMPrediction$default["Bs->\[Mu]\[Mu]"] :=*)
SMInfo["Bs->\[Mu]\[Mu]"] := "f_Bs (with 2+1+1) taken from [2411.04268]; theory prediction from [1908.07011]"


NPInfo["Bs->\[Mu]\[Mu]"] := "[1303.3820]" (* Make this better! *)


(* ::Subsubsection::Closed:: *)
(*Bs->\[Tau]\[Tau] - TO FINISH*)


TheoryExpression["Bs->\[Tau]\[Tau]"] := BsToll[3];


ExpValue$default["Bs->\[Tau]\[Tau]"] := Around[0,6.8]*10^-3/2;
ExpInfo["Bs->\[Tau]\[Tau]"]:=Row[{"From PDG: ", Ref["PDGK"]}]


(*SMPrediction$default["Bs->\[Tau]\[Tau]"] :=*)
SMInfo["Bs->\[Tau]\[Tau]"] := "f_Bs (with 2+1+1) taken from [2411.04268]; theory prediction from [1908.07011] rescaled by ratio of lepton masses"


NPInfo["Bs->\[Tau]\[Tau]"] := "[1303.3820]" (* Make this better! *)


(* ::Section:: *)
(*b -> dll - TO DO*)


ObservableList["b->dll"] := {(*"Bd->ee","Bd->\[Tau]\[Tau]","Bd->\[Mu]\[Mu]"*)}


LowScale$default[Alternatives@@(ObservableList["b->dll"]//Flatten)] := Mass["b"]/.GetParameters[];


(* ::Subsection:: *)
(*Bd->ll - TO FINISH*)


BdToll[l_]:=Lifetime["Bd"]/(128\[Pi]) DecayConstant["Bd"]^2 Mass["Bd"] Sqrt[1-(4me[[l]]^2)/Mass["Bd"]^2]((1-(4me[[l]]^2)/Mass["Bd"]^2)Abs[(WCL["edSRR",{l,l,3,1}]+Conjugate[WCL["edSRL",{l,l,1
1,3}]]-WCL["edSRL",{l,l,3,1}]-Conjugate[WCL["edSRR",{l,l,1,3}]]) Mass["Bd"]^2/(Mass["b"]+Mass["d"])]^2+Abs[2me[[l]](WCL["deVLR",{1,3,l,l}]\[Conjugate]-WCL["edVLL",{l,l,1,3}]\[Conjugate]-WCL["edVRR",{l,l,1,3}]\[Conjugate]+WCL["edVLR",{l,l,1,3}]\[Conjugate])+Mass["Bs"]^2/(Mass["b"]+Mass["d"]) (WCL["edSRR",{l,l,3,1}]-Conjugate[WCL["edSRL",{l,l,1,3}]]-WCL["edSRL",{l,l,3,1}]+Conjugate[WCL["edSRR",{l,l,1,3}]])]^2)


(* RBd = B((Bd\[Rule]\[Mu]\[Mu])^SM)/|Subscript[\[Lambda], t]|^2 taken from [1908.07011] *)
RBd := Around[1.3569,0.0302]*10^-6;


(* ::Subsubsection::Closed:: *)
(*Bd->ee*)


TheoryExpression["Bd->ee"] := BdToll[1];


ExpValue$default["Bd->ee"] := Around[0,3.0]*10^-9/2;
ExpInfo["Bd->ee"]:=Row[{"From PDG: ", Ref["PDGK"]}];


(*SMPrediction$default["Bd->ee"] :=*)
SMInfo["Bd->ee"] := "f_Bd (with 2+1+1) taken from [2411.04268]; theory prediction from [1908.07011] rescaled by ratio of lepton masses"


NPContribution$default["Bd->ee"] := NPFromTheoryExpression["Bd->ee"](*(Abs[C10SM+wCL["10",ind]-wCL["10p",ind]+(wCL["P",ind]-wCL["Pp",ind]) Mass["Bd"]^2/(2 Mass["e"] (Mass["b"]+Mass["d"]))]^2+(1-4 Mass["e"]^2/Mass["Bd"]^2)Abs[(wCL["S",ind]-wCL["Sp",ind]) Mass["Bd"]^2/(2 Mass["e"] (Mass["b"]+Mass["d"]))]^2)/Abs[C10SM]^2-1/.ind->{1,1,1,3}/.WETToLEFT/.GetParameters[]//Expand*)


NPInfo["Bd->ee"]:="[1303.3820]" (* Update! *)


(* ::Subsubsection::Closed:: *)
(*Bd->\[Mu]\[Mu]*)


TheoryExpression["Bd->\[Mu]\[Mu]"] := BdToll[2];


ExpValue$default["Bd->\[Mu]\[Mu]"] := Around[0.5,0.5]*10^-10;
ExpInfo["Bs->\[Mu]\[Mu]"]:="Experimental average of [1812.03017], [2108.09283] and [2212.10311], following the approach from [CMS-PAS-BPH-20-003]."


(*SMPrediction$default["Bd->\[Mu]\[Mu]"] :=*)
SMInfo["Bd->\[Mu]\[Mu]"] := "f_Bd (with 2+1+1) taken from [2411.04268]; theory prediction from [1908.07011]"


NPInfo["Bd->\[Mu]\[Mu]"]:="[1303.3820]" (* Update! *)


(* ::Subsubsection::Closed:: *)
(*Bd->\[Tau]\[Tau]*)


TheoryExpression["Bd->\[Tau]\[Tau]"] := BdToll[3];


ExpValue$default["Bd->\[Tau]\[Tau]"] := Around[0,2.1]*10^-3/2;
ExpInfo["Bd->\[Tau]\[Tau]"]:=Row[{"From PDG: ", Ref["PDGK"]}]


(*SMPrediction$default["Bd->\[Tau]\[Tau]"] :=*)
SMInfo["Bd->\[Tau]\[Tau]"] := "f_Bd (with 2+1+1) taken from [2411.04268]; theory prediction from [1908.07011] rescaled by ratio of lepton masses"


NPInfo["Bd->\[Mu]\[Mu]"]:="[1303.3820]" (* Update! *)


(* ::Section:: *)
(*b -> s\[Nu]\[Nu] - TO DO*)


(* ::Subsubsection::Closed:: *)
(*B -> K\[Nu]\[Nu]*)


(*TheoryExpression["B+->K+\[Nu]\[Nu]"] := ;*)


ExpValue$default["B+->K+\[Nu]\[Nu]"] := Around[2.33,0.67]*CL90to95/2;
ExpInfo["B+->K+\[Nu]\[Nu]"]:="Belle-II results from 2311.14647"


(*SMPrediction$default["B+->K+\[Nu]\[Nu]"] := *)
SMInfo["B+->K+\[Nu]\[Nu]"] := ""


NPInfo["B+->K+\[Nu]\[Nu]"] := "[2301.06990]" (* Update! *)


(* ::Subsubsection::Closed:: *)
(*B -> K*\[Nu]\[Nu]*)


(*TheoryExpression["B0->K0*\[Nu]\[Nu]"] := ;*)


ExpValue$default["B0->K0*\[Nu]\[Nu]"] := Around[0,1.8 10^-5]*CL90to95/2;
ExpInfo["B0->K0*\[Nu]\[Nu]"]:="PDG"


(*SMPrediction$default["B0->K0*\[Nu]\[Nu]"] := *)
SMInfo["B0->K0*\[Nu]\[Nu]"] := "Form-factors from [1503.05534]"


NPInfo["B0->K0*\[Nu]\[Nu]"] := "[2301.06990]" (* Update! *)


(* ::Section:: *)
(*b -> s\[Gamma] - TO DO*)
