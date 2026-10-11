(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`LFV`*)


(* ::Subtitle:: *)
(*LFV observables*)


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


ObservableSectors["LFV"] := {(*"ZLFV","\[Tau]LFV","\[Mu]LFV"*)};
ObservableList["LFV"] := ObservableList/@ObservableSectors["LFV"]


(* ::Section::Closed:: *)
(*\[CapitalDelta]F = 1 - TO DO (avoid redundancies with DF1.m!)*)


(* ::Subsection:: *)
(*b -> dll'*)


(* ::Subsubsection:: *)
(*B^0->e\[Mu]*)


(* ::Subsubsection:: *)
(*B^0->e\[Tau]*)


(* ::Subsubsection:: *)
(*B^0->\[Mu]\[Tau]*)


(* ::Subsection:: *)
(*b -> sll'*)


(* ::Subsubsection:: *)
(*Subscript[B, s]->e\[Mu]*)


(* ::Subsubsection:: *)
(*Subscript[B, s]->e\[Tau]*)


(* ::Subsubsection:: *)
(*Subscript[B, s]->\[Mu]\[Tau]*)


(* ::Subsection:: *)
(*s -> dll'*)


(* ::Subsubsection:: *)
(*Subscript[K, L]->\[Mu]e*)


(* ::Section:: *)
(*Z LFV decays - WIP*)


ObservableList["ZLFV"] = {(*"Z->\[Tau]\[Mu]"*)};


BrZlilj[i_,j_] := 1/Width["ZBoson"] Mass["ZBoson"]/(24\[Pi]) (Abs[WCL["gZeL",{i,j}]]^2+Abs[WCL["gZeR",{i,j}]]^2+ 2*Mass["ZBoson"]^2 Abs[WCL["eZ",{i,j}]]^2+ 2*Mass["ZBoson"]^2 Abs[WCL["eZ",{j,i}]]^2)


(* ::Subsubsection::Closed:: *)
(*Z -> \[Tau]\[Mu] - TO XCHECK*)


TheoryExpression["Z->\[Tau]\[Mu]"] := BrZlilj[2,3]+BrZlilj[3,2]


ExpValue$default["Z->\[Tau]\[Mu]"] := Around[0,6.5]*10^-6/2
ExpInfo["Z->\[Tau]\[Mu]"] := Row[{"ATLAS 2021: ", Hyperlink["2105.12491","https://arxiv.org/pdf/2105.12491"]}]


SMPrediction$default["Z->\[Tau]\[Mu]"] := Around[0,null]
SMInfo["Z->\[Tau]\[Mu]"] := "Forbidden in the SM."


NPInfo["Z->\[Tau]\[Mu]"] := "Lepton masses neglected."


LowScale$default["Z->\[Tau]\[Mu]"] := Mass["ZBoson"]/.GetParameters[]


(* ::Section:: *)
(*\[Tau] LFV decays - WIP*)


ObservableSectors["\[Tau]LFV"] := {(*"\[Tau]->lP","\[Tau]->3l","\[Tau]->l\[Gamma]"*)}
ObservableList["\[Tau]LFV"] := ObservableList/@ObservableSectors["\[Tau]LFV"]


(* ::Subsection:: *)
(*Wilson coefficient combinations (from 2312.14070)*)


cVAd[i_,j_,k_,l_] := WCL["edVRR",{i,j,k,l}]-WCL["deVLR",{i,j,k,l}]-WCL["edVLL",{i,j,k,l}]+WCL["edVLR",{i,j,k,l}]
cSPd[i_,j_,k_,l_] := WCL["edSRR",{i,j,k,l}]-WCL["edSRL",{i,j,k,l}]-WCL["edSRR",{j,i,l,k}]\[Conjugate]+WCL["edSRL",{j,i,l,k}]\[Conjugate]
cAAd[i_,j_,k_,l_] := WCL["edVRR",{i,j,k,l}]-WCL["deVLR",{i,j,k,l}]+WCL["edVLL",{i,j,k,l}]-WCL["edVLR",{i,j,k,l}]
cPPd[i_,j_,k_,l_] := WCL["edSRR",{i,j,k,l}]-WCL["edSRL",{i,j,k,l}]+WCL["edSRR",{j,i,l,k}]\[Conjugate]-WCL["edSRL",{j,i,l,k}]\[Conjugate]


(* ::Subsection::Closed:: *)
(*\[Tau] -> lP - TO FINISH*)


ObservableList["\[Tau]->lP"] = {(*"\[Tau]->\[Mu]KS","\[Tau]->eKS"*)};


(* ::Subsubsection:: *)
(*\[Tau] -> KS l - TO XCHECK*)


Br\[Tau]KSl[lep_] := Lifetime["\[Tau]"] (DecayConstant["K"]^2 Mass["\[Tau]"]^3)/(512 \[Pi]) (1-Mass["K0"]^2/Mass["\[Tau]"]^2)^2 (Abs[cVAd[lep,3,1,2]-cVAd[lep,3,2,1]+Mass["K0"]^2/(Mass["\[Tau]"](Mass["s"]+Mass["d"])) (cSPd[lep,3,1,2]-cSPd[lep,3,2,1])]^2+Abs[cAAd[lep,3,1,2]-cAAd[lep,3,2,1]-Mass["K0"]^2/(Mass["\[Tau]"](Mass["s"]+Mass["d"])) (cPPd[lep,3,1,2]-cPPd[lep,3,2,1])]^2)


(* \[Tau]-> KS \[Mu] *)


TheoryExpression["\[Tau]->\[Mu]KS"] := Br\[Tau]KSl[2]


ExpValue$default["\[Tau]->\[Mu]KS"] := Around[0,1.8]*10^-8*CL90to95/2
ExpInfo["\[Tau]->\[Mu]KS"] := Row[{"Belle II: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->\[Mu]KS"] := Around[0,null]
SMInfo["\[Tau]->\[Mu]KS"] := "Forbidden in the SM."


NPInfo["\[Tau]->\[Mu]KS"] := "Light lepton masses neglected."


LowScale$default["\[Tau]->\[Mu]KS"] := Mass["\[Tau]"]/.GetParameters[]


(* \[Tau]-> KS e *)


TheoryExpression["\[Tau]->eKS"] := Br\[Tau]KSl[1]


ExpValue$default["\[Tau]->eKS"] := Around[0,0.8]*10^-8*CL90to95/2
ExpInfo["\[Tau]->eKS"] := Row[{"Belle II: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->eKS"] := Around[0,null]
SMInfo["\[Tau]->eKS"] := "Forbidden in the SM."


NPInfo["\[Tau]->eKS"] := "Light lepton masses neglected."


LowScale$default["\[Tau]->eKS"] := Mass["\[Tau]"]/.GetParameters[]


(* ::Subsubsection:: *)
(*\[Tau] -> \[Pi] l - TO DO*)


(* ::Subsubsection:: *)
(*\[Tau] -> \[Eta]l - TO DO*)


(* ::Subsubsection:: *)
(*\[Tau] -> \[Eta]' l - TO DO*)


(* ::Subsection::Closed:: *)
(*\[Tau] -> Vl - TO DO*)


(* ::Subsubsection:: *)
(*\[Tau] -> K* l - TO DO*)


(* ::Subsubsection:: *)
(*\[Tau] -> \[Phi] l - TO DO*)


(* ::Subsubsection:: *)
(*\[Tau] -> \[Omega]l - TO DO*)


(* ::Subsubsection:: *)
(*\[Tau] -> \[Rho] l - TO DO*)


(* ::Subsection::Closed:: *)
(*\[Tau] -> 3 l - TO XCHECK*)


ObservableList["\[Tau]->3l"] = {(*"\[Tau]->\[Mu]\[Mu]\[Mu]","\[Tau]->e\[Mu]\[Mu]","\[Tau]->\[Mu]ee","\[Tau]->eee"*)};


(* lj -> 3li *)


leptons={"e","\[Mu]","\[Tau]"}


Brlj3li[j_,i_] := (Lifetime[leptons[[j]]]Mass[leptons[[j]]]^5)/(1536 \[Pi]^3) (2*Abs[WCL["eeVLL",{i,i,i,j}]]^2+2*Abs[WCL["eeVRR",{i,i,i,j}]]^2+Abs[WCL["eeVLR",{i,j,i,i}]]^2+Abs[WCL["eeVLR",{i,i,i,j}]]^2 + 64*4\[Pi]*Param["\[Alpha]EM"](Log[Mass[leptons[[j]]]/Mass[leptons[[i]]]]-11/8) 1/Mass[leptons[[j]]]^2 (Abs[WCL["e\[Gamma]",{j,i}]]^2+Abs[WCL["e\[Gamma]",{i,j}]]^2) + 8*Sqrt[4\[Pi]*Param["\[Alpha]EM"]]*1/Mass[leptons[[j]]]*(Re[WCL["e\[Gamma]",{i,j}](2*WCL["eeVLL",{i,i,i,j}]\[Conjugate]+WCL["eeVLR",{i,j,i,i}]\[Conjugate])] +Re[WCL["e\[Gamma]",{j,i}]\[Conjugate](2*WCL["eeVRR",{i,i,i,j}]\[Conjugate]+WCL["eeVLR",{i,i,i,j}]\[Conjugate])]))


(* lj -> li lk lk *)


Brljlilk[j_,i_,k_] := (Lifetime[leptons[[j]]]Mass[leptons[[j]]]^5)/(1536 \[Pi]^3) (Abs[WCL["eeVLL",{i,j,k,k}]]^2+Abs[WCL["eeVRR",{i,j,k,k}]]^2+Abs[WCL["eeVLR",{i,j,k,k}]]^2+Abs[WCL["eeVLR",{k,k,i,j}]]^2 + 64*4\[Pi]*Param["\[Alpha]EM"](Log[Mass[leptons[[j]]]/Mass[leptons[[k]]]]-3/2) 1/Mass[leptons[[j]]]^2 (Abs[WCL["e\[Gamma]",{j,i}]]^2+Abs[WCL["e\[Gamma]",{i,j}]]^2) + 8*Sqrt[4\[Pi]*Param["\[Alpha]EM"]]*1/Mass[leptons[[j]]]*(Re[WCL["e\[Gamma]",{i,j}](WCL["eeVLL",{i,j,k,k}]\[Conjugate]+WCL["eeVLR",{i,j,k,k}]\[Conjugate])] +Re[WCL["e\[Gamma]",{j,i}]\[Conjugate](WCL["eeVRR",{k,k,i,j}]\[Conjugate]+WCL["eeVLR",{k,k,i,j}]\[Conjugate])]))


(* ::Subsubsection:: *)
(*\[Tau] -> \[Mu]\[Mu]\[Mu] - TO XCHECK*)


TheoryExpression["\[Tau]->\[Mu]\[Mu]\[Mu]"] := Brlj3li[3,2]


ExpValue$default["\[Tau]->\[Mu]\[Mu]\[Mu]"] := Around[0,1.9]*10^-8*CL90to95/2
ExpInfo["\[Tau]->\[Mu]\[Mu]\[Mu]"] := Row[{"From PDG: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->\[Mu]\[Mu]\[Mu]"] := Around[0,null]
SMInfo["\[Tau]->\[Mu]\[Mu]\[Mu]"] := "Forbidden in the SM."


NPInfo["\[Tau]->\[Mu]\[Mu]\[Mu]"] := ""


LowScale$default["\[Tau]->\[Mu]\[Mu]\[Mu]"] := Mass["\[Tau]"]/.GetParameters[]


(* ::Subsubsection:: *)
(*\[Tau] -> \[Mu]ee - TO XCHECK*)


TheoryExpression["\[Tau]->\[Mu]ee"] := Brljlilk[3,2,1]


ExpValue$default["\[Tau]->\[Mu]ee"] := Around[0,1.8]*10^-8*CL90to95/2
ExpInfo["\[Tau]->\[Mu]ee"] := Row[{"From PDG: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->\[Mu]ee"] := Around[0,null]
SMInfo["\[Tau]->\[Mu]ee"] := "Forbidden in the SM."


NPInfo["\[Tau]->\[Mu]ee"] := ""


LowScale$default["\[Tau]->\[Mu]ee"] := Mass["\[Tau]"]/.GetParameters[]


(* ::Subsubsection:: *)
(*\[Tau] -> e\[Mu]\[Mu] - TO XCHECK*)


TheoryExpression["\[Tau]->e\[Mu]\[Mu]"] := Brljlilk[3,1,2]


ExpValue$default["\[Tau]->e\[Mu]\[Mu]"] := Around[0,2.7]*10^-8*CL90to95/2
ExpInfo["\[Tau]->e\[Mu]\[Mu]"] := Row[{"From PDG: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->e\[Mu]\[Mu]"] := Around[0,null]
SMInfo["\[Tau]->e\[Mu]\[Mu]"] := "Forbidden in the SM."


NPInfo["\[Tau]->e\[Mu]\[Mu]"] := ""


LowScale$default["\[Tau]->e\[Mu]\[Mu]"] := Mass["\[Tau]"]/.GetParameters[]


(* ::Subsubsection:: *)
(*\[Tau] -> eee - TO XCHECK*)


TheoryExpression["\[Tau]->eee"] := Brlj3li[3,1]


ExpValue$default["\[Tau]->eee"] := Around[0,2.7]*10^-8*CL90to95/2
ExpInfo["\[Tau]->eee"] := Row[{"From PDG: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->eee"] := Around[0,null]
SMInfo["\[Tau]->eee"] := "Forbidden in the SM."


NPInfo["\[Tau]->eee"] := ""


LowScale$default["\[Tau]->eee"] := Mass["\[Tau]"]/.GetParameters[]


(* ::Subsection::Closed:: *)
(*\[Tau] -> l\[Gamma] - TO XCHECK*)


ObservableList["\[Tau]->l\[Gamma]"] = {(*"\[Tau]->\[Mu]\[Gamma]","\[Tau]->e\[Gamma]"*)};


Brljli\[Gamma][j_,i_] := (Lifetime[leptons[[j]]]Mass[leptons[[j]]]^3)/(4\[Pi]) (Abs[WCL["e\[Gamma]",{i,j}]]^2+Abs[WCL["e\[Gamma]",{j,i}]]^2)


(* ::Subsubsection:: *)
(*\[Tau] -> \[Mu]\[Gamma] - TO XCHECK*)


TheoryExpression["\[Tau]->\[Mu]\[Gamma]"] := Brljli\[Gamma][3,2]


ExpValue$default["\[Tau]->\[Mu]\[Gamma]"] := Around[0,4.2]*10^-8*CL90to95/2
ExpInfo["\[Tau]->\[Mu]\[Gamma]"] := Row[{"From PDG: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->\[Mu]\[Gamma]"] := Around[0,null]
SMInfo["\[Tau]->\[Mu]\[Gamma]"] := "Forbidden in the SM."


NPInfo["\[Tau]->\[Mu]\[Gamma]"] := ""


LowScale$default["\[Tau]->\[Mu]\[Gamma]"] := Mass["\[Tau]"]/.GetParameters[]


(* ::Subsubsection:: *)
(*\[Tau] -> e\[Gamma] - TO XCHECK*)


TheoryExpression["\[Tau]->e\[Gamma]"] := Brljli\[Gamma][3,1]


ExpValue$default["\[Tau]->e\[Gamma]"] := Around[0,4.2]*10^-8*CL90to95/2
ExpInfo["\[Tau]->e\[Gamma]"] := Row[{"From PDG: ", Ref["PDGlep"]}]


SMPrediction$default["\[Tau]->e\[Gamma]"] := Around[0,null]
SMInfo["\[Tau]->e\[Gamma]"] := "Forbidden in the SM."


NPInfo["\[Tau]->e\[Gamma]"] := ""


LowScale$default["\[Tau]->e\[Gamma]"] := Mass["\[Tau]"]/.GetParameters[]


(* ::Section:: *)
(*\[Mu] LFV - TO FINISH*)


ObservableList["\[Mu]LFV"] = {(*"\[Mu]->eee"*)};


(* ::Subsubsection::Closed:: *)
(*\[Mu] -> eee - TO XCHECK*)


TheoryExpression["\[Mu]->eee"] := Brlj3li[2,1]


ExpValue$default["\[Mu]->eee"] := Around[0,1.0]*10^-12*CL90to95/2
ExpInfo["\[Mu]->eee"] := Row[{"From PDG: ", Ref["PDGlep"]}]


SMPrediction$default["\[Mu]->eee"] := Around[0,null]
SMInfo["\[Mu]->eee"] := "Forbidden in the SM."


NPInfo["\[Mu]->eee"] := ""


LowScale$default["\[Mu]->eee"] := Mass["\[Mu]"]/.GetParameters[]
