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


$LFVSectors={"leptonicLFV","EWLFV"(*,"\[CapitalDelta]F=1LFV"*)};


FlavorObservables["LFV"] = FlavorObservables/@$LFVSectors


ObsTable["LFV"] := Grid[{{"LFV",Column[ObsTable/@$LFVSectors]}},Dividers->All];


CL90to95=3.09/2.3;


(* ::Section:: *)
(*Basis change*)


repRot={
	(* axial/vector *)
	wCL["VVu",lb_]:>wCL["VRRu",lb]+wCL["VRLu",lb]+wCL["VLLu",lb]+wCL["VLRu",lb],
	wCL["AVu",lb_]:>wCL["VRRu",lb]+wCL["VRLu",lb]-wCL["VLLu",lb]-wCL["VLRu",lb],
	wCL["VAu",lb_]:>wCL["VRRu",lb]-wCL["VRLu",lb]-wCL["VLLu",lb]+wCL["VLRu",lb],
	wCL["AAu",lb_]:>wCL["VRRu",lb]-wCL["VRLu",lb]+wCL["VLLu",lb]-wCL["VLRu",lb],

	wCL["VVd",lb_]:>wCL["VRRd",lb]+wCL["VRLd",lb]+wCL["VLLd",lb]+wCL["VLRd",lb],
	wCL["AVd",lb_]:>wCL["VRRd",lb]+wCL["VRLd",lb]-wCL["VLLd",lb]-wCL["VLRd",lb],
	wCL["VAd",lb_]:>wCL["VRRd",lb]-wCL["VRLd",lb]-wCL["VLLd",lb]+wCL["VLRd",lb],
	wCL["AAd",lb_]:>wCL["VRRd",lb]-wCL["VRLd",lb]+wCL["VLLd",lb]-wCL["VLRd",lb],

	(* pseudoscalar/scalar *)
	wCL["SSu",lb_]:>wCL["SRRu",lb]+wCL["SRLu",lb]+wCL["SLLu",lb]+wCL["SLRu",lb],
	wCL["PSu",lb_]:>wCL["SRRu",lb]+wCL["SRLu",lb]-wCL["SLLu",lb]-wCL["SLRu",lb],
	wCL["SPu",lb_]:>wCL["SRRu",lb]-wCL["SRLu",lb]-wCL["SLLu",lb]+wCL["SLRu",lb],
	wCL["PPu",lb_]:>wCL["SRRu",lb]-wCL["SRLu",lb]+wCL["SLLu",lb]-wCL["SLRu",lb],

	wCL["SSd",lb_]:>wCL["SRRd",lb]+wCL["SRLd",lb]+wCL["SLLd",lb]+wCL["SLRd",lb],
	wCL["PSd",lb_]:>wCL["SRRd",lb]+wCL["SRLd",lb]-wCL["SLLd",lb]-wCL["SLRd",lb],
	wCL["SPd",lb_]:>wCL["SRRd",lb]-wCL["SRLd",lb]-wCL["SLLd",lb]+wCL["SLRd",lb],
	wCL["PPd",lb_]:>wCL["SRRd",lb]-wCL["SRLd",lb]+wCL["SLLd",lb]-wCL["SLRd",lb],

	(* tensor *)
	wCL["Tu",lb_]:>wCL["TRu",lb]+wCL["TLd",lb],
	wCL["T5u",lb_]:>wCL["TRu",lb]-wCL["TLd",lb],

	wCL["Td",lb_]:>wCL["TRd",lb]+wCL["TLd",lb],
	wCL["T5d",lb_]:>wCL["TRd",lb]-wCL["TLd",lb]
};


rep\[Tau]lP[i_,j_]:={
	(* Axial/vector *)
	CVVuu->wCL["VVu",{i,j,1,1}],CVVdd->wCL["VVd",{i,j,1,1}],CVVss->wCL["VVd",{i,j,2,2}],
	CVAuu->wCL["VAu",{i,j,1,1}],CVAdd->wCL["VAd",{i,j,1,1}],CVAss->wCL["VAd",{i,j,2,2}],
	CAVuu->wCL["AVu",{i,j,1,1}],CAVdd->wCL["AVd",{i,j,1,1}],CAVss->wCL["AVd",{i,j,2,2}],
	CAAuu->wCL["AAu",{i,j,1,1}],CAAdd->wCL["AAd",{i,j,1,1}],CAAss->wCL["AAd",{i,j,2,2}],

	(* Pseudoscalar/scalar *)
	CSSuu->wCL["SSu",{i,j,1,1}],CSSdd->wCL["SSd",{i,j,1,1}],CSSss->wCL["SSd",{i,j,2,2}],
	CSPuu->wCL["SPu",{i,j,1,1}],CSPdd->wCL["SPd",{i,j,1,1}],CSPss->wCL["SPd",{i,j,2,2}],
	CPSuu->wCL["PSu",{i,j,1,1}],CPSdd->wCL["PSd",{i,j,1,1}],CPSss->wCL["PSd",{i,j,2,2}],
	CPPuu->wCL["PPu",{i,j,1,1}],CPPdd->wCL["PPd",{i,j,1,1}],CPPss->wCL["PPd",{i,j,2,2}],

	(* Tensor *)
	CTuu->wCL["Tu",{i,j,1,1}],CTdd->wCL["Td",{i,j,1,1}],CTss->wCL["Td",{i,j,2,2}],
	CT5uu->wCL["Tu",{i,j,1,1}],CT5dd->wCL["Td",{i,j,1,1}],CT5ss->wCL["Td",{i,j,2,2}],

	(* Gluonic *)
	CGR-> wCL["GtR\[ScriptL]",{i,j}],
	CGL-> wCL["GtL\[ScriptL]",{i,j}],

	(* Dipoles *)
	CDR-> wCL["DR\[ScriptL]",{i,j}],
	CDL->wCL["DL\[ScriptL]",{i,j}]
};


LeptonMasses={Mass["e"],Mass["\[Mu]"],Mass["\[Tau]"]};


OlcyrtoLEFT={
(* Leptonic *)
wCL["DL\[ScriptL]",{\[Alpha]_,\[Beta]_}] :> Param["vev"]^2/LeptonMasses[[\[Beta]]] WCL["e\[Gamma]",{\[Beta],\[Alpha]}]\[Conjugate],
wCL["DR\[ScriptL]",{\[Alpha]_,\[Beta]_}] :> Param["vev"]^2/LeptonMasses[[\[Beta]]] WCL["e\[Gamma]",{\[Alpha],\[Beta]}]\[Conjugate],
wCL["VLL\[ScriptL]",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}] :> Param["vev"]^2 WCL["eeVLL",{\[Alpha],\[Beta],\[Gamma],\[Delta]}],
wCL["VRR\[ScriptL]",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}] :> Param["vev"]^2 WCL["eeVRR",{\[Alpha],\[Beta],\[Gamma],\[Delta]}],
wCL["VLR\[ScriptL]",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}] :> Param["vev"]^2 WCL["eeVLR",{\[Alpha],\[Beta],\[Gamma],\[Delta]}],
wCL["VRL\[ScriptL]",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}] :> Param["vev"]^2 WCL["eeVLR",{\[Gamma],\[Delta],\[Alpha],\[Beta]}],
wCL["GtR\[ScriptL]",{\[Alpha]_,\[Beta]_}] :> 0, 
wCL["GtL\[ScriptL]",{\[Alpha]_,\[Beta]_}] :> 0,

(* Semileptonic *)
wCL["VLLu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euVLL",{\[Alpha],\[Beta],i,j}],
wCL["VRRu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euVRR",{\[Alpha],\[Beta],i,j}],
wCL["VLRu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euVLR",{\[Alpha],\[Beta],i,j}],
wCL["VRLu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["ueVLR",{i,j,\[Alpha],\[Beta]}],
wCL["VLLd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edVLL",{\[Alpha],\[Beta],i,j}],
wCL["VRRd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edVRR",{\[Alpha],\[Beta],i,j}],
wCL["VLRd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edVLR",{\[Alpha],\[Beta],i,j}],
wCL["VRLd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["deVLR",{i,j,\[Alpha],\[Beta]}],
wCL["SLLu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euSRR",{j,i,\[Beta],\[Alpha]}]\[Conjugate],
wCL["SRRu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euSRR",{i,j,\[Alpha],\[Beta]}],
wCL["SLRu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euSRL",{j,i,\[Beta],\[Alpha]}]\[Conjugate],
wCL["SRLu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euSRL",{i,j,\[Alpha],\[Beta]}],
wCL["SLLd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edSRR",{j,i,\[Beta],\[Alpha]}]\[Conjugate],
wCL["SRRd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edSRR",{i,j,\[Alpha],\[Beta]}],
wCL["SLRd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edSRL",{j,i,\[Beta],\[Alpha]}]\[Conjugate],
wCL["SRLd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edSRL",{i,j,\[Alpha],\[Beta]}],
wCL["TLu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euTRR",{j,i,\[Beta],\[Alpha]}]\[Conjugate],
wCL["TRu",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["euTRR",{i,j,\[Alpha],\[Beta]}],
wCL["TLd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edTRR",{j,i,\[Beta],\[Alpha]}]\[Conjugate],
wCL["TRd",{\[Alpha]_,\[Beta]_,i_,j_}] :> Param["vev"]^2WCL["edTRR",{i,j,\[Alpha],\[Beta]}]
};


(* ::Section:: *)
(*Leptonic*)


$leptonicLFVSectors={"l->l'\[Gamma]","l->3l'","l->l'P","l->l'V","lN->l'N"};


FlavorObservables["leptonicLFV"] = FlavorObservables/@$leptonicLFVSectors


ObsTable["leptonicLFV"] := Grid[{{"leptonicLFV",Column[ObsTable/@$leptonicLFVSectors]}},Dividers->All];


(* ::Subsection::Closed:: *)
(*l -> l' \[Gamma]*)


FlavorObservables["l->l'\[Gamma]"]={"\[Mu]->e\[Gamma]","\[Tau]->e\[Gamma]","\[Tau]->\[Mu]\[Gamma]"};


ObsTable["l->l'\[Gamma]"] := Grid[{{"l->l'\[Gamma]",Column[FlavorObservables["l->l'\[Gamma]"]]}},Dividers->All];


(* ::Subsubsection::Closed:: *)
(*\[Mu] -> e\[Gamma]*)


ExpValue$default["\[Mu]->e\[Gamma]"] := Around[0,3.1]*10^-13*CL90to95/2;


NumericalInput["\[Mu]->e\[Gamma]"] := 0;


NPContribution$default["\[Mu]->e\[Gamma]"] := (Lifetime["\[Mu]"] Mass["\[Mu]"]^5)/(4\[Pi] Param["vev"]^4) (Abs[wCL["DL\[ScriptL]",{1,2}]]^2+Abs[wCL["DR\[ScriptL]",{1,2}]]^2)/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Mu]->e\[Gamma]"] := Mass["\[Mu]"]/.GetParameters[];


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> e\[Gamma]*)


ExpValue$default["\[Tau]->e\[Gamma]"] := Around[0,3.3]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->e\[Gamma]"] := 0;


NPContribution$default["\[Tau]->e\[Gamma]"] := (Lifetime["\[Tau]"] Mass["\[Tau]"]^5)/(4\[Pi] Param["vev"]^4) (Abs[wCL["DL\[ScriptL]",{1,3}]]^2+Abs[wCL["DR\[ScriptL]",{1,3}]]^2)/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Tau]->e\[Gamma]"] := Mass["\[Tau]"]/.GetParameters[];


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> \[Mu]\[Gamma]*)


ExpValue$default["\[Tau]->\[Mu]\[Gamma]"] := Around[0,4.2]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->\[Mu]\[Gamma]"] := 0;


NPContribution$default["\[Tau]->\[Mu]\[Gamma]"] := (Lifetime["\[Tau]"] Mass["\[Tau]"]^5)/(4\[Pi] Param["vev"]^4) (Abs[wCL["DL\[ScriptL]",{2,3}]]^2+Abs[wCL["DR\[ScriptL]",{2,3}]]^2)/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Tau]->\[Mu]\[Gamma]"] := Mass["\[Tau]"]/.GetParameters[];


(* ::Subsection::Closed:: *)
(*l -> 3 l'*)


FlavorObservables["l->3l'"]={"\[Mu]->eee","\[Tau]->eee","\[Tau]->e\[Mu]\[Mu]","\[Tau]->\[Mu]ee","\[Tau]->\[Mu]\[Mu]\[Mu]"};


ObsTable["l->3l'"] := Grid[{{"l->3l'",Column[FlavorObservables["l->3l'"]]}},Dividers->All];


(* ::Subsubsection::Closed:: *)
(*\[Mu] -> eee*)


ExpValue$default["\[Mu]->eee"] := Around[0,1.0]*10^-12*CL90to95/2;


NumericalInput["\[Mu]->eee"] := 0;


NPContribution$default["\[Mu]->eee"] := (Lifetime["\[Mu]"]Mass["\[Mu]"]^5)/(1536\[Pi]^3 Param["vev"]^4) (2(Abs[wCL["VLL\[ScriptL]",{1,2,1,1}]]^2+Abs[wCL["VRR\[ScriptL]",{1,2,1,1}]]^2)+(Abs[wCL["VLR\[ScriptL]",{1,2,1,1}]]^2+Abs[wCL["VRL\[ScriptL]",{1,2,1,1}]]^2)+64 Param["\[Alpha]EM"] 4\[Pi](Log[Mass["\[Mu]"]/Mass["e"]]-11/8)(Abs[wCL["DL\[ScriptL]",{1,2}]]^2+Abs[wCL["DR\[ScriptL]",{1,2}]]^2)+8Sqrt[Param["\[Alpha]EM"] 4\[Pi]]Re[wCL["DR\[ScriptL]",{1,2}]\[Conjugate](2wCL["VLL\[ScriptL]",{1,2,1,1}]+wCL["VLR\[ScriptL]",{1,2,1,1}])+wCL["DL\[ScriptL]",{1,2}]\[Conjugate](2wCL["VRR\[ScriptL]",{1,2,1,1}]+wCL["VRL\[ScriptL]",{1,2,1,1}])])/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Mu]->eee"] := Mass["\[Mu]"]/.GetParameters[];


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> eee*)


ExpValue$default["\[Tau]->eee"] := Around[0,2.7]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->eee"] := 0;


NPContribution$default["\[Tau]->eee"] := (Lifetime["\[Tau]"]Mass["\[Tau]"]^5)/(1536\[Pi]^3 Param["vev"]^4) (2(Abs[wCL["VLL\[ScriptL]",{1,3,1,1}]]^2+Abs[wCL["VRR\[ScriptL]",{1,3,1,1}]]^2)+(Abs[wCL["VLR\[ScriptL]",{1,3,1,1}]]^2+Abs[wCL["VRL\[ScriptL]",{1,3,1,1}]]^2)+64 Param["\[Alpha]EM"] 4\[Pi](Log[Mass["\[Tau]"]/Mass["e"]]-11/8)(Abs[wCL["DL\[ScriptL]",{1,3}]]^2+Abs[wCL["DR\[ScriptL]",{1,3}]]^2)+8Sqrt[Param["\[Alpha]EM"] 4\[Pi]]Re[wCL["DR\[ScriptL]",{1,3}]\[Conjugate](2wCL["VLL\[ScriptL]",{1,3,1,1}]+wCL["VLR\[ScriptL]",{1,3,1,1}])+wCL["DL\[ScriptL]",{1,3}]\[Conjugate](2wCL["VRR\[ScriptL]",{1,3,1,1}]+wCL["VRL\[ScriptL]",{1,3,1,1}])])/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Tau]->eee"] := Mass["\[Tau]"]/.GetParameters[];


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> e\[Mu]\[Mu]*)


ExpValue$default["\[Tau]->e\[Mu]\[Mu]"] := Around[0,2.7]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->e\[Mu]\[Mu]"] := 0;


NPContribution$default["\[Tau]->e\[Mu]\[Mu]"] := (Lifetime["\[Tau]"]Mass["\[Tau]"]^5)/(1536\[Pi]^3 Param["vev"]^4) (2(Abs[wCL["VLL\[ScriptL]",{1,3,2,2}]]^2+Abs[wCL["VRR\[ScriptL]",{1,3,2,2}]]^2)+(Abs[wCL["VLR\[ScriptL]",{1,3,2,2}]]^2+Abs[wCL["VRL\[ScriptL]",{1,3,2,2}]]^2)+64 Param["\[Alpha]EM"] 4\[Pi](Log[Mass["\[Tau]"]/Mass["e"]]-11/8)(Abs[wCL["DL\[ScriptL]",{1,3}]]^2+Abs[wCL["DR\[ScriptL]",{1,3}]]^2)+8Sqrt[Param["\[Alpha]EM"] 4\[Pi]]Re[wCL["DR\[ScriptL]",{1,3}]\[Conjugate](2wCL["VLL\[ScriptL]",{1,3,2,2}]+wCL["VLR\[ScriptL]",{1,3,2,2}])+wCL["DL\[ScriptL]",{1,3}]\[Conjugate](2wCL["VRR\[ScriptL]",{1,3,2,2}]+wCL["VRL\[ScriptL]",{1,3,2,2}])])/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Tau]->e\[Mu]\[Mu]"] := Mass["\[Tau]"]/.GetParameters[];


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> \[Mu]ee*)


ExpValue$default["\[Tau]->\[Mu]ee"] := Around[0,1.8]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->\[Mu]ee"] := 0;


NPContribution$default["\[Tau]->\[Mu]ee"] := (Lifetime["\[Tau]"]Mass["\[Tau]"]^5)/(1536\[Pi]^3 Param["vev"]^4) (2(Abs[wCL["VLL\[ScriptL]",{2,3,1,1}]]^2+Abs[wCL["VRR\[ScriptL]",{2,3,1,1}]]^2)+(Abs[wCL["VLR\[ScriptL]",{2,3,1,1}]]^2+Abs[wCL["VRL\[ScriptL]",{2,3,1,1}]]^2)+64 Param["\[Alpha]EM"] 4\[Pi](Log[Mass["\[Tau]"]/Mass["\[Mu]"]]-11/8)(Abs[wCL["DL\[ScriptL]",{2,3}]]^2+Abs[wCL["DR\[ScriptL]",{2,3}]]^2)+8Sqrt[Param["\[Alpha]EM"] 4\[Pi]]Re[wCL["DR\[ScriptL]",{2,3}]\[Conjugate](2wCL["VLL\[ScriptL]",{2,3,1,1}]+wCL["VLR\[ScriptL]",{2,3,1,1}])+wCL["DL\[ScriptL]",{2,3}]\[Conjugate](2wCL["VRR\[ScriptL]",{2,3,1,1}]+wCL["VRL\[ScriptL]",{2,3,1,1}])])/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Tau]->\[Mu]ee"] := Mass["\[Tau]"]/.GetParameters[];


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> \[Mu]\[Mu]\[Mu]*)


ExpValue$default["\[Tau]->\[Mu]\[Mu]\[Mu]"] := Around[0,2.1]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->\[Mu]\[Mu]\[Mu]"] := 0;


NPContribution$default["\[Tau]->\[Mu]\[Mu]\[Mu]"] := (Lifetime["\[Tau]"]Mass["\[Tau]"]^5)/(1536\[Pi]^3 Param["vev"]^4) (2(Abs[wCL["VLL\[ScriptL]",{2,3,2,2}]]^2+Abs[wCL["VRR\[ScriptL]",{2,3,2,2}]]^2)+(Abs[wCL["VLR\[ScriptL]",{2,3,2,2}]]^2+Abs[wCL["VRL\[ScriptL]",{2,3,2,2}]]^2)+64 Param["\[Alpha]EM"] 4\[Pi](Log[Mass["\[Tau]"]/Mass["\[Mu]"]]-11/8)(Abs[wCL["DL\[ScriptL]",{2,3}]]^2+Abs[wCL["DR\[ScriptL]",{2,3}]]^2)+8Sqrt[Param["\[Alpha]EM"] 4\[Pi]]Re[wCL["DR\[ScriptL]",{2,3}]\[Conjugate](2wCL["VLL\[ScriptL]",{2,3,2,2}]+wCL["VLR\[ScriptL]",{2,3,2,2}])+wCL["DL\[ScriptL]",{2,3}]\[Conjugate](2wCL["VRR\[ScriptL]",{2,3,2,2}]+wCL["VRL\[ScriptL]",{2,3,2,2}])])/.OlcyrtoLEFT/.GetParameters[];


LowScale["\[Tau]->\[Mu]\[Mu]\[Mu]"] := Mass["\[Tau]"]/.GetParameters[];


(* ::Subsection::Closed:: *)
(*l -> l' P*)


FlavorObservables["l->l'P"]={"\[Tau]->e\[Pi]0","\[Tau]->\[Mu]\[Pi]0","\[Tau]->e\[Eta]","\[Tau]->\[Mu]\[Eta]","\[Tau]->e\[Eta]'","\[Tau]->\[Mu]\[Eta]'"};


ObsTable["l->l'P"] := Grid[{{"l->l'P",Column[FlavorObservables["l->l'P"]]}},Dividers->All];


LowScale[Alternatives@@(FlavorObservables["l->l'P"]//Flatten)] := Mass["\[Tau]"]/.GetParameters[];


DecayConstant["\[Pi]"] = Around[130.2,0.8]*10^-3;
DecayConstant["\[Eta]q"] = Around[108,3]*10^-3;
DecayConstant["\[Eta]s"] = Around[-111,6]*10^-3;
DecayConstant["\[Eta]'q"] = Around[89,3]*10^-3;
DecayConstant["\[Eta]'s"] = Around[136,6]*10^-3;


h\[Eta]q = Around[0.001,0.003];
h\[Eta]s = Around[-0.055,0.003];
h\[Eta]pq = Around[0.001,0.002];
h\[Eta]ps = Around[0.068,0.005];


a\[Pi] = -((1-Mass["u"]/Mass["d"])/(1+Mass["u"]/Mass["d"])) (DecayConstant["\[Pi]"] Mass["\[Pi]+"]^2)/Sqrt[2]
a\[Eta] = Around[-0.022,0.002];
a\[Eta]p = Around[-0.057,-0.002];


(* ::Subsubsection::Closed:: *)
(*\[Tau]->e\[Pi]^0*)


ExpValue$default["\[Tau]->e\[Pi]0"] := Around[0,8.0]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->e\[Pi]0"] := 0;


AL["\[Tau]->e\[Pi]0"]=DecayConstant["\[Pi]"] (CVAuu-CVAdd)/Sqrt[2]+(DecayConstant["\[Pi]"] Mass["\[Pi]+"]^2)/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CSPuu-CSPdd)/Sqrt[2](*-2I a\[Pi]/Param["vev"]^2(CGR-CGL)*);
AR["\[Tau]->e\[Pi]0"]=DecayConstant["\[Pi]"] (CAAuu-CAAdd)/Sqrt[2]-(DecayConstant["\[Pi]"] Mass["\[Pi]+"]^2)/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CPPuu-CPPdd)/Sqrt[2](*+2I a\[Pi]/Param["vev"]^2(CGR+CGL)*);


NPContribution$default["\[Tau]->e\[Pi]0"] := Lifetime["\[Tau]"] Mass["\[Tau]"]^3/(256 \[Pi] Param["vev"]^4) (1-Mass["\[Pi]0"]^2/Mass["\[Tau]"]^2)^2 (Abs[AL["\[Tau]->e\[Pi]0"]]^2+Abs[AR["\[Tau]->e\[Pi]0"]]^2)/.rep\[Tau]lP[1,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau]->\[Mu]\[Pi]^0*)


ExpValue$default["\[Tau]->\[Mu]\[Pi]0"] := Around[0,1.1]*10^-7*CL90to95/2;


NumericalInput["\[Tau]->\[Mu]\[Pi]0"] := 0;


AL["\[Tau]->\[Mu]\[Pi]0"]=DecayConstant["\[Pi]"] (CVAuu-CVAdd)/Sqrt[2]+(DecayConstant["\[Pi]"] Mass["\[Pi]+"]^2)/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CSPuu-CSPdd)/Sqrt[2](*-2I a\[Pi]/Param["vev"]^2(CGR-CGL)*);
AR["\[Tau]->\[Mu]\[Pi]0"]=DecayConstant["\[Pi]"] (CAAuu-CAAdd)/Sqrt[2]-(DecayConstant["\[Pi]"] Mass["\[Pi]+"]^2)/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CPPuu-CPPdd)/Sqrt[2](*+2I a\[Pi]/Param["vev"]^2(CGR+CGL)*);


NPContribution$default["\[Tau]->\[Mu]\[Pi]0"] := Lifetime["\[Tau]"] Mass["\[Tau]"]^3/(256 \[Pi] Param["vev"]^4) (1-Mass["\[Pi]0"]^2/Mass["\[Tau]"]^2)^2 (Abs[AL["\[Tau]->\[Mu]\[Pi]0"]]^2+Abs[AR["\[Tau]->\[Mu]\[Pi]0"]]^2)/.rep\[Tau]lP[2,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> e\[Eta]*)


ExpValue$default["\[Tau]->e\[Eta]"] := Around[0,9.2]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->e\[Eta]"] := 0;


AL["\[Tau]->e\[Eta]"]=DecayConstant["\[Eta]q"] (CVAuu+CVAdd)/Sqrt[2]+DecayConstant["\[Eta]s"] CVAss+h\[Eta]q/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CSPuu+CSPdd)/Sqrt[2]+h\[Eta]s/(2Mass["\[Tau]"] Mass["s"]) CSPss(*-2I a\[Eta]/Param["vev"]^2(CGR-CGL)*);
AR["\[Tau]->e\[Eta]"]=DecayConstant["\[Eta]q"] (CAAuu+CAAdd)/Sqrt[2]+DecayConstant["\[Eta]s"] CAAss-h\[Eta]q/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CPPuu+CPPdd)/Sqrt[2]-h\[Eta]s/(2Mass["\[Tau]"] Mass["s"]) CPPss(*+2I a\[Eta]/Param["vev"]^2(CGR+CGL)*);


NPContribution$default["\[Tau]->e\[Eta]"] := Lifetime["\[Tau]"] Mass["\[Tau]"]^3/(256 \[Pi] Param["vev"]^4) (1-Mass["\[Eta]"]^2/Mass["\[Tau]"]^2)^2 (Abs[AL["\[Tau]->e\[Eta]"]]^2+Abs[AR["\[Tau]->e\[Eta]"]]^2)/.rep\[Tau]lP[1,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> \[Mu]\[Eta]*)


ExpValue$default["\[Tau]->\[Mu]\[Eta]"] := Around[0,6.5]*10^-8*CL90to95/2;


NumericalInput["\[Tau]->\[Mu]\[Eta]"] := 0;


AL["\[Tau]->\[Mu]\[Eta]"]=DecayConstant["\[Eta]q"] (CVAuu+CVAdd)/Sqrt[2]+DecayConstant["\[Eta]s"] CVAss+h\[Eta]q/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CSPuu+CSPdd)/Sqrt[2]+h\[Eta]s/(2Mass["\[Tau]"] Mass["s"]) CSPss(*-2I a\[Eta]/Param["vev"]^2(CGR-CGL)*);
AR["\[Tau]->\[Mu]\[Eta]"]=DecayConstant["\[Eta]q"] (CAAuu+CAAdd)/Sqrt[2]+DecayConstant["\[Eta]s"] CAAss-h\[Eta]q/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CPPuu+CPPdd)/Sqrt[2]-h\[Eta]s/(2Mass["\[Tau]"] Mass["s"]) CPPss(*+2I a\[Eta]/Param["vev"]^2(CGR+CGL)*);


NPContribution$default["\[Tau]->\[Mu]\[Eta]"] := Lifetime["\[Tau]"] Mass["\[Tau]"]^3/(256 \[Pi] Param["vev"]^4) (1-Mass["\[Eta]"]^2/Mass["\[Tau]"]^2)^2 (Abs[AL["\[Tau]->\[Mu]\[Eta]"]]^2+Abs[AR["\[Tau]->\[Mu]\[Eta]"]]^2)/.rep\[Tau]lP[2,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> e\[Eta]'*)


ExpValue$default["\[Tau]->e\[Eta]'"] := Around[0,1.6]*10^-7*CL90to95/2;


NumericalInput["\[Tau]->e\[Eta]'"] := 0;


AL["\[Tau]->e\[Eta]'"]=DecayConstant["\[Eta]'q"] (CVAuu+CVAdd)/Sqrt[2]+DecayConstant["\[Eta]'s"] CVAss+h\[Eta]pq/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CSPuu+CSPdd)/Sqrt[2]+h\[Eta]ps/(2Mass["\[Tau]"] Mass["s"]) CSPss(*-2I a\[Eta]p/Param["vev"]^2(CGR-CGL)*);
AR["\[Tau]->e\[Eta]'"]=DecayConstant["\[Eta]'q"] (CAAuu+CAAdd)/Sqrt[2]+DecayConstant["\[Eta]'s"] CAAss-h\[Eta]pq/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CPPuu+CPPdd)/Sqrt[2]-h\[Eta]ps/(2Mass["\[Tau]"] Mass["s"]) CPPss(*+2I a\[Eta]p/Param["vev"]^2(CGR+CGL)*);


NPContribution$default["\[Tau]->e\[Eta]'"] := Lifetime["\[Tau]"] Mass["\[Tau]"]^3/(256 \[Pi] Param["vev"]^4) (1-Mass["\[Eta]'"]^2/Mass["\[Tau]"]^2)^2 (Abs[AL["\[Tau]->e\[Eta]'"]]^2+Abs[AR["\[Tau]->e\[Eta]'"]]^2)/.rep\[Tau]lP[1,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> \[Mu]\[Eta]'*)


ExpValue$default["\[Tau]->\[Mu]\[Eta]'"] := Around[0,1.3]*10^-7*CL90to95/2;


NumericalInput["\[Tau]->\[Mu]\[Eta]'"] := 0;


AL["\[Tau]->\[Mu]\[Eta]'"]=DecayConstant["\[Eta]'q"] (CVAuu+CVAdd)/Sqrt[2]+DecayConstant["\[Eta]'s"] CVAss+h\[Eta]pq/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CSPuu+CSPdd)/Sqrt[2]+h\[Eta]ps/(2Mass["\[Tau]"] Mass["s"]) CSPss(*-2I a\[Eta]p/Param["vev"]^2(CGR-CGL)*);
AR["\[Tau]->\[Mu]\[Eta]'"]=DecayConstant["\[Eta]'q"] (CAAuu+CAAdd)/Sqrt[2]+DecayConstant["\[Eta]'s"] CAAss-h\[Eta]pq/(2Mass["\[Tau]"] (Mass["u"]+Mass["d"])/2) (CPPuu+CPPdd)/Sqrt[2]-h\[Eta]ps/(2Mass["\[Tau]"] Mass["s"]) CPPss(*+2I a\[Eta]p/Param["vev"]^2(CGR+CGL)*);


NPContribution$default["\[Tau]->\[Mu]\[Eta]'"] := Lifetime["\[Tau]"] Mass["\[Tau]"]^3/(256 \[Pi] Param["vev"]^4) (1-Mass["\[Eta]'"]^2/Mass["\[Tau]"]^2)^2 (Abs[AL["\[Tau]->\[Mu]\[Eta]'"]]^2+Abs[AR["\[Tau]->\[Mu]\[Eta]'"]]^2)/.rep\[Tau]lP[2,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsection:: *)
(*l -> l' V*)


FlavorObservables["l->l'V"]={"\[Tau]->e\[Rho]","\[Tau]->\[Mu]\[Rho]","\[Tau]->e\[Phi]","\[Tau]->\[Mu]\[Phi]"};


ObsTable["l->l'V"] := Grid[{{"l->l'V",Column[FlavorObservables["l->l'V"]]}},Dividers->All];


LowScale[Alternatives@@(FlavorObservables["l->l'V"]//Flatten)] := Mass["\[Tau]"]/.GetParameters[];


DecayConstant["\[Rho]"] = Around[209.4,1.5]*10^-3;
DecayConstant["\[Phi]"] = Around[241,18]*10^-3;


CTbar["\[Rho]"]:=(CTuu-CTdd)/Sqrt[2]-Sqrt[4\[Pi] Param["\[Alpha]EM"]]/Sqrt[2] Mass["\[Tau]"]/Mass["\[Rho]"] (2/3-(-(1/3)))(CDR+CDL)(*(fVT/fV)*)
CT5bar["\[Rho]"]:=(CT5uu-CT5dd)/Sqrt[2]-(Sqrt[4\[Pi] Param["\[Alpha]EM"]]/Sqrt[2]) (Mass["\[Tau]"]/Mass["\[Rho]"]) (2/3-(-(1/3)))(CDR-CDL)(*(fVT/fV)*)


CTbar["\[Phi]"]:=CTss-Sqrt[4\[Pi] Param["\[Alpha]EM"]](-1/3) Mass["\[Tau]"]/Mass["\[Phi]"] (CDR+CDL)(*(fVT/fV)*)
CT5bar["\[Phi]"]:=CT5ss-Sqrt[4\[Pi] Param["\[Alpha]EM"]](-1/3) (Mass["\[Tau]"]/Mass["\[Phi]"]) (CDR-CDL)(*(fVT/fV)*)


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> e\[Rho]*)


ExpValue$default["\[Tau]->e\[Rho]"] := Around[0,1.8]*CL90to95*10^-9/2;


NumericalInput["\[Tau]->e\[Rho]"] := 0;


NPContribution$default["\[Tau]->e\[Rho]"] := Lifetime["\[Tau]"] (DecayConstant["\[Rho]"]^2 Mass["\[Tau]"]^3)/(256\[Pi] Param["vev"]^4) (1-Mass["\[Rho]"]^2/Mass["\[Tau]"]^2)^2 ((1+(2Mass["\[Rho]"]^2)/Mass["\[Tau]"]^2)(Abs[(CVVuu-CVVdd)/Sqrt[2]]^2+Abs[(CAVuu-CAVdd)/Sqrt[2]]^2)+32(1+Mass["\[Rho]"]^2/(2Mass["\[Tau]"]^2))(Abs[CTbar["\[Rho]"]]^2+Abs[CT5bar["\[Rho]"]]^2)(*(fVT^2/fV^2)*)+24 Mass["\[Rho]"]/Mass["\[Tau]"] Re[CTbar["\[Rho]"] ((CVVuu-CVVdd)/Sqrt[2])\[Conjugate]-CT5bar["\[Rho]"] ((CAVuu-CAVdd)/Sqrt[2])\[Conjugate]])/.rep\[Tau]lP[1,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> \[Mu]\[Rho]*)


ExpValue$default["\[Tau]->\[Mu]\[Rho]"] := Around[0,1.2]*CL90to95*10^-8/2;


NumericalInput["\[Tau]->\[Mu]\[Rho]"] := 0;


NPContribution$default["\[Tau]->\[Mu]\[Rho]"] := Lifetime["\[Tau]"] (DecayConstant["\[Rho]"]^2 Mass["\[Tau]"]^3)/(256\[Pi] Param["vev"]^4) (1-Mass["\[Rho]"]^2/Mass["\[Tau]"]^2)^2 ((1+(2Mass["\[Rho]"]^2)/Mass["\[Tau]"]^2)(Abs[(CVVuu-CVVdd)/Sqrt[2]]^2+Abs[(CAVuu-CAVdd)/Sqrt[2]]^2)+32(1+Mass["\[Rho]"]^2/(2Mass["\[Tau]"]^2))(Abs[CTbar["\[Rho]"]]^2+Abs[CT5bar["\[Rho]"]]^2)(*(fVT^2/fV^2)*)+24 Mass["\[Rho]"]/Mass["\[Tau]"] Re[CTbar["\[Rho]"] ((CVVuu-CVVdd)/Sqrt[2])\[Conjugate]-CT5bar["\[Rho]"] ((CAVuu-CAVdd)/Sqrt[2])\[Conjugate]])/.rep\[Tau]lP[2,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> e\[Phi]*)


ExpValue$default["\[Tau]->e\[Phi]"] := Around[0,3.1]*CL90to95*10^-8/2;


NumericalInput["\[Tau]->e\[Phi]"] := 0;


NPContribution$default["\[Tau]->e\[Phi]"] := Lifetime["\[Tau]"] (DecayConstant["\[Phi]"]^2 Mass["\[Tau]"]^3)/(256\[Pi] Param["vev"]^4) (1-Mass["\[Phi]"]^2/Mass["\[Tau]"]^2)^2 ((1+(2Mass["\[Phi]"]^2)/Mass["\[Tau]"]^2)(Abs[CVVss]^2+Abs[CAVss]^2)+32(1+Mass["\[Phi]"]^2/(2Mass["\[Tau]"]^2))(Abs[CTbar["\[Phi]"]]^2+Abs[CT5bar["\[Phi]"]]^2)(*(fVT^2/fV^2)*)+24 Mass["\[Phi]"]/Mass["\[Tau]"] Re[CTbar["\[Phi]"]CVVss\[Conjugate]-CT5bar["\[Phi]"] CAVss\[Conjugate]])/.rep\[Tau]lP[1,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection::Closed:: *)
(*\[Tau] -> \[Mu]\[Phi]*)


ExpValue$default["\[Tau]->\[Mu]\[Phi]"] := Around[0,3.1]*CL90to95*10^-8/2;


NumericalInput["\[Tau]->\[Mu]\[Phi]"] := 0;


NPContribution$default["\[Tau]->\[Mu]\[Phi]"] := Lifetime["\[Tau]"] (DecayConstant["\[Phi]"]^2 Mass["\[Tau]"]^3)/(256\[Pi] Param["vev"]^4) (1-Mass["\[Phi]"]^2/Mass["\[Tau]"]^2)^2 ((1+(2Mass["\[Phi]"]^2)/Mass["\[Tau]"]^2)(Abs[CVVss]^2+Abs[CAVss]^2)+32(1+Mass["\[Phi]"]^2/(2Mass["\[Tau]"]^2))(Abs[CTbar["\[Phi]"]]^2+Abs[CT5bar["\[Phi]"]]^2)(*(fVT^2/fV^2)*)+24 Mass["\[Phi]"]/Mass["\[Tau]"] Re[CTbar["\[Phi]"]CVVss\[Conjugate]-CT5bar["\[Phi]"] CAVss\[Conjugate]])/.rep\[Tau]lP[2,3]/.repRot/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsection:: *)
(*lN -> l' N*)


FlavorObservables["lN->l'N"]={"\[Mu]Au->eAu"(*,"\[Mu]Al->eAl"*)};


ObsTable["lN->l'N"] := Grid[{{"lN->l'N",Column[FlavorObservables["lN->l'N"]]}},Dividers->All];


LowScale[Alternatives@@(FlavorObservables["lN->l'N"]//Flatten)] := Mass["p"]/.GetParameters[];


(* ::Subsubsection:: *)
(*Nucleon EFT*)


(* Nucleon form-factors *)
(* V *)
fVu["p"] = 2;
fVd["p"] = 1;
fVs["p"] = 0;
fVu["n"] = 1;
fVd["n"] = 2;
fVs["n"] = 0;
(* S *)
fSu["p"] = Around[20.8,1.5]*10^-3;
fSd["p"] = Around[41.1,2.8]*10^-3;
fSs["p"] = Around[53,27]*10^-3;
fSu["n"] = Around[18.9,1.4]*10^-3;
fSd["n"] = Around[45.1,2.7]*10^-3;
fSs["n"] = Around[53,27]*10^-3;


(* G - Nb. pay attention to normalization *)
fG["n"] = -((8\[Pi])/9)(1-fSu["n"]-fSd["n"]-fSs["n"]);
fG["p"] = -((8\[Pi])/9)(1-fSu["p"]-fSd["p"]-fSs["p"]);


(* Defining nucleon EFT *)
(* p *)
(* Vector *)
CVRp = (wCL["VRRu",{1,2,1,1}]+wCL["VRLu",{1,2,1,1}])fVu["p"]+(wCL["VRRd",{1,2,1,1}]+wCL["VRLd",{1,2,1,1}])fVd["p"];
CVLp = (wCL["VLRu",{1,2,1,1}]+wCL["VLLu",{1,2,1,1}])fVu["p"]+(wCL["VLRd",{1,2,1,1}]+wCL["VLLd",{1,2,1,1}])fVd["p"];
(* Scalar *)
CSRp = Mass["p"]/Mass["u"] (wCL["SRRu",{1,2,1,1}]+wCL["SRLu",{1,2,1,1}])fSu["p"]+Mass["p"]/Mass["d"] (wCL["SRRd",{1,2,1,1}]+wCL["SRLd",{1,2,1,1}])fSd["p"]+Mass["p"]/Mass["s"] (wCL["SRRd",{1,2,2,2}]+wCL["SRLd",{1,2,2,2}])fSs["p"]+(Mass["\[Mu]"] Mass["p"] )/(4\[Pi] Param["vev"]^2) (0*wCL["GR\[ScriptL]",{1,2}]-Param["vev"]^2/(3Mass["\[Mu]"] Mass["b"]) (wCL["SRRd",{1,2,3,3}]+wCL["SRLd",{1,2,3,3}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["c"]) (wCL["SRRu",{1,2,2,2}]+wCL["SRLu",{1,2,2,2}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["t"]) (wCL["SRRu",{1,2,3,3}]+wCL["SRLu",{1,2,3,3}])) fG["p"];
CSLp = Mass["p"]/Mass["u"] (wCL["SLRu",{1,2,1,1}]+wCL["SLLu",{1,2,1,1}])fSu["p"]+Mass["p"]/Mass["d"] (wCL["SLRd",{1,2,1,1}]+wCL["SLLd",{1,2,1,1}])fSd["p"]+Mass["p"]/Mass["s"] (wCL["SLRd",{1,2,2,2}]+wCL["SLLd",{1,2,2,2}])fSs["p"]+(Mass["\[Mu]"] Mass["p"] )/(4\[Pi] Param["vev"]^2) (0*wCL["GL\[ScriptL]",{1,2}]-Param["vev"]^2/(3Mass["\[Mu]"] Mass["b"]) (wCL["SLRd",{1,2,3,3}]+wCL["SLLd",{1,2,3,3}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["c"]) (wCL["SLRu",{1,2,2,2}]+wCL["SLLu",{1,2,2,2}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["t"]) (wCL["SLRu",{1,2,3,3}]+wCL["SLLu",{1,2,3,3}])) fG["p"];
(* n *)
(* Vector*)
CVRn = (wCL["VRRu",{1,2,1,1}]+wCL["VRLu",{1,2,1,1}])fVu["n"]+(wCL["VRRd",{1,2,1,1}]+wCL["VRLd",{1,2,1,1}])fVd["n"];
CVLn = (wCL["VLRu",{1,2,1,1}]+wCL["VLLu",{1,2,1,1}])fVu["n"]+(wCL["VLRd",{1,2,1,1}]+wCL["VLLd",{1,2,1,1}])fVd["n"];
(* Scalar *)
CSRn = Mass["n"]/Mass["u"] (wCL["SRRu",{1,2,1,1}]+wCL["SRLu",{1,2,1,1}])fSu["n"]+Mass["n"]/Mass["d"] (wCL["SRRd",{1,2,1,1}]+wCL["SRLd",{1,2,1,1}])fSd["n"]+Mass["n"]/Mass["s"] (wCL["SRRd",{1,2,2,2}]+wCL["SRLd",{1,2,2,2}])fSs["n"]+(Mass["\[Mu]"] Mass["n"] )/(4\[Pi] Param["vev"]^2) (0*wCL["GR\[ScriptL]",{1,2}]-Param["vev"]^2/(3Mass["\[Mu]"] Mass["b"]) (wCL["SRRd",{1,2,3,3}]+wCL["SRLd",{1,2,3,3}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["c"]) (wCL["SRRu",{1,2,2,2}]+wCL["SRLu",{1,2,2,2}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["t"]) (wCL["SRRu",{1,2,3,3}]+wCL["SRLu",{1,2,3,3}])) fG["n"];
CSLn = Mass["n"]/Mass["u"] (wCL["SLRu",{1,2,1,1}]+wCL["SLLu",{1,2,1,1}])fSu["n"]+Mass["n"]/Mass["d"] (wCL["SLRd",{1,2,1,1}]+wCL["SLLd",{1,2,1,1}])fSd["n"]+Mass["n"]/Mass["s"] (wCL["SLRd",{1,2,2,2}]+wCL["SLLd",{1,2,2,2}])fSs["n"]+(Mass["\[Mu]"] Mass["n"] )/(4\[Pi] Param["vev"]^2) (0*wCL["GL\[ScriptL]",{1,2}]-Param["vev"]^2/(3Mass["\[Mu]"] Mass["b"]) (wCL["SLRd",{1,2,3,3}]+wCL["SLLd",{1,2,3,3}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["c"]) (wCL["SLRu",{1,2,2,2}]+wCL["SLLu",{1,2,2,2}])-Param["vev"]^2/(3Mass["\[Mu]"] Mass["t"]) (wCL["SLRu",{1,2,3,3}]+wCL["SLLu",{1,2,3,3}])) fG["n"];


(* ::Subsubsection:: *)
(*\[Mu]Au -> eAu*)


\[CapitalGamma]capt["Au"] = 1.32 10^-5 1/10^-12 6.582 10^-25;


(* Overlap integrals *)
DD["Au"] = 0.189;
Vp["Au"] = 0.0974;
Vn["Au"] = 0.146;
Sp["Au"] = 0.0614;
Sn["Au"] = 0.0918;


ExpValue$default["\[Mu]Au->eAu"] := Around[0,7]*10^-13*CL90to95/2;


NumericalInput["\[Mu]Au->eAu"] := 0;


NPContribution$default["\[Mu]Au->eAu"] := Mass["\[Mu]"]^5/(4Param["vev"]^4 \[CapitalGamma]capt["Au"]) (Abs[wCL["DL\[ScriptL]",{1,2}]DD["Au"]+1/2 4(CVRp Vp["Au"]+CSLp Sp["Au"]+CVRn Vn["Au"]+CSLn Sn["Au"])]^2+Abs[wCL["DR\[ScriptL]",{1,2}]DD["Au"]+1/2 4(CVLp Vp["Au"]+CSRp Sp["Au"]+CVLn Vn["Au"]+CSRn Sn["Au"])]^2)/.OlcyrtoLEFT/.GetParameters[]/.Around[a_,b_]->a;


(* ::Subsubsection:: *)
(*\[Mu]Al -> eAl*)


\[CapitalGamma]capt["Al"] = 6.99 10^-7 1/10^-12 6.582 10^-25;


(* Overlap integrals *)
DD["Al"] = 0.0362;
Vp["Al"] = 0.0161;
Vn["Al"] = 0.0173;
Sp["Al"] = 0.0155;
Sn["Al"] = 0.0167;


(*ExpValue$default["\[Mu]Al->eAl"] := Around[0,7]*10^-13*CL90to95/2;*)


(*NumericalInput["\[Mu]Al->eAl"] := 0;*)


(*NPContribution["\[Mu]Al->eAl"] := Mass["\[Mu]"]^5/(4Param["vev"]^4\[CapitalGamma]capt["Al"])(Abs[wCL["DL\[ScriptL]",{1,2}]DD["Al"]+1/24(CVRp Vp["Al"]+CSLp Sp["Al"]+CVRn Vn["Al"]+CSLn Sn["Al"])]^2+Abs[wCL["DR\[ScriptL]",{1,2}]DD["Al"]+1/24(CVLp Vp["Al"]+CSRp Sp["Al"]+CVLn Vn["Al"]+CSRn Sn["Al"])]^2)/.OlcyrtoLEFT/.GetParameters/.Around[a_,b_]->a;*)


(* ::Section:: *)
(*Higgs and Z*)


$EWLFVSectors={"HLFV","ZLFV"};


FlavorObservables["EWLFV"] = FlavorObservables/@$EWLFVSectors


ObsTable["EWLFV"] := Grid[{{"EWLFV",Column[ObsTable/@$EWLFVSectors]}},Dividers->All];


(* ::Subsection:: *)
(*H -> ll'*)


FlavorObservables["HLFV"] = {"H->e\[Mu]","H->e\[Tau]","H->\[Mu]\[Tau]"};


ObsTable["HLFV"] := Grid[{{"HLFV",Column[FlavorObservables["HLFV"]]}},Dividers->All];


(* ::Subsubsection:: *)
(*H -> e\[Mu]*)


ExpValue$default["H->e\[Mu]"] := Around[0,4.4]*10^-5/2;


NumericalInput["H->e\[Mu]"] := 0;


NPContribution$default["H->e\[Mu]"] := 2 (Mass["H"] Param["vev"]^4)/(32\[Pi] Width["H"]) (Abs[WC["eH",{1,2}]+WC["eH",{2,1}]]^2)/.GetParameters[];


LowScale["H->e\[Mu]"] := Mass["H"]/.GetParameters[];


(* ::Subsubsection:: *)
(*H -> e\[Tau]*)


ExpValue$default["H->e\[Tau]"] := Around[0,2.0]*10^-3/2;


NumericalInput["H->e\[Tau]"] := 0;


NPContribution$default["H->e\[Tau]"] := 2 (Mass["H"] Param["vev"]^4)/(32\[Pi] Width["H"]) (Abs[WC["eH",{1,3}]+WC["eH",{3,1}]]^2)/.GetParameters[];


LowScale["H->e\[Tau]"] := Mass["H"]/.GetParameters[];


(* ::Subsubsection:: *)
(*H -> \[Mu]\[Tau]*)


ExpValue$default["H->\[Mu]\[Tau]"] := Around[0,1.5]*10^-3/2;


NumericalInput["H->\[Mu]\[Tau]"] := 0;


NPContribution$default["H->\[Mu]\[Tau]"] := 2 (Mass["H"] Param["vev"]^4)/(32\[Pi] Width["H"]) (Abs[WC["eH",{2,3}]+WC["eH",{3,2}]]^2)/.GetParameters[];


LowScale["H->\[Mu]\[Tau]"] := Mass["H"]/.GetParameters[];


(* ::Subsection:: *)
(*Z -> ll'*)


FlavorObservables["ZLFV"] = {"Z->e\[Mu]","Z->e\[Tau]","Z->\[Mu]\[Tau]"};


ObsTable["ZLFV"] := Grid[{{"ZLFV",Column[FlavorObservables["ZLFV"]]}},Dividers->All];


(* ::Subsubsection:: *)
(*Z -> e\[Mu]*)


ExpValue$default["Z->e\[Mu]"] := Around[0,2.62]*10^-7/2;


NumericalInput["Z->e\[Mu]"] := 0;


NPContribution$default["Z->e\[Mu]"] := 2 (Mass["ZBoson"]^3 Param["vev"]^2)/(24\[Pi] Width["ZBoson"]) (
Abs[WC["Hl1",{1,2}]+WC["Hl3",{1,2}]]^2+Abs[WC["He",{1,2}]]^2
+Abs[Param["cW"]WC["eW",{1,2}]+Param["sW"]WC["eB",{1,2}]]^2+
Abs[Param["cW"]WC["eW",{2,1}]+Param["sW"]WC["eB",{2,1}]]^2)/.GetParameters[];


LowScale["Z->e\[Mu]"] := Mass["ZBoson"]/.GetParameters[];


(* ::Subsubsection:: *)
(*Z -> e\[Tau]*)


ExpValue$default["Z->e\[Tau]"] := Around[0,5.0]*10^-6/2;


NumericalInput["Z->e\[Tau]"] := 0;


NPContribution$default["Z->e\[Tau]"] := 2 (Mass["ZBoson"]^3 Param["vev"]^2)/(24\[Pi] Width["ZBoson"]) (
Abs[WC["Hl1",{1,3}]+WC["Hl3",{1,3}]]^2+Abs[WC["He",{1,3}]]^2
+Abs[Param["cW"]WC["eW",{1,3}]+Param["sW"]WC["eB",{1,3}]]^2+
Abs[Param["cW"]WC["eW",{3,1}]+Param["sW"]WC["eB",{3,1}]]^2)/.GetParameters[];


LowScale["Z->e\[Tau]"] := Mass["ZBoson"]/.GetParameters[];


(* ::Subsubsection:: *)
(*Z -> \[Mu]\[Tau]*)


ExpValue$default["Z->\[Mu]\[Tau]"] := Around[0,6.5]*10^-6/2;


NumericalInput["Z->\[Mu]\[Tau]"] := 0;


NPContribution$default["Z->\[Mu]\[Tau]"] := 2 (Mass["ZBoson"]^3 Param["vev"]^2)/(24\[Pi] Width["ZBoson"]) (
Abs[WC["Hl1",{1,3}]+WC["Hl3",{2,3}]]^2+Abs[WC["He",{2,3}]]^2
+Abs[Param["cW"]WC["eW",{2,3}]+Param["sW"]WC["eB",{2,3}]]^2+
Abs[Param["cW"]WC["eW",{3,2}]+Param["sW"]WC["eB",{3,2}]]^2)/.GetParameters[];


LowScale["Z->\[Mu]\[Tau]"] := Mass["ZBoson"]/.GetParameters[];


(* ::Section::Closed:: *)
(*\[CapitalDelta]F = 1*)


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
