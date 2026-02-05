(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChargedCurrents`*)


(* ::Subtitle:: *)
(*Charged current flavor observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["SEW"]
PackageScope["Phi"]
PackageScope["PhiPPpl\[Nu]Rep"]
PackageScope["PhiPPpl\[Nu]CorrRep"]


(* ::Chapter:: *)
(*Private:*)


(*$ChargedCurrentSectors={"b->c"};*)


(*FlavorObservables["ChargedCurrents"] = FlavorObservables/@$ChargedCurrentSectors*)


(*ObsTable["ChargedCurrents"] := Grid[{{"ChargedCurrents",Column[ObsTable/@$ChargedCurrentSectors]}},Dividers->All];*)


ObservableSectors["ChargedCurrentsOlcyr"] := {"b->cOlcyr","s->uOlcyr"};
ObservableList["ChargedCurrentsOlcyr"] := ObservableList/@ObservableSectors["ChargedCurrentsOlcyr"]


(* ::Section:: *)
(*Semileptonic inputs *)


(* ::Subsection:: *)
(*Semileptonic inputs*)


(* Short-distance corrections -- TODO LATER, Sew, \[Delta] and other stuff *)


SubstituteCC = {};


(* ::Subsection:: *)
(*Importing phase-space integrals*)


(* Importing *)


Get@FileNameJoin[{Global`$DirectoryHighPT,"Flavor","Inputs","PhiPPplnu.wl"}];
Get@FileNameJoin[{Global`$DirectoryHighPT,"Flavor","Inputs","PhiPPplnuCorr.wl"}];


(* Extracting the vectors of Wilson coefficients *)
(* San Diego basis: {cVL,cVLR,cSRR,cSRL,cTRR} *)


VecWC["B0->D+e\[Nu]"] := DeleteDuplicates@Cases[Keys[PhiPPpl\[Nu]Rep["B0->D+e\[Nu]"]],WCL[_,_],Infinity];
VecWC["B0->D+\[Mu]\[Nu]"] := DeleteDuplicates@Cases[Keys[PhiPPpl\[Nu]Rep["B0->D+\[Mu]\[Nu]"]],WCL[_,_],Infinity];
VecWC["B0->D+\[Tau]\[Nu]"] := DeleteDuplicates@Cases[Keys[PhiPPpl\[Nu]Rep["B0->D+\[Tau]\[Nu]"]],WCL[_,_],Infinity];


(* ::Subsection:: *)
(*Reconstructing the observables*)


(* Decays width: mean and uncertainty *)


mean\[CapitalGamma]obs[proc_String] := Sum[Re[c1*c2\[Conjugate]]*Phi[proc,c1,c2],{c1,VecWC[proc]},{c2,VecWC[proc]}](*/.PhiPPpl\[Nu]Rep[proc]/._Phi\[Rule]0/.Around[a_,b_]\[RuleDelayed]a*)


\[Sigma]\[CapitalGamma]obs[proc_String] := Sqrt[Sum[
	Re[c1 c2\[Conjugate]]Re[d1 d2\[Conjugate]]Phi[proc,c1,c2]["Uncertainty"]Phi[proc,d1,d2]["Uncertainty"] PhiCorr[Phi[proc,c1,c2],Phi[proc,d1,d2]],
{c1,cChiral["PtoPpl\[Nu]"]},{c2,cChiral["PtoPpl\[Nu]"]},{d1,cChiral["PtoPpl\[Nu]"]},{d2,cChiral["PtoPpl\[Nu]"]}]](*/.PhiPPpl\[Nu]CorrRep[proc,proc]/._PhiCorr\[Rule]0/.PhiPPpl\[Nu]Rep[proc]*)


\[CapitalGamma]obs[proc_String] := Around[(mean\[CapitalGamma]obs[proc]/.PhiPPpl\[Nu]Rep[proc]/._Phi->0/.Around[a_,b_]:>a),(\[Sigma]\[CapitalGamma]obs[proc]/.PhiPPpl\[Nu]CorrRep[proc,proc]/._PhiCorr->0/.PhiPPpl\[Nu]Rep[proc])]


(* Ratio of decay widths: todo *)


(* ::Section:: *)
(*b -> c l \[Nu]*)


(* List of observables [TODO: to comment observables] *)


ObservableList["b->cOlcyr"] = {"B0->D+e\[Nu]Olcyr", "B0->D+\[Mu]\[Nu]Olcyr", "B0->D+\[Tau]\[Nu]Olcyr", "RD(\[Mu]/e)" ,"RD(\[Tau]/l)"};


(* ::Subsection:: *)
(*Semileptonic*)


(* TO CORRECT: I am not summing on neutrino flavors *)


(* ::Subsubsection:: *)
(*B0 -> D+e\[Nu]*)


(* ::Subsubsection:: *)
(*B0 -> D+\[Mu]\[Nu]*)


(* ::Subsubsection:: *)
(*B0 -> D+\[Tau]\[Nu]*)


(* To be removed later *)


PhiPPpl\[Nu]Rep["B0->D+\[Tau]\[Nu]Olcyr"] := PhiPPpl\[Nu]Rep["B0->D+\[Tau]\[Nu]"]


TheoryExpression["B0->D+\[Tau]\[Nu]Olcyr"] := Lifetime["B0"]*SEW*mean\[CapitalGamma]obs["B0->D+\[Tau]\[Nu]"]/.Around[a_,b_]->a


(* TO CHANGE LATER! *)


ExpValue$default["B0->D+\[Tau]\[Nu]Olcyr"] := Around[{0.1,0.02}]
ExpInfo["B0->D+\[Tau]\[Nu]Olcyr"] := ""


SMPrediction$default["B0->D+\[Tau]\[Nu]Olcyr"] := Lifetime["B0"]*SEW*\[CapitalGamma]obs["B0->D+\[Tau]\[Nu]"]/.a_WCL->SMValue[a,TreeOnly->True]/.GetParameters[Errors->False]/.SubstitutePsi
SMInfo["B0->D+\[Tau]\[Nu]Olcyr"] := "FLAG 2+1 average"


NPContribution$default["B0->D+\[Tau]\[Nu]Olcyr"] := NPFromTheoryExpression["B0->D+\[Tau]\[Nu]Olcyr"]/.SubstitutePsi


LowScale$default["B0->D+\[Tau]\[Nu]Olcyr"] := Mass["b"]/.GetParameters[]


(* ::Subsubsection:: *)
(*RD(\[Mu]/e)*)


(* *)


(* ::Subsection:: *)
(*Leptonic*)


me = {Mass["e"],Mass["\[Mu]"],Mass["\[Tau]"]};


(* ::Subsubsection:: *)
(*Bc -> \[Tau]\[Nu]*)


(* TO BE FIXED: need mc at mb! *)


Bctol\[Nu][lep_] := Lifetime["Bc"]/(64 \[Pi]) DecayConstant["Bc"]^2 Mass["Bc"]me[[lep]]^2 (1-me[[lep]]^2/Mass["Bc"]^2)^2 Sum[Abs[WCL["\[Nu]eduVLL",{j,lep,3,2}]-WCL["\[Nu]eduVLR",{j,lep,3,2}]-Mass["Bc"]^2/(Mass["c"]+Mass["b"]) (WCL["\[Nu]eduSRR",{j,lep,3,2}]-WCL["\[Nu]eduSRL",{j,lep,2,2}])]^2,{j,3}]


ExpValue$default["Bc-\[Tau]\[Nu]Olcyr"] := Around[{0,0.3}]
ExpInfo["Bc-\[Tau]\[Nu]Olcyr"] := "Condition that the total width is not saturated"


LowScale$default["Bc-\[Tau]\[Nu]Olcyr"] := Mass["b"]/.GetParameters[]
