(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChargedCurrents`*)


(* ::Subtitle:: *)
(*Generalities about charged current flavor observables*)


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


$ChargedCurrentSectors={"b->c"};


FlavorObservables["ChargedCurrents"] = FlavorObservables/@$ChargedCurrentSectors


(* ::Section:: *)
(*LEFT Sector*)


LEFTSector[Alternatives@@(FlavorObservables["ChargedCurrents"]//Flatten)] = "cc"


(* ::Section:: *)
(*Semileptonic Inputs*)


(*(* Vectors of Eff. Coeff. *)
(* P\[Rule]P^'\[ScriptL]\[Nu] *)
coeffPPplnu={Abs[1+gV]^2,Abs[gT]^2,Re[(1+gV) Conjugate[gT]],Abs[gS]^2,Re[(1+gV) Conjugate[gS]]};
(* P\[Rule]V\[ScriptL]\[Nu] *)
coeffPVlnu={Abs[1+gVL]^2+Abs[gVR]^2,Re[(1+gVL)Conjugate[gVR]],Abs[gT]^2,Re[(1+gVL)Conjugate[gT]],Re[gVR Conjugate[gT]],Abs[gP]^2,Re[(1+gVL-gVR)Conjugate[gP]]};*)


(*(* Semileptonic replacement *)
RepSemilep={
gVL->WCL[VLudl\[Nu],ind],
gVR->WCL[VRudl\[Nu],ind],
gV->+WCL[VLudl\[Nu],ind]+WCL[VRudl\[Nu],ind],
gA->-WCL[VLudl\[Nu],ind]+WCL[VRudl\[Nu],ind],
gS->+WCL[SLudl\[Nu],ind]+WCL[SRudl\[Nu],ind],
gP->-WCL[SLudl\[Nu],ind]+WCL[SRudl\[Nu],ind],
gT->WCL[Tudl\[Nu],ind]};*)
