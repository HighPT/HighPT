(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SmeftLeftMatching`*)


(* ::Subtitle:: *)
(*Tree-level SMEFT-LEFT Matching *)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["SMEFTLEFTMatching"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["$NPScale"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*cc*)


SMEFTLEFTMatching["cc"]={
WCL["VLudl\[Nu]",{i_,j_,k_,l_}]->-(ConstantInput["vev"]^2/$NPScale^2)Sum[Vckm[i,p]/Vckm[i,j]*(WC["lq3",{k,l,p,j}]+KroneckerDelta[k,l]WC["Hq3",{p,j}]-KroneckerDelta[p,j]WC["Hl3",{k,l}]),{p,3}],
WCL["VRudl\[Nu]",{i_,j_,k_,l_}]->ConstantInput["vev"]^2/(2$NPScale^2) 1/Vckm[i,j] KroneckerDelta[k,l]WC["Hud",{i,j}],
WCL["SLudl\[Nu]",{i_,j_,k_,l_}]->-(ConstantInput["vev"]^2/(2$NPScale^2)) 1/Vckm[i,j] WC["lequ1",{l,k,j,i}]\[Conjugate],
WCL["SRudl\[Nu]",{i_,j_,k_,l_}]->-(ConstantInput["vev"]^2/(2$NPScale^2)) 1/Vckm[i,j] Sum[Vckm[i,p]\[Conjugate]/Vckm[i,j] WC["ledq",{l,k,j,p}],{p,3}],
WCL["Tudl\[Nu]",{i_,j_,k_,l_}]->-(ConstantInput["vev"]^2/(2$NPScale^2)) 1/Vckm[i,j] WC["lequ3",{l,k,j,i}]\[Conjugate]
};


(* ::Section:: *)
(*\[CapitalDelta]F=1*)


(* ::Section:: *)
(*\[CapitalDelta]F=2*)


(* ::Section:: *)
(*ll\[Nu]\[Nu]*)


(* ::Section:: *)
(*ll\[Gamma]*)


(* ::Section:: *)
(*llll*)


(* ::Section:: *)
(*full*)


SMEFTLEFTMatching["full"] = Join[
	SMEFTLEFTMatching["cc"],
	{}
];
