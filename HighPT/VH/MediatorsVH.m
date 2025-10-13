(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Mediator`for VH production*)


(* ::Subtitle:: *)
(*Provides SM mediator implementation for VH production.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["SubstitutionRulesMediatorsVH"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Matching of the form-factors with the couplings of specific mediators*)


(* ::Subsection:: *)
(*SM mediators*)


(* ::Subsubsection:: *)
(*Z*)


SubstitutionRulesMediatorsVH["ZBoson"] = {
	ff[{Vector, 1}, {"ZBoson", SM}, X_, {\[Psi]_[i_], \[Psi]_[j_]}] :> Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(2 * Param["sW"]^3 * Param["cW"]^3) * (WeakIsospin3[\[Psi], X] - Param["sW"]^2 * Charge[\[Psi]]) * KroneckerDelta[i, j]
}
