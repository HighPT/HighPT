(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`EvolutionMatrices`*)


(* ::Subtitle:: *)
(*Evolution Matrices to be used by LEFTRun*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["EvolutionMatrix"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*cc*)


EvolutionMatrix["cc","\[Mu]b","\[Mu]Z"]={
{1,0,0,0,0},
{0,1,0,0,0},
{0,0,1.46,0,-0.02},
{0,0,0,1.46,0},
{0,0,0,0,0.88}
};


EvolutionMatrix["cc","\[Mu]had","\[Mu]Z"]={
{1,0,0,0,0},
{0,1,0,0,0},
{0,0,1.72,0,-0.02},
{0,0,0,1.72,0},
{0,0,0,0,0.82}
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
