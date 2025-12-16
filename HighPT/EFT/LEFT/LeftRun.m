(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`LeftRun`*)


(* ::Subtitle:: *)
(*Running in the Low Energy Effective Theory*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["LEFTRun"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["LEFTAD"]


PackageScope["ReplaceRedundantLEFT"]


(*PackageScope["LEFTSimplify"]*)


(* ::Chapter:: *)
(*Private:*)


ReplaceRedundantLEFT = <|
(* \[Nu]\[Nu]VLL *)
WCL["\[Nu]\[Nu]VLL",{1,1,1,2}]:>WCL["\[Nu]\[Nu]VLL",{1,1,1,2}]+WCL["\[Nu]\[Nu]VLL",{1,2,1,1}],
WCL["\[Nu]\[Nu]VLL",{1,1,1,3}]:>WCL["\[Nu]\[Nu]VLL",{1,1,1,3}]+WCL["\[Nu]\[Nu]VLL",{1,3,1,1}],
WCL["\[Nu]\[Nu]VLL",{1,2,1,3}]:>WCL["\[Nu]\[Nu]VLL",{1,2,1,3}]+WCL["\[Nu]\[Nu]VLL",{1,3,1,2}],
WCL["\[Nu]\[Nu]VLL",{1,1,2,2}]:>WCL["\[Nu]\[Nu]VLL",{1,1,2,2}]+WCL["\[Nu]\[Nu]VLL",{2,2,1,1}],
WCL["\[Nu]\[Nu]VLL",{1,2,2,2}]:>WCL["\[Nu]\[Nu]VLL",{1,2,2,2}]+WCL["\[Nu]\[Nu]VLL",{2,2,1,2}],
WCL["\[Nu]\[Nu]VLL",{1,1,2,3}]:>WCL["\[Nu]\[Nu]VLL",{1,1,2,3}]+WCL["\[Nu]\[Nu]VLL",{2,3,1,1}],
WCL["\[Nu]\[Nu]VLL",{1,2,2,3}]:>WCL["\[Nu]\[Nu]VLL",{1,2,2,3}]+WCL["\[Nu]\[Nu]VLL",{2,3,1,2}],
WCL["\[Nu]\[Nu]VLL",{1,3,2,3}]:>WCL["\[Nu]\[Nu]VLL",{1,3,2,3}]+WCL["\[Nu]\[Nu]VLL",{2,3,1,3}],
WCL["\[Nu]\[Nu]VLL",{2,2,2,3}]:>WCL["\[Nu]\[Nu]VLL",{2,2,2,3}]+WCL["\[Nu]\[Nu]VLL",{2,3,2,2}],
WCL["\[Nu]\[Nu]VLL",{1,2,3,2}]:>WCL["\[Nu]\[Nu]VLL",{1,2,3,2}]+WCL["\[Nu]\[Nu]VLL",{3,2,1,2}],
WCL["\[Nu]\[Nu]VLL",{1,1,3,3}]:>WCL["\[Nu]\[Nu]VLL",{1,1,3,3}]+WCL["\[Nu]\[Nu]VLL",{3,3,1,1}],
WCL["\[Nu]\[Nu]VLL",{1,2,3,3}]:>WCL["\[Nu]\[Nu]VLL",{1,2,3,3}]+WCL["\[Nu]\[Nu]VLL",{3,3,1,2}],
WCL["\[Nu]\[Nu]VLL",{1,3,3,3}]:>WCL["\[Nu]\[Nu]VLL",{1,3,3,3}]+WCL["\[Nu]\[Nu]VLL",{3,3,1,3}],
WCL["\[Nu]\[Nu]VLL",{2,2,3,3}]:>WCL["\[Nu]\[Nu]VLL",{2,2,3,3}]+WCL["\[Nu]\[Nu]VLL",{3,3,2,2}],
WCL["\[Nu]\[Nu]VLL",{2,3,3,3}]:>WCL["\[Nu]\[Nu]VLL",{2,3,3,3}]+WCL["\[Nu]\[Nu]VLL",{3,3,2,3}],
(* eeVLL *)
WCL["eeVLL",{1,1,1,2}]:>WCL["eeVLL",{1,1,1,2}]+WCL["eeVLL",{1,2,1,1}],
WCL["eeVLL",{1,1,1,3}]:>WCL["eeVLL",{1,1,1,3}]+WCL["eeVLL",{1,3,1,1}],
WCL["eeVLL",{1,2,1,3}]:>WCL["eeVLL",{1,2,1,3}]+WCL["eeVLL",{1,3,1,2}],
WCL["eeVLL",{1,1,2,2}]:>WCL["eeVLL",{1,1,2,2}]+WCL["eeVLL",{2,2,1,1}],
WCL["eeVLL",{1,2,2,2}]:>WCL["eeVLL",{1,2,2,2}]+WCL["eeVLL",{2,2,1,2}],
WCL["eeVLL",{1,1,2,3}]:>WCL["eeVLL",{1,1,2,3}]+WCL["eeVLL",{2,3,1,1}],
WCL["eeVLL",{1,2,2,3}]:>WCL["eeVLL",{1,2,2,3}]+WCL["eeVLL",{2,3,1,2}],
WCL["eeVLL",{1,3,2,3}]:>WCL["eeVLL",{1,3,2,3}]+WCL["eeVLL",{2,3,1,3}],
WCL["eeVLL",{2,2,2,3}]:>WCL["eeVLL",{2,2,2,3}]+WCL["eeVLL",{2,3,2,2}],
WCL["eeVLL",{1,2,3,2}]:>WCL["eeVLL",{1,2,3,2}]+WCL["eeVLL",{3,2,1,2}],
WCL["eeVLL",{1,1,3,3}]:>WCL["eeVLL",{1,1,3,3}]+WCL["eeVLL",{3,3,1,1}],
WCL["eeVLL",{1,2,3,3}]:>WCL["eeVLL",{1,2,3,3}]+WCL["eeVLL",{3,3,1,2}],
WCL["eeVLL",{1,3,3,3}]:>WCL["eeVLL",{1,3,3,3}]+WCL["eeVLL",{3,3,1,3}],
WCL["eeVLL",{2,2,3,3}]:>WCL["eeVLL",{2,2,3,3}]+WCL["eeVLL",{3,3,2,2}],
WCL["eeVLL",{2,3,3,3}]:>WCL["eeVLL",{2,3,3,3}]+WCL["eeVLL",{3,3,2,3}],
(* ddVLL *)
WCL["ddVLL",{1,1,1,2}]:>WCL["ddVLL",{1,1,1,2}]+WCL["ddVLL",{1,2,1,1}],
WCL["ddVLL",{1,1,1,3}]:>WCL["ddVLL",{1,1,1,3}]+WCL["ddVLL",{1,3,1,1}],
WCL["ddVLL",{1,2,1,3}]:>WCL["ddVLL",{1,2,1,3}]+WCL["ddVLL",{1,3,1,2}],
WCL["ddVLL",{1,1,2,2}]:>WCL["ddVLL",{1,1,2,2}]+WCL["ddVLL",{2,2,1,1}],
WCL["ddVLL",{1,2,2,2}]:>WCL["ddVLL",{1,2,2,2}]+WCL["ddVLL",{2,2,1,2}],
WCL["ddVLL",{1,1,2,3}]:>WCL["ddVLL",{1,1,2,3}]+WCL["ddVLL",{2,3,1,1}],
WCL["ddVLL",{1,2,2,3}]:>WCL["ddVLL",{1,2,2,3}]+WCL["ddVLL",{2,3,1,2}],
WCL["ddVLL",{1,3,2,3}]:>WCL["ddVLL",{1,3,2,3}]+WCL["ddVLL",{2,3,1,3}],
WCL["ddVLL",{2,2,2,3}]:>WCL["ddVLL",{2,2,2,3}]+WCL["ddVLL",{2,3,2,2}],
WCL["ddVLL",{1,2,3,2}]:>WCL["ddVLL",{1,2,3,2}]+WCL["ddVLL",{3,2,1,2}],
WCL["ddVLL",{1,1,3,3}]:>WCL["ddVLL",{1,1,3,3}]+WCL["ddVLL",{3,3,1,1}],
WCL["ddVLL",{1,2,3,3}]:>WCL["ddVLL",{1,2,3,3}]+WCL["ddVLL",{3,3,1,2}],
WCL["ddVLL",{1,3,3,3}]:>WCL["ddVLL",{1,3,3,3}]+WCL["ddVLL",{3,3,1,3}],
WCL["ddVLL",{2,2,3,3}]:>WCL["ddVLL",{2,2,3,3}]+WCL["ddVLL",{3,3,2,2}],
WCL["ddVLL",{2,3,3,3}]:>WCL["ddVLL",{2,3,3,3}]+WCL["ddVLL",{3,3,2,3}],
(* uuVLL *)
WCL["uuVLL",{1,1,1,2}]:>WCL["uuVLL",{1,1,1,2}]+WCL["uuVLL",{1,2,1,1}],
WCL["uuVLL",{1,1,2,2}]:>WCL["uuVLL",{1,1,2,2}]+WCL["uuVLL",{2,2,1,1}],
WCL["uuVLL",{1,2,2,2}]:>WCL["uuVLL",{1,2,2,2}]+WCL["uuVLL",{2,2,1,2}],
(* eeVRR *)
WCL["eeVRR",{1,1,1,2}]:>WCL["eeVRR",{1,1,1,2}]+WCL["eeVRR",{1,2,1,1}],
WCL["eeVRR",{1,1,1,3}]:>WCL["eeVRR",{1,1,1,3}]+WCL["eeVRR",{1,3,1,1}],
WCL["eeVRR",{1,2,1,3}]:>WCL["eeVRR",{1,2,1,3}]+WCL["eeVRR",{1,3,1,2}],
WCL["eeVRR",{1,1,2,2}]:>WCL["eeVRR",{1,1,2,2}]+WCL["eeVRR",{2,2,1,1}],
WCL["eeVRR",{1,2,2,2}]:>WCL["eeVRR",{1,2,2,2}]+WCL["eeVRR",{2,2,1,2}],
WCL["eeVRR",{1,1,2,3}]:>WCL["eeVRR",{1,1,2,3}]+WCL["eeVRR",{2,3,1,1}],
WCL["eeVRR",{1,2,2,3}]:>WCL["eeVRR",{1,2,2,3}]+WCL["eeVRR",{2,3,1,2}],
WCL["eeVRR",{1,3,2,3}]:>WCL["eeVRR",{1,3,2,3}]+WCL["eeVRR",{2,3,1,3}],
WCL["eeVRR",{2,2,2,3}]:>WCL["eeVRR",{2,2,2,3}]+WCL["eeVRR",{2,3,2,2}],
WCL["eeVRR",{1,2,3,2}]:>WCL["eeVRR",{1,2,3,2}]+WCL["eeVRR",{3,2,1,2}],
WCL["eeVRR",{1,1,3,3}]:>WCL["eeVRR",{1,1,3,3}]+WCL["eeVRR",{3,3,1,1}],
WCL["eeVRR",{1,2,3,3}]:>WCL["eeVRR",{1,2,3,3}]+WCL["eeVRR",{3,3,1,2}],
WCL["eeVRR",{1,3,3,3}]:>WCL["eeVRR",{1,3,3,3}]+WCL["eeVRR",{3,3,1,3}],
WCL["eeVRR",{2,2,3,3}]:>WCL["eeVRR",{2,2,3,3}]+WCL["eeVRR",{3,3,2,2}],
WCL["eeVRR",{2,3,3,3}]:>WCL["eeVRR",{2,3,3,3}]+WCL["eeVRR",{3,3,2,3}],
(* ddVRR *)
WCL["ddVRR",{1,1,1,2}]:>WCL["ddVRR",{1,1,1,2}]+WCL["ddVRR",{1,2,1,1}],
WCL["ddVRR",{1,1,1,3}]:>WCL["ddVRR",{1,1,1,3}]+WCL["ddVRR",{1,3,1,1}],
WCL["ddVRR",{1,2,1,3}]:>WCL["ddVRR",{1,2,1,3}]+WCL["ddVRR",{1,3,1,2}],
WCL["ddVRR",{1,1,2,2}]:>WCL["ddVRR",{1,1,2,2}]+WCL["ddVRR",{2,2,1,1}],
WCL["ddVRR",{1,2,2,2}]:>WCL["ddVRR",{1,2,2,2}]+WCL["ddVRR",{2,2,1,2}],
WCL["ddVRR",{1,1,2,3}]:>WCL["ddVRR",{1,1,2,3}]+WCL["ddVRR",{2,3,1,1}],
WCL["ddVRR",{1,2,2,3}]:>WCL["ddVRR",{1,2,2,3}]+WCL["ddVRR",{2,3,1,2}],
WCL["ddVRR",{1,3,2,3}]:>WCL["ddVRR",{1,3,2,3}]+WCL["ddVRR",{2,3,1,3}],
WCL["ddVRR",{2,2,2,3}]:>WCL["ddVRR",{2,2,2,3}]+WCL["ddVRR",{2,3,2,2}],
WCL["ddVRR",{1,2,3,2}]:>WCL["ddVRR",{1,2,3,2}]+WCL["ddVRR",{3,2,1,2}],
WCL["ddVRR",{1,1,3,3}]:>WCL["ddVRR",{1,1,3,3}]+WCL["ddVRR",{3,3,1,1}],
WCL["ddVRR",{1,2,3,3}]:>WCL["ddVRR",{1,2,3,3}]+WCL["ddVRR",{3,3,1,2}],
WCL["ddVRR",{1,3,3,3}]:>WCL["ddVRR",{1,3,3,3}]+WCL["ddVRR",{3,3,1,3}],
WCL["ddVRR",{2,2,3,3}]:>WCL["ddVRR",{2,2,3,3}]+WCL["ddVRR",{3,3,2,2}],
WCL["ddVRR",{2,3,3,3}]:>WCL["ddVRR",{2,3,3,3}]+WCL["ddVRR",{3,3,2,3}],
(* uuVRR *)
WCL["uuVRR",{1,1,1,2}]:>WCL["uuVRR",{1,1,1,2}]+WCL["uuVRR",{1,2,1,1}],
WCL["uuVRR",{1,1,2,2}]:>WCL["uuVRR",{1,1,2,2}]+WCL["uuVRR",{2,2,1,1}],
WCL["uuVRR",{1,2,2,2}]:>WCL["uuVRR",{1,2,2,2}]+WCL["uuVRR",{2,2,1,2}],
(* eeSRR *)
WCL["eeSRR",{1,1,1,2}]:>WCL["eeSRR",{1,1,1,2}]+WCL["eeSRR",{1,2,1,1}],
WCL["eeSRR",{1,1,1,3}]:>WCL["eeSRR",{1,1,1,3}]+WCL["eeSRR",{1,3,1,1}],
WCL["eeSRR",{1,2,1,3}]:>WCL["eeSRR",{1,2,1,3}]+WCL["eeSRR",{1,3,1,2}],
WCL["eeSRR",{1,1,2,2}]:>WCL["eeSRR",{1,1,2,2}]+WCL["eeSRR",{2,2,1,1}],
WCL["eeSRR",{1,2,2,2}]:>WCL["eeSRR",{1,2,2,2}]+WCL["eeSRR",{2,2,1,2}],
WCL["eeSRR",{1,1,2,3}]:>WCL["eeSRR",{1,1,2,3}]+WCL["eeSRR",{2,3,1,1}],
WCL["eeSRR",{1,2,2,3}]:>WCL["eeSRR",{1,2,2,3}]+WCL["eeSRR",{2,3,1,2}],
WCL["eeSRR",{1,3,2,3}]:>WCL["eeSRR",{1,3,2,3}]+WCL["eeSRR",{2,3,1,3}],
WCL["eeSRR",{2,2,2,3}]:>WCL["eeSRR",{2,2,2,3}]+WCL["eeSRR",{2,3,2,2}],
WCL["eeSRR",{1,2,3,2}]:>WCL["eeSRR",{1,2,3,2}]+WCL["eeSRR",{3,2,1,2}],
WCL["eeSRR",{1,1,3,3}]:>WCL["eeSRR",{1,1,3,3}]+WCL["eeSRR",{3,3,1,1}],
WCL["eeSRR",{1,2,3,3}]:>WCL["eeSRR",{1,2,3,3}]+WCL["eeSRR",{3,3,1,2}],
WCL["eeSRR",{1,3,3,3}]:>WCL["eeSRR",{1,3,3,3}]+WCL["eeSRR",{3,3,1,3}],
WCL["eeSRR",{2,2,3,3}]:>WCL["eeSRR",{2,2,3,3}]+WCL["eeSRR",{3,3,2,2}],
WCL["eeSRR",{2,3,3,3}]:>WCL["eeSRR",{2,3,3,3}]+WCL["eeSRR",{3,3,2,3}],
(* ddS1RR *)
WCL["ddS1RR",{1,1,1,2}]:>WCL["ddS1RR",{1,1,1,2}]+WCL["ddS1RR",{1,2,1,1}],
WCL["ddS1RR",{1,1,1,3}]:>WCL["ddS1RR",{1,1,1,3}]+WCL["ddS1RR",{1,3,1,1}],
WCL["ddS1RR",{1,2,1,3}]:>WCL["ddS1RR",{1,2,1,3}]+WCL["ddS1RR",{1,3,1,2}],
WCL["ddS1RR",{1,1,2,2}]:>WCL["ddS1RR",{1,1,2,2}]+WCL["ddS1RR",{2,2,1,1}],
WCL["ddS1RR",{1,2,2,2}]:>WCL["ddS1RR",{1,2,2,2}]+WCL["ddS1RR",{2,2,1,2}],
WCL["ddS1RR",{1,1,2,3}]:>WCL["ddS1RR",{1,1,2,3}]+WCL["ddS1RR",{2,3,1,1}],
WCL["ddS1RR",{1,2,2,3}]:>WCL["ddS1RR",{1,2,2,3}]+WCL["ddS1RR",{2,3,1,2}],
WCL["ddS1RR",{1,3,2,3}]:>WCL["ddS1RR",{1,3,2,3}]+WCL["ddS1RR",{2,3,1,3}],
WCL["ddS1RR",{2,2,2,3}]:>WCL["ddS1RR",{2,2,2,3}]+WCL["ddS1RR",{2,3,2,2}],
WCL["ddS1RR",{1,2,3,2}]:>WCL["ddS1RR",{1,2,3,2}]+WCL["ddS1RR",{3,2,1,2}],
WCL["ddS1RR",{1,1,3,3}]:>WCL["ddS1RR",{1,1,3,3}]+WCL["ddS1RR",{3,3,1,1}],
WCL["ddS1RR",{1,2,3,3}]:>WCL["ddS1RR",{1,2,3,3}]+WCL["ddS1RR",{3,3,1,2}],
WCL["ddS1RR",{1,3,3,3}]:>WCL["ddS1RR",{1,3,3,3}]+WCL["ddS1RR",{3,3,1,3}],
WCL["ddS1RR",{2,2,3,3}]:>WCL["ddS1RR",{2,2,3,3}]+WCL["ddS1RR",{3,3,2,2}],
WCL["ddS1RR",{2,3,3,3}]:>WCL["ddS1RR",{2,3,3,3}]+WCL["ddS1RR",{3,3,2,3}],
(* ddS8RR *)
WCL["ddS8RR",{1,1,1,2}]:>WCL["ddS8RR",{1,1,1,2}]+WCL["ddS8RR",{1,2,1,1}],
WCL["ddS8RR",{1,1,1,3}]:>WCL["ddS8RR",{1,1,1,3}]+WCL["ddS8RR",{1,3,1,1}],
WCL["ddS8RR",{1,2,1,3}]:>WCL["ddS8RR",{1,2,1,3}]+WCL["ddS8RR",{1,3,1,2}],
WCL["ddS8RR",{1,1,2,2}]:>WCL["ddS8RR",{1,1,2,2}]+WCL["ddS8RR",{2,2,1,1}],
WCL["ddS8RR",{1,2,2,2}]:>WCL["ddS8RR",{1,2,2,2}]+WCL["ddS8RR",{2,2,1,2}],
WCL["ddS8RR",{1,1,2,3}]:>WCL["ddS8RR",{1,1,2,3}]+WCL["ddS8RR",{2,3,1,1}],
WCL["ddS8RR",{1,2,2,3}]:>WCL["ddS8RR",{1,2,2,3}]+WCL["ddS8RR",{2,3,1,2}],
WCL["ddS8RR",{1,3,2,3}]:>WCL["ddS8RR",{1,3,2,3}]+WCL["ddS8RR",{2,3,1,3}],
WCL["ddS8RR",{2,2,2,3}]:>WCL["ddS8RR",{2,2,2,3}]+WCL["ddS8RR",{2,3,2,2}],
WCL["ddS8RR",{1,2,3,2}]:>WCL["ddS8RR",{1,2,3,2}]+WCL["ddS8RR",{3,2,1,2}],
WCL["ddS8RR",{1,1,3,3}]:>WCL["ddS8RR",{1,1,3,3}]+WCL["ddS8RR",{3,3,1,1}],
WCL["ddS8RR",{1,2,3,3}]:>WCL["ddS8RR",{1,2,3,3}]+WCL["ddS8RR",{3,3,1,2}],
WCL["ddS8RR",{1,3,3,3}]:>WCL["ddS8RR",{1,3,3,3}]+WCL["ddS8RR",{3,3,1,3}],
WCL["ddS8RR",{2,2,3,3}]:>WCL["ddS8RR",{2,2,3,3}]+WCL["ddS8RR",{3,3,2,2}],
WCL["ddS8RR",{2,3,3,3}]:>WCL["ddS8RR",{2,3,3,3}]+WCL["ddS8RR",{3,3,2,3}],
(* uuS1RR *)
WCL["uuS1RR",{1,1,1,2}]:>WCL["uuS1RR",{1,1,1,2}]+WCL["uuS1RR",{1,2,1,1}],
WCL["uuS1RR",{1,1,2,2}]:>WCL["uuS1RR",{1,1,2,2}]+WCL["uuS1RR",{2,2,1,1}],
WCL["uuS1RR",{1,2,2,2}]:>WCL["uuS1RR",{1,2,2,2}]+WCL["uuS1RR",{2,2,1,2}],
(* uuS8RR *)
WCL["uuS8RR",{1,1,1,2}]:>WCL["uuS8RR",{1,1,1,2}]+WCL["uuS8RR",{1,2,1,1}],
WCL["uuS8RR",{1,1,2,2}]:>WCL["uuS8RR",{1,1,2,2}]+WCL["uuS8RR",{2,2,1,1}],
WCL["uuS8RR",{1,2,2,2}]:>WCL["uuS8RR",{1,2,2,2}]+WCL["uuS8RR",{2,2,1,2}]
|>;


(*SMEFTInput = {
	DsixTools`\[Lambda]->0.2813,
	DsixTools`m2->8528,
	DsixTools`g->0.6515,
	DsixTools`gp->0.3576,
	DsixTools`gs->1.220
};*)


(* ::Section:: *)
(*LEFTRun*)


Get@FileNameJoin[{Global`$DirectoryHighPT,"EFT","LEFT","LEFTAD.dat"}];


(*LEFTSimplify=Get@FileNameJoin[{Global`$DirectoryHighPT,"RGE","Simplifications","LEFTSimplify.dat"}];*)


LEFTRun::undefinedrunningmode= "The mode `1` is not defined for LEFT Running.";


LEFTRun::nonnumericlowscale = "In \"DsixTools\" running mode lowscale must be a number"


LEFTRun::nocoefficients="No LEFT coefficients found"


LEFTRun[expr_,lowscale_,highscale_]:=Module[
	{
		params,
		evolution,
		mode
	}
	,
	If[lowscale>DsixTools`EWSCALE,Return@expr];
	mode=GetLEFTRGEMode[];
	Switch[
		mode,
		"LL",
		Return[SymmetricToNonRedundantLEFT[NonRedundantToSymmetricLEFT[expr]/.WCLS->WCL/.wc_WCL->(wc+1/(16\[Pi]^2)Log[lowscale/highscale]LEFTAD[wc])]],
		"DsixTools",
		If[NumericQ[lowscale],
			(*temp=(HighPTToDsixToolsLEFT[expr])//DsixTools`D6Simplify;*)
			params=DeleteDuplicates@Cases[expr, _WCL, \[Infinity]];
			(* Deal with the case of a single WCL being evolved *)
			If[MatchQ[params,{}] && MatchQ[Head@expr,WCL],params={expr}];
			params=Complement[params,SanDiegoBasis["\[Nu]"]];
			If[MatchQ[params,{}],Message[LEFTRun::nocoefficients]];
			Print[params];
			evolution=Dispatch[(#1->DsixToolsToHighPTLEFT[DsixTools`LEFTEvolve[HighPTToDsixToolsLEFT[#1],lowscale]]&)/@params];
			(*params=Select[Variables[temp/.Conjugate[a_]->a/.Re->Identity/.Abs->Identity],MemberQ[DsixTools`LEFTParameterList[],#] &];
			temp=temp/.Dispatch[(#1->DsixTools`LEFTEvolve[#1,lowscale]&)/@params];*)
			(*Return[DsixToolsToHighPTLEFT[temp]]*)
			Return[SymmetricToNonRedundantLEFT[NonRedundantToSymmetricLEFT[expr]/.WCLS->WCL/.evolution]],
			Message[LEFTRun::nonnumericlowscale];Abort[]
		];,
		"Off",
		Return[expr],
		_,
		Message[LEFTRun::undefinedrunningmode,LEFTRGEMode];Abort[];
	];
];

