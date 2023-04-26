(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`RGE`*)


(* ::Subtitle:: *)
(*Generic things about RGEs in the EFT*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["RunRGE"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["RunningMode"]


(* ::Chapter:: *)
(*Private:*)


RunningMode="DsixTools"


Needs["DsixTools`"]
DsixTools`SetMatchingLoopOrder[0];


RunRGE[expr_,lowscale_,highscale_]:=SMEFTRun[LEFTRun[expr,lowscale,DsixTools`EWSCALE],DsixTools`EWSCALE,highscale]
