(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Zpole`*)


(* ::Subtitle:: *)
(*Z pole observables*)


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


EWObservables["Zpole"]={"\[CapitalGamma]Z"};


(* ::Section:: *)
(*Useful definitions*)


NZ=Mass["ZBoson"]/(24\[Pi]) (Param["g2"]^2+Param["g1"]^2);


\[CapitalGamma]ZSM=NZ/12 (63-120 Param["sW"]^2+160 Param["sW"]^4);


\[CapitalGamma]ZhadSM=NZ/12 (45-84 Param["sW"]^2+88 Param["sW"]^4);


gZSM[f_,chir_]:=WeakIsospin3[f,chir]-Param["sW"]^2 Charge[f];


\[CapitalDelta]\[CapitalGamma]Zhad=2 NZ (3 Sum[gZSM[u,Left]\[Delta]gZ[u,Left,{i,i}]+gZSM[u,Right]\[Delta]gZ[u,Right,{i,i}],{i,2}]+3 Sum[gZSM[d,Left]\[Delta]gZ[d,Left,{i,i}]+gZSM[d,Right]\[Delta]gZ[d,Right,{i,i}],{i,3}]);


\[CapitalDelta]\[CapitalGamma]Zlep=2 NZ Sum[gZSM[e,Left]\[Delta]gZ[e,Left,{i,i}]+gZSM[e,Right]\[Delta]gZ[e,Right,{i,i}]+gZSM[\[Nu],Left]\[Delta]gZ[\[Nu],Left,{i,i}],{i,3}];


\[CapitalGamma]ZfSM[f_]:=NZ(gZSM[f,Left]^2+gZSM[f,Right]^2);


\[CapitalDelta]\[CapitalGamma]Z[f_,i_]:=2 NZ (gZSM[f,Left]\[Delta]gZ[f,Left,{i,i}]+gZSM[f,Right]\[Delta]gZ[f,Right,{i,i}]);


ASM[f_]:=(gZSM[f,Left]^2-gZSM[f,Right]^2)/(gZSM[f,Left]^2+gZSM[f,Right]^2);


(* ::Section:: *)
(*\[CapitalGamma]Z*)


ExpValue$default["\[CapitalGamma]Z"]:={2.4955,0.0023};


SMPrediction$default["\[CapitalGamma]Z"]:={\[CapitalGamma]ZSM,0}/.GetParameters[];


NPContribution$default["\[CapitalGamma]Z"]:=(\[CapitalDelta]\[CapitalGamma]Zhad+\[CapitalDelta]\[CapitalGamma]Zlep)/.Replace\[Delta]g/.GetParameters[]//Simplify;


(* ::Section:: *)
(*\[Sigma]had*)


(* ::Section:: *)
(*Re*)


(* ::Section:: *)
(*Rq*)


(* ::Section:: *)
(*Af*)


(* ::Section:: *)
(*AFB0e*)


(* ::Section:: *)
(*AFBq*)


(* ::Section:: *)
(*Ruc*)


(* ::Section:: *)
(*Exp Correlations*)


(* ::Subsection:: *)
(*Re and AeFB*)


(* ::Subsection:: *)
(*Rq and RqFB*)


(* ::Subsection:: *)
(*Ae*)


(* ::Subsection:: *)
(*Aq*)
