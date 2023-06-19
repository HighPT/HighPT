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


EWObservables["Zpole"]={
	"\[CapitalGamma]Z",
	"\[Sigma]had",
	"Re","R\[Mu]","R\[Tau]",
	"Rb","Rc",
	"Ae","AeLEP","A\[Mu]","A\[Tau]","A\[Tau]LEP",
	"Ab","Ac",
	"AFB0e","AFB0\[Mu]","AFB0\[Tau]",
	"AFBb","AFBc",
	"Ruc"
	};


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


(* ::Section::Closed:: *)
(*\[CapitalGamma]Z*)


ExpValue$default["\[CapitalGamma]Z"]:={2.4955,0.0023};


SMPrediction$default["\[CapitalGamma]Z"]:={2.4941,0};


NPContribution$default["\[CapitalGamma]Z"]:=(\[CapitalDelta]\[CapitalGamma]Zhad+\[CapitalDelta]\[CapitalGamma]Zlep)/.Replace\[Delta]g/.GetParameters[]//Simplify;


(* ::Section::Closed:: *)
(*\[Sigma]had*)


ExpValue$default["\[Sigma]had"]:={41.4802,0.0325};


SMPrediction$default["\[Sigma]had"]:={41.4842,0};


NPContribution$default["\[Sigma]had"]:=(12\[Pi])/Mass["ZBoson"]^2 (\[CapitalGamma]ZfSM[e]\[CapitalGamma]ZhadSM)/\[CapitalGamma]ZSM^2 (\[CapitalDelta]\[CapitalGamma]Z[e,1]/\[CapitalGamma]ZfSM[e]+\[CapitalDelta]\[CapitalGamma]Zhad/\[CapitalGamma]ZhadSM-2 (\[CapitalDelta]\[CapitalGamma]Zhad+\[CapitalDelta]\[CapitalGamma]Zlep)/\[CapitalGamma]ZSM)/.Replace\[Delta]g/.GetParameters[];


(* ::Section::Closed:: *)
(*Re*)


\[CapitalDelta]Re[i_]:=\[CapitalDelta]\[CapitalGamma]Zhad/\[CapitalGamma]ZhadSM-(\[CapitalGamma]ZhadSM \[CapitalDelta]\[CapitalGamma]Z[e,i])/\[CapitalGamma]ZfSM[e]^2;


(* ::Subsection::Closed:: *)
(*Re*)


ExpValue$default["Re"]:={20.804,0.050};


SMPrediction$default["Re"]:={20.734,0};


NPContribution$default["Re"]:=\[CapitalDelta]Re[1]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*R\[Mu]*)


ExpValue$default["R\[Mu]"]:={20.785,0.033};


SMPrediction$default["R\[Mu]"]:={20.734,0};


NPContribution$default["R\[Mu]"]:=\[CapitalDelta]Re[2]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*R\[Tau]*)


ExpValue$default["R\[Tau]"]:={20.764,0.045};


SMPrediction$default["R\[Tau]"]:={20.781,0};


NPContribution$default["R\[Tau]"]:=\[CapitalDelta]Re[3]/.Replace\[Delta]g/.GetParameters[];


(* ::Section::Closed:: *)
(*Rq*)


\[CapitalDelta]R[q_,i_]:=\[CapitalDelta]\[CapitalGamma]Z[q,i]/\[CapitalGamma]ZhadSM-(\[CapitalGamma]ZfSM[q]\[CapitalDelta]\[CapitalGamma]Zhad)/\[CapitalGamma]ZhadSM^2;


(* ::Subsection::Closed:: *)
(*Rb*)


ExpValue$default["Rb"]:={0.21629,0.00066};


SMPrediction$default["Rb"]:={0.21581,0};


NPContribution$default["Rb"]:=\[CapitalDelta]R[d,3]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*Rc*)


ExpValue$default["Rc"]:={0.1721,0.0030};


SMPrediction$default["Rc"]:={0.17222,0};


NPContribution$default["Rc"]:=\[CapitalDelta]R[u,2]/.Replace\[Delta]g/.GetParameters[];


(* ::Section::Closed:: *)
(*Af*)


\[CapitalDelta]A[f_,i_]:=(4 gZSM[f,Left]gZSM[f,Right])/(gZSM[f,Left]^2+gZSM[f,Right]^2) (gZSM[f,Right]\[Delta]gZ[f,Left,{i,i}]-gZSM[f,Left]\[Delta]gZ[f,Right,{i,i}]);


(* ::Subsection::Closed:: *)
(*Ae*)


ExpValue$default["Ae"]:={0.1516,0.0021};


SMPrediction$default["Ae"]:={0.1470,0};


NPContribution$default["Ae"]:=\[CapitalDelta]A[e,1]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*AeLEP*)


ExpValue$default["AeLEP"]:={0.1498,0.0049};


SMPrediction$default["AeLEP"]:={0.1470,0};


NPContribution$default["AeLEP"]:=\[CapitalDelta]A[e,1]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*A\[Mu]*)


ExpValue$default["A\[Mu]"]:={0.142,0.015};


SMPrediction$default["A\[Mu]"]:={0.1470,0};


NPContribution$default["A\[Mu]"]:=\[CapitalDelta]A[e,2]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*A\[Tau]*)


ExpValue$default["A\[Tau]"]:={0.136,0.015};


SMPrediction$default["A\[Tau]"]:={0.1470,0};


NPContribution$default["A\[Tau]"]:=\[CapitalDelta]A[e,3]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*A\[Tau]LEP*)


ExpValue$default["A\[Tau]LEP"]:={0.1439,0.0043};


SMPrediction$default["A\[Tau]LEP"]:={0.1470,0};


NPContribution$default["A\[Tau]LEP"]:=\[CapitalDelta]A[e,3]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*Ab*)


ExpValue$default["Ab"]:={0.923,0.020};


SMPrediction$default["Ab"]:={0.935,0};


NPContribution$default["Ab"]:=\[CapitalDelta]A[d,3]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*Ac*)


ExpValue$default["Ac"]:={0.670,0.027};


SMPrediction$default["Ac"]:={0.668,0};


NPContribution$default["Ac"]:=\[CapitalDelta]A[u,2]/.Replace\[Delta]g/.GetParameters[];


(* ::Section::Closed:: *)
(*AFB0e*)


\[CapitalDelta]AFB0e[i_]:=3/4 ASM[e](\[CapitalDelta]A[e,1]+\[CapitalDelta]A[e,i]);


(* ::Subsection::Closed:: *)
(*AFB0e*)


ExpValue$default["AFB0e"]:={0.0145,0.0025};


SMPrediction$default["AFB0e"]:={0.0162,0};


NPContribution$default["AFB0e"]:=\[CapitalDelta]AFB0e[1]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*AFB0\[Mu]*)


ExpValue$default["AFB0\[Mu]"]:={0.0169,0.0013};


SMPrediction$default["AFB0\[Mu]"]:={0.0162,0};


NPContribution$default["AFB0\[Mu]"]:=\[CapitalDelta]AFB0e[2]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*AFB0\[Tau]*)


ExpValue$default["AFB0\[Tau]"]:={0.0188,0.0017};


SMPrediction$default["AFB0\[Tau]"]:={0.0162,0};


NPContribution$default["AFB0\[Tau]"]:=\[CapitalDelta]AFB0e[3]/.Replace\[Delta]g/.GetParameters[];


(* ::Section::Closed:: *)
(*AFBq*)


\[CapitalDelta]AFB[q_,i_]:=3/4 (ASM[q]\[CapitalDelta]A[e,1]+ASM[e]\[CapitalDelta]A[q,i]);


(* ::Subsection::Closed:: *)
(*AFBb*)


ExpValue$default["AFBb"]:={0.0996,0.0016};


SMPrediction$default["AFBb"]:={0.1032,0};


NPContribution$default["AFBb"]:=\[CapitalDelta]AFB[d,3]/.Replace\[Delta]g/.GetParameters[];


(* ::Subsection::Closed:: *)
(*AFBc*)


ExpValue$default["AFBc"]:={0.0707,0.0035};


SMPrediction$default["AFBc"]:={0.0736,0};


NPContribution$default["AFBc"]:=\[CapitalDelta]AFB[u,2]/.Replace\[Delta]g/.GetParameters[];


(* ::Section:: *)
(*Ruc*)


ExpValue$default["Ruc"]:={0.166,0.009};


SMPrediction$default["Ruc"]:={0.1722,0};


NPContribution$default["Ruc"]:=(-(\[CapitalGamma]ZfSM[u]^2/(2 \[CapitalGamma]ZhadSM^2))\[CapitalDelta]\[CapitalGamma]Zhad + 1/(2 \[CapitalGamma]ZhadSM) (\[CapitalDelta]\[CapitalGamma]Z[u,1]+\[CapitalDelta]\[CapitalGamma]Z[u,2]))/.Replace\[Delta]g/.GetParameters[];


(* ::Section:: *)
(*Exp Correlations*)


(* ::Subsection::Closed:: *)
(*Re and AFB0e*)


ExpCorrelation["\[Sigma]had","Re"]:=0.105;
ExpCorrelation["\[Sigma]had","R\[Mu]"]:=0.131;
ExpCorrelation["\[Sigma]had","R\[Tau]"]:=0.092;
ExpCorrelation["\[Sigma]had","AFB0e"]:=0.001;
ExpCorrelation["\[Sigma]had","AFB0\[Mu]"]:=0.003;
ExpCorrelation["\[Sigma]had","AFB0\[Tau]"]:=0.002;


ExpCorrelation["Re","R\[Mu]"]:=0.069;
ExpCorrelation["Re","R\[Tau]"]:=0.046;
ExpCorrelation["Re","AFB0e"]:=-0.371;
ExpCorrelation["Re","AFB0\[Mu]"]:=0.02;
ExpCorrelation["Re","AFB0\[Tau]"]:=0.013;


ExpCorrelation["R\[Mu]","R\[Tau]"]:=0.069;
ExpCorrelation["R\[Mu]","AFB0e"]:=0.001;
ExpCorrelation["R\[Mu]","AFB0\[Mu]"]:=0.012;
ExpCorrelation["R\[Mu]","AFB0\[Tau]"]:=-0.003;


ExpCorrelation["R\[Tau]","AFB0e"]:=0.003;
ExpCorrelation["R\[Tau]","AFB0\[Mu]"]:=0.001;
ExpCorrelation["R\[Tau]","AFB0\[Tau]"]:=0.009;


ExpCorrelation["AFB0e","AFB0\[Mu]"]:=-0.024;
ExpCorrelation["AFB0e","AFB0\[Tau]"]:=-0.02;


ExpCorrelation["AFB0\[Mu]","AFB0\[Tau]"]:=0.046;


(* ::Subsection::Closed:: *)
(*Rq and AFBq*)


ExpCorrelation["Rb","Rc"]:=-0.18;
ExpCorrelation["Rb","AFBb"]:=-0.1;
ExpCorrelation["Rb","AFBc"]:=0.07;


ExpCorrelation["Rc","AFBb"]:=0.04;
ExpCorrelation["Rc","AFBc"]:=-0.06;


ExpCorrelation["AFBb","AFBc"]:=0.15;


(* ::Subsection::Closed:: *)
(*Ae*)


ExpCorrelation["Ae","A\[Mu]"]:=0.038;
ExpCorrelation["Ae","A\[Tau]"]:=0.033;


ExpCorrelation["A\[Mu]","A\[Tau]"]:=0.007;


(* ::Subsection::Closed:: *)
(*Aq*)


ExpCorrelation["Ab","Ac"]:=0.11;
