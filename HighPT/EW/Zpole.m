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


ObservableList["Zpole"] := {
	"\[CapitalGamma]Z",
	"\[Sigma]had",
	"Re","R\[Mu]","R\[Tau]",
	"Rb","Rc",
	"Ae","AeLEP","A\[Mu]","A\[Tau]","A\[Tau]LEP",
	"Ab","Ac","As",
	"AFB0e","AFB0\[Mu]","AFB0\[Tau]",
	"AFBb","AFBc",
	"Ruc"
}


(* ::Section:: *)
(*\[CapitalGamma]Z*)


TheoryExpression["\[CapitalGamma]Z"] := Mass["ZBoson"]/(24\[Pi]) (Sum[WCL["gZ\[Nu]L",{i,i}]^2,{i,3}]+Sum[WCL["gZeL",{i,i}]^2+WCL["gZeR",{i,i}]^2,{i,3}]+3*Sum[WCL["gZdL",{i,i}]^2+WCL["gZdR",{i,i}]^2,{i,3}]+3*Sum[WCL["gZuL",{i,i}]^2+WCL["gZuR",{i,i}]^2,{i,2}])


ExpValue$default["\[CapitalGamma]Z"] := Width["ZBoson"][Default];


SMPrediction$default["\[CapitalGamma]Z"] := Around[2.4941,null];


LowScale$default["\[CapitalGamma]Z"] := \[Mu]EW;


(* ::Section:: *)
(*\[Sigma]had*)


GeVtonb = (0.389379 10^6);


TheoryExpression["\[Sigma]had"] := GeVtonb*1/TheoryExpression["\[CapitalGamma]Z"]^2 (12\[Pi])/Mass["ZBoson"]^2 Mass["ZBoson"]/(24\[Pi]) (WCL["gZeL",{1,1}]^2+WCL["gZeR",{1,1}]^2) Mass["ZBoson"]/(24\[Pi]) 3(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])


ExpValue$default["\[Sigma]had"]:=Around[41.4807,0.0325];


SMPrediction$default["\[Sigma]had"]:=Around[41.4842,null];


LowScale$default["\[Sigma]had"] := \[Mu]EW;


(* ::Section:: *)
(*Re*)


RRe[i_] := 3 (Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])/(WCL["gZeL",{i,i}]^2+WCL["gZeR",{i,i}]^2)


(* ::Subsection:: *)
(*Re*)


TheoryExpression["Re"] := RRe[1];


ExpValue$default["Re"]:=Around[20.8038,0.0497];


SMPrediction$default["Re"]:=Around[20.734,null];


LowScale$default["Re"] := \[Mu]EW;


(* ::Subsection:: *)
(*R\[Mu]*)


TheoryExpression["R\[Mu]"] := RRe[2];


ExpValue$default["R\[Mu]"]:=Around[20.7842,0.0335];


SMPrediction$default["R\[Mu]"]:=Around[20.734,null];


LowScale$default["R\[Mu]"] := \[Mu]EW;


(* ::Subsection:: *)
(*R\[Tau]*)


TheoryExpression["R\[Tau]"] := RRe[3];


ExpValue$default["R\[Tau]"]:=Around[20.7644,0.0448];


SMPrediction$default["R\[Tau]"]:=Around[20.781,null];


LowScale$default["R\[Tau]"] := \[Mu]EW;


(* ::Section:: *)
(*Rq*)


RRd[i_] := (WCL["gZdL",{i,i}]^2+WCL["gZdR",{i,i}]^2)/(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])
RRu[i_] := (WCL["gZuL",{i,i}]^2+WCL["gZuR",{i,i}]^2)/(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])


(* ::Subsection:: *)
(*Rb*)


TheoryExpression["Rb"] := RRd[3];


ExpValue$default["Rb"]:=Around[0.21629,0.00066];


SMPrediction$default["Rb"]:=Around[0.21581,null];


LowScale$default["Rb"] := \[Mu]EW;


(* ::Subsection:: *)
(*Rc*)


TheoryExpression["Rc"] := RRu[2];


ExpValue$default["Rc"]:=Around[0.1721,0.0030];


SMPrediction$default["Rc"]:=Around[0.17222,null];


LowScale$default["Rc"] := \[Mu]EW;


(* ::Section:: *)
(*Af*)


Ae[i_] := (WCL["gZeL",{i,i}]^2-WCL["gZeR",{i,i}]^2)/(WCL["gZeL",{i,i}]^2+WCL["gZeR",{i,i}]^2)
Ad[i_] := (WCL["gZdL",{i,i}]^2-WCL["gZdR",{i,i}]^2)/(WCL["gZdL",{i,i}]^2+WCL["gZdR",{i,i}]^2)
Au[i_] := (WCL["gZuL",{i,i}]^2-WCL["gZuR",{i,i}]^2)/(WCL["gZuL",{i,i}]^2+WCL["gZuR",{i,i}]^2)


(* ::Subsection:: *)
(*Ae*)


TheoryExpression["Ae"] := Ae[1];


ExpValue$default["Ae"]:=Around[0.1516,0.0021];


SMPrediction$default["Ae"]:=Around[0.1470,null];


LowScale$default["Ae"] := \[Mu]EW;


(* ::Subsection:: *)
(*AeLEP*)


TheoryExpression["AeLEP"] := Ae[1];


ExpValue$default["AeLEP"]:=Around[0.1498,0.0049];


SMPrediction$default["AeLEP"]:=Around[0.1470,null];


LowScale$default["AeLEP"] := \[Mu]EW;


(* ::Subsection:: *)
(*A\[Mu]*)


TheoryExpression["A\[Mu]"] := Ae[2];


ExpValue$default["A\[Mu]"]:=Around[0.142,0.015];


SMPrediction$default["A\[Mu]"]:=Around[0.1470,null];


LowScale$default["A\[Mu]"] := \[Mu]EW;


(* ::Subsection:: *)
(*A\[Tau]*)


TheoryExpression["A\[Tau]"] := Ae[3];


ExpValue$default["A\[Tau]"]:=Around[0.136,0.015];


SMPrediction$default["A\[Tau]"]:=Around[0.1470,null];


LowScale$default["A\[Tau]"] := \[Mu]EW;


(* ::Subsection:: *)
(*A\[Tau]LEP*)


TheoryExpression["A\[Tau]LEP"] := Ae[3];


ExpValue$default["A\[Tau]LEP"]:=Around[0.1439,0.0043];


SMPrediction$default["A\[Tau]LEP"]:=Around[0.1470,null];


LowScale$default["A\[Tau]LEP"] := \[Mu]EW;


(* ::Subsection:: *)
(*Ab*)


TheoryExpression["Ab"] := Ad[3];


ExpValue$default["Ab"]:=Around[0.923,0.020];


SMPrediction$default["Ab"]:=Around[0.935,null];


LowScale$default["Ab"] := \[Mu]EW;


(* ::Subsection:: *)
(*Ac*)


TheoryExpression["Ac"] := Au[2];


ExpValue$default["Ac"]:=Around[0.670,0.027];


SMPrediction$default["Ac"]:=Around[0.668,null];


LowScale$default["Ac"] := \[Mu]EW;


(* ::Subsection:: *)
(*As*)


TheoryExpression["As"] := Ad[2];


ExpValue$default["As"]:=Around[0.895,0.091];


SMPrediction$default["As"]:=Around[0.936,null];


LowScale$default["As"] := \[Mu]EW;


(* ::Section:: *)
(*AFB0e*)


AFB0e[i_] := 3/4 Ae[1]Ae[i]


(* ::Subsection:: *)
(*AFB0e*)


TheoryExpression["AFB0e"] := AFB0e[1];


ExpValue$default["AFB0e"] := Around[0.0145,0.0025];


SMPrediction$default["AFB0e"] := Around[0.0162,null];


LowScale$default["AFB0e"] := \[Mu]EW;


(* ::Subsection:: *)
(*AFB0\[Mu]*)


TheoryExpression["AFB0\[Mu]"] := AFB0e[2];


ExpValue$default["AFB0\[Mu]"] := Around[0.0169,0.0013];


SMPrediction$default["AFB0\[Mu]"] := Around[0.0162,null];


LowScale$default["AFB0\[Mu]"] := \[Mu]EW;


(* ::Subsection:: *)
(*AFB0\[Tau]*)


TheoryExpression["AFB0\[Tau]"] := AFB0e[3];


ExpValue$default["AFB0\[Tau]"] := Around[0.0188,0.0017];


SMPrediction$default["AFB0\[Tau]"] := Around[0.0162,null];


LowScale$default["AFB0\[Tau]"] := \[Mu]EW;


(* ::Section:: *)
(*AFBq*)


AFBd[i_] := 3/4 Ae[1]Ad[i]


AFBu[i_] := 3/4 Ae[1]Au[i]


(* ::Subsection:: *)
(*AFBb*)


TheoryExpression["AFBb"] := AFBd[3];


ExpValue$default["AFBb"] := Around[0.0996,0.0016];


SMPrediction$default["AFBb"] := Around[0.1032,null];


LowScale$default["AFBb"] := \[Mu]EW;


(* ::Subsection:: *)
(*AFBc*)


TheoryExpression["AFBc"] := AFBu[2];


ExpValue$default["AFBc"] := Around[0.0707,0.0035];


SMPrediction$default["AFBc"] := Around[0.0736,null];


LowScale$default["AFBc"] := \[Mu]EW;


(* ::Section:: *)
(*Ruc*)


TheoryExpression["Ruc"] := Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}]/(2(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}]))


ExpValue$default["Ruc"] := Around[0.166,0.009];


SMPrediction$default["Ruc"] := Around[0.1722,null];


LowScale$default["Ruc"] := \[Mu]EW;


(* ::Section:: *)
(*Exp Correlations*)


(* ::Subsection:: *)
(*Re and AFB0e *)


ExpCorrelation$default["\[CapitalGamma]Z","\[Sigma]had"]:=-0.3249
ExpCorrelation$default["\[CapitalGamma]Z","Re"]:=-0.0110
ExpCorrelation$default["\[CapitalGamma]Z","R\[Mu]"]:=0.0079
ExpCorrelation$default["\[CapitalGamma]Z","R\[Tau]"]:=0.0059
ExpCorrelation$default["\[CapitalGamma]Z","AFB0e"]:=0.0071
ExpCorrelation$default["\[CapitalGamma]Z","AFB0\[Mu]"]:=0.0020
ExpCorrelation$default["\[CapitalGamma]Z","AFB0\[Tau]"]:=0.0013


ExpCorrelation$default["\[Sigma]had","Re"]:=0.105;
ExpCorrelation$default["\[Sigma]had","R\[Mu]"]:=0.131;
ExpCorrelation$default["\[Sigma]had","R\[Tau]"]:=0.092;
ExpCorrelation$default["\[Sigma]had","AFB0e"]:=0.001;
ExpCorrelation$default["\[Sigma]had","AFB0\[Mu]"]:=0.003;
ExpCorrelation$default["\[Sigma]had","AFB0\[Tau]"]:=0.002;


ExpCorrelation$default["Re","R\[Mu]"]:=0.069;
ExpCorrelation$default["Re","R\[Tau]"]:=0.046;
ExpCorrelation$default["Re","AFB0e"]:=-0.371;
ExpCorrelation$default["Re","AFB0\[Mu]"]:=0.02;
ExpCorrelation$default["Re","AFB0\[Tau]"]:=0.013;


ExpCorrelation$default["R\[Mu]","R\[Tau]"]:=0.069;
ExpCorrelation$default["R\[Mu]","AFB0e"]:=0.001;
ExpCorrelation$default["R\[Mu]","AFB0\[Mu]"]:=0.012;
ExpCorrelation$default["R\[Mu]","AFB0\[Tau]"]:=-0.003;


ExpCorrelation$default["R\[Tau]","AFB0e"]:=0.003;
ExpCorrelation$default["R\[Tau]","AFB0\[Mu]"]:=0.001;
ExpCorrelation$default["R\[Tau]","AFB0\[Tau]"]:=0.009;


ExpCorrelation$default["AFB0e","AFB0\[Mu]"]:=-0.024;
ExpCorrelation$default["AFB0e","AFB0\[Tau]"]:=-0.02;


ExpCorrelation$default["AFB0\[Mu]","AFB0\[Tau]"]:=0.046;


(* ::Subsection:: *)
(*Rq and AFBq *)


ExpCorrelation$default["Rb","Rc"]:=-0.18;
ExpCorrelation$default["Rb","AFBb"]:=-0.1;
ExpCorrelation$default["Rb","AFBc"]:=0.07;


ExpCorrelation$default["Rc","AFBb"]:=0.04;
ExpCorrelation$default["Rc","AFBc"]:=-0.06;


ExpCorrelation$default["AFBb","AFBc"]:=0.15;


(* ::Subsection:: *)
(*Ae *)


ExpCorrelation$default["Ae","A\[Mu]"]:=0.038;
ExpCorrelation$default["Ae","A\[Tau]"]:=0.033;


ExpCorrelation$default["A\[Mu]","A\[Tau]"]:=0.007;


(* ::Subsection:: *)
(*Aq *)


ExpCorrelation$default["Ab","Ac"]:=0.11;
