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


PackageScope["FCCZpoleProjections"]


(* ::Chapter:: *)
(*Private:*)


EWObservables["Zpole"]={
	"\[CapitalGamma]Z",
	"\[Sigma]had",
	"Re","R\[Mu]","R\[Tau]",
	"Rb","Rc",
	"Ae","AeLEP","A\[Mu]","A\[Tau]","A\[Tau]LEP",
	"Ab","Ac","As",
	"AFB0e","AFB0\[Mu]","AFB0\[Tau]",
	"AFBb","AFBc",
	"Ruc"
	};


EWObservables["ZpoleNEW"]={
	"\[CapitalGamma]ZNEW",
	"\[Sigma]hadNEW",
	"ReNEW","R\[Mu]NEW","R\[Tau]NEW",
	"RbNEW","RcNEW",
	"AeNEW","AeLEPNEW","A\[Mu]NEW","A\[Tau]NEW","A\[Tau]LEPNEW",
	"AbNEW","AcNEW","AsNEW",
	"AFB0eNEW","AFB0\[Mu]NEW","AFB0\[Tau]NEW",
	"AFBbNEW","AFBcNEW",
	"RucNEW"
	};


ObservableList["ZpoleNEW"] := {
	"\[CapitalGamma]ZNEW",
	"\[Sigma]hadNEW",
	"ReNEW","R\[Mu]NEW","R\[Tau]NEW",
	"RbNEW","RcNEW",
	"AeNEW","AeLEPNEW","A\[Mu]NEW","A\[Tau]NEW","A\[Tau]LEPNEW",
	"AbNEW","AcNEW","AsNEW",
	"AFB0eNEW","AFB0\[Mu]NEW","AFB0\[Tau]NEW",
	"AFBbNEW","AFBcNEW",
	"RucNEW"
}


LowScale[Alternatives@@(Join[EWObservables["Zpole"],EWObservables["ZpoleNEW"]])] := \[Mu]EW;


LowScale$default[Alternatives@@ObservableList["ZpoleNEW"]] := \[Mu]EW;


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


ExpValue$default["\[CapitalGamma]Z"]:=\[CapitalGamma]Z$default;


SMPrediction$default["\[CapitalGamma]Z"]:=Around[2.4941,0];


NPContribution$default["\[CapitalGamma]Z"]:=((\[CapitalDelta]\[CapitalGamma]Zhad+\[CapitalDelta]\[CapitalGamma]Zlep)(*/.Replace\[Delta]g*)/.GetParameters[]//Simplify);


(*ExpValue$FCC["\[CapitalGamma]Z"]:={SMPrediction$default["\[CapitalGamma]Z"][[1]],ExpValue$default["\[CapitalGamma]Z"]/10}*)


(* ::Section::Closed:: *)
(*\[Sigma]had*)


ExpValue$default["\[Sigma]had"]:=Around[41.4807,0.0325];


SMPrediction$default["\[Sigma]had"]:=Around[41.4842,0];


NPContribution$default["\[Sigma]had"]:=((12\[Pi])/Mass["ZBoson"]^2 (\[CapitalGamma]ZfSM[e]\[CapitalGamma]ZhadSM)/\[CapitalGamma]ZSM^2 (\[CapitalDelta]\[CapitalGamma]Z[e,1]/\[CapitalGamma]ZfSM[e]+\[CapitalDelta]\[CapitalGamma]Zhad/\[CapitalGamma]ZhadSM-2 (\[CapitalDelta]\[CapitalGamma]Zhad+\[CapitalDelta]\[CapitalGamma]Zlep)/\[CapitalGamma]ZSM)*(0.389379 10^6)(*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section::Closed:: *)
(*Re*)


\[CapitalDelta]Re[i_]:=\[CapitalDelta]\[CapitalGamma]Zhad/\[CapitalGamma]ZfSM[e]-(\[CapitalGamma]ZhadSM \[CapitalDelta]\[CapitalGamma]Z[e,i])/\[CapitalGamma]ZfSM[e]^2;


(* ::Subsection:: *)
(*Re*)


ExpValue$default["Re"]:=Around[20.8038,0.0497];


SMPrediction$default["Re"]:=Around[20.734,0];


NPContribution$default["Re"]:=(\[CapitalDelta]Re[1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*R\[Mu]*)


ExpValue$default["R\[Mu]"]:=Around[20.7842,0.0335];


SMPrediction$default["R\[Mu]"]:=Around[20.734,0];


NPContribution$default["R\[Mu]"]:=(\[CapitalDelta]Re[2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*R\[Tau]*)


ExpValue$default["R\[Tau]"]:=Around[20.7644,0.0448];


SMPrediction$default["R\[Tau]"]:=Around[20.781,0];


NPContribution$default["R\[Tau]"]:=(\[CapitalDelta]Re[3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section::Closed:: *)
(*Rq*)


\[CapitalDelta]R[q_,i_]:=3 \[CapitalDelta]\[CapitalGamma]Z[q,i]/\[CapitalGamma]ZhadSM-3(\[CapitalGamma]ZfSM[q]\[CapitalDelta]\[CapitalGamma]Zhad)/\[CapitalGamma]ZhadSM^2;


(* ::Subsection:: *)
(*Rb*)


ExpValue$default["Rb"]:=Around[0.21629,0.00066];


SMPrediction$default["Rb"]:=Around[0.21581,0];


NPContribution$default["Rb"]:=(\[CapitalDelta]R[d,3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*Rc*)


ExpValue$default["Rc"]:=Around[0.1721,0.0030];


SMPrediction$default["Rc"]:=Around[0.17222,0];


NPContribution$default["Rc"]:=(\[CapitalDelta]R[u,2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section::Closed:: *)
(*Af*)


\[CapitalDelta]A[f_,i_]:=(4 gZSM[f,Left]gZSM[f,Right])/(gZSM[f,Left]^2+gZSM[f,Right]^2)^2 (gZSM[f,Right]\[Delta]gZ[f,Left,{i,i}]-gZSM[f,Left]\[Delta]gZ[f,Right,{i,i}]);


(* ::Subsection:: *)
(*Ae*)


ExpValue$default["Ae"]:=Around[0.1516,0.0021];


SMPrediction$default["Ae"]:=Around[0.1470,0];


NPContribution$default["Ae"]:=(\[CapitalDelta]A[e,1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*AeLEP*)


ExpValue$default["AeLEP"]:=Around[0.1498,0.0049];


SMPrediction$default["AeLEP"]:=Around[0.1470,0];


NPContribution$default["AeLEP"]:=(\[CapitalDelta]A[e,1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*A\[Mu]*)


ExpValue$default["A\[Mu]"]:=Around[0.142,0.015];


SMPrediction$default["A\[Mu]"]:=Around[0.1470,0];


NPContribution$default["A\[Mu]"]:=(\[CapitalDelta]A[e,2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*A\[Tau]*)


ExpValue$default["A\[Tau]"]:=Around[0.136,0.015];


SMPrediction$default["A\[Tau]"]:=Around[0.1470,0];


NPContribution$default["A\[Tau]"]:=(\[CapitalDelta]A[e,3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*A\[Tau]LEP*)


ExpValue$default["A\[Tau]LEP"]:=Around[0.1439,0.0043];


SMPrediction$default["A\[Tau]LEP"]:=Around[0.1470,0];


NPContribution$default["A\[Tau]LEP"]:=(\[CapitalDelta]A[e,3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*Ab*)


ExpValue$default["Ab"]:=Around[0.923,0.020];


SMPrediction$default["Ab"]:=Around[0.935,0];


NPContribution$default["Ab"]:=(\[CapitalDelta]A[d,3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*Ac*)


ExpValue$default["Ac"]:=Around[0.670,0.027];


SMPrediction$default["Ac"]:=Around[0.668,0];


NPContribution$default["Ac"]:=(\[CapitalDelta]A[u,2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*As*)


ExpValue$default["As"]:=Around[0.895,0.091];


SMPrediction$default["As"]:=Around[0.936,0];


NPContribution$default["As"]:=(\[CapitalDelta]A[d,2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section::Closed:: *)
(*AFB0e*)


\[CapitalDelta]AFB0e[i_]:=3/4 ASM[e](\[CapitalDelta]A[e,1]+\[CapitalDelta]A[e,i]);


(* ::Subsection:: *)
(*AFB0e*)


ExpValue$default["AFB0e"]:=Around[0.0145,0.0025];


SMPrediction$default["AFB0e"]:=Around[0.0162,0];


NPContribution$default["AFB0e"]:=(\[CapitalDelta]AFB0e[1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*AFB0\[Mu]*)


ExpValue$default["AFB0\[Mu]"]:=Around[0.0169,0.0013];


SMPrediction$default["AFB0\[Mu]"]:=Around[0.0162,0];


NPContribution$default["AFB0\[Mu]"]:=(\[CapitalDelta]AFB0e[2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*AFB0\[Tau]*)


ExpValue$default["AFB0\[Tau]"]:=Around[0.0188,0.0017];


SMPrediction$default["AFB0\[Tau]"]:=Around[0.0162,0];


NPContribution$default["AFB0\[Tau]"]:=(\[CapitalDelta]AFB0e[3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section::Closed:: *)
(*AFBq*)


\[CapitalDelta]AFB[q_,i_]:=3/4 (ASM[q]\[CapitalDelta]A[e,1]+ASM[e]\[CapitalDelta]A[q,i]);


(* ::Subsection:: *)
(*AFBb*)


ExpValue$default["AFBb"]:=Around[0.0996,0.0016];


SMPrediction$default["AFBb"]:=Around[0.1032,0];


NPContribution$default["AFBb"]:=(\[CapitalDelta]AFB[d,3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*AFBc*)


ExpValue$default["AFBc"]:=Around[0.0707,0.0035];


SMPrediction$default["AFBc"]:=Around[0.0736,0];


NPContribution$default["AFBc"]:=(\[CapitalDelta]AFB[u,2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section::Closed:: *)
(*Ruc*)


ExpValue$default["Ruc"]:=Around[0.166,0.009];


SMPrediction$default["Ruc"]:=Around[0.1722,0];


NPContribution$default["Ruc"]:=((-3(\[CapitalGamma]ZfSM[u]/(\[CapitalGamma]ZhadSM^2))\[CapitalDelta]\[CapitalGamma]Zhad + 3/(2 \[CapitalGamma]ZhadSM) (\[CapitalDelta]\[CapitalGamma]Z[u,1]+\[CapitalDelta]\[CapitalGamma]Z[u,2]))(*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section:: *)
(*\[CapitalGamma]ZNEW*)


TheoryExpression["\[CapitalGamma]ZNEW"] := Mass["ZBoson"]/(24\[Pi]) (Sum[WCL["gZ\[Nu]L",{i,i}]^2,{i,3}]+Sum[WCL["gZeL",{i,i}]^2+WCL["gZeR",{i,i}]^2,{i,3}]+3*Sum[WCL["gZdL",{i,i}]^2+WCL["gZdR",{i,i}]^2,{i,3}]+3*Sum[WCL["gZuL",{i,i}]^2+WCL["gZuR",{i,i}]^2,{i,2}])


ExpValue$default["\[CapitalGamma]ZNEW"] := \[CapitalGamma]Z$default;


SMPrediction$default["\[CapitalGamma]ZNEW"] := Around[2.4941,null];


NPContribution$default["\[CapitalGamma]ZNEW"] := NPFromTheoryExpression["\[CapitalGamma]ZNEW"]


(* ::Section:: *)
(*\[Sigma]hadNEW*)


GeVtonb = (0.389379 10^6);


TheoryExpression["\[Sigma]hadNEW"] := GeVtonb*1/HighPT`PackageScope`TheoryExpression["\[CapitalGamma]ZNEW"]^2 (12\[Pi])/Mass["ZBoson"]^2 Mass["ZBoson"]/(24\[Pi]) (WCL["gZeL",{1,1}]^2+WCL["gZeR",{1,1}]^2) Mass["ZBoson"]/(24\[Pi]) 3(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])


ExpValue$default["\[Sigma]hadNEW"]:=Around[41.4807,0.0325];


SMPrediction$default["\[Sigma]hadNEW"]:=Around[41.4842,null];


NPContribution$default["\[Sigma]hadNEW"] := NPFromTheoryExpression["\[Sigma]hadNEW"]


(* ::Section:: *)
(*ReNEW*)


RRe[i_] := 3 (Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])/(WCL["gZeL",{i,i}]^2+WCL["gZeR",{i,i}]^2)


(* ::Subsection:: *)
(*Re*)


TheoryExpression["ReNEW"] := RRe[1];


ExpValue$default["ReNEW"]:=Around[20.8038,0.0497];


SMPrediction$default["ReNEW"]:=Around[20.734,null];


NPContribution$default["ReNEW"] := NPFromTheoryExpression["ReNEW"]


(* ::Subsection:: *)
(*R\[Mu]*)


TheoryExpression["R\[Mu]NEW"] := RRe[2];


ExpValue$default["R\[Mu]NEW"]:=Around[20.7842,0.0335];


SMPrediction$default["R\[Mu]NEW"]:=Around[20.734,null];


NPContribution$default["R\[Mu]NEW"] := NPFromTheoryExpression["R\[Mu]NEW"]


(* ::Subsection:: *)
(*R\[Tau]*)


TheoryExpression["R\[Tau]NEW"] := RRe[3];


ExpValue$default["R\[Tau]NEW"]:=Around[20.7644,0.0448];


SMPrediction$default["R\[Tau]NEW"]:=Around[20.781,null];


NPContribution$default["R\[Tau]NEW"] := NPFromTheoryExpression["R\[Tau]NEW"]


(* ::Section:: *)
(*RqNEW*)


RRd[i_] := (WCL["gZdL",{i,i}]^2+WCL["gZdR",{i,i}]^2)/(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])
RRu[i_] := (WCL["gZuL",{i,i}]^2+WCL["gZuR",{i,i}]^2)/(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}])


(* ::Subsection:: *)
(*Rb*)


TheoryExpression["RbNEW"] := RRd[3];


ExpValue$default["RbNEW"]:=Around[0.21629,0.00066];


SMPrediction$default["RbNEW"]:=Around[0.21581,null];


NPContribution$default["RbNEW"] := NPFromTheoryExpression["RbNEW"]


(* ::Subsection:: *)
(*Rc*)


TheoryExpression["RcNEW"] := RRu[2];


ExpValue$default["RcNEW"]:=Around[0.1721,0.0030];


SMPrediction$default["RcNEW"]:=Around[0.17222,null];


NPContribution$default["RcNEW"] := NPFromTheoryExpression["RcNEW"]


(* ::Section:: *)
(*AfNEW*)


Ae[i_] := (WCL["gZeL",{i,i}]^2-WCL["gZeR",{i,i}]^2)/(WCL["gZeL",{i,i}]^2+WCL["gZeR",{i,i}]^2)
Ad[i_] := (WCL["gZdL",{i,i}]^2-WCL["gZdR",{i,i}]^2)/(WCL["gZdL",{i,i}]^2+WCL["gZdR",{i,i}]^2)
Au[i_] := (WCL["gZuL",{i,i}]^2-WCL["gZuR",{i,i}]^2)/(WCL["gZuL",{i,i}]^2+WCL["gZuR",{i,i}]^2)


(* ::Subsection:: *)
(*Ae*)


TheoryExpression["AeNEW"] := Ae[1];


ExpValue$default["AeNEW"]:=Around[0.1516,0.0021];


SMPrediction$default["AeNEW"]:=Around[0.1470,null];


NPContribution$default["AeNEW"] := NPFromTheoryExpression["AeNEW"]


(* ::Subsection:: *)
(*AeLEP*)


TheoryExpression["AeLEPNEW"] := Ae[1];


ExpValue$default["AeLEPNEW"]:=Around[0.1498,0.0049];


SMPrediction$default["AeLEPNEW"]:=Around[0.1470,null];


NPContribution$default["AeLEPNEW"] := NPFromTheoryExpression["AeLEPNEW"]


(* ::Subsection:: *)
(*A\[Mu]*)


TheoryExpression["A\[Mu]NEW"] := Ae[2];


ExpValue$default["A\[Mu]NEW"]:=Around[0.142,0.015];


SMPrediction$default["A\[Mu]NEW"]:=Around[0.1470,null];


NPContribution$default["A\[Mu]NEW"] := NPFromTheoryExpression["A\[Mu]NEW"]


(* ::Subsection:: *)
(*A\[Tau]*)


TheoryExpression["A\[Tau]NEW"] := Ae[3];


ExpValue$default["A\[Tau]NEW"]:=Around[0.136,0.015];


SMPrediction$default["A\[Tau]NEW"]:=Around[0.1470,null];


NPContribution$default["A\[Tau]NEW"] := NPFromTheoryExpression["A\[Tau]NEW"]


(* ::Subsection:: *)
(*A\[Tau]LEP*)


TheoryExpression["A\[Tau]LEPNEW"] := Ae[3];


ExpValue$default["A\[Tau]LEPNEW"]:=Around[0.1439,0.0043];


SMPrediction$default["A\[Tau]LEPNEW"]:=Around[0.1470,null];


NPContribution$default["A\[Tau]LEPNEW"] := NPFromTheoryExpression["A\[Tau]LEPNEW"]


(* ::Subsection:: *)
(*Ab*)


TheoryExpression["AbNEW"] := Ad[3];


ExpValue$default["AbNEW"]:=Around[0.923,0.020];


SMPrediction$default["AbNEW"]:=Around[0.935,null];


NPContribution$default["AbNEW"] := NPFromTheoryExpression["AbNEW"]


(* ::Subsection:: *)
(*Ac*)


TheoryExpression["AcNEW"] := Au[2];


ExpValue$default["AcNEW"]:=Around[0.670,0.027];


SMPrediction$default["AcNEW"]:=Around[0.668,null];


NPContribution$default["AcNEW"] := NPFromTheoryExpression["AcNEW"]


(* ::Subsection:: *)
(*As*)


TheoryExpression["AsNEW"] := Ad[2];


ExpValue$default["AsNEW"]:=Around[0.895,0.091];


SMPrediction$default["AsNEW"]:=Around[0.936,null];


NPContribution$default["AsNEW"] := NPFromTheoryExpression["AsNEW"]


(* ::Section:: *)
(*AFB0eNEW*)


AFB0e[i_] := 3/4 Ae[1]Ae[i]


(* ::Subsection:: *)
(*AFB0e*)


TheoryExpression["AFB0eNEW"] := AFB0e[1];


ExpValue$default["AFB0eNEW"] := Around[0.0145,0.0025];


SMPrediction$default["AFB0eNEW"] := Around[0.0162,null];


NPContribution$default["AFB0eNEW"] := NPFromTheoryExpression["AFB0eNEW"];


(* ::Subsection:: *)
(*AFB0\[Mu]*)


TheoryExpression["AFB0\[Mu]NEW"] := AFB0e[2];


ExpValue$default["AFB0\[Mu]NEW"] := Around[0.0169,0.0013];


SMPrediction$default["AFB0\[Mu]NEW"] := Around[0.0162,null];


NPContribution$default["AFB0\[Mu]NEW"] := NPFromTheoryExpression["AFB0\[Mu]NEW"];


(* ::Subsection:: *)
(*AFB0\[Tau]*)


TheoryExpression["AFB0\[Tau]NEW"] := AFB0e[3];


ExpValue$default["AFB0\[Tau]NEW"] := Around[0.0188,0.0017];


SMPrediction$default["AFB0\[Tau]NEW"] := Around[0.0162,null];


NPContribution$default["AFB0\[Tau]NEW"] := NPFromTheoryExpression["AFB0\[Tau]NEW"];


(* ::Section:: *)
(*AFBqNEW*)


AFBd[i_] := 3/4 Ae[1]Ad[i]


AFBu[i_] := 3/4 Ae[1]Au[i]


(* ::Subsection:: *)
(*AFBb*)


TheoryExpression["AFBbNEW"] := AFBd[3];


ExpValue$default["AFBbNEW"] := Around[0.0996,0.0016];


SMPrediction$default["AFBbNEW"] := Around[0.1032,null];


NPContribution$default["AFBbNEW"] := NPFromTheoryExpression["AFBbNEW"];


(* ::Subsection:: *)
(*AFBc*)


TheoryExpression["AFBcNEW"] := AFBu[2];


ExpValue$default["AFBcNEW"] := Around[0.0707,0.0035];


SMPrediction$default["AFBcNEW"] := Around[0.0736,null];


NPContribution$default["AFBcNEW"] := NPFromTheoryExpression["AFBcNEW"];


(* ::Section:: *)
(*RucNEW*)


TheoryExpression["RucNEW"] := Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}]/(2(Sum[WCL["gZdL",{j,j}]^2+WCL["gZdR",{j,j}]^2,{j,3}]+Sum[WCL["gZuL",{j,j}]^2+WCL["gZuR",{j,j}]^2,{j,2}]))


ExpValue$default["RucNEW"] := Around[0.166,0.009];


SMPrediction$default["RucNEW"] := Around[0.1722,null];


NPContribution$default["RucNEW"] := NPFromTheoryExpression["RucNEW"];


(* ::Section:: *)
(*Exp Correlations*)


(* ::Subsection:: *)
(*Re and AFB0e*)


ExpCorrelation["\[CapitalGamma]Z","\[Sigma]had"]:=-0.3249
ExpCorrelation["\[CapitalGamma]Z","Re"]:=-0.0110
ExpCorrelation["\[CapitalGamma]Z","R\[Mu]"]:=0.0079
ExpCorrelation["\[CapitalGamma]Z","R\[Tau]"]:=0.0059
ExpCorrelation["\[CapitalGamma]Z","AFB0e"]:=0.0071
ExpCorrelation["\[CapitalGamma]Z","AFB0\[Mu]"]:=0.0020
ExpCorrelation["\[CapitalGamma]Z","AFB0\[Tau]"]:=0.0013


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


(* ::Subsection:: *)
(*Re and AFB0e NEW*)


ExpCorrelation["\[CapitalGamma]ZNEW","\[Sigma]hadNEW"]:=-0.3249
ExpCorrelation["\[CapitalGamma]ZNEW","ReNEW"]:=-0.0110
ExpCorrelation["\[CapitalGamma]ZNEW","R\[Mu]NEW"]:=0.0079
ExpCorrelation["\[CapitalGamma]ZNEW","R\[Tau]NEW"]:=0.0059
ExpCorrelation["\[CapitalGamma]ZNEW","AFB0eNEW"]:=0.0071
ExpCorrelation["\[CapitalGamma]ZNEW","AFB0\[Mu]NEW"]:=0.0020
ExpCorrelation["\[CapitalGamma]ZNEW","AFB0\[Tau]NEW"]:=0.0013


ExpCorrelation["\[Sigma]hadNEW","ReNEW"]:=0.105;
ExpCorrelation["\[Sigma]hadNEW","R\[Mu]NEW"]:=0.131;
ExpCorrelation["\[Sigma]hadNEW","R\[Tau]NEW"]:=0.092;
ExpCorrelation["\[Sigma]hadNEW","AFB0eNEW"]:=0.001;
ExpCorrelation["\[Sigma]hadNEW","AFB0\[Mu]NEW"]:=0.003;
ExpCorrelation["\[Sigma]hadNEW","AFB0\[Tau]NEW"]:=0.002;


ExpCorrelation["ReNEW","R\[Mu]NEW"]:=0.069;
ExpCorrelation["ReNEW","R\[Tau]NEW"]:=0.046;
ExpCorrelation["ReNEW","AFB0eNEW"]:=-0.371;
ExpCorrelation["ReNEW","AFB0\[Mu]NEW"]:=0.02;
ExpCorrelation["ReNEW","AFB0\[Tau]NEW"]:=0.013;


ExpCorrelation["R\[Mu]NEW","R\[Tau]NEW"]:=0.069;
ExpCorrelation["R\[Mu]NEW","AFB0eNEW"]:=0.001;
ExpCorrelation["R\[Mu]NEW","AFB0\[Mu]NEW"]:=0.012;
ExpCorrelation["R\[Mu]NEW","AFB0\[Tau]NEW"]:=-0.003;


ExpCorrelation["R\[Tau]NEW","AFB0eNEW"]:=0.003;
ExpCorrelation["R\[Tau]NEW","AFB0\[Mu]NEW"]:=0.001;
ExpCorrelation["R\[Tau]NEW","AFB0\[Tau]NEW"]:=0.009;


ExpCorrelation["AFB0eNEW","AFB0\[Mu]NEW"]:=-0.024;
ExpCorrelation["AFB0eNEW","AFB0\[Tau]NEW"]:=-0.02;


ExpCorrelation["AFB0\[Mu]NEW","AFB0\[Tau]NEW"]:=0.046;


(* ::Subsection:: *)
(*Rq and AFBq*)


ExpCorrelation["Rb","Rc"]:=-0.18;
ExpCorrelation["Rb","AFBb"]:=-0.1;
ExpCorrelation["Rb","AFBc"]:=0.07;


ExpCorrelation["Rc","AFBb"]:=0.04;
ExpCorrelation["Rc","AFBc"]:=-0.06;


ExpCorrelation["AFBb","AFBc"]:=0.15;


(* ::Subsection:: *)
(*Rq and AFBq NEW*)


ExpCorrelation["RbNEW","RcNEW"]:=-0.18;
ExpCorrelation["RbNEW","AFBbNEW"]:=-0.1;
ExpCorrelation["RbNEW","AFBcNEW"]:=0.07;


ExpCorrelation["RcNEW","AFBbNEW"]:=0.04;
ExpCorrelation["RcNEW","AFBcNEW"]:=-0.06;


ExpCorrelation["AFBbNEW","AFBcNEW"]:=0.15;


(* ::Subsection:: *)
(*Ae*)


ExpCorrelation["Ae","A\[Mu]"]:=0.038;
ExpCorrelation["Ae","A\[Tau]"]:=0.033;


ExpCorrelation["A\[Mu]","A\[Tau]"]:=0.007;


(* ::Subsection:: *)
(*Ae NEW*)


ExpCorrelation["AeNEW","A\[Mu]NEW"]:=0.038;
ExpCorrelation["AeNEW","A\[Tau]NEW"]:=0.033;


ExpCorrelation["A\[Mu]NEW","A\[Tau]NEW"]:=0.007;


(* ::Subsection:: *)
(*Aq*)


ExpCorrelation["Ab","Ac"]:=0.11;


(* ::Subsection:: *)
(*Aq NEW*)


ExpCorrelation["AbNEW","AcNEW"]:=0.11;


(* ::Section:: *)
(*FCC projections*)


FCCZpoleProjections=<|
	"\[CapitalGamma]Z"->0.000004,
	"\[Sigma]had"->0,
	"Re"->0,
	"R\[Mu]"->0,
	"R\[Tau]"->0,
	"Rb"->0,
	"Rc"->0,
	"Ae"->0,
	"AeLEP"->0,
	"A\[Mu]"->0,
	"A\[Tau]"->0,
	"A\[Tau]LEP"->0,
	"Ab"->0,
	"Ac"->0,
	"As"->0,
	"AFB0e"->0,
	"AFB0\[Mu]"->0,
	"AFB0\[Tau]"->0,
	"AFBb"->0,
	"AFBc"->0,
	"Ruc"->0
|>;
