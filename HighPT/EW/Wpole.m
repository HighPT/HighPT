(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Wpole`*)


(* ::Subtitle:: *)
(*W pole observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["FCCWpoleProjections"]


(* ::Chapter:: *)
(*Private:*)


EWObservables["Wpole"]={
	"mW",
	"\[CapitalGamma]W",
	"W->e\[Nu]","W->\[Mu]\[Nu]","W->\[Tau]\[Nu]",
	"W->\[Mu]\[Nu]/W->e\[Nu]_CDF","W->e\[Nu]/W->\[Mu]\[Nu]_LHCb","W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS","W->\[Tau]\[Nu]/W->e\[Nu]","W->\[Tau]\[Nu]/W->\[Mu]\[Nu]",
	"RWc"
	};


EWObservables["WpoleNEW"]={
	"mWNEW",
	"\[CapitalGamma]WNEW",
	"W->e\[Nu]NEW","W->\[Mu]\[Nu]NEW","W->\[Tau]\[Nu]NEW",
	"W->\[Mu]\[Nu]/W->e\[Nu]_CDFNEW","W->e\[Nu]/W->\[Mu]\[Nu]_LHCbNEW","W->\[Mu]\[Nu]/W->e\[Nu]_ATLASNEW","W->\[Tau]\[Nu]/W->e\[Nu]NEW","W->\[Tau]\[Nu]/W->\[Mu]\[Nu]NEW",
	"RWcNEW"
	};


ObservableList["WpoleNEW"] := {
	"mWNEW",
	"\[CapitalGamma]WNEW",
	"W->e\[Nu]NEW","W->\[Mu]\[Nu]NEW","W->\[Tau]\[Nu]NEW",
	"W->\[Mu]\[Nu]/W->e\[Nu]_CDFNEW","W->e\[Nu]/W->\[Mu]\[Nu]_LHCbNEW","W->\[Mu]\[Nu]/W->e\[Nu]_ATLASNEW","W->\[Tau]\[Nu]/W->e\[Nu]NEW","W->\[Tau]\[Nu]/W->\[Mu]\[Nu]NEW",
	"RWcNEW"
}


LowScale[Alternatives@@(Join[EWObservables["Wpole"],{}(*EWObservables["WpoleNEW"]*)])] := \[Mu]EW;


LowScale$default[Alternatives@@(ObservableList["WpoleNEW"])] := \[Mu]EW;


(* ::Section:: *)
(*Useful definitions*)


NW=Param["g2"]^2/(48\[Pi]);


mWSM=(Param["g2"]Param["vev"])/2;


\[CapitalGamma]WSM=9 NW mWSM;


(* ::Section:: *)
(*mW*)


(*\[Delta]mW=-((Param["vev"]^2 Param["g2"]^2)/(4(Param["g2"]^2-Param["g1"]^2)))WC["HD",{}]-(Param["vev"]^2 Param["g2"]Param["g1"])/(Param["g2"]^2-Param["g1"]^2) WC["HWB",{}]+(Param["vev"]^2 Param["g1"]^2)/(4(Param["g2"]^2-Param["g1"]^2)) (WC["ll",{1,2,2,1}]-2 WC["Hl3",{2,2}]-2 WC["Hl3",{1,1}]);*)


ExpValue$default["mW"]:=Around[80.379,0.012];


SMPrediction$default["mW"]:=Around[80.356,0];


NPContribution$default["mW"]:=((mWSM \[Delta]mW[])(*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section:: *)
(*\[CapitalGamma]W*)


ExpValue$default["\[CapitalGamma]W"]:=\[CapitalGamma]W$default;


SMPrediction$default["\[CapitalGamma]W"]:=Around[2.088,0];


NPContribution$default["\[CapitalGamma]W"]:=((2 NW mWSM (3 Sum[Re[Vckm[i,j]\[Conjugate]\[Delta]gW["q",Left,{i,j}]],{i,2},{j,3}]+Sum[\[Delta]gW["l",Left,{i,i}],{i,3}])+\[CapitalGamma]WSM \[Delta]mW[])(*/.Replace\[Delta]g*)/.GetParameters[]);                                                                                                                                                                      ;


(* ::Section:: *)
(*Branching fractions*)


\[CapitalDelta]BrWl\[Nu][i_]:=16/81 \[Delta]gW["l",Left,{i,i}]-2/81 Sum[(1-KroneckerDelta[i,j])\[Delta]gW["l",Left,{j,j}],{j,3}]-2/27 Sum[Re[Vckm[j,k]\[Conjugate]\[Delta]gW["q",Left,{j,k}]],{j,2},{k,3}];


(* ::Subsection:: *)
(*W -> e\[Nu]*)


ExpValue$default["W->e\[Nu]"]:=Around[0.1071,0.0016];


SMPrediction$default["W->e\[Nu]"]:=Around[0.1082,0];


NPContribution$default["W->e\[Nu]"]:=(\[CapitalDelta]BrWl\[Nu][1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]*)


ExpValue$default["W->\[Mu]\[Nu]"]:=Around[0.1063,0.0015];


SMPrediction$default["W->\[Mu]\[Nu]"]:=Around[0.1082,0];


NPContribution$default["W->\[Mu]\[Nu]"]:=(\[CapitalDelta]BrWl\[Nu][2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]*)


ExpValue$default["W->\[Tau]\[Nu]"]:=Around[0.1138,0.0021];


SMPrediction$default["W->\[Tau]\[Nu]"]:=Around[0.1081,0];


NPContribution$default["W->\[Tau]\[Nu]"]:=(\[CapitalDelta]BrWl\[Nu][3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section:: *)
(*Universality ratios*)


\[CapitalDelta]BrWl\[Nu]ratio[i_,j_]:=2 \[Delta]gW["l",Left,{i,i}]-2 \[Delta]gW["l",Left,{j,j}];


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (CDF)*)


ExpValue$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDF"]:=Around[0.982,0.024];


SMPrediction$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDF"]:=Around[1.000,0];


NPContribution$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDF"]:=(\[CapitalDelta]BrWl\[Nu]ratio[2,1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*W -> e\[Nu]/W -> \[Mu]\[Nu] (LHCb)*)


ExpValue$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"]:=Around[1.020,0.019];


SMPrediction$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"]:=Around[1.000,0];


NPContribution$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"]:=\[CapitalDelta]BrWl\[Nu]ratio[1,2](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (ATLAS)*)


ExpValue$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"]:=Around[1.003,0.010];


SMPrediction$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"]:=Around[1.000,0];


NPContribution$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"]:=\[CapitalDelta]BrWl\[Nu]ratio[2,1](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> e\[Nu] (PDG, D0)*)


ExpValue$default["W->\[Tau]\[Nu]/W->e\[Nu]"]:=Around[0.961,0.061];


SMPrediction$default["W->\[Tau]\[Nu]/W->e\[Nu]"]:=Around[0.999,0];


NPContribution$default["W->\[Tau]\[Nu]/W->e\[Nu]"]:=\[CapitalDelta]BrWl\[Nu]ratio[3,1](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> \[Mu]\[Nu] (ATLAS)*)


ExpValue$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"]:=Around[0.992,0.013];


SMPrediction$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"]:=Around[0.999,0];


NPContribution$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"]:=\[CapitalDelta]BrWl\[Nu]ratio[3,2](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Section:: *)
(*RWc*)


ExpValue$default["RWc"]:=Around[0.49,0.04];


SMPrediction$default["RWc"]:=Around[0.50,0];


NPContribution$default["RWc"]:=(((2 Vckm[2,2]\[Delta]gW["q",Left,{2,2}])/(Vckm[1,1]^2+Vckm[2,2]^2)-2 Vckm[2,2]^2/(Vckm[1,1]^2+Vckm[2,2]^2)^2 (Vckm[1,1]\[Delta]gW["q",Left,{1,1}]+Vckm[2,2]\[Delta]gW["q",Left,{2,2}]))(*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section:: *)
(*mW NEW*)


TheoryExpression["mWNEW"] := WCL["mW",{}]


ExpValue$default["mWNEW"] := Around[80.379,0.012];


SMPrediction$default["mWNEW"] := Around[80.356,null];


NPContribution$default["mWNEW"] := NPFromTheoryExpression["mWNEW"]


(* ::Section:: *)
(*\[CapitalGamma]W NEW*)


Wcouplingsum = (*Sum[Abs[WCL["gWlL",{i,j}]]^2,{i,3},{j,3}]+3*Sum[Abs[WCL["gWqL",{i,j}]]^2+Abs[WCL["gWqR",{i,j}]]^2,{i,2},{j,3}]*)Sum[WCL["gWlL",{i,j}]^2,{i,3},{j,3}]+3*Sum[WCL["gWqL",{i,j}]^2+WCL["gWqR",{i,j}]^2,{i,2},{j,3}]


TheoryExpression["\[CapitalGamma]WNEW"] := (*Mass["WBoson"]*)WCL["mW",{}]/(24\[Pi])*Wcouplingsum(*(Sum[WCL["gWlL",{i,j}]^2,{i,3},{j,3}]+3*Sum[WCL["gWqL",{i,j}]^2+WCL["gWqR",{i,j}]^2,{i,2},{j,3}])*)


ExpValue$default["\[CapitalGamma]WNEW"] := \[CapitalGamma]W$default;


SMPrediction$default["\[CapitalGamma]WNEW"] := Around[2.088,null];


NPContribution$default["\[CapitalGamma]WNEW"] := NPFromTheoryExpression["\[CapitalGamma]WNEW"]


(* ::Section:: *)
(*Branching fractions NEW*)


BrWlnu[i_,j_] := (*1/TheoryExpression["\[CapitalGamma]WNEW"] Mass["WBoson"]/(24\[Pi])**)WCL["gWlL",{i,j}]^2/Wcouplingsum


(* ::Subsection:: *)
(*W -> e\[Nu]*)


TheoryExpression["W->e\[Nu]NEW"] := Sum[BrWlnu[1,j],{j,3}]


ExpValue$default["W->e\[Nu]NEW"] := Around[0.1071,0.0016];


SMPrediction$default["W->e\[Nu]NEW"] := Around[0.1082,null];


NPContribution$default["W->e\[Nu]NEW"] := NPFromTheoryExpression["W->e\[Nu]NEW"]


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]*)


TheoryExpression["W->\[Mu]\[Nu]NEW"] := Sum[BrWlnu[2,j],{j,3}]


ExpValue$default["W->\[Mu]\[Nu]NEW"] := Around[0.1063,0.0015];


SMPrediction$default["W->\[Mu]\[Nu]NEW"] := Around[0.1082,null];


NPContribution$default["W->\[Mu]\[Nu]NEW"] := NPFromTheoryExpression["W->\[Mu]\[Nu]NEW"]


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]*)


TheoryExpression["W->\[Tau]\[Nu]NEW"] := Sum[BrWlnu[3,j],{j,3}]


ExpValue$default["W->\[Tau]\[Nu]NEW"] := Around[0.1138,0.0021];


SMPrediction$default["W->\[Tau]\[Nu]NEW"] := Around[0.1081,null];


NPContribution$default["W->\[Tau]\[Nu]NEW"] := NPFromTheoryExpression["W->\[Tau]\[Nu]NEW"]


(* ::Section:: *)
(*Universality ratios NEW*)


(*\[CapitalDelta]BrWl\[Nu]ratio[i_,j_]:=2 \[Delta]gW["l",Left,{i,i}]-2 \[Delta]gW["l",Left,{j,j}];*)


(* W->l_i \[Nu] / W->l_k \[Nu] *)
BrWlnuRatio[i_,k_] := Sum[WCL["gWlL",{i,j}]^2,{j,3}]/Sum[WCL["gWlL",{k,l}]^2,{l,3}]


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (CDF)*)


TheoryExpression["W->\[Mu]\[Nu]/W->e\[Nu]_CDFNEW"] := BrWlnuRatio[2,1]


ExpValue$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDFNEW"] := Around[0.982,0.024];


SMPrediction$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDFNEW"] := Around[1.000,null];


NPContribution$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDFNEW"] := NPFromTheoryExpression["W->\[Mu]\[Nu]/W->e\[Nu]_CDFNEW"]


(* ::Subsection:: *)
(*W -> e\[Nu]/W -> \[Mu]\[Nu] (LHCb)*)


TheoryExpression["W->e\[Nu]/W->\[Mu]\[Nu]_LHCbNEW"] := BrWlnuRatio[1,2]


ExpValue$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCbNEW"] := Around[1.020,0.019];


SMPrediction$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCbNEW"] := Around[1.000,null];


NPContribution$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCbNEW"] := NPFromTheoryExpression["W->e\[Nu]/W->\[Mu]\[Nu]_LHCbNEW"]


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (ATLAS)*)


TheoryExpression["W->\[Mu]\[Nu]/W->e\[Nu]_ATLASNEW"] := BrWlnuRatio[2,1]


ExpValue$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLASNEW"] := Around[1.003,0.010];


SMPrediction$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLASNEW"] := Around[1.000,null];


NPContribution$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLASNEW"] := NPFromTheoryExpression["W->\[Mu]\[Nu]/W->e\[Nu]_ATLASNEW"]


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> e\[Nu] (PDG, D0)*)


TheoryExpression["W->\[Tau]\[Nu]/W->e\[Nu]NEW"] := BrWlnuRatio[3,1]


ExpValue$default["W->\[Tau]\[Nu]/W->e\[Nu]NEW"] := Around[0.961,0.061];


SMPrediction$default["W->\[Tau]\[Nu]/W->e\[Nu]NEW"] := Around[0.999,null];


NPContribution$default["W->\[Tau]\[Nu]/W->e\[Nu]NEW"] := NPFromTheoryExpression["W->\[Tau]\[Nu]/W->e\[Nu]NEW"]


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> \[Mu]\[Nu] (ATLAS)*)


TheoryExpression["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]NEW"] := BrWlnuRatio[3,2]


ExpValue$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]NEW"] := Around[0.992,0.013];


SMPrediction$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]NEW"] := Around[0.999,null];


NPContribution$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]NEW"] := NPFromTheoryExpression["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]NEW"]


(* ::Section:: *)
(*RWc NEW*)


TheoryExpression["RWcNEW"] := (WCL["gWqL",{2,2}]^2+WCL["gWqR",{2,2}]^2)/Sum[WCL["gWqL",{i,i}]^2+WCL["gWqR",{i,i}]^2,{i,2}]


ExpValue$default["RWcNEW"] := Around[0.49,0.04];


SMPrediction$default["RWcNEW"] := Around[0.50,null];


NPContribution$default["RWcNEW"] := NPFromTheoryExpression["RWcNEW"]


(* ::Section:: *)
(*Exp Correlations*)


ExpCorrelation["W->e\[Nu]","W->\[Mu]\[Nu]"]:=0.136;
ExpCorrelation["W->e\[Nu]","W->\[Tau]\[Nu]"]:=-0.201;


ExpCorrelation["W->\[Mu]\[Nu]","W->\[Tau]\[Nu]"]:=-0.122;


(* ::Section:: *)
(*Exp Correlations NEW*)


ExpCorrelation["W->e\[Nu]NEW","W->\[Mu]\[Nu]NEW"]:=0.136;
ExpCorrelation["W->e\[Nu]NEW","W->\[Tau]\[Nu]NEW"]:=-0.201;


ExpCorrelation["W->\[Mu]\[Nu]NEW","W->\[Tau]\[Nu]NEW"]:=-0.122;


(* ::Section:: *)
(*FCC projections*)


FCCWpoleProjections=<|
	"mW"->0,
	"\[CapitalGamma]W"->0,
	"W->e\[Nu]"->0,
	"W->\[Mu]\[Nu]"->0,
	"W->\[Tau]\[Nu]"->0,
	"W->\[Mu]\[Nu]/W->e\[Nu]_CDF"->0,
	"W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"->0,
	"W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"->0,
	"W->\[Tau]\[Nu]/W->e\[Nu]"->0,
	"W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"->0,
	"RWc"->0
|>
