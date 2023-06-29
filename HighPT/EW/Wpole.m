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
	"We\[Nu]","W\[Mu]\[Nu]","W\[Tau]\[Nu]",
	"W\[Mu]\[Nu]We\[Nu]CDF","We\[Nu]W\[Mu]\[Nu]LHCb","W\[Mu]\[Nu]We\[Nu]ATLAS","W\[Tau]\[Nu]We\[Nu]","W\[Tau]\[Nu]W\[Mu]\[Nu]",
	"RWc"
	};


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


ExpValue$default["We\[Nu]"]:=Around[0.1071,0.0016];


SMPrediction$default["We\[Nu]"]:=Around[0.1082,0];


NPContribution$default["We\[Nu]"]:=(\[CapitalDelta]BrWl\[Nu][1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]*)


ExpValue$default["W\[Mu]\[Nu]"]:=Around[0.1063,0.0015];


SMPrediction$default["W\[Mu]\[Nu]"]:=Around[0.1082,0];


NPContribution$default["W\[Mu]\[Nu]"]:=(\[CapitalDelta]BrWl\[Nu][2](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]*)


ExpValue$default["W\[Tau]\[Nu]"]:=Around[0.1138,0.0021];


SMPrediction$default["W\[Tau]\[Nu]"]:=Around[0.1081,0];


NPContribution$default["W\[Tau]\[Nu]"]:=(\[CapitalDelta]BrWl\[Nu][3](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section:: *)
(*Universality ratios*)


\[CapitalDelta]BrWl\[Nu]ratio[i_,j_]:=2 \[Delta]gW["l",Left,{i,i}]-2 \[Delta]gW["l",Left,{j,j}];


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (CDF)*)


ExpValue$default["W\[Mu]\[Nu]We\[Nu]CDF"]:=Around[0.982,0.024];


SMPrediction$default["W\[Mu]\[Nu]We\[Nu]CDF"]:=Around[1.000,0];


NPContribution$default["W\[Mu]\[Nu]We\[Nu]CDF"]:=(\[CapitalDelta]BrWl\[Nu]ratio[2,1](*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Subsection:: *)
(*W -> e\[Nu]/W -> \[Mu]\[Nu] (LHCb)*)


ExpValue$default["We\[Nu]W\[Mu]\[Nu]LHCb"]:=Around[1.020,0.019];


SMPrediction$default["We\[Nu]W\[Mu]\[Nu]LHCb"]:=Around[1.000,0];


NPContribution$default["We\[Nu]W\[Mu]\[Nu]LHCb"]:=\[CapitalDelta]BrWl\[Nu]ratio[1,2](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (ATLAS)*)


ExpValue$default["W\[Mu]\[Nu]We\[Nu]ATLAS"]:=Around[1.003,0.010];


SMPrediction$default["W\[Mu]\[Nu]We\[Nu]ATLAS"]:=Around[1.000,0];


NPContribution$default["W\[Mu]\[Nu]We\[Nu]ATLAS"]:=\[CapitalDelta]BrWl\[Nu]ratio[2,1](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> e\[Nu] (PDG, D0)*)


ExpValue$default["W\[Tau]\[Nu]We\[Nu]"]:=Around[0.961,0.061];


SMPrediction$default["W\[Tau]\[Nu]We\[Nu]"]:=Around[0.999,0];


NPContribution$default["W\[Tau]\[Nu]We\[Nu]"]:=\[CapitalDelta]BrWl\[Nu]ratio[3,1](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> \[Mu]\[Nu] (ATLAS)*)


ExpValue$default["W\[Tau]\[Nu]W\[Mu]\[Nu]"]:=Around[0.992,0.013];


SMPrediction$default["W\[Tau]\[Nu]W\[Mu]\[Nu]"]:=Around[0.999,0];


NPContribution$default["W\[Tau]\[Nu]W\[Mu]\[Nu]"]:=\[CapitalDelta]BrWl\[Nu]ratio[3,2](*/.Replace\[Delta]g*)/.GetParameters[];


(* ::Section:: *)
(*RWc*)


ExpValue$default["RWc"]:=Around[0.49,0.04];


SMPrediction$default["RWc"]:=Around[0.50,0];


NPContribution$default["RWc"]:=(((2 Vckm[2,2]\[Delta]gW["q",Left,{2,2}])/(Vckm[1,1]^2+Vckm[2,2]^2)-2 Vckm[2,2]^2/(Vckm[1,1]^2+Vckm[2,2]^2)^2 (Vckm[1,1]\[Delta]gW["q",Left,{1,1}]+Vckm[2,2]\[Delta]gW["q",Left,{2,2}]))(*/.Replace\[Delta]g*)/.GetParameters[]);


(* ::Section:: *)
(*Exp Correlations*)


ExpCorrelation["We\[Nu]","W\[Mu]\[Nu]"]:=0.136;
ExpCorrelation["We\[Nu]","W\[Tau]\[Nu]"]:=-0.201;


ExpCorrelation["W\[Mu]\[Nu]","W\[Tau]\[Nu]"]:=-0.122;


(* ::Section:: *)
(*FCC projections*)


FCCWpoleProjections=<|
	"mW"->0,
	"\[CapitalGamma]W"->0,
	"We\[Nu]"->0,
	"W\[Mu]\[Nu]"->0,
	"W\[Tau]\[Nu]"->0,
	"W\[Mu]\[Nu]We\[Nu]CDF"->0,
	"We\[Nu]W\[Mu]\[Nu]LHCb"->0,
	"W\[Mu]\[Nu]We\[Nu]ATLAS"->0,
	"W\[Tau]\[Nu]We\[Nu]"->0,
	"W\[Tau]\[Nu]W\[Mu]\[Nu]"->0,
	"RWc"->0
|>
