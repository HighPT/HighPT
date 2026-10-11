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


(* ::Chapter:: *)
(*Private:*)


ObservableList["Wpole"] := {
	"mW",
	"\[CapitalGamma]W",
	"W->e\[Nu]","W->\[Mu]\[Nu]","W->\[Tau]\[Nu]",
	"W->\[Mu]\[Nu]/W->e\[Nu]_CDF","W->e\[Nu]/W->\[Mu]\[Nu]_LHCb","W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS","W->\[Tau]\[Nu]/W->e\[Nu]","W->\[Tau]\[Nu]/W->\[Mu]\[Nu]",
	"RWc"
}


(*LowScale$default[Alternatives@@(ObservableList["Wpole"])] := \[Mu]EW;*)


(* ::Section:: *)
(*mW *)


TheoryExpression["mW"] := WCL["mW",{}]


ExpValue$default["mW"] := Around[80.379,0.012];


SMPrediction$default["mW"] := Around[80.356,null];


LowScale$default["mW"] := \[Mu]EW


(* ::Section:: *)
(*\[CapitalGamma]W *)


Wcouplingsum = Sum[WCL["gWlL",{i,j}]^2,{i,3},{j,3}]+3*Sum[WCL["gWqL",{i,j}]^2+WCL["gWqR",{i,j}]^2,{i,2},{j,3}]


TheoryExpression["\[CapitalGamma]W"] := WCL["mW",{}]/(24\[Pi])*Wcouplingsum


ExpValue$default["\[CapitalGamma]W"] := Width["WBoson"][Default];


SMPrediction$default["\[CapitalGamma]W"] := Around[2.088,null];


LowScale$default["\[CapitalGamma]W"] := \[Mu]EW


(* ::Section:: *)
(*Branching fractions *)


BrWlnu[i_,j_] := WCL["gWlL",{i,j}]^2/Wcouplingsum


(* ::Subsection:: *)
(*W -> e\[Nu]*)


TheoryExpression["W->e\[Nu]"] := Sum[BrWlnu[1,j],{j,3}]


ExpValue$default["W->e\[Nu]"] := Around[0.1071,0.0016];


SMPrediction$default["W->e\[Nu]"] := Around[0.1082,null];


LowScale$default["W->e\[Nu]"] := \[Mu]EW


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]*)


TheoryExpression["W->\[Mu]\[Nu]"] := Sum[BrWlnu[2,j],{j,3}]


ExpValue$default["W->\[Mu]\[Nu]"] := Around[0.1063,0.0015];


SMPrediction$default["W->\[Mu]\[Nu]"] := Around[0.1082,null];


LowScale$default["W->\[Mu]\[Nu]"] := \[Mu]EW


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]*)


TheoryExpression["W->\[Tau]\[Nu]"] := Sum[BrWlnu[3,j],{j,3}]


ExpValue$default["W->\[Tau]\[Nu]"] := Around[0.1138,0.0021];


SMPrediction$default["W->\[Tau]\[Nu]"] := Around[0.1081,null];


LowScale$default["W->\[Tau]\[Nu]"] := \[Mu]EW


(* ::Section:: *)
(*Universality ratios *)


(* W->l_i \[Nu] / W->l_k \[Nu] *)
BrWlnuRatio[i_,k_] := Sum[WCL["gWlL",{i,j}]^2,{j,3}]/Sum[WCL["gWlL",{k,l}]^2,{l,3}]


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (CDF)*)


TheoryExpression["W->\[Mu]\[Nu]/W->e\[Nu]_CDF"] := BrWlnuRatio[2,1]


ExpValue$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDF"] := Around[0.982,0.024];


SMPrediction$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDF"] := Around[1.000,null];


LowScale$default["W->\[Mu]\[Nu]/W->e\[Nu]_CDF"] := \[Mu]EW


(* ::Subsection:: *)
(*W -> e\[Nu]/W -> \[Mu]\[Nu] (LHCb)*)


TheoryExpression["W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"] := BrWlnuRatio[1,2]


ExpValue$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"] := Around[1.020,0.019];


SMPrediction$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"] := Around[1.000,null];


LowScale$default["W->e\[Nu]/W->\[Mu]\[Nu]_LHCb"] := \[Mu]EW


(* ::Subsection:: *)
(*W -> \[Mu]\[Nu]/W -> e\[Nu] (ATLAS)*)


TheoryExpression["W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"] := BrWlnuRatio[2,1]


ExpValue$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"] := Around[1.003,0.010];


SMPrediction$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"] := Around[1.000,null];


LowScale$default["W->\[Mu]\[Nu]/W->e\[Nu]_ATLAS"] := \[Mu]EW


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> e\[Nu] (PDG, D0)*)


TheoryExpression["W->\[Tau]\[Nu]/W->e\[Nu]"] := BrWlnuRatio[3,1]


ExpValue$default["W->\[Tau]\[Nu]/W->e\[Nu]"] := Around[0.961,0.061];


SMPrediction$default["W->\[Tau]\[Nu]/W->e\[Nu]"] := Around[0.999,null];


LowScale$default["W->\[Tau]\[Nu]/W->e\[Nu]"] := \[Mu]EW


(* ::Subsection:: *)
(*W -> \[Tau]\[Nu]/W -> \[Mu]\[Nu] (ATLAS)*)


TheoryExpression["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"] := BrWlnuRatio[3,2]


ExpValue$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"] := Around[0.992,0.013];


SMPrediction$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"] := Around[0.999,null];


LowScale$default["W->\[Tau]\[Nu]/W->\[Mu]\[Nu]"] := \[Mu]EW


(* ::Section:: *)
(*RWc *)


TheoryExpression["RWc"] := (WCL["gWqL",{2,2}]^2+WCL["gWqR",{2,2}]^2)/Sum[WCL["gWqL",{i,i}]^2+WCL["gWqR",{i,i}]^2,{i,2}]


ExpValue$default["RWc"] := Around[0.49,0.04];


SMPrediction$default["RWc"] := Around[0.50,null];


LowScale$default["RWc"] := \[Mu]EW


(* ::Section:: *)
(*Exp Correlations *)


ExpCorrelation$default["W->e\[Nu]","W->\[Mu]\[Nu]"]:=0.136;
ExpCorrelation$default["W->e\[Nu]","W->\[Tau]\[Nu]"]:=-0.201;


ExpCorrelation$default["W->\[Mu]\[Nu]","W->\[Tau]\[Nu]"]:=-0.122;
