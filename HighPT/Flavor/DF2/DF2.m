(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`DF2`*)


(* ::Subtitle:: *)
(*Implementation of \[CapitalDelta]F=2 observables*)


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


$\[CapitalDelta]F2Sectors={(*"sd","cu","bd",*)"bsmixing"};


FlavorObservables["\[CapitalDelta]F=2"] = FlavorObservables/@$\[CapitalDelta]F2Sectors


ObsTable["\[CapitalDelta]F=2"] := Grid[{{"\[CapitalDelta]F=2",Column[ObsTable/@$\[CapitalDelta]F2Sectors]}},Dividers->All];


(* ::Section:: *)
(*Basis change (SUSY to LEFT)*)


SUSYToLEFT={
wCL["1",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)WCL["ddVLL",{i,j,i,j}],
wCL["2",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)(WCL["ddS1RR",{j,i,j,i}]\[Conjugate]-(1/6)WCL["ddS8RR",{j,i,j,i}]\[Conjugate]),
wCL["3",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)(1/2 WCL["ddS8RR",{j,i,j,i}]\[Conjugate]),
wCL["4",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)(-WCL["ddV8LR",{i,j,i,j}]),
wCL["5",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)(-2*WCL["ddV1LR",{i,j,i,j}]+1/3 WCL["ddV8LR",{i,j,i,j}]),
wCL["1p",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)WCL["ddVRR",{i,j,i,j}],
wCL["2p",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)(WCL["ddS1RR",{i,j,i,j}]-(1/6)WCL["ddS8RR",{i,j,i,j}]),
wCL["3p",{i_,j_,i_,j_}]:>(*((4\[Pi]^2)/(Param["GF"]Mass["WBosom"])^21/(Vckm[3,j]Vckm[3,i]\[Conjugate])^2)**)(1/2 WCL["ddS8RR",{i,j,i,j}])
}


(* ::Section:: *)
(*Inputs for matrix elements*)


(*DecayConstant["Bs"] := Around[0.2303,0.0013];*)


BagParameter["Bs"] := {
	Around[0.813,0.035],
	Around[0.817,0.043],
	Around[0.816,0.057],
	Around[1.033,0.047],
	Around[0.941,0.038]
}


DownQuarkMasses={Mass["d"],Mass["s"],Mass["b"]};


r\[Chi][m_][i_,j_]:=(Mass[m]/(DownQuarkMasses[[i]]+DownQuarkMasses[[j]]))^2
NN = {-(5/12),1/12,1/2,1/6};
dbq = {0,0,1/6,3/2};


(* ::Section:: *)
(*K*)


(* ::Section:: *)
(*D*)


(* ::Section:: *)
(*Bd*)


(* ::Section:: *)
(*Bs*)


FlavorObservables["bsmixing"]={"\[CapitalDelta]MBs"};


ObsTable["bsmixing"] := Grid[{{"bsmixing",Column[FlavorObservables["bsmixing"]]}},Dividers->All];


LowScale["\[CapitalDelta]MBs"] := Mass["Bs"]/.GetParameters[]


Bscoeff={
	wCL["1",{3,2,3,2}]+wCL["1p",{3,2,3,2}],
	wCL["2",{3,2,3,2}]+wCL["2p",{3,2,3,2}],
	wCL["3",{3,2,3,2}]+wCL["3p",{3,2,3,2}],
	wCL["4",{3,2,3,2}],
	wCL["5",{3,2,3,2}]
	};


M12bs = 1/(2 Mass["Bs"])*(2/3 Mass["Bs"]^2 DecayConstant["Bs"]^2 BagParameter["Bs"][[1]]*Bscoeff[[1]]+Sum[NN[[a-1]]*(r\[Chi]["Bs"][3,2]+dbq[[a-1]])*Mass["Bs"]^2 DecayConstant["Bs"]^2 BagParameter["Bs"][[1]]*Bscoeff[[a]],{a,2,5}])


S0[x_] := (x(4-11x+x^2))/(4(x-1)^2)+(3x^3 Log[x])/(2(x-1)^3)


ExpValue$default["\[CapitalDelta]MBs"] := Around[1.1683,0.0013]*10^-11;


NumericalInput["\[CapitalDelta]MBs"] := (Param["GF"]Mass["WBoson"])^2/(4\[Pi]^2)*(LEFTRun[M12bs/.SUSYToLEFT/.GetParameters[Errors->True],Mass["Bs"]/.GetParameters[],DsixTools`EWSCALE]/.WCL["ddVLL",{3,2,3,2}]->S0[Mass["t"]^2/Mass["WBoson"]^2]/._WCL->0)/.GetParameters[Errors->True]
InputDependence["\[CapitalDelta]MBs"] := (Vckm[3,2]Vckm[3,3]\[Conjugate])^2


SMPrediction$default["\[CapitalDelta]MBs"] := 2*Re[NumericalInput["\[CapitalDelta]MBs"]*InputDependence["\[CapitalDelta]MBs"]]/.GetParameters[Errors->True]//Simplify


NPContribution$default["\[CapitalDelta]MBs"] := 1/SMPrediction$default["\[CapitalDelta]MBs"]*2Re[M12bs]/.SUSYToLEFT/.GetParameters[]/.Around[a_,b_]->a
