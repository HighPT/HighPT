(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`InputRedefinition`*)


(* ::Subtitle:: *)
(*Implementation of the input redefinitions*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["DefineRedefinitions"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["InputRedefinition"]


PackageScope["ParameterRedefinition"]
PackageScope["ParameterRedefinition$default"]
PackageScope["ParameterRedefinition$current"]


PackageScope["WolfensteinParametrization"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Wolfenstein parametrization*)


WolfensteinParametrization[\[Lambda]_,A_,\[Rho]_,\[Eta]_]:={
	Vckm[1,1] -> 1-\[Lambda]^2/2-\[Lambda]^4/8,
	Vckm[1,2] -> \[Lambda],
	Vckm[1,3] -> A*\[Lambda]^3 (1+1/2 \[Lambda]^2)(\[Rho]-I*\[Eta]),
	Vckm[2,1] -> -\[Lambda]+A^2 \[Lambda]^5 (1/2-\[Rho]-I*\[Eta]),
	Vckm[2,2] -> 1-\[Lambda]^2/2-\[Lambda]^4/8 (1+4A^2),
	Vckm[2,3] -> A*\[Lambda]^2,
	Vckm[3,1] -> A*\[Lambda]^3 (1-\[Rho]-I*\[Eta]),
	Vckm[3,2] -> -A*\[Lambda]^2+A*\[Lambda]^4 (1/2-\[Rho]-I*\[Eta]),
	Vckm[3,3] -> 1-1/2 A^2 \[Lambda]^4
};


(* ::Section:: *)
(*Default redefinition of individual parameters*)


ParameterRedefinition[_]:=0


(* ::Subsection:: *)
(*GF*)


ParameterRedefinition$default[Param["GF"]] := Param["GF"]Param["vev"]^2 (WC["Hl3",{2,2}]+WC["Hl3",{1,1}]-1/2 WC["ll",{1,2,2,1}]-1/2 WC["ll",{2,1,1,2}]);


(* ::Subsection:: *)
(*CKM*)


LEFTLabels = {"\[Nu]eduVLL","\[Nu]eduVLR","\[Nu]eduSRR","\[Nu]eduSRL","\[Nu]eduTRR"};


r[_,_,_] := 0;
r[1,2,1]={1,7.4,1.7*10^-1,2*10^-2,5*10^-3,3.4*10^-1};
r[1,3,1]={1,1.7,6.7,3.9*10^-5,1.3*10^-3,7.7*10^-2};
r[1,3,2]={1,1.1,5.8,1.2,2.9,5*10^-2};
r[2,3,1]={1,5.3*10^-1,8.7*10^-1,5*10^-4,10^-3,2.4*10^-2};
r[2,3,2]={1,5.3*10^-1,8.7*10^-1,10^-1,2.2*10^-1,2.4*10^-2};


rho[Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},{i_,j_,l_}] := r[i,j,l][[1]];
rho[Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},{i_,j_,l_}] := r[i,j,l][[2]];
rho["\[Nu]eduTRR","\[Nu]eduTRR",{i_,j_,l_}] := r[i,j,l][[3]];
rho[Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},{i_,j_,l_}] := r[i,j,l][[4]];
rho[Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},{i_,j_,l_}] := r[i,j,l][[4]];
rho[Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},"\[Nu]eduTRR",{i_,j_,l_}] := r[i,j,l][[5]];
rho["\[Nu]eduTRR",Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},{i_,j_,l_}] := r[i,j,l][[5]];
rho[Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},"\[Nu]eduTRR",{i_,j_,l_}] := r[i,j,l][[6]];
rho["\[Nu]eduTRR",Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},{i_,j_,l_}] := r[i,j,l][[6]];


R[i_,j_,l_] := Sum[rho[\[Alpha],\[Beta],{i,j,l}]*WCL[\[Alpha],{l,l,j,i}]\[Conjugate]*WCL[\[Beta],{l,l,j,i}],{\[Alpha],LEFTLabels},{\[Beta],LEFTLabels}]/.WCL["\[Nu]eduVLL",a_]->1+WCL["\[Nu]eduVLL",a];


CKMRedefinition[Vus] := Vckm[1,2]*(Sqrt[R[1,2,1]]-1);
CKMRedefinition[Vub] := Vckm[1,3]*(Sqrt[1/2 (R[1,3,1]+R[1,3,2])]-1);
CKMRedefinition[Vcb] := Vckm[2,3]*(Sqrt[1/2 (R[2,3,1]+R[2,3,2])]-1);


\[Lambda] = Vus;
A = Vcb/Vus^2;
\[Rho] = Vub/(Vcb*Vus) Cos[\[Gamma]];
\[Eta] = Vub/(Vcb*Vus) Sin[\[Gamma]];


ParameterRedefinition$default[Param["Wolfenstein\[Lambda]"]] := Sum[D[\[Lambda],i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];


ParameterRedefinition$default[Param["WolfensteinA"]] := Sum[D[A,i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];


ParameterRedefinition$default[Param["Wolfenstein\[Rho]bar"]] := Sum[D[\[Rho],i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];


ParameterRedefinition$default[Param["Wolfenstein\[Eta]bar"]] := Sum[D[\[Eta],i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];


(* ::Section:: *)
(*User - defined redefinitions*)


$InputParameters={
	Param["GF"],
	Param["Wolfenstein\[Lambda]"],
	Param["WolfensteinA"],
	Param["Wolfenstein\[Rho]bar"],
	Param["Wolfenstein\[Eta]bar"]
};


Options[DefineRedefinitions] = {
	"GF" :> ParameterRedefinition$current[Param["GF"]],
	"\[Lambda]"  :> ParameterRedefinition$current[Param["Wolfenstein\[Lambda]"]],
	"A"  :> ParameterRedefinition$current[Param["WolfensteinA"]],
	"\[Rho]"  :> ParameterRedefinition$current[Param["Wolfenstein\[Rho]bar"]],
	"\[Eta]"  :> ParameterRedefinition$current[Param["Wolfenstein\[Eta]bar"]]
};


DefineRedefinitions[Default] := DefineRedefinitions[
	"GF" -> ParameterRedefinition$default[Param["GF"]],
	"\[Lambda]"  -> ParameterRedefinition$default[Param["Wolfenstein\[Lambda]"]],
	"A"  -> ParameterRedefinition$default[Param["WolfensteinA"]],
	"\[Rho]"  -> ParameterRedefinition$default[Param["Wolfenstein\[Rho]bar"]],
	"\[Eta]"  -> ParameterRedefinition$default[Param["Wolfenstein\[Eta]bar"]]
]


DefineRedefinitions[OptionsPattern[]] := Module[
	{
	$GF = OptionValue["GF"],
	$\[Lambda]  = OptionValue["\[Lambda]"], 
	$A  = OptionValue["A"],
	$\[Rho]  = OptionValue["\[Rho]"], 
	$\[Eta]  = OptionValue["\[Eta]"]
	}
	,
	ParameterRedefinition$current[Param["GF"]]              = If[MatchQ[$GF, Default], $GF = ParameterRedefinition$default[Param["GF"]], $GF];
	ParameterRedefinition$current[Param["Wolfenstein\[Lambda]"]]    = If[MatchQ[$\[Lambda], Default], $\[Lambda] = ParameterRedefinition$default[Param["Wolfenstein\[Lambda]"]], $\[Lambda]];
	ParameterRedefinition$current[Param["WolfensteinA"]]    = If[MatchQ[$A, Default], $A = ParameterRedefinition$default[Param["WolfensteinA"]], $A];
	ParameterRedefinition$current[Param["Wolfenstein\[Rho]bar"]] = If[MatchQ[$\[Rho], Default], $\[Rho] = ParameterRedefinition$default[Param["Wolfenstein\[Rho]bar"]], $\[Rho]];
	ParameterRedefinition$current[Param["Wolfenstein\[Eta]bar"]] = If[MatchQ[$\[Eta], Default], $\[Eta] = ParameterRedefinition$default[Param["Wolfenstein\[Eta]bar"]], $\[Eta]];
	Table[ParameterRedefinition[i_] := ParameterRedefinition$current[i], {i,$InputParameters}];
]


(* ::Section:: *)
(*Input Redefinition*)


InputRedefinition[obs_]:=-(1/InputDependence[obs])Sum[(D[InputDependence[obs]/.WolfensteinParametrization[Param["Wolfenstein\[Lambda]"],Param["WolfensteinA"],Param["Wolfenstein\[Rho]bar"],Param["Wolfenstein\[Eta]bar"]],i])*ParameterRedefinition[i],{i,$InputParameters}]/.GetParameters[];
