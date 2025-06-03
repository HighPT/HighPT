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


PackageExport["InputList"]
PackageExport["SetInputRedefinitions"]
PackageExport["GetInputRedefinitionMode"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["InputRedefinition"]


(*PackageScope["ParameterRedefinition"]
PackageScope["ParameterRedefinition$default"]
PackageScope["ParameterRedefinition$current"]*)


PackageScope["WolfensteinParametrization"]


PackageScope["InputRedefinition"]
PackageScope["InputRedefinition$default"]
PackageScope["InputRedefinition$current"]
PackageScope["SMValue"]
PackageScope["SMEFTValue"]
PackageScope["SMEFTValues"]
PackageScope["ParamsAsInputs"]
PackageScope["InputFunction"]
PackageScope["InputShift"]
PackageScope["ApplyRedefinitions"]


PackageScope["RedefinitionFlag"]


(* ::Chapter:: *)
(*Private:*)


(*Print["Defining input redefinitions..."]*)


RedefinitionFlag = 1;


SetInputRedefinitions[x_Integer] := Module[
	{},
	If[!MatchQ[x,0|1],Print["Please enter 0 or 1."];Abort[]];
    If[!MatchQ[x,RedefinitionFlag],
		RedefinitionFlag = x;
		ChangeObservable/@Flatten[ObservableList["Flavor"]];
		Print["Redefinition behaviour changed, all flavour observables updated."];
	];
]


GetInputRedefinitionMode[] := RedefinitionFlag


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


InputList$default = {Param["\[Alpha]EM"],Mass["ZBoson"],Param["GF"],Param["|Vus|"],Param["|Vcb|"],Param["|Vub|"],Param["\[Gamma]"]}


(*InputRedefinition[_]:=0*)


(* ::Subsection:: *)
(*GF*)


InputRedefinition$default[Param["GF"]] := Param["GF"]Param["vev"]^2 (WC["Hl3",{2,2}]+WC["Hl3",{1,1}]-1/2 WC["ll",{1,2,2,1}](*-1/2 WC["ll",{2,1,1,2}]*))(*/.GetParameters[]*);


(* ::Subsection::Closed:: *)
(*CKM*)


(* ::Subsubsection:: *)
(*Vus*)


InputRedefinition$default[Param["|Vus|"]] := Module[
	{
	BrKplus, BrKL,
	BrNPKplus, BrNPKL,
	BrNPKplusExpanded, BrNPKLExpanded
	}
	,
	BrKplus = TheoryExpression["K+->\[Pi]0e\[Nu]"]/.SubstitutePsi/.Around[a_,b_]:>a/.GetParameters[];
	(*Print["Extracted K+ BR"];*)
	BrNPKplus = 1/SMPrediction$default["K+->\[Pi]0e\[Nu]"]["Value"] ((BrKplus/.a_WCL:>(SMValue[a] + a + D[SMValue[a],Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (BrKplus/.a_WCL:>SMValue[a]))/.GetParameters[];
	(*Print["Extracted NP part"];*)
	BrNPKplusExpanded = Normal[Series[
			ComplexExpand[
				BrNPKplus/.WCL[lab_,ind_]:>eps*ReWCL[lab,ind]+I*eps*ImWCL[lab,ind]/.WC[lab_,ind_]:>eps*ReWC[lab,ind]+I*eps*ImWC[lab,ind]
			],
			{eps,0,1}
		]]/.eps->1/.ReWCL[lab_,ind_]:>Re[WCL[lab,ind]]/.ReWC[lab_,ind_]:>Re[WC[lab,ind]];
	(*Print["Finished expanding"];*)
	BrKL = TheoryExpression["KL->\[Pi]-e\[Nu]"]/.SubstitutePsi/.Around[a_,b_]:>a/.GetParameters[];
	BrNPKL = 1/SMPrediction$default["KL->\[Pi]-e\[Nu]"]["Value"] ((BrKL/.a_WCL:>(SMValue[a] + a + D[SMValue[a],Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (BrKL/.a_WCL:>SMValue[a]))/.GetParameters[];
	BrNPKLExpanded = Normal[Series[
			ComplexExpand[
				BrNPKL/.WCL[lab_,ind_]:>eps*ReWCL[lab,ind]+I*eps*ImWCL[lab,ind]/.WC[lab_,ind_]:>eps*ReWC[lab,ind]+I*eps*ImWC[lab,ind]
			],
			{eps,0,1}
		]]/.eps->1/.ReWCL[lab_,ind_]:>Re[WCL[lab,ind]]/.ReWC[lab_,ind_]:>Re[WC[lab,ind]];
	Return[1/2 Param["|Vus|"] 1/2 (BrNPKplusExpanded+BrNPKLExpanded)/.GetParameters[]/.Around[a_,b_]->a]
];


(* ::Subsubsection:: *)
(*Vcb*)


(*InputRedefinition$default[Param["|Vcb|"]] := 0;*)


InputRedefinition$default[Param["|Vcb|"]] := Module[
	{
	Br,
	BrNP,
	BrNPExpanded
	}
	,
	Br = TheoryExpression["B->Dl\[Nu]_iso"]/.SubstitutePsi/.Around[a_,b_]:>a/.GetParameters[];
	BrNP = 1/SMPrediction$default["B->Dl\[Nu]_iso"]["Value"] ((Br/.a_WCL:>(SMValue[a] + a + D[SMValue[a],Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (Br/.a_WCL:>SMValue[a]))/.GetParameters[];
	BrNPExpanded = Normal[Series[
			ComplexExpand[
				BrNP/.WCL[lab_,ind_]:>eps*ReWCL[lab,ind]+I*eps*ImWCL[lab,ind]/.WC[lab_,ind_]:>eps*ReWC[lab,ind]+I*eps*ImWC[lab,ind]
			],
			{eps,0,1}
		]]/.eps->1/.ReWCL[lab_,ind_]:>Re[WCL[lab,ind]]/.ReWC[lab_,ind_]:>Re[WC[lab,ind]];
	Return[1/2 Param["|Vcb|"](BrNPExpanded)/.GetParameters[]/.Around[a_,b_]->a]
];


(* ::Subsubsection:: *)
(*Vub*)


(*InputRedefinition$default[Param["|Vub|"]] := 0;*)


InputRedefinition$default[Param["|Vub|"]] := Module[
	{
	Br,
	BrNP,
	BrNPExpanded
	}
	,
	Br = TheoryExpression["B0->\[Pi]-l\[Nu]_high"]/.SubstitutePsi/.Around[a_,b_]:>a/.GetParameters[];
	BrNP = 1/SMPrediction$default["B0->\[Pi]-l\[Nu]_high"]["Value"] ((Br/.a_WCL:>(SMValue[a] + a + D[SMValue[a],Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (Br/.a_WCL:>SMValue[a]))/.GetParameters[];
	BrNPExpanded = Normal[Series[
			ComplexExpand[
				BrNP/.WCL[lab_,ind_]:>eps*ReWCL[lab,ind]+I*eps*ImWCL[lab,ind]/.WC[lab_,ind_]:>eps*ReWC[lab,ind]+I*eps*ImWC[lab,ind]
			],
			{eps,0,1}
		]]/.eps->1/.ReWCL[lab_,ind_]:>Re[WCL[lab,ind]]/.ReWC[lab_,ind_]:>Re[WC[lab,ind]]/.ImWCL[lab_,ind_]:>Im[WCL[lab,ind]]/.ImWC[lab_,ind_]:>Im[WC[lab,ind]];
	Return[1/2 Param["|Vub|"](BrNPExpanded)/.GetParameters[]/.Around[a_,b_]->a]
];


(* ::Subsubsection:: *)
(*\[Gamma]*)


InputRedefinition$default[Param["\[Gamma]"]] := 0;


(*LEFTLabels = {"\[Nu]eduVLL","\[Nu]eduVLR","\[Nu]eduSRR","\[Nu]eduSRL","\[Nu]eduTRR"};*)


(*r[_,_,_] := 0;
r[1,2,1]={1,7.4,1.7*10^-1,2*10^-2,5*10^-3,3.4*10^-1};
r[1,3,1]={1,1.7,6.7,3.9*10^-5,1.3*10^-3,7.7*10^-2};
r[1,3,2]={1,1.1,5.8,1.2,2.9,5*10^-2};
r[2,3,1]={1,5.3*10^-1,8.7*10^-1,5*10^-4,10^-3,2.4*10^-2};
r[2,3,2]={1,5.3*10^-1,8.7*10^-1,10^-1,2.2*10^-1,2.4*10^-2};*)


(*rho[Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},{i_,j_,l_}] := r[i,j,l][[1]];
rho[Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},{i_,j_,l_}] := r[i,j,l][[2]];
rho["\[Nu]eduTRR","\[Nu]eduTRR",{i_,j_,l_}] := r[i,j,l][[3]];
rho[Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},{i_,j_,l_}] := r[i,j,l][[4]];
rho[Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},{i_,j_,l_}] := r[i,j,l][[4]];
rho[Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},"\[Nu]eduTRR",{i_,j_,l_}] := r[i,j,l][[5]];
rho["\[Nu]eduTRR",Alternatives@@{"\[Nu]eduVLL","\[Nu]eduVLR"},{i_,j_,l_}] := r[i,j,l][[5]];
rho[Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},"\[Nu]eduTRR",{i_,j_,l_}] := r[i,j,l][[6]];
rho["\[Nu]eduTRR",Alternatives@@{"\[Nu]eduSRR","\[Nu]eduSRL"},{i_,j_,l_}] := r[i,j,l][[6]];*)


(*R[i_,j_,l_] := Sum[rho[\[Alpha],\[Beta],{i,j,l}]*WCL[\[Alpha],{l,l,j,i}]\[Conjugate]*WCL[\[Beta],{l,l,j,i}],{\[Alpha],LEFTLabels},{\[Beta],LEFTLabels}]/.WCL["\[Nu]eduVLL",a_]->1+WCL["\[Nu]eduVLL",a];*)


(*CKMRedefinition[Vus] := Vckm[1,2]*(Sqrt[R[1,2,1]]-1);
CKMRedefinition[Vub] := Vckm[1,3]*(Sqrt[1/2 (R[1,3,1]+R[1,3,2])]-1);
CKMRedefinition[Vcb] := Vckm[2,3]*(Sqrt[1/2 (R[2,3,1]+R[2,3,2])]-1);*)


(*\[Lambda] = Vus;
A = Vcb/Vus^2;
\[Rho] = Vub/(Vcb*Vus) Cos[\[Gamma]];
\[Eta] = Vub/(Vcb*Vus) Sin[\[Gamma]];*)


(*ParameterRedefinition$default[Param["Wolfenstein\[Lambda]"]] := Sum[D[\[Lambda],i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];*)


(*ParameterRedefinition$default[Param["WolfensteinA"]] := Sum[D[A,i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];*)


(*ParameterRedefinition$default[Param["Wolfenstein\[Rho]bar"]] := Sum[D[\[Rho],i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];*)


(*ParameterRedefinition$default[Param["Wolfenstein\[Eta]bar"]] := Sum[D[\[Eta],i]*CKMRedefinition[i],{i,{Vus,Vub,Vcb}}]/.Vus->Abs[Vckm[1,2]]/.Vcb->Abs[Vckm[2,3]]/.Vub->Abs[Vckm[1,3]]/.Cos[\[Gamma]]->Re[Vckm[1,3]]/Abs[Vckm[1,3]]/.Sin[\[Gamma]]->-(Im[Vckm[1,3]]/Abs[Vckm[1,3]])/.GetParameters[];*)


(* ::Subsection:: *)
(*\[Alpha]EM*)


InputRedefinition$default[Param["\[Alpha]EM"]] := Simplify[SMEFTValue[Param["\[Alpha]EM"]]-(SMEFTValue[Param["\[Alpha]EM"]]/._WC->0)]


(* ::Subsection:: *)
(*mZ*)


(*InputRedefinition$default[Mass["ZBoson"]] := Mass["ZBoson"]/2 Param["vev"]^2 (1/2 WC["HD",{}]+Sqrt[4\[Pi] Param["\[Alpha]EM"]] Param["vev"]/Mass["ZBoson"] WC["HWB",{}]);*)


InputRedefinition$default[Mass["ZBoson"]] := SMEFTValue[Mass["ZBoson"]]-(SMEFTValue[Mass["ZBoson"]]/._WC->0)


(* ::Section:: *)
(*Initialize - to redo*)


InputRedefinition[x_] := InputRedefinition$default[x]


(* ::Section:: *)
(*Expressing all other parameters in terms of inputs (default)*)


InputFunction[x_] := x


InputFunction[Param["sW"]]    := Sqrt[1/2 (1-Sqrt[1-(4\[Pi] Param["\[Alpha]EM"])/(Sqrt[2]Param["GF"]Mass["ZBoson"]^2)])]
InputFunction[Param["cW"]]    := Sqrt[1-InputFunction[Param["sW"]]^2]
InputFunction[Param["g1"]]    := Sqrt[4\[Pi] Param["\[Alpha]EM"]]/Sqrt[1-InputFunction[Param["sW"]]^2]
InputFunction[Param["g2"]]    := Sqrt[4\[Pi] Param["\[Alpha]EM"]]/InputFunction[Param["sW"]]
InputFunction[Param["gZ"]]    := InputFunction[Param["g2"]]/InputFunction[Param["cW"]]
InputFunction[Param["vev"]]   := 1/Sqrt[Sqrt[2]Param["GF"]]
InputFunction[Mass["WBoson"]] := Mass["ZBoson"]Sqrt[1-InputFunction[Param["sW"]]^2]
InputFunction[x_Vckm]         := x/.WolfensteinParametrization[WolfensteinExtract[Param["|Vus|"],Param["|Vcb|"],Param["|Vub|"],Param["\[Gamma]"]]/.List->Sequence]//Simplify


ParamsAsInputs[expr_] := Module[
{derived,rep}
,
derived = {Param["cW"],Param["sW"],Param["g1"],Param["g2"],Param["g3"],Param["gZ"],Param["vev"],Mass["WBoson"],Table[Vckm[i,j],{i,3},{j,3}]}//Flatten;
rep=Table[i->InputFunction[i],{i,derived}];
Return[expr/.rep]
]


(* ::Section:: *)
(*User - defined redefinitions - to do*)


InputList=InputList$default;


(*$InputParameters={
	Param["GF"],
	Param["Wolfenstein\[Lambda]"],
	Param["WolfensteinA"],
	Param["Wolfenstein\[Rho]bar"],
	Param["Wolfenstein\[Eta]bar"]
};*)


(*Options[DefineRedefinitions] = {
	"GF" :> ParameterRedefinition$current[Param["GF"]],
	"\[Lambda]"  :> ParameterRedefinition$current[Param["Wolfenstein\[Lambda]"]],
	"A"  :> ParameterRedefinition$current[Param["WolfensteinA"]],
	"\[Rho]"  :> ParameterRedefinition$current[Param["Wolfenstein\[Rho]bar"]],
	"\[Eta]"  :> ParameterRedefinition$current[Param["Wolfenstein\[Eta]bar"]]
};*)


(*DefineRedefinitions[Default] := DefineRedefinitions[
	"GF" -> ParameterRedefinition$default[Param["GF"]],
	"\[Lambda]"  -> ParameterRedefinition$default[Param["Wolfenstein\[Lambda]"]],
	"A"  -> ParameterRedefinition$default[Param["WolfensteinA"]],
	"\[Rho]"  -> ParameterRedefinition$default[Param["Wolfenstein\[Rho]bar"]],
	"\[Eta]"  -> ParameterRedefinition$default[Param["Wolfenstein\[Eta]bar"]]
]*)


(*DefineRedefinitions[OptionsPattern[]] := Module[
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
]*)


(* ::Section:: *)
(*Standard Model values for (LEFT) Wilson Coefficients*)


C10SM  =-4.18869


SMValue[x_WCL] := 0


SMValue[WCL["edVLL",{\[Alpha]_,\[Beta]_,i_,j_}]] := 1/(Sqrt[2]\[Pi])Param["\[Alpha]EM"] Param["GF"]*Vckm[3,i]\[Conjugate]Vckm[3,j] KroneckerDelta[\[Alpha],\[Beta]] C10SM;


SMValue[WCL["\[Nu]eduVLL",{\[Alpha]_,\[Beta]_,i_,j_}]] := -2*Sqrt[2]Param["GF"]KroneckerDelta[\[Alpha],\[Beta]]Vckm[j,i]\[Conjugate]


SMValue[WCL["gZeL",{i_,j_}]] := -Param["gZ"](*(Param["g2"]/Param["cW"])*)(-(1/2)+Param["sW"]^2)KroneckerDelta[i,j]
SMValue[WCL["gZeR",{i_,j_}]] := -Param["gZ"](Param["sW"]^2)KroneckerDelta[i,j]
SMValue[WCL["gZ\[Nu]L",{i_,j_}]] := -Param["gZ"](+(1/2))KroneckerDelta[i,j]
SMValue[WCL["gZdL",{i_,j_}]] := -Param["gZ"](-(1/2)+1/3 Param["sW"]^2)KroneckerDelta[i,j]
SMValue[WCL["gZdR",{i_,j_}]] := -Param["gZ"](1/3 Param["sW"]^2)KroneckerDelta[i,j]
SMValue[WCL["gZuL",{i_,j_}]] := -Param["gZ"](+(1/2)-2/3 Param["sW"]^2)KroneckerDelta[i,j]
SMValue[WCL["gZuR",{i_,j_}]] := -Param["gZ"](-(2/3) Param["sW"]^2)KroneckerDelta[i,j]


SMValue[WCL["mW",{}]] := 1/2 Param["g2"]Param["vev"]
SMValue[WCL["gWqL",{i_,j_}]] := -(Param["g2"]/Sqrt[2])Vckm[i,j]
SMValue[WCL["gWlL",{i_,j_}]] := -(Param["g2"]/Sqrt[2])KroneckerDelta[i,j]


(* ::Section:: *)
(*SMEFT values of parameters*)


SMEFTValues[expr_] := (Series[expr/.a_Param:>SMEFTValue[a](*/.b_Mass->SMEFTValue[b]*)/.c_WC:>eps*c,{eps,0,1}]//Normal)/.eps->1


SMEFTValue[x_] := x


SMEFTValue[x_WCL] := (Series[SMValue[x]/.a_Param:>SMEFTValue[a]/.b_WC:>eps*b,{eps,0,1}]//Normal)/.eps->1


(* ::Subsection:: *)
(*Higgs mass, vev, self-coupling*)


CHkin = Param["vev"]^2 (WC["HBox",{}]-1/4 WC["HD",{}])


(*SMEFTValue[Param["\[Lambda]"]] := *)
(*SMEFTValue[Param["vev"]] := Param["vev"](1+(3 Param["vev"]^2)/(8 Param["\[Lambda]"])WC["H",{}])
SMEFTValue[Mass["H"]] := 2 Param["\[Lambda]"] Param["vev"]^2(1 - (3 Param["vev"]^2)/(2 Param["\[Lambda]"])WC["H",{}]+2*CHkin)*)


(* ::Subsection:: *)
(*Yukawas - to do*)


(* ::Subsection:: *)
(*Gauge couplings*)


(*SMEFTValue[Param["g1"]] := Param["g1"](1+Param["vev"]^2WC["HB",{}])
SMEFTValue[Param["g2"]] := Param["g2"](1+Param["vev"]^2WC["HW",{}])
SMEFTValue[Param["g3"]] := Param["g3"](1+Param["vev"]^2WC["HG",{}])*)


SMEFTValue[Param["\[Alpha]EM"]] := (Param["g1"]^2 Param["g2"]^2)/(4 \[Pi] (Param["g1"]^2+Param["g2"]^2)) (1-2 (Param["g1"] Param["g2"] Param["vev"]^2 WC["HWB",{}])/( Param["g1"]^2+Param["g2"]^2))


SMEFTValue[Param["gZ"]] := Sqrt[Param["g2"]^2+Param["g1"]^2](1+(Param["g1"]Param["g2"]Param["vev"]^2)/(Param["g1"]^2+Param["g2"]^2) WC["HWB",{}])


(* ::Subsection:: *)
(*Mixing angle*)


SMEFTValue[Param["sW"]] := Param["g1"]/Sqrt[Param["g2"]^2+Param["g1"]^2] (1+Param["vev"]^2/2 Param["g2"]/Param["g1"] (Param["g2"]^2-Param["g1"]^2)/(Param["g2"]^2+Param["g1"]^2) WC["HWB",{}])(*//SMEFTValues*)
SMEFTValue[Param["cW"]] := (Param["g2"]/Sqrt[Param["g2"]^2+Param["g1"]^2]) (1+Param["vev"]^2/2 Param["g2"]/Param["g1"] (Param["g2"]^2-Param["g1"]^2)/(Param["g2"]^2+Param["g1"]^2) WC["HWB",{}])(*//SMEFTValues*)


(* ::Subsection:: *)
(*Gauge boson masses*)


SMEFTValue[Mass["WBoson"]] := (1/2) Param["g2"]Param["vev"](*//SMEFTValues*)


SMEFTValue[Mass["ZBoson"]] := (Series[Sqrt[1/4 Param["vev"]^2 (Param["g1"]^2+Param["g2"]^2) + 1/8 Param["vev"]^4 WC["HD",{}](Param["g1"]^2+Param["g2"]^2) + 1/2 Param["vev"]^4 Param["g1"]Param["g2"]WC["HWB",{}]]/.a_WC:>eps*a,{eps,0,1}]//Normal)/.eps->1(*//SMEFTValues*)


(* ::Section:: *)
(*Redefine Inputs in generic expressions*)


InputShift[x_WCL] := Sum[(D[SMValue[x]//ParamsAsInputs,i]/.GetParameters[])*(InputRedefinition[i]/.GetParameters[]),{i,InputList}]


(*InputRedefinition[obs_]:=-(1/InputDependence[obs])Sum[(D[InputDependence[obs]/.WolfensteinParametrization[Param["Wolfenstein\[Lambda]"],Param["WolfensteinA"],Param["Wolfenstein\[Rho]bar"],Param["Wolfenstein\[Eta]bar"]],i])*ParameterRedefinition[i],{i,$InputParameters}]/.GetParameters[];*)


ApplyRedefinitions[expr_] := expr/.a_WCL:>(a+(SMEFTValue[a]-(SMEFTValue[a]/._WC->0))+InputShift[a]*RedefinitionFlag)/.GetParameters[]


(*Print["DONE!"]*)
