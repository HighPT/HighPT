(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`DsixToolsInterface`*)


(* ::Subtitle:: *)
(*Match the HighPT notation to the DsixTools one*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["DsixToolsToHighPTSMEFT"]
PackageScope["DsixToolsToHighPTLEFT"]
PackageScope["DsixToolsToHighPTSM"]


PackageScope["HighPTToDsixToolsSMEFT"]
PackageScope["HighPTToDsixToolsLEFT"]


PackageScope["SOLDToHighPT"]
PackageScope["HighPTToSOLD"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*SMEFT Map*)


(* ::Subsection::Closed:: *)
(*DsixTools*)


SMEFTMap = {
	(* H^6 *)
	{"H",DsixTools`CH},
	(* H^4 D^2 *)
	{"HD",DsixTools`CHD},
	{"HBox",DsixTools`CHbox},
	(* H^2 X^2 *)
	{"HG",DsixTools`CHG},
	{"HGt",DsixTools`CHGtilde},
	{"HW",DsixTools`CHW},
	{"HWt",DsixTools`CHWtilde},
	{"HB",DsixTools`CHB},
	{"HBt",DsixTools`CHBtilde},
	{"HWB",DsixTools`CHWB},
	{"HWtB",DsixTools`CHWtildeB},
	(* X^3 *)
	{"G",DsixTools`CG},
	{"Gt",DsixTools`CGtilde},
	{"W",DsixTools`CW},
	{"Wt",DsixTools`CWtilde},
	(* Psi^2 H^3 *)
	{"uH",DsixTools`CuH},
	{"dH",DsixTools`CdH},
	{"eH",DsixTools`CeH},
	(* Psi^2 X H *)
	{"eW",DsixTools`CeW},
	{"eB",DsixTools`CeB},
	{"uG",DsixTools`CuG},
	{"uW",DsixTools`CuW},
	{"uB",DsixTools`CuB},
	{"dG",DsixTools`CdG},
	{"dW",DsixTools`CdW},
	{"dB",DsixTools`CdB},
	(* Psi^2 H^2 D *)
	{"Hl1",DsixTools`CHl1},
	{"Hl3",DsixTools`CHl3},
	{"He",DsixTools`CHe},
	{"Hq1",DsixTools`CHq1},
	{"Hq3",DsixTools`CHq3},
	{"Hu",DsixTools`CHu},
	{"Hd",DsixTools`CHd},
	{"Hud",DsixTools`CHud},
	(* Psi^4 *)
	{"lq1",DsixTools`Clq1},
	{"lq3",DsixTools`Clq3},
	{"eu",DsixTools`Ceu},
	{"ed",DsixTools`Ced},
	{"lu",DsixTools`Clu},
	{"ld",DsixTools`Cld},
	{"eq",DsixTools`Cqe},
	{"ledq",DsixTools`Cledq},
	{"lequ1",DsixTools`Clequ1},
	{"lequ3",DsixTools`Clequ3},
	{"qq1",DsixTools`Cqq1},
	{"qq3",DsixTools`Cqq3},
	{"uu",DsixTools`Cuu},
	{"dd",DsixTools`Cdd},
	{"ud1",DsixTools`Cud1},
	{"ud8",DsixTools`Cud8},
	{"qu1",DsixTools`Cqu1},
	{"qu8",DsixTools`Cqu8},
	{"qd1",DsixTools`Cqd1},
	{"qd8",DsixTools`Cqd8},
	{"quqd1",DsixTools`Cquqd1},
	{"quqd8",DsixTools`Cquqd8},
	{"ll",DsixTools`Cll},
	{"ee",DsixTools`Cee},
	{"le",DsixTools`Cle}
};


(* ::Subsection:: *)
(*SOLD*)


SMEFTMapSOLD = {
	(* H^6 *)
	{"H",SOLD`alphaOH},
	(* H^4 D^2 *)
	{"HD",SOLD`alphaOHD},
	{"HBox",SOLD`alphaOHBox},
	(* H^2 X^2 *)
	{"HG",SOLD`alphaOHG},
	{"HGt",SOLD`alphaOHGt},
	{"HW",SOLD`alphaOHW},
	{"HWt",SOLD`alphaOHWt},
	{"HB",SOLD`alphaOHB},
	{"HBt",SOLD`alphaOHBt},
	{"HWB",SOLD`alphaOHWB},
	{"HWtB",SOLD`alphaOHWBt},
	(* X^3 *)
	{"G",SOLD`alphaO3G},
	{"Gt",SOLD`alphaO3Gt},
	{"W",SOLD`alphaO3W},
	{"Wt",SOLD`alphaO3Wt},
	(* Psi^2 H^3 *)
	{"uH",SOLD`alphaOuH},
	{"dH",SOLD`alphaOdH},
	{"eH",SOLD`alphaOeH},
	(* Psi^2 X H *)
	{"eW",SOLD`alphaOeW},
	{"eB",SOLD`alphaOeB},
	{"uG",SOLD`alphaOuG},
	{"uW",SOLD`alphaOuW},
	{"uB",SOLD`alphaOuB},
	{"dG",SOLD`alphaOdG},
	{"dW",SOLD`alphaOdW},
	{"dB",SOLD`alphaOdB},
	(* Psi^2 H^2 D *)
	{"Hl1",SOLD`alphaOHl1},
	{"Hl3",SOLD`alphaOHl3},
	{"He",SOLD`alphaOHe},
	{"Hq1",SOLD`alphaOHq1},
	{"Hq3",SOLD`alphaOHq3},
	{"Hu",SOLD`alphaOHu},
	{"Hd",SOLD`alphaOHd},
	{"Hud",SOLD`alphaOHud},
	(* Psi^4 *)
	{"lq1",SOLD`alphaOlq1},
	{"lq3",SOLD`alphaOlq3},
	{"eu",SOLD`alphaOeu},
	{"ed",SOLD`alphaOed},
	{"lu",SOLD`alphaOlu},
	{"ld",SOLD`alphaOld},
	{"eq",SOLD`alphaOqe},
	{"ledq",SOLD`alphaOledq},
	{"lequ1",SOLD`alphaOlequ1},
	{"lequ3",SOLD`alphaClequ3},
	{"qq1",SOLD`alphaOqq1},
	{"qq3",SOLD`alphaOqq3},
	{"uu",SOLD`alphaOuu},
	{"dd",SOLD`alphaOdd},
	{"ud1",SOLD`alphaOud1},
	{"ud8",SOLD`alphaOud8},
	{"qu1",SOLD`alphaOqu1},
	{"qu8",SOLD`alphaOqu8},
	{"qd1",SOLD`alphaOqd1},
	{"qd8",SOLD`alphaOqd8},
	{"quqd1",SOLD`alphaOquqd1},
	{"quqd8",SOLD`alphaOquqd8},
	{"ll",SOLD`alphaOll},
	{"ee",SOLD`alphaOee},
	{"le",SOLD`alphaOle}
};


(* ::Section::Closed:: *)
(*LEFT Map*)


LEFTMap = {
	{"G",DsixTools`LG},
	{"Gt",DsixTools`LGtilde},
	(*{"\[Nu]",DsixTools`L},*)
	{"\[Nu]\[Gamma]",DsixTools`L\[Nu]\[Gamma]},
	{"e\[Gamma]",DsixTools`Le\[Gamma]},
	{"u\[Gamma]",DsixTools`Lu\[Gamma]},
	{"d\[Gamma]",DsixTools`Ld\[Gamma]},
	{"uG",DsixTools`LuG},
	{"dG",DsixTools`LdG},
	{"\[Nu]\[Nu]VLL",DsixTools`L\[Nu]\[Nu]VLL},
	{"eeVLL",DsixTools`LeeVLL},
	{"\[Nu]eVLL",DsixTools`L\[Nu]eVLL},
	{"\[Nu]uVLL",DsixTools`L\[Nu]uVLL},
	{"\[Nu]dVLL",DsixTools`L\[Nu]dVLL},
	{"euVLL",DsixTools`LeuVLL},
	{"edVLL",DsixTools`LedVLL},
	{"\[Nu]eduVLL",DsixTools`L\[Nu]eduVLL},
	{"uuVLL",DsixTools`LuuVLL},
	{"ddVLL",DsixTools`LddVLL},
	{"udV1LL",DsixTools`LudV1LL},
	{"udV8LL",DsixTools`LudV8LL},
	{"eeVRR",DsixTools`LeeVRR},
	{"euVRR",DsixTools`LeuVRR},
	{"edVRR",DsixTools`LedVRR},
	{"uuVRR",DsixTools`LuuVRR},
	{"ddVRR",DsixTools`LddVRR},
	{"udV1RR",DsixTools`LudV1RR},
	{"udV8RR",DsixTools`LudV8RR},
	{"\[Nu]eVLR",DsixTools`L\[Nu]eVLR},
	{"eeVLR",DsixTools`LeeVLR},
	{"\[Nu]uVLR",DsixTools`L\[Nu]uVLR},
	{"\[Nu]dVLR",DsixTools`L\[Nu]dVLR},
	{"euVLR",DsixTools`LeuVLR},
	{"edVLR",DsixTools`LedVLR},
	{"ueVLR",DsixTools`LueVLR},
	{"deVLR",DsixTools`LdeVLR},
	{"\[Nu]eduVLR",DsixTools`L\[Nu]eduVLR},
	{"uuV1LR",DsixTools`LuuV1LR},
	{"uuV8LR",DsixTools`LuuV8LR},
	{"udV1LR",DsixTools`LudV1LR},
	{"udV8LR",DsixTools`LudV8LR},
	{"duV1LR",DsixTools`LduV1LR},
	{"duV8LR",DsixTools`LduV8LR},
	{"ddV1LR",DsixTools`LddV1LR},
	{"ddV8LR",DsixTools`LddV8LR},
	{"udduV1LR",DsixTools`LudduV1LR},
	{"udduV8LR",DsixTools`LudduV8LR},
	{"eeSRR",DsixTools`LeeSRR},
	{"euSRR",DsixTools`LeuSRR},
	{"euTRR",DsixTools`LeuTRR},
	{"edSRR",DsixTools`LedSRR},
	{"edTRR",DsixTools`LedTRR},
	{"\[Nu]eduSRR",DsixTools`L\[Nu]eduSRR},
	{"\[Nu]eduTRR",DsixTools`L\[Nu]eduTRR},
	{"uuS1RR",DsixTools`LuuS1RR},
	{"uuS8RR",DsixTools`LuuS8RR},
	{"udS1RR",DsixTools`LudS1RR},
	{"udS8RR",DsixTools`LudS8RR},
	{"ddS1RR",DsixTools`LddS1RR},
	{"ddS8RR",DsixTools`LddS8RR},
	{"udduS1RR",DsixTools`LudduS1RR},
	{"udduS8RR",DsixTools`LudduS8RR},
	{"euSRL",DsixTools`LeuSRL},
	{"edSRL",DsixTools`LedSRL},
	{"\[Nu]eduSRL",DsixTools`L\[Nu]eduSRL}
};


(* ::Section:: *)
(*DsixTools -> HighPT*)


(* ::Subsection::Closed:: *)
(*SMEFT*)


DsixToolsToHighPTSMEFTAss = <| (#1[[2]]->#1[[1]])&/@SMEFTMap |>


(*DsixToolsToHighPTSMEFT::missingcoefficient = "The mapping  of `1` to the HighPT notation is not known"(*. The coefficient will be set to zero"*);*)


(*DsixToolsToHighPTSMEFT::nocoefficients = "No SMEFT coefficients found.";*)


(*DsixToolsToHighPTSMEFT[expr_] := Module[
	{
	var,
	repl,
	tmp
	}
	,
	tmp=DsixToolsToHighPTSM[expr];
	(*Print[Variables[(expr(*//DsixTools`D6Simplify*))/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a]];*)
	var=Select[Variables[Variables[tmp(*//DsixTools`D6Simplify*)]/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a],MemberQ[DsixTools`SMEFTParameterList[],#] &];
	(*Print[var];*)
	repl={};
	Do[
		If[
			Head[i]===Symbol,
			If[
				DsixToolsToHighPTSMEFTAss[i] =!= Missing["KeyAbsent",i],
				AppendTo[repl,i->WC[DsixToolsToHighPTSMEFTAss[i],{}]],
				AppendTo[repl,i->i];Message[DsixToolsToHighPTSMEFT::missingcoefficient,i];
			];,
			If[
				DsixToolsToHighPTSMEFTAss[Head[i]] =!= Missing["KeyAbsent",Head[i]],
				AppendTo[repl,i->WC[DsixToolsToHighPTSMEFTAss[Head[i]],Delete[i,0]//List]],
				AppendTo[repl,i->i];Message[DsixToolsToHighPTSMEFT::missingcoefficient,i];
			];
		],
		{i,var}
	];
	If[Length[repl]==0,Message[DsixToolsToHighPTSMEFT::nocoefficients],0];
	Return[(tmp(*//DsixTools`D6Simplify*))/.repl/.WC["eq",{\[Alpha]_,\[Beta]_,i_,j_}]->WC["eq",{i,j,\[Alpha],\[Beta]}]]
];*)


DsixToolsToHighPTSMEFTDispatch = Dispatch[{
	Table[
		SMEFTMap[[i,2]]->WC[SMEFTMap[[i,1]],{}],
		{i,1,15}
	],
	Table[
		SMEFTMap[[i,2]][a_,b_]->WC[SMEFTMap[[i,1]],{a,b}],
		{i,16,34}
	],
	Table[
		SMEFTMap[[i,2]][a_,b_,c_,f_]->WC[SMEFTMap[[i,1]],{a,b,c,f}],
		{i,35,59}
	]
	}//Flatten
];


DsixToolsToHighPTSMEFT[expr_]:=DsixToolsToHighPTSM[expr]/.DsixToolsToHighPTSMEFTDispatch/.WC["eq",{\[Alpha]_,\[Beta]_,i_,j_}]->WC["eq",{i,j,\[Alpha],\[Beta]}]


(* ::Subsection::Closed:: *)
(*LEFT*)


DsixToolsToHighPTLEFTAss = <| (#1[[2]]->#1[[1]])&/@LEFTMap |>


(*DsixToolsToHighPTLEFT::missingcoefficient = "The mapping  of `1` to the HighPT notation is not known"(*. The coefficient will be set to zero"*);*)


(*DsixToolsToHighPTLEFT::nocoefficients = "No LEFT coefficients found.";*)


(*DsixToolsToHighPTLEFT[expr_] := Module[
	{
	var,
	repl,
	tmp
	}
	,
	tmp=DsixToolsToHighPTSM[expr];
	var=Select[(Variables[(tmp(*//DsixTools`D6Simplify*))/.Re->Identity/.Abs->Identity]/.Conjugate[a_]->a)//DeleteDuplicates,MemberQ[DsixTools`LEFTParameterList[],#] &];
	repl={};
	Do[
		If[
			Head[i]===Symbol,
			If[
				DsixToolsToHighPTLEFTAss[i] =!= Missing["KeyAbsent",i],
				AppendTo[repl,i->WCL[DsixToolsToHighPTLEFTAss[i],{}]],
				AppendTo[repl,i->i];Message[DsixToolsToHighPTLEFT::missingcoefficient,i];
			];,
			If[
				DsixToolsToHighPTLEFTAss[Head[i]] =!= Missing["KeyAbsent",Head[i]],
				AppendTo[repl,i->WCL[DsixToolsToHighPTLEFTAss[Head[i]],Delete[i,0]//List]],
				AppendTo[repl,i->i];Message[DsixToolsToHighPTLEFT::missingcoefficient,i];
			];
		],
		{i,var}
	];
	If[Length[repl]==0,Message[DsixToolsToHighPTLEFT::nocoefficients],0];
	Return[(tmp(*//DsixTools`D6Simplify*))/.repl]
];*)


DsixToolsToHighPTLEFTDispatch = Dispatch[{
	Table[
		LEFTMap[[i,2]]->WCL[LEFTMap[[i,1]],{}],
		{i,1,2}
	],
	Table[
		LEFTMap[[i,2]][a_,b_]->WCL[LEFTMap[[i,1]],{a,b}],
		{i,3,8}
	],
	Table[
		LEFTMap[[i,2]][a_,b_,c_,f_]->WCL[LEFTMap[[i,1]],{a,b,c,f}],
		{i,9,64}
	]
	}//Flatten
];


DsixToolsToHighPTLEFT[expr_]:=DsixToolsToHighPTSM[expr]/.DsixToolsToHighPTLEFTDispatch


(* ::Subsection::Closed:: *)
(*Other*)


DsixToolsToHighPTSM[expr_]:=expr/.{
	DsixTools`gs->Param["g3"],
	DsixTools`g->Param["g2"],
	DsixTools`gp->Param["g1"],
	DsixTools`m2->Mass["H"]^2,
	DsixTools`\[Lambda]->Param["\[Lambda]"],
	DsixTools`Gu[i_,j_]:>Yukawa["u",{i,j}],
	DsixTools`Gd[i_,j_]:>Yukawa["d",{i,j}],
	DsixTools`Ge[i_,j_]:>Yukawa["e",{i,j}],
	DsixTools`eQED:>Sqrt[4\[Pi] Param["\[Alpha]EM"]],
	DsixTools`gQCD:>Sqrt[4\[Pi] Param["\[Alpha]S"]],
	DsixTools`Md[i_,j_]:>DiagonalMatrix[{Mass["d"],Mass["s"],Mass["b"]}][[i,j]],
	DsixTools`Me[i_,j_]:>DiagonalMatrix[{Mass["e"],Mass["\[Mu]"],Mass["\[Tau]"]}][[i,j]],
	DsixTools`Mu[i_,j_]:>DiagonalMatrix[{Mass["u"],Mass["c"],Mass["t"]}][[i,j]]
	}


(* ::Section:: *)
(*SOLD -> HighPT*)


(* ::Subsection:: *)
(*SMEFT*)


SOLDToHighPTAss = <| (#1[[2]]->#1[[1]])&/@SMEFTMapSOLD |>


SOLDToHighPTDispatch = Dispatch[{
	Table[
		SMEFTMapSOLD[[i,2]]->WC[SMEFTMapSOLD[[i,1]],{}],
		{i,1,15}
	],
	Table[
		SMEFTMapSOLD[[i,2]][a_,b_]->WC[SMEFTMapSOLD[[i,1]],{a,b}],
		{i,16,34}
	],
	Table[
		SMEFTMapSOLD[[i,2]][a_,b_,c_,f_]->WC[SMEFTMapSOLD[[i,1]],{a,b,c,f}],
		{i,35,59}
	]
	}//Flatten
];


SOLDToHighPTSM[expr_]:=expr


SOLDToHighPT[expr_]:=SOLDToHighPTSM[expr]/.SOLDToHighPTDispatch/.WC["eq",{\[Alpha]_,\[Beta]_,i_,j_}]->WC["eq",{i,j,\[Alpha],\[Beta]}]


(* ::Section:: *)
(*HighPT -> DsixTools*)


(* ::Subsection:: *)
(*SMEFT*)


HighPTToDsixToolsSMEFT[expr_] := expr/.Dispatch[(WC[#1[[1]],flav_]->#1[[2]][flav])&/@SMEFTMap]/.List->Sequence/.DsixTools`Cqe[i_,j_,\[Alpha]_,\[Beta]_]:>DsixTools`Cqe[\[Alpha],\[Beta],i,j]/.a_[]->a


(* ::Subsection:: *)
(*LEFT*)


HighPTToDsixToolsLEFT[expr_] := expr/.Dispatch[(WCL[#1[[1]],flav_]->#1[[2]][flav])&/@LEFTMap]/.List->Sequence/.a_[]->a


(* ::Section:: *)
(*HighPT -> SOLD*)


(* ::Subsection:: *)
(*SMEFT*)


HighPTToSOLD[expr_] := expr/.Dispatch[(WC[#1[[1]],flav_]->#1[[2]][flav])&/@SMEFTMapSOLD]/.List->Sequence/.SOLD`alphaOqe[i_,j_,\[Alpha]_,\[Beta]_]:>SOLD`alphaOqe[\[Alpha],\[Beta],i,j]/.a_[]->a
