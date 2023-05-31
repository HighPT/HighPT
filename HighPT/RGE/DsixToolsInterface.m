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


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*SMEFT Map*)


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


(* ::Section:: *)
(*LEFT Map*)


LEFTMap = {
	(*{"\[Nu]",DsixTools`L},*)
	{"\[Nu]\[Gamma]",DsixTools`L\[Nu]\[Gamma]},
	{"e\[Gamma]",DsixTools`Le\[Gamma]},
	{"u\[Gamma]",DsixTools`Lu\[Gamma]},
	{"d\[Gamma]",DsixTools`Ld\[Gamma]},
	{"uG",DsixTools`LuG},
	{"dG",DsixTools`LdG},
	{"G",DsixTools`LG},
	{"Gt",DsixTools`LGtilde},
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


(* ::Subsection:: *)
(*SMEFT*)


DsixToolsToHighPTSMEFTAss = <| (#1[[2]]->#1[[1]])&/@SMEFTMap |>


DsixToolsToHighPTSMEFT::missingcoefficient = "The mapping  of `1` to the HighPT notation is not known"(*. The coefficient will be set to zero"*);


DsixToolsToHighPTSMEFT::nocoefficients = "No SMEFT coefficients found.";


DsixToolsToHighPTSMEFT[expr_] := Module[
	{
	var,
	repl
	}
	,
	(*Print[Variables[(expr(*//DsixTools`D6Simplify*))/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a]];*)
	var=Select[Variables[(expr(*//DsixTools`D6Simplify*))/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a],MemberQ[DsixTools`SMEFTParameterList[],#] &];
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
	Return[(expr(*//DsixTools`D6Simplify*))/.repl/.WC["eq",{\[Alpha]_,\[Beta]_,i_,j_}]->WC["eq",{i,j,\[Alpha],\[Beta]}]]
];


(* ::Subsection:: *)
(*LEFT*)


DsixToolsToHighPTLEFTAss = <| (#1[[2]]->#1[[1]])&/@LEFTMap |>


DsixToolsToHighPTLEFT::missingcoefficient = "The mapping  of `1` to the HighPT notation is not known"(*. The coefficient will be set to zero"*);


DsixToolsToHighPTLEFT::nocoefficients = "No LEFT coefficients found.";


DsixToolsToHighPTLEFT[expr_] := Module[
	{
	var,
	repl
	}
	,
	var=Select[Variables[(expr(*//DsixTools`D6Simplify*))/.Re->Identity/.Abs->Identity/.Conjugate[a_]->a],MemberQ[DsixTools`LEFTParameterList[],#] &];
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
	Return[(expr(*//DsixTools`D6Simplify*))/.repl]
];


(* ::Subsection:: *)
(*Other*)


DsixToolsToHighPTSM[expr_]:=expr/.{
	DsixTools`gs->Param["g3"],
	DsixTools`g->Param["g2"],
	DsixTools`gp->Param["g1"],
	DsixTools`m2->Mass["H"]^2,
	DsixTools`\[Lambda]->Param["\[Lambda]"]
	}


(* ::Section:: *)
(*HighPT -> DsixTools*)


(* ::Subsection:: *)
(*SMEFT*)


HighPTToDsixToolsSMEFT[expr_] := expr/.Dispatch[(WC[#1[[1]],flav_]->#1[[2]][flav])&/@SMEFTMap]/.List->Sequence/.DsixTools`Cqe[i_,j_,\[Alpha]_,\[Beta]_]:>DsixTools`Cqe[\[Alpha],\[Beta],i,j]/.a_[]->a


(* ::Subsection:: *)
(*LEFT*)


HighPTToDsixToolsLEFT[expr_] := expr/.Dispatch[(WCL[#1[[1]],flav_]->#1[[2]][flav])&/@LEFTMap]/.List->Sequence/.a_[]->a
