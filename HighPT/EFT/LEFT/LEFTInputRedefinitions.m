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
PackageScope["InputRedefinitionAssociation"]


(*PackageScope["ParameterRedefinition"]
PackageScope["ParameterRedefinition$default"]
PackageScope["ParameterRedefinition$current"]*)


PackageScope["WolfensteinParametrization"]


PackageScope["InputRedefinition"]
PackageScope["InputRedefinition$default"]
PackageScope["InputRedefinition$current"]
PackageScope["SMEFTValue"]
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


InputList$default = {(*Param["\[Alpha]EM"],Mass["ZBoson"],*)Param["GF"],Param["|Vus|"],Param["|Vcb|"],Param["|Vub|"],Param["\[Gamma]"]}


(*InputRedefinition[_]:=0*)


(* ::Subsection:: *)
(*GF*)


(*InputRedefinition$default[Param["GF"]] := Param["GF"]Param["vev"]^2 (WC["Hl3",{2,2}]+WC["Hl3",{1,1}]-1/2 WC["ll",{1,2,2,1}](*-1/2 WC["ll",{2,1,1,2}]*))(*/.GetParameters[]*);*)


(*CL621 = 1/2 ( WC["ll",{2,1,1,2}] +  WC["ll",{1,2,2,1}]) - 2 WC["Hl3",{1,1}] - 2 WC["Hl3",{2,2}];
CL821 = 1/2 (WC["l4H21",{2,1,1,2}]+WC["l4H21",{1,2,2,1}]+WC["l4H22",{2,1,1,2}]-WC["l4H22",{1,2,2,1}]);
CL6x621 = -(WC["Hl1",{2,1}]-WC["Hl3",{2,1}])(WC["Hl1",{1,2}]+WC["Hl3",{1,2}]);
CL62summed = 2 Sum[Abs[WC["ll",{i,j,1,2}]]^2,{i,3},{j,3}] +
	3 Abs[WC["Hl1",{1,2}]+WC["Hl3",{1,2}]]^2 + 
	Sum[Abs[WC["Hl3",{i,1}]]^2+Abs[WC["Hl3",{i,2}]]^2,{i,3}] + 
	2 WC["Hl3",{1,1}]WC["Hl3",{2,2}] + 
	2 Sum[Re[WC["ll",{i,i,1,2}]Conjugate[WC["Hl1",{1,2}]+WC["Hl3",{1,2}]]] + Re[WC["ll",{2,i,1,2}]Conjugate[WC["Hl3",{i,1}]]] + Re[WC["ll",{i,1,1,2}]Conjugate[WC["Hl3",{i,2}]]],{i,3}] +
	2 Re[(WC["Hl1",{1,2}]+WC["Hl3",{1,2}])Conjugate[WC["Hl3",{1,2}]]];
CR62summed = Sum[Abs[WC["le",{i,j,1,2}]]^2,{i,3},{j,3}] +
	3 Abs[WC["He",{1,2}]]^2 +
	2 Sum[Re[WC["le",{i,i,1,2}]Conjugate[WC["He",{1,2}]]],{i,3}]*)


(*InputRedefinition$default[Param["GF"]] := -(1/(2Sqrt[2]))CL621-Param["vev"]^2/(8Sqrt[2]) CL621^2-Param["vev"]^2/(2Sqrt[2]) Re[CL821+CL6x621]+Param["vev"]^2/(8Sqrt[2]) CL62summed+Param["vev"]^2/(8Sqrt[2]) CR62summed*)


(*InputRedefinition$default[Param["GF"]] := 1/(2*Sqrt[2])(Sqrt[Sum[Abs[WCL["\[Nu]eVLL",{i,j,1,2}]]^2+Abs[WCL["\[Nu]eVLR",{i,j,1,2}]]^2,{i,3},{j,3}]]/.a_WCL:>SMValue[a]+a)-1/(Sqrt[2]Param["vev"]^2);*)


InputRedefinition$default[Param["GF"]] = Get@FileNameJoin[{Global`$DirectoryHighPT,"EFT","LEFT","GF8.dat"}]


(* ::Subsection:: *)
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
	BrNPKplus = 1/SMPrediction$default["K+->\[Pi]0e\[Nu]"]["Value"] ((BrKplus/.a_WCL:>(SMValue[a,TreeOnly->True] + a - D[SMValue[a,TreeOnly->True]//ParamsAsInputs//FullSimplify,Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (BrKplus/.a_WCL:>SMValue[a,TreeOnly->True]))/.GetParameters[];
	(*Print["Extracted NP part"];*)
	BrNPKplusExpanded = Normal[Series[
			ComplexExpand[
				BrNPKplus/.WCL[lab_,ind_]:>eps*ReWCL[lab,ind]+I*eps*ImWCL[lab,ind]/.WC[lab_,ind_]:>eps*ReWC[lab,ind]+I*eps*ImWC[lab,ind]
			],
			{eps,0,1}
		]]/.eps->1/.ReWCL[lab_,ind_]:>Re[WCL[lab,ind]]/.ReWC[lab_,ind_]:>Re[WC[lab,ind]];
	(*Print["Finished expanding"];*)
	BrKL = TheoryExpression["KL->\[Pi]-e\[Nu]"]/.SubstitutePsi/.Around[a_,b_]:>a/.GetParameters[];
	BrNPKL = 1/SMPrediction$default["KL->\[Pi]-e\[Nu]"]["Value"] ((BrKL/.a_WCL:>(SMValue[a,TreeOnly->True] + a - D[SMValue[a,TreeOnly->True]//ParamsAsInputs//FullSimplify,Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (BrKL/.a_WCL:>SMValue[a,TreeOnly->True]))/.GetParameters[];
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
	BrNP = 1/SMPrediction$default["B->Dl\[Nu]_iso"]["Value"] ((Br/.a_WCL:>(SMValue[a,TreeOnly->True] + a - D[SMValue[a,TreeOnly->True]//ParamsAsInputs//FullSimplify,Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (Br/.a_WCL:>SMValue[a,TreeOnly->True]))/.GetParameters[];
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
	BrNP = 1/SMPrediction$default["B0->\[Pi]-l\[Nu]_high"]["Value"] ((Br/.a_WCL:>(SMValue[a,TreeOnly->True] + a - D[SMValue[a,TreeOnly->True]//ParamsAsInputs//FullSimplify,Param["GF"]]*InputRedefinition$default[Param["GF"]])) - (Br/.a_WCL:>SMValue[a,TreeOnly->True]))/.GetParameters[];
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


(* ::Section:: *)
(*Define redefinitions (Standard input scheme)*)


DefineRedefinitions::invalidinput


Options[DefineRedefinitions] = {
	"GF"    -> "current",
	"|Vus|" -> "current",
	"|Vcb|" -> "current",
	"|Vub|" -> "current",
	"\[Gamma]"     -> "current"
};


DefineRedefinitions[Default] := DefineRedefinitions[
	"GF"    -> InputRedefinition$default[Param["GF"]],
	"|Vus|" -> InputRedefinition$default[Param["|Vus|"]],
	"|Vcb|" -> InputRedefinition$default[Param["|Vcb|"]],
	"|Vub|" -> InputRedefinition$default[Param["|Vub|"]],
	"\[Gamma]"     -> InputRedefinition$default[Param["GF"]]
]


DefineRedefinitions[OptionsPattern[]] := Module[
	{
	(*opt = {"GF","\[Alpha]EM","mZ","|Vus|","|Vcb|","|Vub|","\[Gamma]"}*)tmp
	}
	,
	(*Print["Entered DefineRedefinitions"];*)
	If[!MatchQ[OptionValue["GF"],"current"],
		InputRedefinition[Param["GF"]] := OptionValue["GF"]
	];
	If[!MatchQ[OptionValue["|Vus|"],"current"],
		InputRedefinition[Param["GF"]] := OptionValue["GF"]
	];
	If[!MatchQ[OptionValue["|Vus|"],"current"],
		InputRedefinition[Param["|Vus|"]] := OptionValue["|Vus|"]
	];
	If[!MatchQ[OptionValue["|Vcb|"],"current"],
		InputRedefinition[Param["|Vcb|"]] := OptionValue["|Vcb|"]
	];
	If[!MatchQ[OptionValue["|Vub|"],"current"],
		InputRedefinition[Param["|Vub|"]] := OptionValue["|Vub|"]
	];
	If[!MatchQ[OptionValue["\[Gamma]"],"current"],
		InputRedefinition[Param["\[Gamma]"]] := OptionValue["\[Gamma]"]
	];
	InputRedefinitionAssociation = Association[Table[
		i -> (<|
			"d6" -> EFTTruncate[InputRedefinition[i], EFTorder->2],
			"d8" -> EFTTruncate[InputRedefinition[i], EFTorder->4, OperatorDimension->8] - EFTTruncate[InputRedefinition[i], EFTorder->2]
		|>/.GetParameters[]),
		{i,InputList}
	]];
]


(*InputRedefinition[x_] := InputRedefinition$default[x]*)


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
InputFunction[Param["\[Lambda]"]]     := Mass["H"]^2/Param["vev"]^2


ParamsAsInputs[expr_] := Module[
	{derived,rep}
	,
	derived = {
		Param["cW"],Param["sW"],
		Param["g1"],Param["g2"],Param["g3"],
		Param["gZ"],
		Param["vev"],
		Mass["WBoson"],
		Table[Vckm[i,j],{i,3},{j,3}],
		Param["\[Lambda]"]
	}//Flatten;
	rep=Table[i->InputFunction[i],{i,derived}];
	Return[expr//.rep]
]


(* ::Section:: *)
(*User - defined redefinitions - to do*)


InputList=InputList$default;


(* ::Section:: *)
(*Redefine Inputs in generic expressions - re-check d8 part after GF update (might be obsolete)*)


(* ::Subsection:: *)
(*Old implementation*)


(*InputShift[x_WCL] := -Sum[(D[SMValue[x]//ParamsAsInputs,i]/.GetParameters[])*(InputRedefinition[i]/.GetParameters[]),{i,InputList}]*)


(*ApplyRedefinitions[expr_] := expr/.a_WCL:>(a+(SMEFTValue[a]-(SMEFTValue[a]/._WC->0))+InputShift[a]*RedefinitionFlag)/.GetParameters[]*)


(* ::Subsection:: *)
(*New implementation (up to d = 8)*)


Options[ApplyRedefinitions] = {
	EFTorder :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[]
};


ApplyRedefinitions[expr_,OptionsPattern[]] := Module[
	{
		LEFTcoeff, pieces,
		gradSM, hessSM,gradd6,
		rules
	}
	,
	(* Return expression if redefinitions are switched off *)
	If[MatchQ[RedefinitionFlag,0], Return[expr]];
	
	(* Find all LEFT coefficients *)
	LEFTcoeff=Cases[expr,_WCL,All]//DeleteDuplicates;
	
	(* Extract SM and d=6 piece (latter needed for d=8 redefinitions) *)
	pieces = Association@Table[
		i->Association[
			"SM"->SMValue[i],
			"d6"->MatchToSMEFT[i,OperatorDimension->6]
		],
		{i,LEFTcoeff}
	];
	
	(* Compute necessary derivatives *)
	gradSM = Association[Table[
		i -> Association[Table[
			j -> (D[pieces[i]["SM"]//ParamsAsInputs,j]/.GetParameters[]),
			{j,InputList}
		]],
		{i,LEFTcoeff}
	]];
	gradd6 = Association[Table[
		i -> Association[Table[
			j -> (D[pieces[i]["d6"]//ParamsAsInputs,j]/.GetParameters[]),
			{j,InputList}
		]],
		{i,LEFTcoeff}
	]];
	hessSM = Association[Table[
		i -> Association[Table[
			j -> Association[Table[
				k -> (D[pieces[i]["SM"]//ParamsAsInputs,j,k]/.GetParameters[]),
				{k,InputList}
			]],
			{j,InputList}
		]],
		{i,LEFTcoeff}
	]];
	
	(* Assemble substitution rules *)
	If[MatchQ[OptionValue[OperatorDimension],6],
		rules = Association[Table[
			i -> (i-Sum[gradSM[i][j]*InputRedefinitionAssociation[j]["d6"],{j,InputList}]),
			{i,LEFTcoeff}
		]],
		rules = Association[Table[
			i -> (i-Sum[gradSM[i][j]*(InputRedefinitionAssociation[j]["d6"]+InputRedefinitionAssociation[j]["d8"]),{j,InputList}]+1/2 Sum[hessSM[i][j,k]*InputRedefinitionAssociation[j]["d6"]*InputRedefinitionAssociation[k]["d6"],{j,InputList},{k,InputList}]-Sum[gradd6[i][j]*InputRedefinitionAssociation[j]["d6"],{j,InputList}]),
			{i,LEFTcoeff}
		]]
	];
	Return[Chop[expr/.rules]]
];
