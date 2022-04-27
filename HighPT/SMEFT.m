(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`SMEFT`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["MatchToSMEFT"]


PackageExport["WC"]


PackageExport["SetEFTorder"]


PackageExport["GetEFTorder"]


PackageExport["SetOperatorDimension"]


PackageExport["GetOperatorDimension"]


PackageScope["CanonizeFF"]


PackageScope["SubstitutionRulesSMEFT"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*EFT power counting*)


(* ::Subsection:: *)
(*EFT operator dimensions*)


(* default mass dimension up to which operators are considered *)
$OperatorDimension= 6;


SetOperatorDimension::usage= "SetOperatorDimension[d]
	Speciefies the maximum mass dimension d of effective operators that should be considered in all following computations.
	Allowed values are d\[Element]{4,6,8}. The default value is d=6.";


GetOperatorDimension::usage= "GetOperatorDimension[d]
	Returns the maximum mass dimension d of effective operators that are considered in all computations by default at the moment.
	The value of d can be modified using SetOperatorDimension[d].";


SetOperatorDimension::dimensioninvalid= "The operator mass dimension specified d=`1` is not valid. The allowed values are d \[Epsilon] {4, 6, 8}.";


SetOperatorDimension[d:4|6|8]:= (
	$OperatorDimension= d;
	(*
	Print["Default for EFT operator mass dimension d set to d=", d, "."]
	*)
);


SetOperatorDimension[d:Except[4|6|8]]:= Message[SetOperatorDimension::dimensioninvalid, d]


GetOperatorDimension[]:= $OperatorDimension


(* ::Subsection:: *)
(*Subscript[\[CapitalLambda], NP] suppression factors*)


(* Powers of suppression factors that should be considered in cross sections *)
$EFTorder= 4;


SetEFTorder::usage= "SetEFTorder[n]
	Sets a global flag specifying that cross sections should be expanded up to and including terms of order (\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\)\!\(\*SuperscriptBox[\()\), \(-n\)]\).
	Allowed values are n \[Element] {0, 2, 4} and the default value is n=4.";


GetEFTorder::usage= "GetEFTorder[]
	Returns current the global value n used for the EFT series truncation (\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\)\!\(\*SuperscriptBox[\()\), \(-n\)]\).
	The returned value can be modified using SetEFTorder[n].";


SetEFTorder::invalidEFTorder= "The given argument n=`1` is not a valid EFT order. The allowed values are n \[Element] {0, 2, 4}.";


SetEFTorder[n:(0|2|4)]:= (
	$EFTorder= n;
	(*
	Print["Default for EFT series truncation set to ", HoldForm[("(\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\))")^-n], "."]
	*)
);


SetEFTorder[n:Except[0|2|4]]:= (Message[SetEFTorder::invalidEFTorder,d];Abort[])


GetEFTorder[]:= $EFTorder


(* ::Section:: *)
(*Wilson coefficients*)


WC::usage= 
"WC[\"label\",{\[Alpha],\[Beta],i,j}]
	Wilson coefficient associated to the four-fermion operator denoted by label with lepton (quark) flavor indices \[Alpha],\[Beta] (i,j).
	Possible values for \"label\" are:
	\"lq1\", \"lq3\", \"eu\", \"ed\", \"lu\", \"ld\", \"eq\" (NOTICE: in the Warsaw basis this is the operator \"qe\"), \"ledq\", \"lequ1\", \"lequ3\".

WC[\"label\",{r,s}]
	Wilson coefficient associated to the two-fermion operator denoted by label with flavor indices r,s which can be either lepton or quark indices.
	Possible values for \"label\" are:
	\"eW\", \"eB\", \"uW\", \"uB\", \"dW\", \"dB\", \"Hl1\", \"Hl3\", \"Hq1\", \"Hq3\", \"He\", \"Hu\", \"Hd\", \"Hud\".";


(* ::Subsection:: *)
(*Formatting*)


Format[WC[label_,{indices__}],TraditionalForm]:= Module[
	{num=StringTake[ToString[label],-1]},
	If[NumericQ[ToExpression[num]],
		Subsuperscript["\[ScriptCapitalC]",Underscript[StringDrop[ToString[label],-1],StringJoin[ToString/@{indices}]],StringJoin["(",num,")"]],
		Subscript["\[ScriptCapitalC]",Underscript[ToString[label],StringJoin[ToString/@{indices}]]]
	]
]


(* ::Subsection:: *)
(*Flavor indices*)


(* remove unwanted heads *)
WC[x_,{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:= WC[x,{a,b,i,j}]
WC[x_,{f1_[a_],f2_[b_]}]:= WC[x,{a,b}]


(* ::Section:: *)
(*Matching the form-factors to the SMEFT*)


Options[MatchToSMEFT]= {
	EFTorder :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[]
};


MatchToSMEFT::usage="MatchToSMEFT[arg, \[CapitalLambda]]
	Replaces the form factors (FF) in the argument arg with the corresponding Wilson coefficients in the SMEFT using \[CapitalLambda] as the EFT suppression scale.
	The options are: 
	EFTorder \[Rule] n, 
		Specifies that the result is expanded up to and including terms of order \!\(\*SuperscriptBox[\(\[CapitalLambda]\), \(-n\)]\). The default is n=GetEFTorder[].
	OperatorDimension \[Rule] d,
		Specifies that EFT operators up to mass dimension d should be included. The default is d=GetOperatorDimension[].";


MatchToSMEFT::remainingFF= "Not all form-factors have been replaced. The remaining FF are: `1`"


MatchToSMEFT[arg_, \[CapitalLambda]NP_, OptionsPattern[]]:= Module[
	{\[Epsilon], subst, temp= arg}
	,
	(* make \[Epsilon] real *)
	\[Epsilon]/:Conjugate[\[Epsilon]]:= \[Epsilon];
	
	(* get substitution rules *)
	subst= SubstitutionRulesSMEFT[OptionValue[OperatorDimension], \[Epsilon]]; (* \[Epsilon] = \[Vee]^2/(\[CapitalLambda]^2) *)
	
	(* apply substitution rules *)
	temp= temp/.CanonizeFF;
	temp= temp/.subst;
	
	(* check that no form-factors are left *)
	If[!FreeQ[temp,_FF], Message[MatchToSMEFT::remainingFF, DeleteDuplicates@Cases[temp,_FF,All]]];
	
	(* substitute in constants *)
	temp= temp/.ReplaceConstants[];
	
	(* EFT truncation of results *)
	temp= ExpandConjugate[temp];
	(*temp= EchoTiming[Expand[temp], "Expand"];*)
	(*
	temp= EchoTiming[Collect[temp,\[Epsilon]], "Collect"];
	Switch[OptionValue[EFTorder],
		0, temp= temp /. \[Epsilon] -> 0,
		2, temp= temp /. Power[\[Epsilon],n_]/;n>1 -> 0,
		4, temp= temp /. Power[\[Epsilon],n_]/;n>2 -> 0
	];
	*)
	
	(* truncate the EFT series at the desired order *)
	temp= Normal@Series[temp,{\[Epsilon],0,OptionValue[EFTorder]/2}];
	
	(* expand result *)
	temp= Expand[temp];
		
	(* substitute in the power counting parameter *)
	temp= temp/.\[Epsilon] -> (ConstantInput["vev"]/\[CapitalLambda]NP)^2;
	(* substitute vev *)
	temp= temp/.ReplaceConstants[];
	
	(* result *)
	Return[temp/.{Complex[a_,0.]:> a, Complex[b_,0]:> b}]
]


(* ::Subsection:: *)
(*Wilson coefficient (WC) substitution rules*)


(*(* returns the replacement rules to match the form-factors to the d<=8 SMEFT in the Warsaw basis *)
SubstitutionRulesSMEFT[dim_, \[Epsilon]_]:= Module[{list,f6,f8,$DelayedRule},
	(* there are some redundant rule below, since we now canonize the FF before substituting them *)
	list= {
		(* SCALAR *)
		(* NC *)

		(*FF[Scalar, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ1", {a,b,i,j}] * f6,*)
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		(*FF[Scalar, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ledq", {a,b,i,j}] * f6,*)
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		(*FF[Scalar, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_u}]:> \[Epsilon] * WC["lequ1", {a,b,i,j}] * f6,*)
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		(*FF[Scalar, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_u}]:> \[Epsilon] * WC["ledq", {a,b,i,j}] * f6,*)
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Tensor *)
		(* NC *)
		(*FF[Tensor, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ3", {a,b,i,j}] * f6,*)
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		(*FF[Tensor, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_u}]:> \[Epsilon] * WC["lequ3", {a,b,i,j}] * f6,*)
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Vector UP *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * (WC["lq1", {a,b,i,j}] - WC["lq3", {a,b,i,j}]) * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["l2q2H21", {a,b,i,j}]+WC["l2q2H22", {a,b,i,j}]-WC["l2q2H23", {a,b,i,j}]-WC["l2q2H24", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["q2H2D31", {i,j}]-WC["q2H2D32", {i,j}]-WC["q2H2D33", {i,j}]+WC["q2H2D34", {i,j}])+
		                                gZ[u,Left,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["eu", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["e2u2H2", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["u2H2D31", {i,j}]-WC["u2H2D32", {i,j}])+
		                                gZ[u,Right,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["lu", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["l2u2H21", {a,b,i,j}]+WC["l2u2H22", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["u2H2D31", {i,j}]-WC["u2H2D32", {i,j}])+
		                                gZ[u,Right,{i,j}]*(WC["l2H2D31", {a,b,i,j}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["eq", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["q2e2H21", {a,b,i,j}]+WC["q2e2H22", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["q2H2D31", {i,j}]-WC["q2H2D32", {i,j}]-WC["q2H2D33", {i,j}]+WC["q2H2D34", {i,j}])+
		                                gZ[u,Left,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		
		(* Vector DOWN *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * (WC["lq1", {a,b,i,j}] + WC["lq3", {a,b,i,j}]) * f6 +
		f8 * \[Epsilon]^2 * 1/2 * (WC["l2q2H21", {a,b,i,j}]+WC["l2q2H22", {a,b,i,j}]+WC["l2q2H23", {a,b,i,j}]+WC["l2q2H24", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["q2H2D31", {j,i}]\[Conjugate]-WC["q2H2D32", {j,i}]\[Conjugate]+WC["q2H2D33", {j,i}]\[Conjugate]-WC["q2H2D34", {j,i}]\[Conjugate])+
                                        gZ[d,Left,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
        
        FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["ld", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * (WC["l2d2H21", {a,b,i,j}]+WC["l2d2H22", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["d2H2D31", {j,i}]\[Conjugate]-WC["d2H2D32", {j,i}]\[Conjugate])+
                                        gZ[d,Right,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["eq", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * (WC["q2e2H21", {a,b,i,j}]+WC["q2e2H22", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["q2H2D31", {j,i}]\[Conjugate]-WC["q2H2D32", {j,i}]\[Conjugate]+WC["q2H2D33", {j,i}]\[Conjugate]-WC["q2H2D34", {j,i}]\[Conjugate])+
                                        gZ[d,Left,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["ed", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * WC["e2d2H2", {a,b,i,j}]+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["d2H2D31", {j,i}]\[Conjugate]-WC["d2H2D32", {j,i}]\[Conjugate])+
                                        gZ[d,Right,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		(* energy enhanced UP *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["l2q2D21", {a,b,i,j}]+WC["l2q2D22", {a,b,i,j}]-WC["l2q2D23", {a,b,i,j}]-WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D22", {a,b,i,j}]-WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{1,0}}, {Left,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["l2u2D21", {a,b,i,j}]+WC["l2u2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["l2u2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["q2e2D21", {a,b,i,j}]+WC["q2e2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["q2e2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["e2u2D21", {a,b,i,j}]+WC["e2u2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["e2u2D22", {a,b,i,j}],
		
		(* energy enhanced DOWN *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["l2q2D21", {a,b,i,j}]+WC["l2q2D22", {a,b,i,j}]+WC["l2q2D23", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D22", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{1,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["l2d2D21", {a,b,i,j}]+WC["l2d2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["l2d2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> f8 *  \[Epsilon]^2 * (WC["q2e2D21", {a,b,i,j}]+WC["q2e2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["q2e2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["e2d2D21", {a,b,i,j}]+WC["e2d2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["e2d2D22", {a,b,i,j}],
		
		(* CC *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> 
		\[Epsilon] * 2 * WC["lq3", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * (WC["l2q2H23",{a,b,i,j}]+I*WC["l2q2H25",{a,b,i,j}])+
		f8 * \[Epsilon]^2 * (-1) Mass[WBoson]^2/ConstantInput["vev"]^2 * ((WC["l2H2D33",{a,b}]-WC["l2H2D34",{b,a}]\[Conjugate])*KroneckerDelta[i,j]+
		                               (WC["q2H2D33",{j,i}]\[Conjugate]+WC["q2H2D34",{i,j}])*KroneckerDelta[a,b]),
		                               
		(* energy enhanced CC *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D23", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_u,j_d}]:> f8 * \[Epsilon]^2 * 4 * (WC["l2q2D24", {a,b,i,j}]),
		
		(*FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_d,j_u}]:> \[Epsilon] * 2 * WC["lq3", {a,b,i,j}] * f6,*)
		FF[Vector, {"regular",{0,0}}, {OrderlessPatternSequence[Right,_]}, {a_,b_,i_u,j_d}]:> 0,
		(*FF[Vector, {"regular",{0,0}}, {OrderlessPatternSequence[Right,_]}, {a_,b_,i_d,j_u}]:> 0,*)
		
		(* Standard Model *)
		(* NC *)
		FF[Vector, {Photon,SM}, {_,_}, {l_[a_],l_[b_],q_[i_],q_[j_]}]:> gA[l,{a,b}] * gA[q,{i,j}],
		FF[Vector, {ZBoson,SM}, {\[Chi]l_,\[Chi]q_}, {l_[a_],l_[b_],q_[i_],q_[j_]}]:> gZ[l,\[Chi]l,{a,b}] * gZ[q,\[Chi]q,{i,j}],
		(* CC *)
		FF[Vector, {WBoson,SM}, {Left,Left}, {l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> gW[{a,b}] * gW[{i,j}],
		
		(* Gauge coupling modifications *)
		(* NC *)
		FF[Vector, {Photon,0}, ___]:> 0,
		(*FF[Vector, {ZBoson,0}, {\[Chi]l_,\[Chi]q_}, {l_[a_],l_[b_],d[i_],d[j_]}]:> gZ[l,\[Chi]l,{a,b}] * \[CapitalDelta]gZd[\[Chi]q,\[Epsilon],{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * gZ[d,\[Chi]q,{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * \[CapitalDelta]gZd[\[Chi]q,\[Epsilon],{i,j}] * f6,
		FF[Vector, {ZBoson,0}, {\[Chi]l_,\[Chi]q_}, {l_[a_],l_[b_],u[i_],u[j_]}]:> gZ[l,\[Chi]l,{a,b}] * \[CapitalDelta]gZu[\[Chi]q,\[Epsilon],{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * gZ[u,\[Chi]q,{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * \[CapitalDelta]gZu[\[Chi]q,\[Epsilon],{i,j}] * f6,
		*)
		
		(* Gauge coupling modifications UP*)
		FF[Vector, {ZBoson,0}, {Left,Left}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hq1",{i,j}] - WC["Hq3",{i,j}])+gZ[u,Left,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["q2H4D1",{i,j}]-2*WC["q2H4D2",{i,j}])+gZ[u,Left,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]-WC["q2H2D33",{i,j}]+WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Left,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Left,Right}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hu",{i,j}])+gZ[u,Right,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hu",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["u2H4D",{i,j}])+gZ[u,Right,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["u2H2D31",{i,j}]-WC["u2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Right,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Left}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hq1",{i,j}] - WC["Hq3",{i,j}])+gZ[u,Left,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])*(WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*(WC["q2H4D1",{i,j}]-2*WC["q2H4D2",{i,j}])+gZ[u,Left,{i,j}]*(WC["e2H4D",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]-WC["q2H2D33",{i,j}]+WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Left,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Right}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hu",{i,j}])+gZ[u,Right,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hu",{i,j}]*WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*WC["u2H4D",{i,j}]+gZ[u,Right,{i,j}]*WC["e2H4D",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["u2H2D31",{i,j}]-WC["u2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Right,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		
		(* Gauge coupling modifications DOWN*)
		FF[Vector, {ZBoson,0}, {Left,Left}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hq1",{i,j}] + WC["Hq3",{i,j}])+gZ[d,Left,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["q2H4D1",{i,j}]+2*WC["q2H4D2",{i,j}])+gZ[d,Left,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]+WC["q2H2D33",{i,j}]-WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Left,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Left,Right}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hd",{i,j}])+gZ[d,Right,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hd",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["d2H4D",{i,j}])+gZ[d,Right,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["d2H2D31",{i,j}]-WC["d2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Right,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Left}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hq1",{i,j}] + WC["Hq3",{i,j}])+gZ[d,Left,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])*(WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*(WC["q2H4D1",{i,j}]+2*WC["q2H4D2",{i,j}])+gZ[d,Left,{i,j}]*(WC["e2H4D",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]+WC["q2H2D33",{i,j}]-WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Left,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Right}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hd",{i,j}])+gZ[d,Right,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hd",{i,j}]*WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*WC["d2H4D",{i,j}]+gZ[d,Right,{i,j}]*WC["e2H4D",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["d2H2D31",{i,j}]-WC["d2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Right,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		
		(* CC *)
		
		(* Gauge coupling modifications CC*)
		FF[Vector, {WBoson,0}, {Left,Left}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:>
		f6 * \[Epsilon] * Mass[WBoson]^2/ConstantInput["vev"]^2 * 2 *(WC["Hl3",{b,a}]\[Conjugate]*KroneckerDelta[i,j]+WC["Hq3",{i,j}]*KroneckerDelta[a,b])+
		f6 * \[Epsilon] * Mass[WBoson]^2/ConstantInput["vev"]^2 * 2 * WC["Hl3",{b,a}]\[Conjugate] * WC["Hq3",{i,j}]+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * KroneckerDelta[i,j] * (WC["l2H4D2",{b,a}]\[Conjugate]-WC["l2H4D3",{b,a}]\[Conjugate]+WC["l2H4D4",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * KroneckerDelta[a,b] * (WC["q2H4D2",{i,j}]-WC["q2H4D3",{i,j}]+WC["q2H4D4",{j,i}]\[Conjugate])+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * (-1) * KroneckerDelta[i,j] * (WC["l2H2D33",{a,b}]-WC["l2H2D34",{b,a}]\[Conjugate])+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * (-1) * KroneckerDelta[a,b] * (WC["q2H2D34",{i,j}]-WC["q2H2D33",{j,i}]\[Conjugate]),
		
		FF[Vector, {WBoson,0}, {Left,Right}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:>
		f6 * \[Epsilon] Mass[WBoson]^2/ConstantInput["vev"]^2 * KroneckerDelta[a,b] * WC["Hud",{i,j}],
		
		(*FF[Vector, {WBoson,0}, {Left,Left}, {l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> 2*\[Pi]*ConstantInput["\[Alpha]EM"] * \[Epsilon] * (KroneckerDelta[i,j] * WC["Hl3",{a,b}] + KroneckerDelta[a,b] * WC["Hq3",{i,j}])*f6 + 2*\[Pi]*ConstantInput["\[Alpha]EM"] * \[Epsilon]^2 * WC["Hl3",{a,b}] * WC["Hq3",{i,j}]*f6,
		FF[Vector, {WBoson,0}, {Left,Right}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:> \[Pi]*ConstantInput["\[Alpha]EM"] * \[Epsilon] * KroneckerDelta[a,b] * WC["Hud",{i,j}] * f6,
		FF[Vector, {WBoson,0}, {Left,Right}, {l1_[a_],l2_[b_],d[i_],u[j_]}]:> \[Pi]*ConstantInput["\[Alpha]EM"] * \[Epsilon] * KroneckerDelta[a,b] * WC["Hud",{j,i}]\[Conjugate] * f6,*)
		
		(* Lepton Dipoles *)
		(* NC *)
		(*FF[DipoleL, {Photon,0}, {Right,_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> -Sqrt[2]* gA[q,{i,j}] * \[Epsilon] * (ConstantInput["sW"]*WC["eW",{a,b}] - ConstantInput["cW"]*WC["eB",{a,b}]) * f6,*)
		FF[DipoleL, {Photon,0}, {Left,_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gA[q,{i,j}] * \[Epsilon] * (ConstantInput["sW"]*WC["eW",{b,a}]\[Conjugate] - ConstantInput["cW"]*WC["eB",{b,a}]\[Conjugate]) * f6,
		(*FF[DipoleL, {ZBoson,0}, {Right,\[Chi]q_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> -Sqrt[2]* gZ[q,\[Chi]q,{i,j}] * \[Epsilon] * (ConstantInput["cW"]*WC["eW",{a,b}] + ConstantInput["sW"]*WC["eB",{a,b}]) * f6,*)
		FF[DipoleL, {ZBoson,0}, {Left,\[Chi]q_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gZ[q,\[Chi]q,{i,j}] * \[Epsilon] * (ConstantInput["cW"]*WC["eW",{b,a}]\[Conjugate] + ConstantInput["sW"]*WC["eB",{b,a}]\[Conjugate]) * f6,
		(* CC *)
		FF[DipoleL, {WBoson,0}, {Right,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{a,b}] * f6,
		FF[DipoleL, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> -Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{b,a}]\[Conjugate] * f6,
		
		(* Quark Dipoles *)
		(* NC *)
		(*FF[DipoleQ, {Photon,0}, {_,Right},{l_[a_],l_[b_],d[i_],d[j_]}]:> Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (ConstantInput["sW"]*WC["dW",{i,j}] - ConstantInput["cW"]*WC["dB",{i,j}]) * f6,*)
		FF[DipoleQ, {Photon,0}, {_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (ConstantInput["sW"]*WC["dW",{j,i}]\[Conjugate] - ConstantInput["cW"]*WC["dB",{j,i}]\[Conjugate]) * f6,
		(*FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Right},{l_[a_],l_[b_],d[i_],d[j_]}]:> Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (ConstantInput["cW"]*WC["dW",{i,j}] + ConstantInput["sW"]*WC["dB",{i,j}]) * f6,*)
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (ConstantInput["cW"]*WC["dW",{j,i}]\[Conjugate] + ConstantInput["sW"]*WC["dB",{j,i}]\[Conjugate]) * f6,
		
		(*FF[DipoleQ, {Photon,0}, {_,Right},{l_[a_],l_[b_],u[i_],u[j_]}]:> - Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (ConstantInput["sW"]*WC["uW",{i,j}] + ConstantInput["cW"]*WC["uB",{i,j}]) * f6,*)
		FF[DipoleQ, {Photon,0}, {_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (ConstantInput["sW"]*WC["uW",{j,i}]\[Conjugate] + ConstantInput["cW"]*WC["uB",{j,i}]\[Conjugate]) * f6,
		(*FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Right},{l_[a_],l_[b_],u[i_],u[j_]}]:> - Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (ConstantInput["cW"]*WC["uW",{i,j}] - ConstantInput["sW"]*WC["uB",{i,j}]) * f6,*)
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (ConstantInput["cW"]*WC["uW",{j,i}]\[Conjugate] - ConstantInput["sW"]*WC["uB",{j,i}]\[Conjugate]) * f6,
		
		(* CC *)
		FF[DipoleQ, {WBoson,0}, {Left,Right},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> - Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["dW",{i,j}] * f6,
		(*FF[DipoleQ, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],d[i_],u[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["dW",{j,i}]\[Conjugate] * f6,*)
		(*FF[DipoleQ, {WBoson,0}, {Left,Right},{l1_[a_],l2_[b_],d[i_],u[j_]}]:> - Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["uW",{i,j}] * f6,*)
		FF[DipoleQ, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["uW",{j,i}]\[Conjugate] * f6
	};
	
	list = list /. RuleDelayed->$DelayedRule;
	list = list /. {f6->If[dim<=4,0,1], f8->If[dim<=6,0,1]};
	list = list /. $DelayedRule -> RuleDelayed;
	Return[list]
	
	(*Return[list/.f6->If[dim<=4,0,1]/.f8->If[dim<=6,0,1]];*)
]*)


(* returns the replacement rules to match the form-factors to the d<=8 SMEFT in the Warsaw basis *)
SubstitutionRulesSMEFT[dim_, \[Epsilon]_]:= Module[{list,f6,f8,$DelayedRule},
	(* there are some redundant rule below, since we now canonize the FF before substituting them *)
	list= {
		(* SCALAR *)
		(* NC *)
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Tensor *)
		(* NC *)
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Vector UP *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * (WC["lq1", {a,b,i,j}] - WC["lq3", {a,b,i,j}]) * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["l2q2H21", {a,b,i,j}]+WC["l2q2H22", {a,b,i,j}]-WC["l2q2H23", {a,b,i,j}]-WC["l2q2H24", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["q2H2D31", {i,j}]-WC["q2H2D32", {i,j}]-WC["q2H2D33", {i,j}]+WC["q2H2D34", {i,j}])+
		                                gZ[u,Left,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["eu", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["e2u2H2", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["u2H2D31", {i,j}]-WC["u2H2D32", {i,j}])+
		                                gZ[u,Right,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["lu", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["l2u2H21", {a,b,i,j}]+WC["l2u2H22", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["u2H2D31", {i,j}]-WC["u2H2D32", {i,j}])+
		                                gZ[u,Right,{i,j}]*(WC["l2H2D31", {a,b,i,j}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		                          
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_u,j_u}]:> 
		\[Epsilon] * WC["eq", {a,b,i,j}] * f6+
		f8 * 1/2 * \[Epsilon]^2 * (WC["q2e2H21", {a,b,i,j}]+WC["q2e2H22", {a,b,i,j}])+
		f8 * 1/2 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["q2H2D31", {i,j}]-WC["q2H2D32", {i,j}]-WC["q2H2D33", {i,j}]+WC["q2H2D34", {i,j}])+
		                                gZ[u,Left,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		
		(* Vector DOWN *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * (WC["lq1", {a,b,i,j}] + WC["lq3", {a,b,i,j}]) * f6 +
		f8 * \[Epsilon]^2 * 1/2 * (WC["l2q2H21", {a,b,i,j}]+WC["l2q2H22", {a,b,i,j}]+WC["l2q2H23", {a,b,i,j}]+WC["l2q2H24", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["q2H2D31", {j,i}]\[Conjugate]-WC["q2H2D32", {j,i}]\[Conjugate]+WC["q2H2D33", {j,i}]\[Conjugate]-WC["q2H2D34", {j,i}]\[Conjugate])+
                                        gZ[d,Left,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
        
        FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["ld", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * (WC["l2d2H21", {a,b,i,j}]+WC["l2d2H22", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Left,{a,b}]*(WC["d2H2D31", {j,i}]\[Conjugate]-WC["d2H2D32", {j,i}]\[Conjugate])+
                                        gZ[d,Right,{i,j}]*(WC["l2H2D31", {a,b}]-WC["l2H2D32", {a,b}]+WC["l2H2D33", {a,b}]-WC["l2H2D34", {a,b}])),
		
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["eq", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * (WC["q2e2H21", {a,b,i,j}]+WC["q2e2H22", {a,b,i,j}])+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["q2H2D31", {j,i}]\[Conjugate]-WC["q2H2D32", {j,i}]\[Conjugate]+WC["q2H2D33", {j,i}]\[Conjugate]-WC["q2H2D34", {j,i}]\[Conjugate])+
                                        gZ[d,Left,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_d}]:> 
		\[Epsilon] * WC["ed", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * 1/2 * WC["e2d2H2", {a,b,i,j}]+
		f8 * \[Epsilon]^2 * 1/2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (gZ[e,Right,{a,b}]*(WC["d2H2D31", {j,i}]\[Conjugate]-WC["d2H2D32", {j,i}]\[Conjugate])+
                                        gZ[d,Right,{i,j}]*(WC["e2H2D31", {a,b}]-WC["e2H2D32", {a,b}])),
		
		(* energy enhanced UP *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["l2q2D21", {a,b,i,j}]+WC["l2q2D22", {a,b,i,j}]-WC["l2q2D23", {a,b,i,j}]-WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D22", {a,b,i,j}]-WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{1,0}}, {Left,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["l2u2D21", {a,b,i,j}]+WC["l2u2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["l2u2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["q2e2D21", {a,b,i,j}]+WC["q2e2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Left}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["q2e2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * (WC["e2u2D21", {a,b,i,j}]+WC["e2u2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Right}, {a_,b_,i_u,j_u}]:> f8 * \[Epsilon]^2 * 2 * WC["e2u2D22", {a,b,i,j}],
		
		(* energy enhanced DOWN *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["l2q2D21", {a,b,i,j}]+WC["l2q2D22", {a,b,i,j}]+WC["l2q2D23", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D22", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{1,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["l2d2D21", {a,b,i,j}]+WC["l2d2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["l2d2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> f8 *  \[Epsilon]^2 * (WC["q2e2D21", {a,b,i,j}]+WC["q2e2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Left}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["q2e2D22", {a,b,i,j}],
		FF[Vector, {"regular",{1,0}}, {Right,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * (WC["e2d2D21", {a,b,i,j}]+WC["e2d2D22", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Right,Right}, {a_,b_,i_d,j_d}]:> f8 * \[Epsilon]^2 * 2 * WC["e2d2D22", {a,b,i,j}],
		
		(* CC *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> 
		\[Epsilon] * 2 * WC["lq3", {a,b,i,j}] * f6+
		f8 * \[Epsilon]^2 * (WC["l2q2H23",{a,b,i,j}]+I*WC["l2q2H25",{a,b,i,j}])+
		f8 * \[Epsilon]^2 * (-1) Mass[WBoson]^2/ConstantInput["vev"]^2 * ((WC["l2H2D33",{a,b}]-WC["l2H2D34",{b,a}]\[Conjugate])*KroneckerDelta[i,j]+
		                               (WC["q2H2D33",{j,i}]\[Conjugate]+WC["q2H2D34",{i,j}])*KroneckerDelta[a,b]),
		                               
		FF[Vector, {"regular",{0,0}}, {OrderlessPatternSequence[Right,_]}, {a_,b_,i_u,j_d}]:> 0,
		
		(* energy enhanced CC *)
		FF[Vector, {"regular",{1,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> f8 * \[Epsilon]^2 * 2 * (WC["l2q2D23", {a,b,i,j}]+WC["l2q2D24", {a,b,i,j}]),
		FF[Vector, {"regular",{0,1}}, {Left,Left}, {a_,b_,i_u,j_d}]:> f8 * \[Epsilon]^2 * 4 * (WC["l2q2D24", {a,b,i,j}]),
		
		(* Gauge coupling modifications *)
		(* NC *)
		FF[Vector, {Photon,0}, ___]:> 0,
		
		(* Gauge coupling modifications UP*)
		FF[Vector, {ZBoson,0}, {Left,Left}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hq1",{i,j}] - WC["Hq3",{i,j}])+gZ[u,Left,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["q2H4D1",{i,j}]-2*WC["q2H4D2",{i,j}])+gZ[u,Left,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]-WC["q2H2D33",{i,j}]+WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Left,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Left,Right}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hu",{i,j}])+gZ[u,Right,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hu",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["u2H4D",{i,j}])+gZ[u,Right,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["u2H2D31",{i,j}]-WC["u2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Right,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Left}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hq1",{i,j}] - WC["Hq3",{i,j}])+gZ[u,Left,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])*(WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*(WC["q2H4D1",{i,j}]-2*WC["q2H4D2",{i,j}])+gZ[u,Left,{i,j}]*(WC["e2H4D",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]-WC["q2H2D33",{i,j}]+WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Left,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Right}, {l_[a_],l_[b_],u[i_],u[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hu",{i,j}])+gZ[u,Right,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hu",{i,j}]*WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*WC["u2H4D",{i,j}]+gZ[u,Right,{i,j}]*WC["e2H4D",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["u2H2D31",{i,j}]-WC["u2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[u,Right,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		
		(* Gauge coupling modifications DOWN*)
		FF[Vector, {ZBoson,0}, {Left,Left}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hq1",{i,j}] + WC["Hq3",{i,j}])+gZ[d,Left,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["q2H4D1",{i,j}]+2*WC["q2H4D2",{i,j}])+gZ[d,Left,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]+WC["q2H2D33",{i,j}]-WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Left,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Left,Right}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Left,{a,b}]*(WC["Hd",{i,j}])+gZ[d,Right,{i,j}]*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hd",{i,j}])*(WC["Hl1",{a,b}] + WC["Hl3",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Left,{a,b}]*(WC["d2H4D",{i,j}])+gZ[d,Right,{i,j}]*(WC["l2H4D1",{a,b}]+2*WC["l2H4D2",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Left,{a,b}] * (WC["d2H2D31",{i,j}]-WC["d2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Right,{i,j}] * (WC["l2H2D31",{a,b}]-WC["l2H2D32",{a,b}]+WC["l2H2D33",{a,b}]-WC["l2H2D34",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Left}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hq1",{i,j}] + WC["Hq3",{i,j}])+gZ[d,Left,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])*(WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*(WC["q2H4D1",{i,j}]+2*WC["q2H4D2",{i,j}])+gZ[d,Left,{i,j}]*(WC["e2H4D",{a,b}]))+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["q2H2D31",{i,j}]-WC["q2H2D32",{i,j}]+WC["q2H2D33",{i,j}]-WC["q2H2D34",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Left,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		FF[Vector, {ZBoson,0}, {Right,Right}, {l_[a_],l_[b_],d[i_],d[j_]}]:> 
		f6 * (-2) * Mass[ZBoson]^2/ConstantInput["vev"]^2 * \[Epsilon] * (gZ[l,Right,{a,b}]*(WC["Hd",{i,j}])+gZ[d,Right,{i,j}]*(WC["He",{a,b}]))+
		f6 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (WC["Hd",{i,j}]*WC["He",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^2/ConstantInput["vev"]^2 * (-1) * (gZ[l,Right,{a,b}]*WC["d2H4D",{i,j}]+gZ[d,Right,{i,j}]*WC["e2H4D",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[l,Right,{a,b}] * (WC["d2H2D31",{i,j}]-WC["d2H2D32",{i,j}])+
		f8 * \[Epsilon]^2 * Mass[ZBoson]^4/ConstantInput["vev"]^4 * 1/2 * gZ[d,Right,{i,j}] * (WC["e2H2D31",{a,b}]-WC["e2H2D32",{a,b}]),
		
		
		(* CC *)
		
		(* Gauge coupling modifications CC*)
		FF[Vector, {WBoson,0}, {Left,Left}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:>
		f6 * \[Epsilon] * Mass[WBoson]^2/ConstantInput["vev"]^2 * 2 *(WC["Hl3",{b,a}]\[Conjugate]*KroneckerDelta[i,j]+WC["Hq3",{i,j}]*KroneckerDelta[a,b])+
		f6 * \[Epsilon] * Mass[WBoson]^2/ConstantInput["vev"]^2 * 2 * WC["Hl3",{b,a}]\[Conjugate] * WC["Hq3",{i,j}]+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * KroneckerDelta[i,j] * (WC["l2H4D2",{b,a}]\[Conjugate]-WC["l2H4D3",{b,a}]\[Conjugate]+WC["l2H4D4",{a,b}])+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * KroneckerDelta[a,b] * (WC["q2H4D2",{i,j}]-WC["q2H4D3",{i,j}]+WC["q2H4D4",{j,i}]\[Conjugate])+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * (-1) * KroneckerDelta[i,j] * (WC["l2H2D33",{a,b}]-WC["l2H2D34",{b,a}]\[Conjugate])+
		f8 * \[Epsilon]^2 * Mass[WBoson]^2/ConstantInput["vev"]^2 * (-1) * KroneckerDelta[a,b] * (WC["q2H2D34",{i,j}]-WC["q2H2D33",{j,i}]\[Conjugate]),
		
		FF[Vector, {WBoson,0}, {Left,Right}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:>
		f6 * \[Epsilon] Mass[WBoson]^2/ConstantInput["vev"]^2 * KroneckerDelta[a,b] * WC["Hud",{i,j}],
				
		(* Lepton Dipoles *)
		(* NC *)
		FF[DipoleL, {Photon,0}, {Left,_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gA[q,{i,j}] * \[Epsilon] * (ConstantInput["sW"]*WC["eW",{b,a}]\[Conjugate] - ConstantInput["cW"]*WC["eB",{b,a}]\[Conjugate]) * f6,
		FF[DipoleL, {ZBoson,0}, {Left,\[Chi]q_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gZ[q,\[Chi]q,{i,j}] * \[Epsilon] * (ConstantInput["cW"]*WC["eW",{b,a}]\[Conjugate] + ConstantInput["sW"]*WC["eB",{b,a}]\[Conjugate]) * f6,
		(* CC *)
		FF[DipoleL, {WBoson,0}, {Right,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{a,b}] * f6,
		FF[DipoleL, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> -Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{b,a}]\[Conjugate] * f6,
		
		(* Quark Dipoles *)
		(* NC *)
		FF[DipoleQ, {Photon,0}, {_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (ConstantInput["sW"]*WC["dW",{j,i}]\[Conjugate] - ConstantInput["cW"]*WC["dB",{j,i}]\[Conjugate]) * f6,
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (ConstantInput["cW"]*WC["dW",{j,i}]\[Conjugate] + ConstantInput["sW"]*WC["dB",{j,i}]\[Conjugate]) * f6,
		
		FF[DipoleQ, {Photon,0}, {_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (ConstantInput["sW"]*WC["uW",{j,i}]\[Conjugate] + ConstantInput["cW"]*WC["uB",{j,i}]\[Conjugate]) * f6,
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (ConstantInput["cW"]*WC["uW",{j,i}]\[Conjugate] - ConstantInput["sW"]*WC["uB",{j,i}]\[Conjugate]) * f6,
		
		(* CC *)
		FF[DipoleQ, {WBoson,0}, {Left,Right},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> - Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["dW",{i,j}] * f6,
		FF[DipoleQ, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/ConstantInput["sW"] * \[Epsilon] * KroneckerDelta[a,b] * WC["uW",{j,i}]\[Conjugate] * f6
	};
	
	list = list /. RuleDelayed->$DelayedRule;
	list = list /. {f6->If[dim<=4,0,1], f8->If[dim<=6,0,1]};
	list = list /. $DelayedRule -> RuleDelayed;
	Return[list]
]


(* ::Section:: *)
(*Gauge boson couplings*)


(* ::Subsection:: *)
(*Photon*)


(* SM coupling of the photon *)
gA[particle_, {p_,r_}]:= Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]] * Charge[particle] * KroneckerDelta[p,r]


(* ::Subsection:: *)
(*Z boson*)


(* SM coupling of the Z boson *)
gZ[particle_, chirality_,{p_,r_}]:= Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/(ConstantInput["sW"]*ConstantInput["cW"])*(WeakIsospin3[particle, chirality] - ConstantInput["sW"]^2*Charge[particle])*KroneckerDelta[p,r]


(* ::Subsubsection:: *)
(*Modifications*)


\[CapitalDelta]gZe[Left, \[Epsilon]_, {a_,b_}]:= - Sqrt[\[Pi]*ConstantInput["\[Alpha]EM"]]/(ConstantInput["sW"]*ConstantInput["cW"]) * \[Epsilon] * (WC["Hl1",{a,b}] + WC["Hl3",{a,b}])
\[CapitalDelta]gZe[Right, \[Epsilon]_, {a_,b_}]:= - Sqrt[\[Pi]*ConstantInput["\[Alpha]EM"]]/(ConstantInput["sW"]*ConstantInput["cW"]) * \[Epsilon] * WC["He",{a,b}]


\[CapitalDelta]gZd[Left, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*ConstantInput["\[Alpha]EM"]]/(ConstantInput["sW"]*ConstantInput["cW"]) * \[Epsilon] * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])
\[CapitalDelta]gZd[Right, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*ConstantInput["\[Alpha]EM"]]/(ConstantInput["sW"]*ConstantInput["cW"]) * \[Epsilon] * WC["Hd",{i,j}]


\[CapitalDelta]gZu[Left, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*ConstantInput["\[Alpha]EM"]]/(ConstantInput["sW"]*ConstantInput["cW"]) * \[Epsilon] * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])
\[CapitalDelta]gZu[Right, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*ConstantInput["\[Alpha]EM"]]/(ConstantInput["sW"]*ConstantInput["cW"]) * \[Epsilon] * WC["Hu",{i,j}]


(* ::Subsection:: *)
(*W boson*)


(* SM coupling of the W boson in weak eigenbasis -> flavor diagonal *)
gW[{p_,r_}]:= Sqrt[4*\[Pi]*ConstantInput["\[Alpha]EM"]]/(Sqrt[2]*ConstantInput["sW"]) * KroneckerDelta[p,r]


(* ::Section:: *)
(*Canonize FF*)


CanonizeFF = Dispatch[
	{
	(* CC *)
	FF[Vector, type_, {X_,Y_}, {a_,b_,i_d,j_u}]                        :> FF[Vector, type, {X,Y}, {b,a,j,i}]\[Conjugate],
	FF[lorentz:(Scalar|Tensor), type_, {Right,Right}, {a_,b_,i_d,j_u}] :> FF[lorentz, type, {Left,Left}, {b,a,j,i}]\[Conjugate],
	FF[lorentz:(Scalar|Tensor), type_, {Left,Left}, {a_,b_,i_d,j_u}]   :> FF[lorentz, type, {Right,Right}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Left,Right}, {a_,b_,i_d,j_u}]                   :> FF[Scalar, type, {Right,Left}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Right,Left}, {a_,b_,i_d,j_u}]                   :> FF[Scalar, type, {Left,Right}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Left,X_}, {a_,b_,i_d,j_u}]                     :> - FF[DipoleL, type, {Right,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Right,X_}, {a_,b_,i_d,j_u}]                    :> - FF[DipoleL, type, {Left,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Left}, {a_,b_,i_d,j_u}]                     :> - FF[DipoleQ, type, {X,Right}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Right}, {a_,b_,i_d,j_u}]                    :> - FF[DipoleQ, type, {X,Left}, {b,a,j,i}]\[Conjugate],
	(* NC *)
	FF[lorentz:(Scalar|Tensor), type_, {Right,Right}, {a_,b_,i_u,j_u}] :> FF[lorentz, type, {Left,Left}, {b,a,j,i}]\[Conjugate],
	FF[lorentz:(Scalar|Tensor), type_, {Right,Right}, {a_,b_,i_d,j_d}] :> FF[lorentz, type, {Left,Left}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Right,Left}, {a_,b_,i_u,j_u}]                    :> FF[Scalar, type, {Left,Right}, {b,a,j,i}]\[Conjugate],
	FF[Scalar, type_, {Right,Left}, {a_,b_,i_d,j_d}]                    :> FF[Scalar, type, {Left,Right}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Right,X_}, {a_,b_,i_u,j_u}]                     :> - FF[DipoleL, type, {Left,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleL, type_, {Right,X_}, {a_,b_,i_d,j_d}]                     :> - FF[DipoleL, type, {Left,X}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Right}, {a_,b_,i_u,j_u}]                     :> - FF[DipoleQ, type, {X,Left}, {b,a,j,i}]\[Conjugate],
	FF[DipoleQ, type_, {X_,Right}, {a_,b_,i_d,j_d}]                     :> - FF[DipoleQ, type, {X,Left}, {b,a,j,i}]\[Conjugate]
	}
];


(* ::Section:: *)
(*Hermitian WC*)


HermitianWC4= Alternatives[
	(* Psi^4 *)
	"lq1", "lq3", "eu", "ed", "lu", "ld", "eq",
	(* Psi^4 H^2 *)
	"l2q2H21","l2q2H22","l2q2H23","l2q2H24","l2q2H25",
	"l2u2H21","l2u2H22","l2d2H21","l2d2H22",
	"q2e2H21","q2e2H22",
	"e2u2H2", "e2d2H2",
	(* Psi^4 D^2 *)
	"l2q2D21","l2q2D22","l2q2D23","l2q2D24",
	"l2u2D21","l2u2D22",
	"l2d2D21","l2d2D22",
	"q2e2D21","q2e2D22",
	"e2u2D21","e2u2D22",
	"e2d2D21","e2d2D22"
];


HermitianWC2= Alternatives[
	(* Psi^2 H^2 D *)
	"Hl1", "Hl3", "He", "Hq1", "Hq3", "Hu", "Hd",
	(* Psi^2 H^4 D *)
	"l2H4D1","l2H4D2","l2H4D3","l2H4D4",
	"q2H4D1","q2H4D2","q2H4D3","q2H4D4",
	"e2H4D","u2H4D","d2H4D",
	(* Psi^2 H^2 D^3 *)
	"l2H2D31","l2H2D32","l2H2D33","l2H2D34",
	"e2H2D31","e2H2D32",
	"q2H2D31","q2H2D32","q2H2D33","q2H2D34",
	"u2H2D31","u2H2D32",
	"d2H2D31","d2H2D32"
];


(* ::Subsection:: *)
(*Index relabeling redundancies*)


(* ::Subsubsection:: *)
(*4 fermion operators*)


WC[herm:HermitianWC4,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WC[herm,{b,a,j,i}]\[Conjugate] /; a>b


WC[herm:HermitianWC4,{a_Integer,b_Integer,i_Integer,j_Integer}]:= WC[herm,{b,a,j,i}]\[Conjugate] /; (a==b && i>j)


(* ::Subsubsection:: *)
(*2 fermion operators*)


WC[herm:HermitianWC2,{p_Integer,r_Integer}]:= WC[herm,{r,p}]\[Conjugate] /; p>r


(* ::Subsection:: *)
(*Real coefficients on diagonal*)


WC/:Conjugate[WC[herm:HermitianWC4,{a_Integer,a_Integer,i_Integer,i_Integer}]]:= WC[herm,{a,a,i,i}]


WC/:Conjugate[WC[herm:HermitianWC2,{p_Integer,p_Integer}]]:= WC[herm,{p,p}]


(* ::Section:: *)
(*WC argument check*)


WC::unknownWClabel= "The label `1` is not an allowed label for Wilson coefficients (WC)."


WC::wrongindexnumber= "The number of flavor indices specified for the Wilson coefficients is incorrect: `1`"


(* ::Subsubsection:: *)
(*psi^2*)


$WCList2=List[
	(* Psi^2 H^2 D *)
	"Hl1", "Hl3", "He", "Hq1", "Hq3", "Hu", "Hd",
	(* Psi^2 H^4 D *)
	"l2H4D1","l2H4D2","l2H4D3","l2H4D4",
	"q2H4D1","q2H4D2","q2H4D3","q2H4D4",
	"e2H4D","u2H4D","d2H4D",
	(* Psi^2 H^2 D^3 *)
	"l2H2D31","l2H2D32","l2H2D33","l2H2D34",
	"e2H2D31","e2H2D32",
	"q2H2D31","q2H2D32","q2H2D33","q2H2D34",
	"u2H2D31","u2H2D32",
	"d2H2D31","d2H2D32",
	
	(* non-hermitain *)
	"Hud", "eW", "eB", "uW", "uB", "dW", "dB"
]


(* ::Subsubsection:: *)
(*psi^4*)


$WCList4=List[
	(* Psi^4 *)
	"lq1", "lq3", "eu", "ed", "lu", "ld", "eq",
	(* Psi^4 H^2 *)
	"l2q2H21","l2q2H22","l2q2H23","l2q2H24","l2q2H25",
	"l2u2H21","l2u2H22","l2d2H21","l2d2H22",
	"q2e2H21","q2e2H22",
	"e2u2H2", "e2d2H2",
	(* Psi^4 D^2 *)
	"l2q2D21","l2q2D22","l2q2D23","l2q2D24",
	"l2u2D21","l2u2D22",
	"l2d2D21","l2d2D22",
	"q2e2D21","q2e2D22",
	"e2u2D21","e2u2D22",
	"e2d2D21","e2d2D22",
	
	(* non-hermitian *)
	"ledq", "lequ1", "lequ3"
];


(* ::Subsection:: *)
(*Check WC label*)


WC[l:Except[Alternatives@@Join[$WCList2, $WCList4, {_Pattern, _Blank, _Except, _BlankNullSequence, _BlankSequence}]],___]:=(
	Message[WC::unknownWClabel,l];
	Abort[]
)


(* ::Subsection:: *)
(*Check WC indices*)


(*WC[l:Alternatives@@$WCList2, ind:Except[{_,_}]]:=(
	Message[WC::wrongindexnumber, TraditionalForm[HoldForm[$wc[l,ind]]/.$wc->WC]];
	Abort[]
)*)


(*WC[l:Alternatives@@$WCList4, ind:Except[{_,_,_,_}]]:=(
	Message[WC::wrongindexnumber, TraditionalForm[HoldForm[$wc[l,ind]]/.$wc->WC]];
	Abort[]
)*)
