(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`Mediator`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["SubstituteFF"]


PackageExport["Coupling"]


PackageScope["$MediatorList"]


PackageScope["SubstitutionRulesMediators"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Couplings*)


Coupling::usage=
"Coupling[\"x1L\",{r,s}]
	Coupling of an internally defined mediator. r and s generically denote either quark or lepton flavor indices.";


(* removes particle heads from couplings *)
Coupling[label_,{x_[p_],y_[r_]}] := Coupling[label,{p,r}]


(* ::Subsection:: *)
(*Formatting*)


Format[Coupling[label_,{indices__}],TraditionalForm]:= Module[
	{last=StringTake[ToString[label],-1],name},
	Switch[last,
		"t",
			name=OverTilde[StringTake[ToString[label],1]];,
		"b",
			name=OverBar[StringTake[ToString[label],1]];,
		_,
			name=StringTake[ToString[label],1]
	];
	(*Subsuperscript[
		name,
		Underscript[StringTake[label,2;;2],StringJoin[ToString/@{indices}]],
		StringTake[label,3;;3]
	]*)
	DisplayForm@SubscriptBox[
		RowBox[{"[",Subsuperscript[
			name,
			StringTake[label,2;;2],
			StringTake[label,3;;3]
		],"]"}],
		StringJoin[ToString/@{indices}]
	]
]


(* ::Subsection:: *)
(*Flavor indices*)


(*(* remove unwanted heads *)
WC[x_,{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:= WC[x,{a,b,i,j}]
WC[x_,{f1_[a_],f2_[b_]}]:= WC[x,{a,b}]*)


(* ::Section:: *)
(*Mediators*)


$MediatorList= <|
	"Zp"  -> {{"s"}, {"NC"},      {Vector}},
	(*"Zt",  -> {"s", "CC",      {Vector}}*)
	"Wp"  -> {{"s"}, {"NC","CC"}, {Vector}},
	"S1"  -> {{"u"}, {"NC","CC"}, {Scalar,Vector,Tensor}},
	"S1t" -> {{"u"}, {"NC"},      {Vector}},
	"U1"  -> {{"t"}, {"NC","CC"}, {Scalar,Vector}},
	"U1t" -> {{"t"}, {"NC"},      {Vector}},
	"R2"  -> {{"t"}, {"NC","CC"}, {Scalar,Vector,Tensor}},
	"R2t" -> {{"t"}, {"NC"},      {Vector}},
	"V2"  -> {{"u"}, {"NC","CC"}, {Scalar,Vector}},
	"V2t" -> {{"u"}, {"NC"},      {Vector}},
	"S3"  -> {{"u"}, {"NC","CC"}, {Vector}},
	"U3"  -> {{"t"}, {"NC","CC"}, {Vector}}
|>


$MediatorCouplings= <|
	"Zp" -> {Coupling["g1u",{i,j}],Coupling["g1d",{i,j}],Coupling["g1e",{\[Alpha],\[Beta]}],Coupling["g1q",{i,j}],Coupling["g1l",{\[Alpha],\[Beta]}]},
	(*"Zt" -> {Coupling["g1qt",{i,j}],Coupling["g1lt",{\[Alpha],\[Beta]}]},*)
	"Wp" -> {Coupling["g3q",{i,j}],Coupling["g3l",{\[Alpha],\[Beta]}]},
	"S1" -> {Coupling["y1L",{i,\[Alpha]}],Coupling["y1R",{i,\[Alpha]}],Coupling["y1Rb",{i,\[Alpha]}]},
	"S1t" -> {Coupling["y1Rt",{i,\[Alpha]}]},
	"U1" -> {Coupling["x1L",{i,\[Alpha]}], Coupling["x1R",{i,\[Alpha]}],Coupling["x1Rb",{i,\[Alpha]}]},
	"U1t" -> {Coupling["x1Rt",{i,\[Alpha]}]},
	"R2" -> {Coupling["y2L",{i,\[Alpha]}],Coupling["y2R",{i,\[Alpha]}]},
	"R2t" -> {Coupling["y2Lt",{i,\[Alpha]}],Coupling["y2Rt",{i,\[Alpha]}]},
	"V2" -> {Coupling["x2L",{i,\[Alpha]}],Coupling["x2R",{i,\[Alpha]}]},
	"V2t" -> {Coupling["x2Lt",{i,\[Alpha]}],Coupling["x2Rt",{i,\[Alpha]}]},
	"S3" -> {Coupling["y3L",{i,\[Alpha]}]},
	"U3" -> {Coupling["x3L",{i,\[Alpha]}]}
|>


(*$MediatorLagrangians= <|
	"U1" -> "L"
|>*)


(* ::Section:: *)
(*Hermitian couplings*)


HermitianCouplings= Alternatives[
	(* Z' *)
	"g1u","g1d","g1e","g1q","g1l",
	(* W' *)
	"g3q","g3l"
]


Coupling[herm:HermitianCouplings,{p_Integer,r_Integer}]:= Coupling[herm,{r,p}]\[Conjugate] /; p>r


(* ::Section:: *)
(*Matching the form-factors to the mediators*)


(* ::Subsection:: *)
(*SM mediators*)


(* ::Subsubsection:: *)
(*\[Gamma]*)


SubstitutionRulesMediators[Photon]={
	(* Vector *)
	(* NC *)
	FF[Vector, {Photon,SM}, {_,_}, {l_[a_],l_[b_],q_[i_],q_[j_]}] :> gA[l,{a,b}] * gA[q,{i,j}]
}


(* ::Subsubsection:: *)
(*Z*)


SubstitutionRulesMediators[ZBoson]={
	(* Vector *)
	(* NC *)
	FF[Vector, {ZBoson,SM}, {\[Chi]l_,\[Chi]q_}, {l_[a_],l_[b_],q_[i_],q_[j_]}] :> gZ[l,\[Chi]l,{a,b}] * gZ[q,\[Chi]q,{i,j}]
}


(* ::Subsubsection:: *)
(*W*)


SubstitutionRulesMediators[WBoson]={
	(* Vector *)
	(* NC *)
	FF[Vector, {WBoson,SM}, {Left,Left}, {l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> gW[{a,b}] * gW[{i,j}]
}


(* ::Subsubsection:: *)
(*SM gauge couplings*)


(* ::Text:: *)
(*Photon*)


(* SM coupling of the photon *)
gA[particle_, {p_,r_}]:= Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] * Charge[particle] * KroneckerDelta[p,r]


(* ::Text:: *)
(*Z boson*)


(* SM coupling of the Z boson *)
gZ[particle_, chirality_,{p_,r_}]:= Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/(Param["sW"]*Param["cW"])*(WeakIsospin3[particle, chirality] - Param["sW"]^2*Charge[particle])*KroneckerDelta[p,r]


(* ::Text:: *)
(*W boson*)


(* SM coupling of the W boson in weak eigenbasis -> flavor diagonal *)
gW[{p_,r_}]:= Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]/(Sqrt[2]*Param["sW"]) * KroneckerDelta[p,r]


(* ::Subsection:: *)
(*NP mediators*)


(* ::Subsubsection:: *)
(*Z'*)


SubstitutionRulesMediators["Zp"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"Zp",0}, {Left,Left},   {a_,b_,i_u,j_u}] :> Coupling["g1q",{i,j}]Coupling["g1l",{a,b}],
	FF[Vector, {"Zp",0}, {Left,Right},  {a_,b_,i_u,j_u}] :> Coupling["g1u",{i,j}]Coupling["g1l",{a,b}],
	FF[Vector, {"Zp",0}, {Right,Left},  {a_,b_,i_u,j_u}] :> Coupling["g1q",{i,j}]Coupling["g1e",{a,b}],
	FF[Vector, {"Zp",0}, {Right,Right}, {a_,b_,i_u,j_u}] :> Coupling["g1u",{i,j}]Coupling["g1e",{a,b}],
	FF[Vector, {"Zp",0}, {Left,Left},   {a_,b_,i_d,j_d}] :> Coupling["g1q",{i,j}]Coupling["g1l",{a,b}],
	FF[Vector, {"Zp",0}, {Left,Right},  {a_,b_,i_d,j_d}] :> Coupling["g1d",{i,j}]Coupling["g1l",{a,b}],
	FF[Vector, {"Zp",0}, {Right,Left},  {a_,b_,i_d,j_d}] :> Coupling["g1q",{i,j}]Coupling["g1e",{a,b}],
	FF[Vector, {"Zp",0}, {Right,Right}, {a_,b_,i_d,j_d}] :> Coupling["g1d",{i,j}]Coupling["g1e",{a,b}]
}


(* ::Subsubsection:: *)
(*Z'~*)


(*
SubstitutionRulesMediators["Zt"]={}
*)


(* ::Subsubsection:: *)
(*W'*)


SubstitutionRulesMediators["Wp"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"Wp",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> -Coupling["g1q",{i,j}]Coupling["g1l",{a,b}],
	FF[Vector, {"Wp",0}, {Left,Left}, {a_,b_,i_d,j_d}] :> Coupling["g1q",{i,j}]Coupling["g1l",{a,b}],
	(* CC *)
	FF[Vector, {"Wp",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> 2 Coupling["g1q",{i,j}]Coupling["g1l",{a,b}]
}


FF[_, {"Wp",0}, {OrderlessPatternSequence[Right,_]}, _] = 0


(* ::Subsubsection:: *)
(*S1*)


SubstitutionRulesMediators["S1"]={
	(* Scalar *)
	(* NC *)
	FF[Scalar, {"S1",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> 1/2 Coupling["y1L",{j,b}]Coupling["y1R",{i,a}]\[Conjugate],
	(* CC *)
	FF[Scalar, {"S1",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> -(1/2)Coupling["y1L",{j,b}]Coupling["y1R",{i,a}]\[Conjugate],
	
	(* Vector *)
	(* NC *)
	FF[Vector, {"S1",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> -(1/2)Coupling["y1L",{j,b}]Coupling["y1L",{i,a}]\[Conjugate],
	FF[Vector, {"S1",0}, {Right,Right}, {a_,b_,i_u,j_u}] :> -(1/2)Coupling["y1R",{j,b}]Coupling["y1R",{i,a}]\[Conjugate],
	(* CC *)
	FF[Vector, {"S1",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> 1/2 Coupling["y1L",{j,b}]Coupling["y1L",{i,a}]\[Conjugate],
	
	(* Tensor *)
	(* NC *)
	FF[Tensor, {"S1",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> -(1/8)Coupling["y1L",{j,b}]Coupling["y1R",{i,a}]\[Conjugate],
	(* CC *)
	FF[Tensor, {"S1",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> 1/8 Coupling["y1L",{j,b}]Coupling["y1R",{i,a}]\[Conjugate]
}


FF[_, {"S1",0}, _, {_,_,_d,_d}] = 0
FF[_, {"S1",0}, {OrderlessPatternSequence[Right,Left]}, _] = 0
FF[Scalar|Tensor, {"S1",0}, {Right,Right}, {_,_,_u,_d}] = 0
FF[Scalar|Tensor, {"S1",0}, {Left,Left}, {_,_,_d,_u}] = 0
FF[Vector, {"S1",0}, {Right,Right}, {_,_,_u,_d}|{_,_,_d,_u}] = 0


(* ::Subsubsection:: *)
(*S1~*)


SubstitutionRulesMediators["S1t"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"S1t",0}, {Right,Right}, {a_,b_,i_d,j_d}] :> 1/2 Coupling["y1Rt",{j,b}]Coupling["y1Rt",{i,a}]\[Conjugate]
}


FF[_, {"S1t",0}, {OrderlessPatternSequence[Left,_]}, _] = 0
FF[_, {"S1t",0}, _, {_,_,_u,_u}] = 0


(* ::Subsubsection:: *)
(*U1*)


SubstitutionRulesMediators["U1"]={
	(* Scalar *)
	(* NC *)
	FF[Scalar,{"U1",0}, {Left,Right},   {a_,b_,i_d,j_d}] :> -2 Coupling["x1L",{i,b}]Coupling["x1R",{j,a}]\[Conjugate],
	(* CC *)
	FF[Scalar,{"U1",0}, {Left,Right},   {a_,b_,i_u,j_d}] :> -2 Coupling["x1L",{i,b}]Coupling["x1R",{j,a}]\[Conjugate],
	
	(* Vector *)
	(* NC *)
	FF[Vector, {"U1",0}, {Left,Left},   {a_,b_,i_d,j_d}] :> Coupling["x1L",{j,a}]\[Conjugate]Coupling["x1L",{i,b}],
	FF[Vector, {"U1",0}, {Right,Right}, {a_,b_,i_d,j_d}] :> Coupling["x1R",{j,a}]\[Conjugate]Coupling["x1R",{i,b}],
	(* CC *)
	FF[Vector, {"U1",0}, {Left,Left},   {a_,b_,i_u,j_d}] :> Coupling["x1L",{j,a}]\[Conjugate]Coupling["x1L",{i,b}]
}


FF[_, {"U1",0}, _, {_,_,_u,_u}] = 0
FF[Vector, {"U1",0}, {OrderlessPatternSequence[Left,Right]}, _] = 0

FF[Scalar, {"U1",0}, {Right,_}, {_,_,_u,_d}] = 0
FF[Scalar, {"U1",0}, {Left,_}, {_,_,_d,_u}] = 0

FF[Scalar, {"U1",0}, {Right,Right}, _] = 0
FF[Scalar, {"U1",0}, {Left,Left}, _] = 0


(* ::Subsubsection:: *)
(*U1~*)


SubstitutionRulesMediators["U1t"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"U1t",0}, {Right,Right}, {a_,b_,i_u,j_u}] :> Coupling["x1Rt",{i,b}]Coupling["x1Rt",{j,a}]\[Conjugate]
}


FF[_, {"U1t",0}, _, {_,_,_d,_d}] = 0
FF[_, {"U1t",0}, {OrderlessPatternSequence[Left,_]}, _] = 0


(* ::Subsubsection:: *)
(*R2*)


SubstitutionRulesMediators["R2"]={
	(* Scalar *)
	(* NC *)
	FF[Scalar, {"R2",0}, {Left,Left},   {a_,b_,i_u,j_u}] :> -(1/2)Coupling["y2R",{j,a}]\[Conjugate]Coupling["y2L",{i,b}],
	(* CC *)
	FF[Scalar, {"R2",0}, {Left,Left},   {a_,b_,i_u,j_d}] :> 1/2 Coupling["y2R",{j,a}]\[Conjugate]Coupling["y2L",{i,b}],

	(* Vector *)
	(* NC *)
	FF[Vector, {"R2",0}, {Left,Right},  {a_,b_,i_u,j_u}] :> 1/2 Coupling["y2L",{j,a}]\[Conjugate]Coupling["y2L",{i,b}],
	FF[Vector, {"R2",0}, {Right,Left},  {a_,b_,i_u,j_u}] :> 1/2 Coupling["y2R",{j,a}]\[Conjugate]Coupling["y2R",{i,b}],
	FF[Vector, {"R2",0}, {Right,Left},  {a_,b_,i_d,j_d}] :> 1/2 Coupling["y2R",{j,a}]\[Conjugate]Coupling["y2R",{i,b}],
	
	(* Tensor *)
	(* NC *)
	FF[Tensor, {"R2",0}, {Left,Left},   {a_,b_,i_u,j_u}] :> -(1/8)Coupling["y2R",{j,a}]\[Conjugate]Coupling["y2L",{i,b}],
	(* CC *)
	FF[Tensor, {"R2",0}, {Left,Left},   {a_,b_,i_u,j_d}] :> 1/8 Coupling["y2R",{j,a}]\[Conjugate]Coupling["y2L",{i,b}]
}


FF[Scalar|Tensor, {"R2",0}, {Right,_}, {_,_,_u,_d}] = 0
FF[Scalar|Tensor, {"R2",0}, {Left,_}, {_,_,_d,_u}]  = 0

FF[Scalar|Tensor, {"R2",0}, _, {_,_,_d,_d}] = 0
FF[Scalar, {"R2",0}, {OrderlessPatternSequence[Left,Right]}, _] = 0

FF[Vector, {"R2",0}, {Left,Left}, _] = 0
FF[Vector, {"R2",0}, {Right,Right}, _] = 0
FF[Vector, {"R2",0}, {Left,Right}, {_,_,_d,_d}] = 0

FF[Vector, {"R2",0}, _, {_,_,_u,_d}|{_,_,_d,_u}] = 0


(* ::Subsubsection:: *)
(*R2~*)


SubstitutionRulesMediators["R2t"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"R2t",0}, {Left,Right}, {a_,b_,i_d,j_d}] :> 1/2 Coupling["y2Lt",{i,b}]Coupling["y2Lt",{j,a}]\[Conjugate]
}


FF[_, {"R2t",0}, _, {_,_,_u,_u}] = 0
FF[_, {"R2t",0}, {Left,Left}, _] = 0
FF[_, {"R2t",0}, {Right,Left}, _] = 0
FF[_, {"R2t",0}, {Right,Right}, _] = 0


(* ::Subsubsection:: *)
(*V2*)


SubstitutionRulesMediators["V2"]={
	(* Scalar *)
	(* NC *)
	FF[Scalar, {"V2",0}, {Left,Right}, {a_,b_,i_d,j_d}] :> 2 Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2L",{j,b}],
	(* CC *)
	FF[Scalar, {"V2",0}, {Left,Right}, {a_,b_,i_u,j_d}] :> 2 Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2L",{j,b}],
	
	(* Vector *)
	(* NC *)
	FF[Vector, {"V2",0}, {Right,Left}, {a_,b_,i_u,j_u}] :> - Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2R",{j,b}],
	FF[Vector, {"V2",0}, {Left,Right}, {a_,b_,i_d,j_d}] :> - Coupling["x2L",{i,a}]\[Conjugate]Coupling["x2L",{j,b}],
	FF[Vector, {"V2",0}, {Right,Left}, {a_,b_,i_d,j_d}] :> - Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2R",{j,b}]
}


FF[Vector, {"V2",0}, _, {_,_,_u,_d}|{_,_,_d,_u}] = 0
FF[Vector, {"V2",0}, {Left,Left}|{Right,Right}, _] = 0
FF[Vector, {"V2",0}, {Left,Right}, {_,_,_u,_u}] = 0
FF[Scalar, {"V2",0}, _, {_,_,_u,_u}] = 0
FF[Scalar, {"V2",0}, {Left,Left}|{Right,Right}, _] = 0
FF[Scalar, {"V2",0}, {Left,Right}, {_,_,_d,_u}] = 0
FF[Scalar, {"V2",0}, {Right,Left}, {_,_,_u,_d}] = 0


(* ::Subsubsection:: *)
(*V2~*)


SubstitutionRulesMediators["V2t"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"V2t",0}, {Left,Right}, {a_,b_,i_u,j_u}] :> Coupling["x2Lt",{i,a}]\[Conjugate]Coupling["x2Lt",{j,b}]
}


FF[_, {"V2t",0}, _, {_,_,_d,_d}] = 0
FF[_, {"V2t",0}, {Right,Right}, _] = 0
FF[_, {"V2t",0}, {Left,Left}, _] = 0
FF[_, {"V2t",0}, {Right,Left}, _] = 0


(* ::Subsubsection:: *)
(*S3*)


SubstitutionRulesMediators["S3"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"S3",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> -(1/2)Coupling["y3L",{j,b}]Coupling["y3L",{i,a}]\[Conjugate],
	FF[Vector, {"S3",0}, {Left,Left}, {a_,b_,i_d,j_d}] :> -Coupling["y3L",{j,b}]Coupling["y3L",{i,a}]\[Conjugate],
	(* CC *)
	FF[Vector, {"S3",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> -(1/2)Coupling["y3L",{j,b}]Coupling["y3L",{i,a}]\[Conjugate]
}


FF[_, {"S3",0}, {OrderlessPatternSequence[Right,_]}, _] = 0


(* ::Subsubsection:: *)
(*U3*)


SubstitutionRulesMediators["U3"]={
	(* Vector *)
	(* NC *)
	FF[Vector, {"U3",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> 2 Coupling["x3L",{i,b}]Coupling["x3L",{j,a}]\[Conjugate],
	FF[Vector, {"U3",0}, {Left,Left}, {a_,b_,i_d,j_d}] :> Coupling["x3L",{i,b}]Coupling["x3L",{j,a}]\[Conjugate],
	(* CC *)
	FF[Vector, {"U3",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> -Coupling["x3L",{i,b}]Coupling["x3L",{j,a}]\[Conjugate]
}


FF[_, {"U3",0}, {OrderlessPatternSequence[Right,_]}, _] = 0


(* ::Section:: *)
(*Substitute form-factors*)


SubstituteFF::usage= "SubstituteFF[expr]
	Substitutes all form-factors FF[...] in the given argument expr by the corresponding Wilson coefficients or coupling constants depenting on the run mode.
	If running in the SMEFT mode, SubstituteFF takes the following options:
		EFTorder \[RuleDelayed] n,
			Specifies that the result is expanded up to and including terms of order \!\(\*SuperscriptBox[\(\[CapitalLambda]\), \(-n\)]\). The default is n=GetEFTorder[].
		OperatorDimension \[Rule] d,
			Specifies that EFT operators up to mass dimension d should be included. The default is d=GetOperatorDimension[].
		EFTscale -> \[CapitalLambda],
			Specifies the EFT cutoff scale used for the substitutions. The default is \[CapitalLambda]=1000 (GeV).
	In the mediator mode these Options are ignored."


SubstituteFF::remainingFF= "Not all form-factors have been replaced. The remaining FF are: `1`"


Options[SubstituteFF]= {
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	EFTscale             -> 1000
};


SubstituteFF[arg_, OptionsPattern[]]:= Module[
	{
		\[Epsilon], (* \[Epsilon] = \[Vee]^2/(\[CapitalLambda]^2) *)
		subst,
		temp= arg
	}
	,
	If[$RunMode === "SMEFT",
		(* make power counting parameter \[Epsilon] real *)
		\[Epsilon]/:Conjugate[\[Epsilon]]:= \[Epsilon];
		(* automatic truncation of EFT series *)
		(* much faster than any other way of truncating the EFT series! *)
		Switch[(OptionValue[EFTorder]/2),
			0, \[Epsilon]=0,
			_, \[Epsilon]/:Power[\[Epsilon],n_/;n>(OptionValue[EFTorder]/2)] = 0
		];
	];
	
	(* get mediator substitution rules *)
	subst = Flatten@Table[
		SubstitutionRulesMediators[med]
		,
		{med, Keys[GetMediators[]]}
	];
	
	(* add SMEFT substitution rules *)
	If[$RunMode === "SMEFT",
		subst = Join[
			subst,
			SubstitutionRulesSMEFT[OptionValue[OperatorDimension], \[Epsilon]]
		]
	];
	
	subst= Dispatch[subst];
	
	(* apply substitution rules *)
	temp= MyTiming[temp/.CanonizeFF, "FF canonization"];
	temp= MyTiming[temp/.subst, "FF substitution"];
	
	(* check that no form-factors are left *)
	If[!FreeQ[temp,_FF], Message[SubstituteFF::remainingFF, DeleteDuplicates@Cases[temp,_FF,All]]];
	
	(* substitute in constants *)
	temp= temp/.ReplaceConstants[];
	temp= ExpandConjugate[temp];
	
	(* truncate the EFT series at the desired order *)
	If[$RunMode === "SMEFT",
		(* next line no longer necessary with automatic EFT series truncation, applying Expand is enough *)
		(*
		temp= EchoTiming[Normal@Series[temp,{\[Epsilon],0,OptionValue[EFTorder]/2}], "EFT Series"];
		*)
		temp= MyTiming[Expand[temp],"Expand"];
		(* substitute in the power counting parameter *)
		temp= temp/.\[Epsilon] -> (Param["vev"]/OptionValue[EFTscale])^2;
		(* substitute vev *)
		temp= temp/.ReplaceConstants[];
	];
	
	(* result *)
	Return[temp/.{Complex[a_,0.]:> a, Complex[b_,0]:> b}/.{0.->0}]
]
