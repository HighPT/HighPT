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


PackageExport["Coupling"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Couplings*)


Coupling::usage=
"Coupling[\"x1L\",{r,s}]
	Coupling of an internally defined mediator. r and s generically denote either quark or lepton flavor indices.";


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
Subsuperscript[name,Underscript[StringTake[label,2;;2],StringJoin[ToString/@{indices}]],StringTake[label,3;;3]]
]


(* ::Subsection:: *)
(*Flavor indices*)


(*(* remove unwanted heads *)
WC[x_,{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:= WC[x,{a,b,i,j}]
WC[x_,{f1_[a_],f2_[b_]}]:= WC[x,{a,b}]*)


(* ::Section:: *)
(*Mediators*)


$MediatorList= {
	"Zp",
	(*"Zt",*)
	"Wp",
	"S1",
	"S1t",
	"U1",
	"U1t",
	"R2",
	"R2t",
	"V2",
	"V2t",
	"S3",
	"U3"
}


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


SubstitutionRulesMediators["Zp"]:={
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


(*SubstitutionRulesMediators["Zt"]:={}*)


SubstitutionRulesMediators["Wp"]:={
	(* Vector *)
	(* NC *)
	FF[Vector, {"Wp",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> -Coupling["g1q",{i,j}]Coupling["g1l",{a,b}],
	FF[Vector, {"Wp",0}, {Left,Left}, {a_,b_,i_d,j_d}] :> Coupling["g1q",{i,j}]Coupling["g1l",{a,b}],
	(* CC *)
	FF[Vector, {"Wp",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> 2 Coupling["g1q",{i,j}]Coupling["g1l",{a,b}]
}


SubstitutionRulesMediators["S1"]:={
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


SubstitutionRulesMediators["S1t"]:={
	(* Vector *)
	(* NC *)
	FF[Vector, {"S1t",0}, {Right,Right}, {a_,b_,i_d,j_d}] :> 1/2 Coupling["y1Rt",{j,b}]Coupling["y1Rt",{i,a}]\[Conjugate]
}


SubstitutionRulesMediators["U1"]:={
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


SubstitutionRulesMediators["U1t"]:={
	(* Vector *)
	(* NC *)
	FF[Vector, {"U1t",0}, {Right,Right}, {a_,b_,i_u,j_u}] :> Coupling["x1Rt",{i,b}]Coupling["x1Rt",{j,a}]\[Conjugate]
}


SubstitutionRulesMediators["R2"]:={
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


SubstitutionRulesMediators["R2t"]:={
	(* Vector *)
	(* NC *)
	FF[Vector, {"R2t",0}, {Left,Right}, {a_,b_,i_d,j_d}] :> 1/2 Coupling["y2Lt",{i,b}]Coupling["y2Lt",{j,a}]\[Conjugate]
}


SubstitutionRulesMediators["V2"]:={
	(* Scalar *)
	(* NC *)
	FF[Scalar, {"V2",0}, {Left,Right}, {a_,b_,i_d,j_d}] :> 2 Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2L",{j,b}],
	(* CC *)
	FF[Scalar, {"V2",0}, {Left,Right}, {a_,b_,i_u,j_d}] :> 2 Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2L",{j,b}],
	
	(* Vector *)
	(* NC *)
	FF[Vector, {"V2",0}, {Right,Left}, {a_,b_,i_u,j_u}] :> - Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2R",{j,b}],
	FF[Vector, {"V2",0}, {Left,Right}, {a_,b_,i_d,j_d}] :> - Coupling["x2L",{i,a}]\[Conjugate]Coupling["x2L",{j,b}],
	FF[Vector, {"V2",0}, {Left,Right}, {a_,b_,i_d,j_d}] :> - Coupling["x2R",{i,a}]\[Conjugate]Coupling["x2R",{j,b}]
}


SubstitutionRulesMediators["V2t"]:={
	(* Vector *)
	(* NC *)
	FF[Vector, {"V2t",0}, {Left,Right}, {a_,b_,i_u,j_u}] :> Coupling["x2Lt",{i,a}]\[Conjugate]Coupling["x2Lt",{j,b}]
}


SubstitutionRulesMediators["S3"]:={
	(* Vector *)
	(* NC *)
	FF[Vector, {"S3",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> -(1/2)Coupling["y3L",{j,b}]Coupling["y3L",{i,a}]\[Conjugate],
	FF[Vector, {"S3",0}, {Left,Left}, {a_,b_,i_d,j_d}] :> -Coupling["y3L",{j,b}]Coupling["y3L",{i,a}]\[Conjugate],
	(* CC *)
	FF[Vector, {"S3",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> -(1/2)Coupling["y3L",{j,b}]Coupling["y3L",{i,a}]\[Conjugate]
}


SubstitutionRulesMediators["U3"]:={
	(* Vector *)
	(* NC *)
	FF[Vector, {"U3",0}, {Left,Left}, {a_,b_,i_u,j_u}] :> 2 Coupling["x3L",{i,b}]Coupling["x3L",{j,a}]\[Conjugate],
	FF[Vector, {"U3",0}, {Left,Left}, {a_,b_,i_d,j_d}] :> Coupling["x3L",{i,b}]Coupling["x3L",{j,a}]\[Conjugate],
	(* CC *)
	FF[Vector, {"U3",0}, {Left,Left}, {a_,b_,i_u,j_d}] :> -Coupling["x3L",{i,b}]Coupling["x3L",{j,a}]\[Conjugate]
}
