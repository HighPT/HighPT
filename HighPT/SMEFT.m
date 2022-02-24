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


PackageExport["WCxf"]
PackageExport["MapToWCxf"]


PackageExport["SetEFTorder"]


PackageExport["GetEFTorder"]


PackageExport["SetOperatorDimension"]


PackageExport["GetOperatorDimension"]


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


Options[MatchToSMEFT]= {EFTorder :> GetEFTorder[], OperatorDimension :> GetOperatorDimension[]};


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
	temp= temp/.subst;
	
	(* check that no form-factors are left *)
	If[!FreeQ[temp,_FF], Message[MatchToSMEFT::remainingFF, DeleteDuplicates@Cases[temp,_FF,All]]];
	
	(* substitute in constants *)
	(*temp= temp/.ReplaceConstants[];   TO UNCOMMENT*)
	
	(* EFT truncation of results *)
	temp= Expand@ ExpandConjugate[temp];
	Switch[OptionValue[EFTorder],
		0, temp= temp /. \[Epsilon] -> 0,
		2, temp= temp /. Power[\[Epsilon],n_]/;n>1 -> 0,
		4, temp= temp /. Power[\[Epsilon],n_]/;n>2 -> 0
	];
	
		
	(* substitute in the power counting parameter *)
	temp= temp/.\[Epsilon] -> (VEV/\[CapitalLambda]NP)^2;
	(* substitute vev *)
	(*temp= temp/.ReplaceConstants[];    TO UNCOMMENT*)
	
	(* result *)
	Return[temp/.{Complex[a_,0.]:> a, Complex[b_,0]:> b}]
]


(* ::Subsection:: *)
(*Wilson coefficient (WC) substitution rules*)


(* returns the replacement rules to match the form-factors to the d=6 SMEFT in the Warsaw basis *)
SubstitutionRulesSMEFT[dim_, \[Epsilon]_]:= Module[{list,f6,f8},
	list= {
		(* SCALAR *)
		(* NC *)
		FF[Scalar, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ1", {a,b,i,j}] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		FF[Scalar, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ledq", {a,b,i,j}] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		FF[Scalar, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_u}]:> \[Epsilon] * WC["lequ1", {a,b,i,j}] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ1", {b,a,j,i}]\[Conjugate] * f6,
		FF[Scalar, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_u}]:> \[Epsilon] * WC["ledq", {a,b,i,j}] * f6,
		FF[Scalar, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["ledq", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Tensor *)
		(* NC *)
		FF[Tensor, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ3", {a,b,i,j}] * f6,
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> - \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		(* CC *)
		FF[Tensor, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_u}]:> \[Epsilon] * WC["lequ3", {a,b,i,j}] * f6,
		FF[Tensor, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * WC["lequ3", {b,a,j,i}]\[Conjugate] * f6,
		
		(* Vector *)
		(* NC *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_u}]:> \[Epsilon] * (WC["lq1", {a,b,i,j}] - WC["lq3", {a,b,i,j}]) * f6,
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_d,j_d}]:> \[Epsilon] * (WC["lq1", {a,b,i,j}] + WC["lq3", {a,b,i,j}]) * f6,
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_u,j_u}]:> \[Epsilon] * WC["eu", {a,b,i,j}] * f6,
		FF[Vector, {"regular",{0,0}}, {Right,Right}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ed", {a,b,i,j}] * f6,
		FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_u,j_u}]:> \[Epsilon] * WC["lu", {a,b,i,j}] * f6,
		FF[Vector, {"regular",{0,0}}, {Left,Right}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["ld", {a,b,i,j}] * f6,
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_u,j_u}]:> \[Epsilon] * WC["eq", {a,b,i,j}] * f6,
		FF[Vector, {"regular",{0,0}}, {Right,Left}, {a_,b_,i_d,j_d}]:> \[Epsilon] * WC["eq", {a,b,i,j}] * f6,
		(* CC *)
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_u,j_d}]:> \[Epsilon] * 2 * WC["lq3", {a,b,i,j}] * f6,
		FF[Vector, {"regular",{0,0}}, {Left,Left}, {a_,b_,i_d,j_u}]:> \[Epsilon] * 2 * WC["lq3", {a,b,i,j}] * f6,
		FF[Vector, {"regular",{0,0}}, {OrderlessPatternSequence[Right,_]}, {a_,b_,i_u,j_d}]:> 0,
		FF[Vector, {"regular",{0,0}}, {OrderlessPatternSequence[Right,_]}, {a_,b_,i_d,j_u}]:> 0,
		
		(* Standard Model *)
		(* NC *)
		FF[Vector, {Photon,SM}, {_,_}, {l_[a_],l_[b_],q_[i_],q_[j_]}]:> gA[l,{a,b}] * gA[q,{i,j}],
		FF[Vector, {ZBoson,SM}, {\[Chi]l_,\[Chi]q_}, {l_[a_],l_[b_],q_[i_],q_[j_]}]:> gZ[l,\[Chi]l,{a,b}] * gZ[q,\[Chi]q,{i,j}],
		(* CC *)
		FF[Vector, {WBoson,SM}, {Left,Left}, {l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> gW[{a,b}] * gW[{i,j}],
		
		(* Gauge coupling modifications *)
		(* NC *)
		FF[Vector, {Photon,0}, ___]:> 0,
		FF[Vector, {ZBoson,0}, {\[Chi]l_,\[Chi]q_}, {l_[a_],l_[b_],d[i_],d[j_]}]:> gZ[l,\[Chi]l,{a,b}] * \[CapitalDelta]gZd[\[Chi]q,\[Epsilon],{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * gZ[d,\[Chi]q,{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * \[CapitalDelta]gZd[\[Chi]q,\[Epsilon],{i,j}] * f6,
		FF[Vector, {ZBoson,0}, {\[Chi]l_,\[Chi]q_}, {l_[a_],l_[b_],u[i_],u[j_]}]:> gZ[l,\[Chi]l,{a,b}] * \[CapitalDelta]gZu[\[Chi]q,\[Epsilon],{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * gZ[u,\[Chi]q,{i,j}] * f6 + \[CapitalDelta]gZe[\[Chi]l,\[Epsilon],{a,b}] * \[CapitalDelta]gZu[\[Chi]q,\[Epsilon],{i,j}] * f6,
		(*
		FF[Vector, {Photon,1}, ___]:> 0, (* TO DO *)
		FF[Vector, {ZBoson,1}, ___]:> 0, (* TO DO *)
		*)
		(* CC *)
		FF[Vector, {WBoson,0}, {Left,Left}, {l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> 2*\[Pi]*\[Alpha]EM * \[Epsilon] * (KroneckerDelta[i,j] * WC["Hl3",{a,b}] + KroneckerDelta[a,b] * WC["Hq3",{i,j}])*f6 + 2*\[Pi]*\[Alpha]EM * \[Epsilon]^2 * WC["Hl3",{a,b}] * WC["Hq3",{i,j}]*f6,
		FF[Vector, {WBoson,0}, {Left,Right}, {l1_[a_],l2_[b_],u[i_],d[j_]}]:> \[Pi]*\[Alpha]EM * \[Epsilon] * KroneckerDelta[a,b] * WC["Hud",{i,j}] * f6,
		FF[Vector, {WBoson,0}, {Left,Right}, {l1_[a_],l2_[b_],d[i_],u[j_]}]:> \[Pi]*\[Alpha]EM * \[Epsilon] * KroneckerDelta[a,b] * WC["Hud",{j,i}]\[Conjugate] * f6,
		(*
		FF[Vector, {WBoson,1}, ___]:> 0, (* TO DO *)
		*)
		
		(* Lepton Dipoles *)
		(* NC *)
		FF[DipoleL, {Photon,0}, {Right,_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gA[q,{i,j}] * \[Epsilon] * (sW*WC["eW",{a,b}] - cW*WC["eB",{a,b}]) * f6,
		FF[DipoleL, {Photon,0}, {Left,_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> -Sqrt[2]* gA[q,{i,j}] * \[Epsilon] * (sW*WC["eW",{b,a}]\[Conjugate] - cW*WC["eB",{b,a}]\[Conjugate]) * f6,
		FF[DipoleL, {ZBoson,0}, {Right,\[Chi]q_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> Sqrt[2]* gZ[q,\[Chi]q,{i,j}] * \[Epsilon] * (cW*WC["eW",{a,b}] + sW*WC["eB",{a,b}]) * f6,
		FF[DipoleL, {ZBoson,0}, {Left,\[Chi]q_},{l_[a_],l_[b_],q_[i_],q_[j_]}]:> -Sqrt[2]* gZ[q,\[Chi]q,{i,j}] * \[Epsilon] * (cW*WC["eW",{b,a}]\[Conjugate] + sW*WC["eB",{b,a}]\[Conjugate]) * f6,
		(* CC *)
		FF[DipoleL, {WBoson,0}, {Right,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> - Sqrt[2]* Sqrt[4*\[Pi]*\[Alpha]EM]/sW * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{a,b}] * f6,
		FF[DipoleL, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],q1_[i_],q2_[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*\[Alpha]EM]/sW * \[Epsilon] * KroneckerDelta[i,j] * WC["eW",{b,a}]\[Conjugate] * f6,
		
		(* Quark Dipoles *)
		(* NC *)
		FF[DipoleQ, {Photon,0}, {_,Right},{l_[a_],l_[b_],d[i_],d[j_]}]:> Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (sW*WC["dW",{i,j}] - cW*WC["dB",{i,j}]) * f6,
		FF[DipoleQ, {Photon,0}, {_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (sW*WC["dW",{j,i}]\[Conjugate] - cW*WC["dB",{j,i}]\[Conjugate]) * f6,
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Right},{l_[a_],l_[b_],d[i_],d[j_]}]:> Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (cW*WC["dW",{i,j}] + sW*WC["dB",{i,j}]) * f6,
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Left},{l_[a_],l_[b_],d[i_],d[j_]}]:> - Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (cW*WC["dW",{j,i}]\[Conjugate] + sW*WC["dB",{j,i}]\[Conjugate]) * f6,
		
		FF[DipoleQ, {Photon,0}, {_,Right},{l_[a_],l_[b_],u[i_],u[j_]}]:> - Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (sW*WC["uW",{i,j}] + cW*WC["uB",{i,j}]) * f6,
		FF[DipoleQ, {Photon,0}, {_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gA[l,{a,b}] * \[Epsilon] * (sW*WC["uW",{j,i}]\[Conjugate] + cW*WC["uB",{j,i}]\[Conjugate]) * f6,
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Right},{l_[a_],l_[b_],u[i_],u[j_]}]:> - Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (cW*WC["uW",{i,j}] - sW*WC["uB",{i,j}]) * f6,
		FF[DipoleQ, {ZBoson,0}, {\[Chi]l_,Left},{l_[a_],l_[b_],u[i_],u[j_]}]:> Sqrt[2]* gZ[l,\[Chi]l,{a,b}] * \[Epsilon] * (cW*WC["uW",{j,i}]\[Conjugate] - sW*WC["uB",{j,i}]\[Conjugate]) * f6,
		
		(* CC *)
		FF[DipoleQ, {WBoson,0}, {Left,Right},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> - Sqrt[2]* Sqrt[4*\[Pi]*\[Alpha]EM]/sW * \[Epsilon] * KroneckerDelta[a,b] * WC["dW",{i,j}] * f6,
		FF[DipoleQ, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],d[i_],u[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*\[Alpha]EM]/sW * \[Epsilon] * KroneckerDelta[a,b] * WC["dW",{j,i}]\[Conjugate] * f6,
		FF[DipoleQ, {WBoson,0}, {Left,Right},{l1_[a_],l2_[b_],d[i_],u[j_]}]:> - Sqrt[2]* Sqrt[4*\[Pi]*\[Alpha]EM]/sW * \[Epsilon] * KroneckerDelta[a,b] * WC["uW",{i,j}] * f6,
		FF[DipoleQ, {WBoson,0}, {Left,Left},{l1_[a_],l2_[b_],u[i_],d[j_]}]:> Sqrt[2]* Sqrt[4*\[Pi]*\[Alpha]EM]/sW * \[Epsilon] * KroneckerDelta[a,b] * WC["uW",{j,i}]\[Conjugate] * f6
	};
	Return[list/.f6->If[dim<=4,0,1]/.f8->If[dim<=6,0,1]];
]


(* ::Section:: *)
(*Gauge boson couplings*)


(* ::Subsection:: *)
(*Photon*)


(* SM coupling of the photon *)
gA[particle_, {p_,r_}]:= Sqrt[4*\[Pi]*\[Alpha]EM] * Charge[particle] * KroneckerDelta[p,r]


(* ::Subsection:: *)
(*Z boson*)


(* SM coupling of the Z boson *)
gZ[particle_, chirality_,{p_,r_}]:= Sqrt[4*\[Pi]*\[Alpha]EM]/(sW*cW)*(WeakIsospin3[particle, chirality] - sW^2*Charge[particle])*KroneckerDelta[p,r]


(* ::Subsubsection:: *)
(*Modifications*)


\[CapitalDelta]gZe[Left, \[Epsilon]_, {a_,b_}]:= - Sqrt[\[Pi]*\[Alpha]EM]/(sW*cW) * \[Epsilon] * (WC["Hl1",{a,b}] + WC["Hl3",{a,b}])
\[CapitalDelta]gZe[Right, \[Epsilon]_, {a_,b_}]:= - Sqrt[\[Pi]*\[Alpha]EM]/(sW*cW) * \[Epsilon] * WC["He",{a,b}]


\[CapitalDelta]gZd[Left, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*\[Alpha]EM]/(sW*cW) * \[Epsilon] * (WC["Hq1",{i,j}] + WC["Hq3",{i,j}])
\[CapitalDelta]gZd[Right, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*\[Alpha]EM]/(sW*cW) * \[Epsilon] * WC["Hd",{i,j}]


\[CapitalDelta]gZu[Left, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*\[Alpha]EM]/(sW*cW) * \[Epsilon] * (WC["Hq1",{i,j}] - WC["Hq3",{i,j}])
\[CapitalDelta]gZu[Right, \[Epsilon]_, {i_,j_}]:= - Sqrt[\[Pi]*\[Alpha]EM]/(sW*cW) * \[Epsilon] * WC["Hu",{i,j}]


(* ::Subsection:: *)
(*W boson*)


(* SM coupling of the W boson in weak eigenbasis -> flavor diagonal *)
gW[{p_,r_}]:= Sqrt[4*\[Pi]*\[Alpha]EM]/(Sqrt[2]*sW) * KroneckerDelta[p,r]


(* ::Section:: *)
(*Hermitian WC*)


HermitianWC4= Alternatives["lq1", "lq3", "eu", "ed", "lu", "ld", "eq"];


HermitianWC2= Alternatives["Hl1", "Hl3", "He", "Hq1", "Hq3", "Hu", "Hd"];


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
(*WCxf*)


WCxf::usage= "WCxf[\"coef\"]
	Denotes the Wilson coefficient that is labeld by coef in the Warsaw basis as specified by the WCxf format.";


MapToWCxf::usage= "MapToWCxf
	List of replacement rules that maps the HighPTio Wilson coefficient notation to the WCxf conventions.";


MapToWCxf= {
	(* C_qe *)
	WC["eq", {\[Alpha]_,\[Beta]_,i_,j_}] :> Module[{ret, ind={i,j,\[Alpha],\[Beta]}},
		If[(ind[[1]]>ind[[2]]) || (ind[[1]]==ind[[2]] && ind[[3]]>ind[[4]]),
			ind={ind[[2]],ind[[1]],ind[[4]],ind[[3]]};
			ret= Conjugate@ WCxf["qe" <> "_" <> ToString[ind[[1]]] <> ToString[ind[[2]]] <> ToString[ind[[3]]] <> ToString[ind[[4]]]]
			,
			ret= WCxf["qe" <> "_" <> ToString[ind[[1]]] <> ToString[ind[[2]]] <> ToString[ind[[3]]] <> ToString[ind[[4]]]]
		];
		ret
	],
	
	(* four fermion operators except for C_qe *)
	WC[a:Except["eq",_String], {\[Alpha]_,\[Beta]_,i_,j_}] :> WCxf[a <> "_" <> ToString[\[Alpha]] <> ToString[\[Beta]] <> ToString[i] <> ToString[j]],
	
	(* two fermion operators *)
	WC[a_String, {p_,r_}] :> WCxf[StringReplace[a, "H"->"phi"] <> "_" <> ToString[p] <> ToString[r]]
};
