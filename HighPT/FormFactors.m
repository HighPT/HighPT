(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`FormFactors`*)


(* ::Subtitle:: *)
(*Form factor implementation.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["FF"]


PackageExport["Scalar"]
PackageExport["Tensor"]
PackageExport["Vector"]
PackageExport["DipoleL"]
PackageExport["DipoleQ"]


PackageExport["SubstituteFF"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["RotateMassToWeakBasis"]


PackageScope["SpinSummedAmplitude2"]


PackageScope["ExpandFormFactors"]


PackageScope["SChannelSum"]
PackageScope["TChannelSum"]
PackageScope["UChannelSum"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*FormFactor*)


(* ::Subsection::Closed:: *)
(*Usage*)


FF::usage= "FF[lorentz, type, {\!\(\*SubscriptBox[\(\[Chi]\), \(\[ScriptL]\)]\),\!\(\*SubscriptBox[\(\[Chi]\), \(q\)]\)}, {\[Alpha],\[Beta],i,j}] represents a form factor with Lorentz structure lorentz \[Element] {Scalar, Vector, Tensor, DipoleL, DipoleQ}. The chirality of the lepton (quark) current is \!\(\*SubscriptBox[\(\[Chi]\), \(\[ScriptL]\)]\) (\!\(\*SubscriptBox[\(\[Chi]\), \(q\)]\)). The field content of the corresponding operator is (\!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(1\)], \(_\)]\)\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\))(\!\(\*OverscriptBox[SubscriptBox[\(q\), \(1\)], \(_\)]\)\!\(\*SubscriptBox[\(q\), \(2\)]\)) with \!\(\*SubscriptBox[\(\[ScriptL]\), \(1, 2\)]\)\[Element]{e,\[Nu]}, \!\(\*SubscriptBox[\(q\), \(1, 2\)]\)\[Element]{u,d} and \[Alpha],\[Beta],i,j\[Element]{1,2,3} are the corresponding flavor indices. The type of the form factors are divided into regular and singular: {\"regular\",{\!\(\*SubscriptBox[\(n\), \(s\)]\),\!\(\*SubscriptBox[\(n\), \(t\)]\)}} is the form factor for an EFT contact interaction with the scaling \!\(\*SuperscriptBox[OverscriptBox[\(s\), \(^\)], SubscriptBox[\(n\), \(s\)]]\)\!\(\*SuperscriptBox[OverscriptBox[\(t\), \(^\)], SubscriptBox[\(n\), \(t\)]]\) in the partonic Mandelstam variables, whereas {mediator, 0 | SM} is the form factor for a non-local interaction through the specified mediator \[Element] {\"Photon\", \"ZBoson\", \"WBoson\", ...}. The singular form factors are divided into Standard Model contributions (SM) and NP contributions (0).
FF \[Rule] bool is an option that specifies whether the output should be given in terms of form factors (bool=True) or in terms of Wilson coefficients or coupling constants (bool=False), where the latter is the default. This option can be used with: DifferentialCrossSection, CrossSection, EventYield, ChiSquareLHC.";


Scalar::usage= "Scalar denotes a scalar form factor FF[Scalar,...]."


Vector::usage= "Vector denotes a vector form factor FF[Vector,...]."


Tensor::usage= "Tensor denotes a tensor form factor FF[Tensor,...]."


DipoleL::usage= "DipoleL denotes a lepton dipole form factor FF[DipoleL,...]."


DipoleQ::usage= "DipoleQ denotes a quark dipole form factor FF[DipoleQ,...]."


SpinSummedAmplitude2::usage= "SpinSummedAmplitude2[s, t, {\[Alpha],\[Beta],i,j}] computes the modulo square of the amplitude summed over all spin configurations and multiplies the result with the spin-color average factor \!\(\*FractionBox[\(1\), \(12\)]\).";


ExpandFormFactors::usage= "ExpandFormFactors[arg] performs the form factor expansion for all appearances of FormFactor[...] inside arg.";


FormFactor::usage="FormFactor[type, s,t, {X,Y}, {\!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\)[\[Alpha]],\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)[\[Beta]],\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]] denotes the form-factor for operators of the given type \[Element] {Scalar, Vector, Tensor, DipoleL, DipoleQ}. The form factor depends on the partonic Mandelstam variables s and t. The chirality in the lepton current is X \[Element] {Left, Right} and in the quark current it is Y \[Element] {Left, Right}. The flavor indices are \[Alpha], \[Beta] for the leptons and i, j for the quarks.";


FormFactorVector::usage= "FormFactorVector[s,t,{X,Y},{\[Alpha],\[Beta],i,j}] returns the vector of all FormFactors with: the partonic Mandestam variables s and t; the chirality X in the lepton current, the chirality Y in the quark current; the lepton flavor indices \[Alpha], \[Beta] and the quark flavor indices i,j.";


InterferenceMatrix::usage= "InterferenceMatrix[z, {X,Y}] gives the matrix determining the interference pattern of the different form factor Lorentz structures. The first argument z must be the ratio of the partonic Mandelstam variables t/s. The second argument {X,Y} specifies the chiralities of both lepton and quark current.";


ComputeInterferencePattern::usage= "ComputeInterferencePattern[s, t, {X,Y}, {\[Alpha],\[Beta],i,j}] performs the matrix multiplication \!\(\*SuperscriptBox[\(F\), \(\[Dagger]\)]\)(s,t).M(t/s).F(s,t).";


(* ::Subsection::Closed:: *)
(*Errors*)


(* ::Text:: *)
(*Check type*)


FormFactor::unknowntype= "The type `1` is not valid. Allowed types are: Scalar | Tensor | Vector | DipoleL | DipoleQ.";


FormFactor[
	x:Except[ Scalar | Tensor | Vector | DipoleL | DipoleQ | _Pattern | _Black | _BlankSequence | _BlankNullSequence ]
	,
	___
]:= (Message[FormFactor::typeunknown, x]; Abort[])


(* ::Text:: *)
(*Check chirality*)


FormFactor::unknownchirality= "The fourth argument of FormFactor `1` must be a list of two elements denoting the chirality of the projectos in the lepton and in the quark current {X,Y} where X,Y \[Element] {Left, Rigth}.";


FormFactor[_,_,_,x:Except[{_,_}],___]:= (Message[FormFactor::unknownchirality, x]; Abort[])


(* ::Text:: *)
(*Check indices*)


FormFactor::unknownindices= "The fifth argument of FormFactor `1` must be a list of four elements denoting the flavor indices of leptons and quarks {\[Alpha],\[Beta],i,j}.";


FormFactor[_,_,_,_,x:Except[{_,_,_,_}]]:= (Message[FormFactor::unknownindices, x]; Abort[])


(* ::Subsection::Closed:: *)
(*Formatting*)


MakeBoxes[FormFactor[type_,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}], TraditionalForm] := SubsuperscriptBox[RowBox[{"[",SubsuperscriptBox["F", MakeBoxes[type,TraditionalForm], RowBox[{" ",MakeBoxes[X,TraditionalForm],MakeBoxes[Y,TraditionalForm]}]],"(",ToString[s],",",ToString[t],")","]"}], RowBox[{ToString[i],ToString[j]}], RowBox[{ToString[\[Alpha]],ToString[\[Beta]]}]]


MakeBoxes[FF[type_,med_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}], TraditionalForm] := SubsuperscriptBox[RowBox[{"[",SubsuperscriptBox["F", RowBox[{MakeBoxes[type,TraditionalForm], MakeBoxes[med,TraditionalForm]}], RowBox[{" ",MakeBoxes[X,TraditionalForm],MakeBoxes[Y,TraditionalForm]}]],"]"}], RowBox[{ToString[i],ToString[j]}], RowBox[{ToString[\[Alpha]],ToString[\[Beta]]}]]


Format[Scalar, TraditionalForm]  := "S"
Format[Vector, TraditionalForm]  := "V"
Format[Tensor, TraditionalForm]  := "T"
Format[DipoleL, TraditionalForm] := "\!\(\*SubscriptBox[\(D\), \(l\)]\)"
Format[DipoleQ, TraditionalForm] := "\!\(\*SubscriptBox[\(D\), \(q\)]\)"


MakeBoxes["regular", TraditionalForm] := ToBoxes["reg"]


MakeBoxes["Photon", TraditionalForm] := ToBoxes["\[Gamma]"]
MakeBoxes["ZBoson", TraditionalForm] := ToBoxes["Z"]
MakeBoxes["WBoson", TraditionalForm] := ToBoxes["W"]


MakeBoxes[Left, TraditionalForm]  := ToBoxes["L"]
MakeBoxes[Right, TraditionalForm] := ToBoxes["R"]


(* ::Section::Closed:: *)
(*FormFactorVector*)


FormFactorVector[s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:= Transpose[
	{{
		FormFactor[Scalar, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[Vector, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[Tensor, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[DipoleL, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[DipoleQ, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}]
	}}
]


(* ::Section::Closed:: *)
(*InterferenceMatrix*)


InterferenceMatrix::unknownchirality= "The second argument of InterferenceMatrix `1` must be a list of two elements denoting the chirality of the projectos in the lepton and in the quark current {X,Y} where X,Y \[Element] {Left, Rigth}.";


InterferenceMatrix[_,x:Except[{_,_}]]:= (Message[InterferenceMatrix::unknownchirality,x];Abort[])


InterferenceMatrix[s_, t_, {X_, Y_}]:= 
{
	{MSS[], 0, MST[t/s, X, Y], 0, 0},
	{0, MVV[t/s, X, Y], 0, 0, 0},
	{MTS[t/s, X, Y], 0, MTT[t/s, X, Y], 0, 0},
	{0, 0, 0, s/Param["vev"]^2*MDD[t/s, X, Y], 0},
	{0, 0, 0, 0, s/Param["vev"]^2*MDD[t/s, X, Y]}
}


(* ::Subsection::Closed:: *)
(*Individual entries of the interference matrix*)


MSS[]:= 1/4


MVV[z_, X_, Y_]:= (KroneckerDelta[X,Y] * (1 + 2*z) + z^2)


MTT[z_, X_, Y_]:= 4 * KroneckerDelta[X,Y] * (1+2*z)^2


MDD[z_, X_, Y_]:= -z * (1+z)


MST[z_, X_, Y_]:= -KroneckerDelta[X,Y] * (1+2*z)


MTS[z_, X_, Y_]:= MST[z,X,Y]


(* ::Section:: *)
(*Spin-summed amplitude square*)


SpinSummedAmplitude2[s_, t_, {\[Alpha]_,\[Beta]_,i_,j_}]:= Module[
	{XX,YY,result}
	,
	(* perform matrix multiplication of FormFactors and InterferenceMatrix *)
	With[{expr= ComputeInterferencePattern[s, t, {XX,YY}, {\[Alpha],\[Beta],i,j}]},
		(* sum over all chiralities *)
		result= Sum[
			expr/.{XX->X,YY->Y}
			,
			{X,{Left,Right}},{Y,{Left,Right}}
		]
	];
	(* Set KroneckerDelta[L,R]=0 *)
	result= result/.(KroneckerDelta[OrderlessPatternSequence[Left,Right]]->0);
	(* rescale result *)
	Return[1/12*(4*s^2)/Param["vev"]^4*result]
]


(* ::Subsection:: *)
(*Perform matrix multiplication of FormFactors with the InterferenceMatrix*)


ComputeInterferencePattern[s_, t_, {X_,Y_}, {a_,b_,i_,j_}]:= Module[
	{res}
	,
	res= ConjugateTranspose@FormFactorVector[s,t,{X,Y},{a,b,i,j}] . InterferenceMatrix[s,t,{X,Y}] . FormFactorVector[s,t,{X,Y},{a,b,i,j}];
	res= First@Flatten[res];
	Return[res]
]


(* ::Section:: *)
(*ExpandFormFactors*)


(* ::Subsection::Closed:: *)
(*Split FormFactor into regular and singular part*)


RegularFF::usage= "RegularFF denotes the entire regular part of a form-factor.";


SingularFF::usage= "SingularFF denotes the entire singular part of a form-factor.";


SplitFF::usage= "SplitFF returns the rule that splits FormFactor into RegularFF and SingularFF.";


(* Split FormFactor into regular and singular part *)
SplitFF= FormFactor[type_,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:> RegularFF[type,s,t,{X,Y},{\[Alpha],\[Beta],i,j}] + SingularFF[type,s,t,{X,Y},{\[Alpha],\[Beta],i,j}];


(* dipoles do not have a regular part up to d=8 *)
RegularFF[DipoleL|DipoleQ, ___]:= 0


(* ::Subsection:: *)
(*d=8 treatment*)


$d8::usage= "$d8 is an auxiliary variable to count powers of d=8 operators. With the property \!\(\*SuperscriptBox[\($d8\), \(2\)]\)=0.";


$d8/:Power[$d8,n_/;n>2]:= 0


$d8/:Conjugate[$d8]:= $d8


(* ::Subsection:: *)
(*Expand regular form factors*)


Options[ExpandRegularFF] = {OperatorDimension :> GetOperatorDimension[]};


(* expands the regular form-factor up to d=6 or d=8 terms as specified. *)
ExpandRegularFF[OptionsPattern[]]:= Module[
	{
		d8,
		rule= {}
	}
	,
	(* check whether to consider d=8 operators *)
	If[OptionValue[OperatorDimension]==8,
		d8= 1,
		d8= 0
	];
	
	If[$RunMode==="SMEFT",
		rule= {
			RegularFF[Vector,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:> FF[Vector,{"regular",{0,0}},{X,Y},{\[Alpha],\[Beta],i,j}] + d8 * $d8 * (s/(Param["vev"]^2)*FF[Vector,{"regular",{1,0}},{X,Y},{\[Alpha],\[Beta],i,j}] + t/(Param["vev"]^2)*FF[Vector,{"regular",{0,1}},{X,Y},{\[Alpha],\[Beta],i,j}]),
			RegularFF[type:Except[Vector],s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:> FF[type,{"regular",{0,0}},{X,Y},{\[Alpha],\[Beta],i,j}]
		}
		,
		(* The following needs modifications if we want to allow for a mixed run mode. *)
		rule= RegularFF[___]:> 0
	];
	Return[rule]
]


(* ::Subsection::Closed:: *)
(*Expand singular form factors*)


Options[ExpandSingularFF] = {OperatorDimension:> GetOperatorDimension[]};


(* expands the regular form-factor up to d=6 or d=8 terms as specified. *)
ExpandSingularFF[OptionsPattern[]]:= Module[
	{d8}
	,
	(* check whether to consider d=8 operators *)
	If[OptionValue[OperatorDimension]===8,
		d8= 1,
		d8= 0
	];
	
	SingularFF[type_,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:> Plus[
		(* SM contribution *)
		If[MatchQ[type,Vector],
			SChannelSum[s, FF[type,{"s",SM},{X,Y},{\[Alpha],\[Beta],i,j}]],
			0
		],
		(* NP contribution *)
		SChannelSum[s, FF[type,{"s",0},{X,Y},{\[Alpha],\[Beta],i,j}]],
		TChannelSum[t, FF[type,{"t",0},{X,Y},{\[Alpha],\[Beta],i,j}]],
		UChannelSum[-s-t, FF[type,{"u",0},{X,Y},{\[Alpha],\[Beta],i,j}]]
	]
]


(* ::Subsubsection:: *)
(*ChannelSum*)


SChannelSum::usage= "SChannelSum[s,FF] denotes the sum of all propagator in the s-channel multiplied with the corresponding form factor FF. SChannelSum[s, FF]=\!\(\*UnderscriptBox[\(\[Sum]\), \(A\)]\)\!\(\*FractionBox[SuperscriptBox[\(\[ScriptV]\), \(2\)], \(s - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\)\!\(\*SubscriptBox[\(FF\), \(A\)]\).";


TChannelSum::usage= "TChannelSum[t,FF] denotes the sum of all propagator in the t-channel multiplied with the corresponding form-factor FF. TChannelSum[t, FF]=\!\(\*UnderscriptBox[\(\[Sum]\), \(A\)]\)\!\(\*FractionBox[SuperscriptBox[\(\[ScriptV]\), \(2\)], \(t - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\)\!\(\*SubscriptBox[\(FF\), \(A\)]\).";


UChannelSum::usage= "UChannelSum[u,FF] denotes the sum of all propagator in the u-channel (where u=-s-t) multiplied with the corresponding form-factor FF. USChannelSum[-s-t, FF]=\!\(\*UnderscriptBox[\(\[Sum]\), \(A\)]\)\!\(\*FractionBox[SuperscriptBox[\(\[ScriptV]\), \(2\)], \(\(-s\) - t - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\)\!\(\*SubscriptBox[\(FF\), \(A\)]\).";


(* If form factors are set to zers also the corresponding channel sum vanishes *)
SChannelSum[_,0]:=0
TChannelSum[_,0]:=0
UChannelSum[_,0]:=0


(* ::Subsection:: *)
(*Expand the full FormFactors*)


Options[ExpandFormFactors] = {OperatorDimension :> GetOperatorDimension[]};


ExpandFormFactors[arg_, OptionsPattern[]]:= Module[
	{
		temp= arg,
		dim= OptionValue[OperatorDimension],
		$aux
	}
	,
	(* $aux is real *)
	$aux/:Conjugate[$aux]:=$aux;
	
	(* split FF into regular and singular part *)
	temp= temp/.SplitFF;
	
	(* expand regular part of FF *)
	temp= temp/.ExpandRegularFF[OperatorDimension->dim];
	
	(* expand singular part of FF *)
	temp= temp/.ExpandSingularFF[OperatorDimension->dim];
	
	(* remove interference of d=8 FF with d=6 FF regular and singular *)
	temp= Expand[temp];
	temp= temp/.{
		FF[Vector,{"regular",{0,0}}, rest___]:> ($aux * FF[Vector,{"regular",{0,0}}, rest]),
		SChannelSum[s_, FF[type_,{"s",0},rest___]]:> ($aux * SChannelSum[s, FF[type,{"s",0},rest]]),
		TChannelSum[t_, FF[type_,{"t",0},rest___]]:> ($aux * TChannelSum[t, FF[type,{"t",0},rest]]),
		UChannelSum[uu_, FF[type_,{"u",0},rest___]]:> ($aux * UChannelSum[uu, FF[type,{"u",0},rest]]), (* if u_ is used as pattern here a new variables are exported in the HighPT` context. *)
		FF[Vector,{"regular",pow:({1,0}|{0,1})}, rest___]:> ($aux^2 * FF[Vector,{"regular",pow}, rest])
	};
	temp= (Expand@ExpandConjugate@Expand[temp])/.Power[$aux,n_/;n>=5]:>0;
	
	Return[
		Expand[ExpandConjugate[temp]]/.{$d8->1, $aux->1}
	]
]


(* ::Section:: *)
(*Basic form factor properties*)


(* ::Subsection::Closed:: *)
(*Require neutral form factors*)


FF[_,_,{_,_},{_e,_e,_d,_u}]:=0
FF[_,_,{_,_},{_e,_e,_u,_d}]:=0
FF[_,_,{_,_},{_\[Nu],_\[Nu],_d,_u}]:=0
FF[_,_,{_,_},{_\[Nu],_\[Nu],_u,_d}]:=0
FF[_,_,{_,_},{_e,_\[Nu],_u,_u}]:=0
FF[_,_,{_,_},{_e,_\[Nu],_d,_d}]:=0
FF[_,_,{_,_},{_\[Nu],_e,_u,_u}]:=0
FF[_,_,{_,_},{_\[Nu],_e,_d,_d}]:=0


(* ::Subsection::Closed:: *)
(*Simplifications for Tensor and Scalar form factors*)


(* The following constraints follow from Hypercharge conservation similar to the scalar and tensor operators in the SMEFT *)
FF[Tensor,_,{Right,Right},{_,_,_,_d}] := 0
FF[Tensor,_,{Left,Left},{_,_,_d,_}]   := 0


FF[Scalar,_,{Right,Right},{_,_,_,_d}] := 0
FF[Scalar,_,{Left,Left},{_,_,_d,_}]   := 0

FF[Scalar,_,{Right,Left},{_,_,_u,_}] := 0
FF[Scalar,_,{Left,Right},{_,_,_,_u}] := 0


(* opposite chiralities are already removed for Tensors *)
FF[Tensor,_,{Right,Left},{_,_,_,_}] := 0
FF[Tensor,_,{Left,Right},{_,_,_,_}] := 0


(* ::Subsection:: *)
(*Singular form factor constraints*)


(* ::Subsubsection::Closed:: *)
(*SM form factors are flavor diagonal, except for the W coupling to quarks*)


FF[Vector, {_,SM}, {_,_}, {l1_[\[Alpha]_?IntegerQ],l2_[\[Beta]_?IntegerQ],_,_}]                := 0 /; (\[Alpha]!=\[Beta])
FF[Vector, {Except["WBoson"],SM}, {_,_}, {_,_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}] := 0 /; (i!=j)


(* ::Text:: *)
(*Quark dipoles must be diagonal in leptons*)


FF[DipoleQ, {"Photon"|"ZBoson"|"WBoson",0}, {_,_}, {l1_[\[Alpha]_?IntegerQ],l2_[\[Beta]_?IntegerQ],_,_}] := 0 /; (\[Alpha]!=\[Beta])


(* ::Text:: *)
(*Lepton dipoles must be diagonal in quarks for Photon and ZBosons*)


FF[DipoleL, {"Photon"|"ZBoson",0}, {_,_}, {_,_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}] := 0 /; (i!=j)


(* ::Subsubsection::Closed:: *)
(*Remove SM FF for other s-channel mediators*)


FF[Vector, {field:Except["s"], SM}, ___] := 0 /; !MatchQ[field,"Photon"|"ZBoson"|"WBoson"]


(* ::Subsubsection::Closed:: *)
(*Remove Z-coupling modifications that are both LFV and QFV at the same time*)


(* ::Text:: *)
(*This can also be removed at d=8, because the SM interference must be LF conserving*)


FF[Vector, {"ZBoson",0}, {_,_}, {l1_[\[Alpha]_?IntegerQ],l2_[\[Beta]_?IntegerQ],q1_[i_?IntegerQ],q2_[j_?IntegerQ]}]:= 0 /; ((\[Alpha]!=\[Beta])&&(i!=j))


(* ::Subsubsection::Closed:: *)
(*W-boson only couples to left handed fermions in SM*)


FF[Vector, {"WBoson",SM}, {OrderlessPatternSequence[Right,_]},___]:= 0


(* ::Text:: *)
(*In the SMEFT a right-handed coupling to quarks is possible*)


FF[Vector, {"WBoson",0}, {Right,_},___]:= 0


FF[DipoleL, {"WBoson",0}, {_,Right},___]:= 0
FF[DipoleQ, {"WBoson",0}, {Right,_},___]:= 0


(* ::Subsubsection::Closed:: *)
(*The photon vertex does not receive corrections in the SMEFT at d=6*)


FF[Vector, {"Photon",Except[SM]},___]:= 0


(* ::Subsection::Closed:: *)
(*For CC processes the regular vector FF only contribute to the LL chirality configuration*)


FF[Vector, {"regular",_}, {OrderlessPatternSequence[Right,_]}, {_,_,_u,_d}|{_,_,_d,_u}]:= 0


(* ::Subsection::Closed:: *)
(*Consider only left-handed neutrinos*)


FF[Vector, _, {Right,_},{OrderlessPatternSequence[_\[Nu],_],_,_}]:= 0


(* ::Section:: *)
(*Weak basis rotation for form factors*)


(* After rotating the FF to the weak basis the SM contibution by the W should be diagonalized in quark generation space *)
DiagonalizeWBosonSM= {
	(* W-boson *)
	FF[Vector, {"WBoson",SM}, {Left,Left},{l1_,l2_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}]/;i!=j-> 0,
	FF[DipoleL, {"WBoson",0}, {_,Left},{l1_,l2_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}]/;i!=j-> 0
}


(* For definitions of Vu and Vd see Parameters.m *)
RotateMassToWeakBasis[expr_]:= Module[{ccRules, ncRules},
	ccRules= {
		(* ud *)
		FF[Vector, x_, {\[Chi]L_, Left}, {a_e, b_\[Nu], u[i_], d[j_]}]:> (Sum[
			Vu[[i,k]] * FF[Vector, x, {\[Chi]L, Left}, {a, b, u[k], d[n]}] * Vd\[ConjugateTranspose][[n,j]]
			,
			{k,1,3},{n,1,3}
		]/.DiagonalizeWBosonSM)
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Left}, {a_e, b_\[Nu], u[i_], d[j_]}]:> (Sum[
			FF[type, x, {\[Chi]L, Left}, {a, b, u[i], d[k]}] * Vd\[ConjugateTranspose][[k,j]]
			,
			{k,1,3}
		]/.DiagonalizeWBosonSM)
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Right}, {a_e, b_\[Nu], u[i_], d[j_]}]:> (Sum[
			Vu[[i,k]] * FF[type, x, {\[Chi]L, Right}, {a, b, u[k], d[j]}]
			,
			{k,1,3}
		]/.DiagonalizeWBosonSM)
		,
		(* du *)
		FF[Vector, x_, {\[Chi]L_, Left}, {a_\[Nu], b_e, d[i_], u[j_]}]:> (Sum[
			Vd[[i,k]] * FF[Vector, x, {\[Chi]L, Left}, {a, b, d[k], u[n]}] * Vu\[ConjugateTranspose][[n,j]]
			,
			{k,1,3},{n,1,3}
		]/.DiagonalizeWBosonSM)
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Left}, {a_\[Nu], b_e, d[i_], u[j_]}]:> (Sum[
			FF[type, x, {\[Chi]L, Left}, {a, b, d[i], u[k]}] * Vu\[ConjugateTranspose][[k,j]]
			,
			{k,1,3}
		]/.DiagonalizeWBosonSM)
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Right}, {a_\[Nu], b_e, d[i_], u[j_]}]:> (Sum[
			Vd[[i,k]] * FF[type, x, {\[Chi]L, Right}, {a, b, d[k], u[j]}]
			,
			{k,1,3}
		]/.DiagonalizeWBosonSM)
	};
	
	ncRules= {
		(* uu *)
		FF[Vector, x_, {\[Chi]L_, Left}, {a_, b_, u[i_], u[j_]}]:> Sum[
			Vu[[i,k]] * FF[Vector, x, {\[Chi]L, Left}, {a, b, u[k], u[n]}] * Vu\[ConjugateTranspose][[n,j]]
			,
			{k,1,3},{n,1,3}
		]
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Left}, {a_, b_, u[i_], u[j_]}]:> Sum[
			FF[type, x, {\[Chi]L, Left}, {a, b, u[i], u[n]}] * Vu\[ConjugateTranspose][[n,j]]
			,
			{n,1,3}
		]
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Right}, {a_, b_, u[i_], u[j_]}]:> Sum[
			Vu[[i,k]] * FF[type, x, {\[Chi]L, Right}, {a, b, u[k], u[j]}]
			,
			{k,1,3}
		]
		,
		(* dd *)
		FF[Vector, x_, {\[Chi]L_, Left}, {a_, b_, d[i_], d[j_]}]:> Sum[
			Vd[[i,k]] * FF[Vector, x, {\[Chi]L, Left}, {a, b, d[k], d[n]}] * Vd\[ConjugateTranspose][[n,j]]
			,
			{k,1,3},{n,1,3}
		]
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Left}, {a_, b_, d[i_], d[j_]}]:> Sum[
			FF[type, x, {\[Chi]L, Left}, {a, b, d[i], d[n]}] * Vd\[ConjugateTranspose][[n,j]]
			,
			{n,1,3}
		]
		,
		FF[type:Except[Vector], x_, {\[Chi]L_, Right}, {a_, b_, d[i_], d[j_]}]:> Sum[
			Vd[[i,k]] * FF[type, x, {\[Chi]L, Right}, {a, b, d[k], d[j]}]
			,
			{k,1,3}
		]
	};
	
	Return[expr/.ccRules/.ncRules]
]


(* ::Section::Closed:: *)
(*Substitute form factors*)


SubstituteFF::usage= "SubstituteFF[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] substitutes all form factors FF[...] in the given argument \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] by the corresponding Wilson coefficients or coupling constants. If running in the SMEFT mode, SubstituteFF takes the following options: EFTorder \[RuleDelayed] \[ScriptN] specifies that the result is expanded up to and including terms of order \!\(\*SuperscriptBox[\(\[CapitalLambda]\), \(-\[ScriptN]\)]\). The default is \[ScriptN]=GetEFTorder[]. OperatorDimension \[Rule] \[ScriptD] specifies that EFT operators up to mass dimension \[ScriptD] should be included. The default is \[ScriptD]=GetOperatorDimension[]; EFTscale -> \[CapitalLambda] sdpecifies the EFT cutoff scale used for the substitutions. The default is \[CapitalLambda]=1000 (GeV). In the mediator mode these Options are ignored."


SubstituteFF::remainingFF= "Not all form factors have been replaced. The remaining FF are: `1`"


Options[SubstituteFF]= {
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	EFTscale          -> 1000
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
		(*
		(* this is no longer necessary with automatic EFT series truncation *)
		temp= MyTiming[Normal@Series[temp,{\[Epsilon],0,OptionValue[EFTorder]/2}], "EFT Series"];
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
