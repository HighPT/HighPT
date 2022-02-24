(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`FormFactors*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


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


(* ::Subsection:: *)
(*Internal*)


PackageScope["FormFactor"]


(*PackageScope["MyComplexExpand"]*)
PackageScope["MyExpand"]
PackageScope["ExpandConjugate"]


PackageScope["RotateFFtoWeakEigenbasis"]
PackageScope["DiagonalizeWBosonSM"]


(* ::Text:: *)
(*Form-factor types*)


PackageScope["FormFactorVector"]


PackageScope["InterferenceMatrix"]


PackageScope["SpinSummedAmplitude2"]


PackageScope["ExpandFormFactors"]


PackageScope["SChannelSum"]
PackageScope["TChannelSum"]
PackageScope["UChannelSum"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*FormFactor*)


(* ::Subsection:: *)
(*Usage*)


FormFactor::usage="FormFactor[type, s,t, {X,Y}, {\!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\)[\[Alpha]],\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)[\[Beta]],\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]]
	Denotes the form-factor for operators of the given type \[Element] {Scalar, Vector, Tensor, DipoleL, DipoleQ}. 
	The form-factor depends on the partonic Mandelstam variables s and t.
	The chirality in the lepton current is X \[Element] {Left, Right} and in the quark current it is Y \[Element] {Left, Right}.
	The flavor indices are \[Alpha], \[Beta] for the leptons and i, j for the quarks.";


FF::usage= "FF[lorentz, type, {\!\(\*SubscriptBox[\(\[Chi]\), \(\[ScriptL]\)]\),\!\(\*SubscriptBox[\(\[Chi]\), \(q\)]\)}, {\[Alpha],\[Beta],i,j}]
	Represents a form factor with Lorentz structure lorentz \[Element] {Scalar, Vector, Tensor, DipoleL, DipoleQ}.
	The chirality of the lepton (quark) current is \!\(\*SubscriptBox[\(\[Chi]\), \(\[ScriptL]\)]\) (\!\(\*SubscriptBox[\(\[Chi]\), \(q\)]\)).
	The field contant of the corresponding operator is (\!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(1\)], \(_\)]\)\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\))(\!\(\*OverscriptBox[SubscriptBox[\(q\), \(1\)], \(_\)]\)\!\(\*SubscriptBox[\(q\), \(2\)]\)) with \!\(\*SubscriptBox[\(\[ScriptL]\), \(1, 2\)]\)\[Element]{e,\[Nu]}, \!\(\*SubscriptBox[\(q\), \(1, 2\)]\)\[Element]{u,d} and \[Alpha],\[Beta],i,j\[Element]{1,2,3} are the corresponding flavor indices.
	The types of form factors are divided into regular and singular contributions:
		{\"regular\",{\!\(\*SubscriptBox[\(n\), \(s\)]\),\!\(\*SubscriptBox[\(n\), \(t\)]\)}} : form factor for an EFT contact interaction with the scaling \!\(\*SuperscriptBox[OverscriptBox[\(s\), \(^\)], SubscriptBox[\(n\), \(s\)]]\)\!\(\*SuperscriptBox[OverscriptBox[\(t\), \(^\)], SubscriptBox[\(n\), \(t\)]]\) in the partonic Mandelstam variables
		{mediator, 0 | SM}: form factor for a non-contact interaction through the specified mediator \[Element] {Photon, ZBoson, WBoson, ...}
			Form factors are divided into Standard Model contributions (SM) and NP contributions (0).";


Scalar::usage= "Scalar
	Denotes a scalar form factor: FF[Scalar,...]."


Vector::usage= "Vector
	Denotes a vector form factor: FF[Vector,...]."


Tensor::usage= "Tensor
	Denotes a tensor form factor: FF[Tensor,...]."


DipoleL::usage= "DipoleL
	Denotes a lepton dipole form factor: FF[DipoleL,...]."


DipoleQ::usage= "DipoleQ
	Denotes a quark dipole form factor: FF[DipoleQ,...]."


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*Formatting*)


MakeBoxes[FormFactor[type_,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}], TraditionalForm]:= SubsuperscriptBox[RowBox[{"[",SubsuperscriptBox["F", MakeBoxes[type,TraditionalForm], RowBox[{" ",MakeBoxes[X,TraditionalForm],MakeBoxes[Y,TraditionalForm]}]],"(",ToString[s],",",ToString[t],")","]"}], RowBox[{ToString[i],ToString[j]}], RowBox[{ToString[\[Alpha]],ToString[\[Beta]]}]]


MakeBoxes[FF[type_,med_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}], TraditionalForm]:= SubsuperscriptBox[RowBox[{"[",SubsuperscriptBox["F", RowBox[{MakeBoxes[type,TraditionalForm], MakeBoxes[med,TraditionalForm]}], RowBox[{" ",MakeBoxes[X,TraditionalForm],MakeBoxes[Y,TraditionalForm]}]],"]"}], RowBox[{ToString[i],ToString[j]}], RowBox[{ToString[\[Alpha]],ToString[\[Beta]]}]]


Format[Scalar, TraditionalForm]:= "S"
Format[Vector, TraditionalForm]:= "V"
Format[Tensor, TraditionalForm]:= "T"
Format[DipoleL, TraditionalForm]:= "Dl"
Format[DipoleQ, TraditionalForm]:= "Dq"


MakeBoxes["regular", TraditionalForm]:= ToBoxes["reg"]


MakeBoxes[Left, TraditionalForm]:= ToBoxes["L"]
MakeBoxes[Right, TraditionalForm]:= ToBoxes["R"]


(* ::Section:: *)
(*FormFactorVector*)


(* ::Subsection:: *)
(*Usage*)


FormFactorVector::usage= "FormFactorVector[s,t,{X,Y},{\[Alpha],\[Beta],i,j}]
	Returns the vector of all FormFactors with:
		the partonic Mandestam variables s and t;
		the chirality X in the lepton current, the chirality Y in the quark current; 
		the lepton flavor indices \[Alpha], \[Beta] and the quark flavor indices i,j.";


(* ::Subsection:: *)
(*Definition*)


FormFactorVector[s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:= Transpose[
	{{
		FormFactor[Scalar, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[Vector, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[Tensor, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[DipoleL, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}],
		FormFactor[DipoleQ, s,t, {X,Y}, {\[Alpha],\[Beta],i,j}]
	}}
]


(* ::Section:: *)
(*InterferenceMatrix*)


(* ::Subsection:: *)
(*Usage*)


InterferenceMatrix::usage= "InterferenceMatrix[z, {X,Y}]
	Gives the matrix determining the interference pattern of the different form-factor Lorentz structures.
	The first argument z must be the ratio of the partonic Mandelstam variables t/s.
	The second argument {X,Y} specifies the chiralities of both lepton and quark current.";


(* ::Subsection:: *)
(*Errors*)


InterferenceMatrix::unknownchirality= "The second argument of InterferenceMatrix `1` must be a list of two elements denoting the chirality of the projectos in the lepton and in the quark current {X,Y} where X,Y \[Element] {Left, Rigth}.";


InterferenceMatrix[_,x:Except[{_,_}]]:= (Message[InterferenceMatrix::unknownchirality,x];Abort[])


(* ::Subsection:: *)
(*Definition*)


InterferenceMatrix[z_, {X_, Y_}]:= 
{
	{MSS[], 0, MST[z, X, Y], 0, 0},
	{0, MVV[z, X, Y], 0, 0, 0},
	{MTS[z, X, Y], 0, MTT[z, X, Y], 0, 0},
	{0, 0, 0, MDD[z, X, Y], 0},
	{0, 0, 0, 0, MDD[z, X, Y]}
}


(* ::Subsection:: *)
(*Individual entries of the interference matrix*)


MSS[]:= 1/4


MVV[z_, X_, Y_]:= (KroneckerDelta[X,Y] * (1 + 2*z) + z^2)


MTT[z_, X_, Y_]:= 4 * KroneckerDelta[X,Y] * (1+2*z)^2


MDD[z_, X_, Y_]:= -z * (1+z)


MST[z_, X_, Y_]:= -KroneckerDelta[X,Y] * (1+2*z)


MTS[z_, X_, Y_]:= MST[z,X,Y]


(* ::Section:: *)
(*Spin-summed amplitude square*)


(* ::Subsection:: *)
(*Usage*)


SpinSummedAmplitude2::usage= "SpinSummedAmplitude2[s, t, {\[Alpha],\[Beta],i,j}]
	Computes the modulo square of the amplitude summed over all spin configurations and multiplies the result with the spin-color average factor \!\(\*FractionBox[\(1\), \(12\)]\).";


(* ::Subsection:: *)
(*Definition*)


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
	Return[1/12*(4*s^2)/VEV^4*result]
]


(* ::Subsection:: *)
(*Auxiliary function to perform the matrix multiplication of the FormFactors with the InterferenceMatrix*)


ComputeInterferencePattern::usage= "ComputeInterferencePattern[s, t, {X,Y}, {\[Alpha],\[Beta],i,j}]
	Performs the matrix multiplication \!\(\*SuperscriptBox[\(F\), \(\[Dagger]\)]\)(s,t).M(t/s).F(s,t).";


ComputeInterferencePattern[s_, t_, {X_,Y_}, {a_,b_,i_,j_}]:= Module[
	{res}
	,
	res= ConjugateTranspose@FormFactorVector[s,t,{X,Y},{a,b,i,j}] . InterferenceMatrix[t/s,{X,Y}] . FormFactorVector[s,t,{X,Y},{a,b,i,j}];
	res= First@Flatten[res];
	Return[res]
]


(* ::Section:: *)
(*ExpandFormFactors*)


(* ::Subsection:: *)
(*Split FormFactor into regular and singular part*)


RegularFF::usage= "RegularFF
	Denotes the entire regular part of a form-factor.";


SingularFF::usage= "SingularFF
	Denotes the entire singular part of a form-factor.";


SplitFF::usage= "SplitFF
	Returns the rule that splits FormFactor into RegularFF and SingularFF.";


(* Split FormFactor into regular and singular part *)
SplitFF= FormFactor[type_,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:> RegularFF[type,s,t,{X,Y},{\[Alpha],\[Beta],i,j}] + SingularFF[type,s,t,{X,Y},{\[Alpha],\[Beta],i,j}];


(* dipoles do not have a regular part *)
RegularFF[DipoleL|DipoleQ, ___]:= 0


(* ::Subsection:: *)
(*Expand the different parts of the form-factors*)


$d8::usage= "$d8
	Is an ausiliary variable to count power of d=8 operators. With the property \!\(\*SuperscriptBox[\($d8\), \(2\)]\)=0.";


$d8/:Power[$d8,n_/;n>=2]:= 0


$d8/:Conjugate[$d8]:= $d8


(* ::Subsubsection:: *)
(*Check if d=8 [delete?]*)


D8Q[arg_]:= Switch[arg,
	8, True,
	6, False,
	_, Abort[]
]


(* ::Subsubsection:: *)
(*Expand regular form-factors*)


Options[ExpandRegularFF] = {OperatorDimension:> GetOperatorDimension[]};


(* expands the regular form-factor up to d=6 or d=8 terms as specified. *)
ExpandRegularFF[OptionsPattern[]]:= Module[
	{
		d8(*= Boole@ D8Q[OptionValue[EFTorder]]*),
		rule= {}
	}
	,
	(* check whether to consider d=8 operators *)
	If[OptionValue[OperatorDimension]==8,
		d8= 1,
		d8= 0
	];
	
	If[$RunMode==="SMEFT",
		rule= (RegularFF[type_,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:> FF[type,{"regular",{0,0}},{X,Y},{\[Alpha],\[Beta],i,j}] + d8 * $d8 * (s/VEV^2*FF[type,{"regular",{1,0}},{X,Y},{\[Alpha],\[Beta],i,j}] + t/VEV^2*FF[type,{"regular",{0,1}},{X,Y},{\[Alpha],\[Beta],i,j}]))
		,
		(* The following needs modifications if we want to allow for a mixed run mode. *)
		rule= RegularFF[___]:> 0
	];
	Return[rule]
]


(* ::Subsubsection:: *)
(*Expand singular form-factors*)


Options[ExpandSingularFF] = {OperatorDimension:> GetOperatorDimension[]};


(* expands the regular form-factor up to d=6 or d=8 terms as specified. *)
ExpandSingularFF[OptionsPattern[]]:= Module[
	{d8(*= Boole@ D8Q[OptionValue[EFTorder]]*)}
	,
	(* check whether to consider d=8 operators *)
	If[OptionValue[OperatorDimension]===8,
		d8= 1,
		d8= 0
	];
	
	SingularFF[type_,s_,t_,{X_,Y_},{\[Alpha]_,\[Beta]_,i_,j_}]:> Plus[
		(* Add SM contribution *)
		If[MatchQ[type,Vector],
			SChannelSum[s, FF[type,{"s",SM},{X,Y},{\[Alpha],\[Beta],i,j}]],
			0
		],
		SChannelSum[s, FF[type,{"s",0},{X,Y},{\[Alpha],\[Beta],i,j}]],
		d8 * $d8 * t * SChannelSum[s, FF[type,{"s",1},{X,Y},{\[Alpha],\[Beta],i,j}]], (* this probably does not exist anyway? *)
		TChannelSum[t, FF[type,{"t",0},{X,Y},{\[Alpha],\[Beta],i,j}]],
		UChannelSum[-s-t, FF[type,{"u",0},{X,Y},{\[Alpha],\[Beta],i,j}]]
	]
]


(* ::Subsubsection:: *)
(*ChannelSum*)


SChannelSum::usage= "SChannelSum[s,FF]
	Denotes the sum of all propagator in the s-channel multiplied with the corresponding form-factor FF.
	SChannelSum[s, FF]=\!\(\*UnderscriptBox[\(\[Sum]\), \(A\)]\)\!\(\*FractionBox[SuperscriptBox[\(\[ScriptV]\), \(2\)], \(s - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\)\!\(\*SubscriptBox[\(FF\), \(A\)]\)";


TChannelSum::usage= "TChannelSum[t,FF]
	Denotes the sum of all propagator in the t-channel multiplied with the corresponding form-factor FF.
	TChannelSum[t, FF]=\!\(\*UnderscriptBox[\(\[Sum]\), \(A\)]\)\!\(\*FractionBox[SuperscriptBox[\(\[ScriptV]\), \(2\)], \(t - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\)\!\(\*SubscriptBox[\(FF\), \(A\)]\)";


UChannelSum::usage= "UChannelSum[u,FF]
	Denotes the sum of all propagator in the u-channel (where u=-s-t) multiplied with the corresponding form-factor FF.
	USChannelSum[-s-t, FF]=\!\(\*UnderscriptBox[\(\[Sum]\), \(A\)]\)\!\(\*FractionBox[SuperscriptBox[\(\[ScriptV]\), \(2\)], \(\(-s\) - t - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\)\!\(\*SubscriptBox[\(FF\), \(A\)]\)";


(* If form factors are set to zers also the corresponding channel sum vanishes *)
SChannelSum[_,0]:=0
TChannelSum[_,0]:=0
UChannelSum[_,0]:=0


(* ::Subsection:: *)
(*Expand the full FormFactors*)


(* ::Subsubsection:: *)
(*Usage*)


ExpandFormFactors::usage= "ExpandFormFactors[arg] performs the form-factor expansion for all appearances of FormFactor[...] inside arg.";


(* ::Subsubsection:: *)
(*Definitions*)


Options[ExpandFormFactors] = {OperatorDimension:> GetOperatorDimension[]};


ExpandFormFactors[arg_, OptionsPattern[]]:= Module[
	{
		temp= arg,
		dim= OptionValue[OperatorDimension]
	}
	,
	(* split FF into regular and singular part *)
	temp= temp/.SplitFF;
	(* expand regular part of FF *)
	temp= temp/.ExpandRegularFF[OperatorDimension->dim];
	(* expand singular part of FF *)
	temp= temp/.ExpandSingularFF[OperatorDimension->dim];
	
	Return[
		Expand[ExpandConjugate[temp]]/.$d8->1
	]
]


(* ::Section:: *)
(*Basic form factor properties*)


(* ::Subsubsection:: *)
(*Simplifications for Tensor and Scalar form-factors*)


(* ::Text:: *)
(*FF should be neutral*)


FF[_,_,{_,_},{_e,_e,_d,_u}]:=0
FF[_,_,{_,_},{_e,_e,_u,_d}]:=0
FF[_,_,{_,_},{_\[Nu],_\[Nu],_d,_u}]:=0
FF[_,_,{_,_},{_\[Nu],_\[Nu],_u,_d}]:=0
FF[_,_,{_,_},{_e,_\[Nu],_u,_u}]:=0
FF[_,_,{_,_},{_e,_\[Nu],_d,_d}]:=0
FF[_,_,{_,_},{_\[Nu],_e,_u,_u}]:=0
FF[_,_,{_,_},{_\[Nu],_e,_d,_d}]:=0


(* ::Text:: *)
(*The following constraints follow from Hypercharge conservation similar to the scalar and tensor operators in the SMEFT*)


FF[Tensor,_,{Right,Right},{_,_,_,_d}]:= 0
FF[Tensor,_,{Left,Left},{_,_,_d,_}]:= 0

(* opposite chiralities are already removed for Tensors *)
FF[Tensor,_,{Right,Left},{_,_,_,_}]:= 0
FF[Tensor,_,{Left,Right},{_,_,_,_}]:= 0


FF[Scalar,_,{Right,Right},{_,_,_,_d}]:= 0
FF[Scalar,_,{Left,Left},{_,_,_d,_}]:= 0

FF[Scalar,_,{Right,Left},{_,_,_u,_}]:= 0
FF[Scalar,_,{Left,Right},{_,_,_,_u}]:= 0


(* ::Subsubsection:: *)
(*SM form factors are flavor diagonal (except for the W coupling to quarks)*)


FF[Vector, {_,SM}, {_,_}, {l1_[\[Alpha]_?IntegerQ],l2_[\[Beta]_?IntegerQ],_,_}]:= 0 /; (\[Alpha]!=\[Beta])
FF[Vector, {Except[WBoson],SM}, {_,_}, {_,_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}]:= 0 /; (i!=j)


(* ::Text:: *)
(*Quark dipoles must be diagonal in leptons*)


FF[DipoleQ, {Photon|ZBoson|WBoson,0}, {l1_[\[Alpha]_?IntegerQ],l2_[\[Beta]_?IntegerQ],_,_}]:= 0 /; (\[Alpha]!=\[Beta])


(* ::Text:: *)
(*Lepton dipoles must be diagonal in quarks for Photon and ZBosons*)


FF[DipoleL, {Photon|ZBoson,0}, {_,_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}]:= 0 /; (i!=j)


(* ::Subsubsection:: *)
(*W-boson only couples to left handed fermions in SM*)


FF[Vector, {WBoson,SM}, {OrderlessPatternSequence[Right,_]},___]:= 0


(* ::Text:: *)
(*In the SMEFT a right-handed coupling to quarks is possible*)


FF[Vector, {WBoson,0}, {Right,_},___]:= 0


FF[DipoleL, {WBoson,0}, {_,Right},___]:= 0
FF[DipoleQ, {WBoson,0}, {Right,_},___]:= 0


(* ::Subsubsection:: *)
(*Consider only left-handed neutrinos*)


FF[Vector, _, {Right,_},{OrderlessPatternSequence[_\[Nu],_],_,_}]:= 0


(* ::Section:: *)
(*Weak basis rotation for form factors*)


(* After rotating the FF to the weak basis the SM contibution by the W should be diagonalized in quark generation space *)
DiagonalizeWBosonSM= {
	FF[Vector, {WBoson,SM}, {Left,Left},{l1_,l2_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}]/;i!=j-> 0,
	FF[DipoleL, {WBoson,0}, {_,Left},{l1_,l2_,q1_[i_?IntegerQ],q2_[j_?IntegerQ]}]/;i!=j-> 0
}


(* transforms form factors with mass basis flavor indices to form factors with weak basis flavor indices and diagonalizes WBoson contributions in the latter basis *)
RotateFFtoWeakEigenbasis= {
	FF[
		type:(Scalar|Vector|Tensor|DipoleL|DipoleQ),
		x_,
		{\[Chi]L_, Left},
		{a_\[Nu], b_e, d[i_], u[j_]}
	]:> (Sum[
		CKM[[j,k]] * FF[type, x, {\[Chi]L, Left}, {a, b, d[i], u[k]}],
		{k,1,3}
	]/.DiagonalizeWBosonSM)
	,
	FF[
		Vector,
		x_,
		{\[Chi]L_, Left},
		{a_e, b_\[Nu], u[i_], d[j_]}
	]:> (Sum[
		(CKM[[i,k]])\[Conjugate] * FF[Vector, x, {\[Chi]L, Left}, {a, b, u[k], d[j]}],
		{k,1,3}
	]/.DiagonalizeWBosonSM)
	,
	FF[
		type:(Scalar|Tensor|DipoleL|DipoleQ),
		x_,
		{\[Chi]L_, Right},
		{a_e, b_\[Nu], u[i_], d[j_]}
	]:> Sum[
		(CKM[[i,k]])\[Conjugate] * FF[type, x, {\[Chi]L, Right}, {a, b, u[k], d[j]}],
		{k,1,3}
	]
};


(* ::Section:: *)
(*Auxiliary functions*)


(* ::Subsection:: *)
(*ExpandConjugate*)


ExpandConjugate::usage= "ExpandConjugate[arg]
	Expands out all occurances of Conjugate[a+b] in arg.";


ExpandConjugate[arg_]:= (arg //. {Conjugate[x_Plus]:> Conjugate/@x, Conjugate[x_Times]:> Conjugate/@x})


(* ::Subsection:: *)
(*MyExpand*)


MyExpand::usage= "MyExpand[arg]
	Does what Expand[arg] does, just much faster for large sums.";


MyExpand[arg:(_Plus|_List)]:= MyExpand/@arg


MyExpand[arg:Except[_Plus|_List]]:= Expand[arg]
