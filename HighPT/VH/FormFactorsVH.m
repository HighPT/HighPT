(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`FormFactorsVH`*)


(* ::Subtitle:: *)
(*Form factor implementation for VH production*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection::Closed:: *)
(*Exported*)


(* FFs notation for external use *)
PackageExport["ff"]


(* This has to be made PRIVATE later -- here only for testing implementation *)
PackageExport["FormFactorVH"]
PackageExport["SpinSumAmplitudeSqVH"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["ExpandFormFactorsVH"]
PackageScope["ReplaceChannelSumsVH"]
PackageScope["SChannelSumVH"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*FormFactorVH*)


(* ::Subsection::Closed:: *)
(*Usage*)


ff::usage="ff[{Lorentz, index}, type, s, t, X, {\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]] denotes the form-factor for operators of the given type \[Element] {Scalar, Vector, Tensor}. For each possible lorentz structures there can be more than one form-factor which is denoted by \[IAcute]ndex. The form factor depends on the partonic Mandelstam variables s and t. The chirality in the lepton current is X \[Element] {Left, Right} and in the quark current it is Y \[Element] {Left, Right}. The flavor indices are i, j for the quarks.";


FormFactorVH::usage="FormFactorVH[{type, index}, s,t, X, {\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]] denotes the form-factor for operators of the given type \[Element] {Scalar, Vector, Tensor}. For each possible lorentz structures there can be more than one form-factor which is denoted by \[IAcute]ndex. The form factor depends on the partonic Mandelstam variables s and t. The chirality in the lepton current is X \[Element] {Left, Right} and in the quark current it is Y \[Element] {Left, Right}. The flavor indices are i, j for the quarks.";


(* ::Subsection::Closed:: *)
(*Errors*)


(* ::Text:: *)
(*Check  types	*)


FormFactorVH::unknowntype = "The type `1` is not valid type. Allowed types are: Scalar | Tensor | Vector.";


FormFactorVH[
  {
    x:Except[ Scalar | Tensor | Vector | _Pattern | _Blank | _BlankSequence | _BlankNullSequence ],
    _
  },
  ___
] := (Message[FormFactorVH::unknowntype, x]; Abort[]);


(* ::Text:: *)
(*Check  FFs indices	*)


allowedFFIndices[
  x:Except[ Scalar | Tensor | Vector ]
] := (Message[FormFactorVH::unknowntype]; Abort[]);


allowedFFIndices::usage = "allowedFFIndices[type] returns the possible indices for a FF of type \[Element] {Scalar, Tensor, Vector}"


allowedFFIndices = <|Vector -> {1, 2}, Scalar -> {1, 2}, Tensor -> {1}|>;


FormFactorVH::unknownindex = "Unknown index `2` for type `1`. Allowed indices are `3`.";


FormFactorVH[{type_, x_}, ___] /; !MatchQ[x, _Pattern | _Blank | _BlankSequence | _BlankNullSequence | Alternatives@@allowedFFIndices[type]] := 
	(Message[FormFactorVH::unknownindex, type, x, allowedFFIndices[type]]; Abort[]);	


(* ::Text:: *)
(*Check quark indices	*)


FormFactorVH::unknownquarkindices = "The fifth element must denoted the quark indices {i, j}";


FormFactorVH[{_, _}, _, _, _, x:Except[{_, _}]] := (Message[FormFactorVH::unknownquarkindices]; Abort[]);


(* ::Subsection::Closed:: *)
(*Formatting*)


MakeBoxes[FormFactorVH[{type_, index_}, s_,t_, X_,{i_,j_}], TraditionalForm] := SubscriptBox[
	RowBox[{"[",SubsuperscriptBox["f", RowBox[{MakeBoxes[type,TraditionalForm], ", ", MakeBoxes[index, TraditionalForm]}], RowBox[{" ",MakeBoxes[X,TraditionalForm]}]],"(",ToString[s],",",ToString[t],")","]"}], RowBox[{ToString[i],ToString[j]}]]


MakeBoxes[ff[{lorentz_, index_}, type_, X_,{i_,j_}], TraditionalForm] := SubscriptBox[
	RowBox[{"[",SubsuperscriptBox["f", RowBox[{MakeBoxes[lorentz, TraditionalForm], ", ", MakeBoxes[index, TraditionalForm],  MakeBoxes[type]}], RowBox[{" ",MakeBoxes[X,TraditionalForm]}]], "]"}], RowBox[{ToString[i],ToString[j]}]]


(* ::Section:: *)
(*FormFactorVectorVH*)


FormFactorVectorVH::usage= "FormFactorVectorVH[s, t, X, {i, j}] returns the vector of all FormFactorsVH with: the partonic Mandestam variables s and t; the chirality X in the quark current and the quark flavor indices i,j.";


FormFactorVectorVH[s_, t_, X_, {\[Psi]1_[i_], \[Psi]2_[j_]}] := Transpose[
	{{
		FormFactorVH[{Vector, 1}, s, t, X, {\[Psi]1[i], \[Psi]2[j]}],
		FormFactorVH[{Vector, 2}, s, t, X, {\[Psi]1[i], \[Psi]2[j]}],
		FormFactorVH[{Scalar, 1}, s, t, X, {\[Psi]1[i], \[Psi]2[j]}],
		FormFactorVH[{Scalar, 2}, s, t, X, {\[Psi]1[i], \[Psi]2[j]}],
		FormFactorVH[{Tensor, 1}, s, t, X, {\[Psi]1[i], \[Psi]2[j]}]
	}}
];


(* ::Section:: *)
(*InterferenceMatrixVH*)


InterferenceMatrixVH[s_, t_, mV_] := 
	{
	   {MV11[s, t, mV], MV12[s, t, mV], 0, 0, 0},
	   {MV12[s, t, mV], MV22[s, t, mV], 0, 0, 0},
	   {0, 0, MST11[s, t, mV], MST12[s, t, mV], MST13[s, t, mV]},
	   {0, 0, MST12[s, t, mV], MST22[s, t, mV], MST23[s, t, mV]},
	   {0, 0, MST13[s, t, mV], MST23[s, t, mV], MST33[s, t, mV]}
	}


(* ::Subsection:: *)
(*Individual entries of the interference matrix*)


MV11[s_, t_, mV_] := 2 s + t -(s t)/mV^2-t^2/mV^2-Mass["Higgs"]^2+(t Mass["Higgs"]^2)/mV^2


MV12[s_, t_, mV_] := s + mV^2 - Mass["Higgs"]^2


MV22[s_, t_, mV_] := Module[{u = -s -t + mV^2 + Mass["Higgs"]^2}, 1/s * (mV^2 * (2 * s - Mass["Higgs"]^2) + (t^2 + u^2)/2)]


MST11[s_, t_, mV_]:= Param["vev"]^2 /(4 * mV^4 * s) * ((s - mV^2)^2 - 2 Mass["Higgs"]^2 * (s + mV^2) + Mass["Higgs"]^2)


MST12[s_, t_, mV_]:= Module[{u = -s - t + mV^2 + Mass["Higgs"]^2}, Param["vev"]^2 * (u - t)/ (4 * mV^2 * s) * (s + mV^2 - Mass["Higgs"]^2)]


MST13[s_, t_, mV_]:= Module[{u = -s - t + mV^2 + Mass["Higgs"]^2}, 1/2 * Param["vev"]^2 * (t - u) / s]


MST22[s_, t_, mV_]:= Module[{u = -s - t + mV^2 + Mass["Higgs"]^2}, Param["vev"]^2 *(1 + (t - u)^2 / (4 * s * mV^2))]


MST23[s_, t_, mV_]:= 1/2 * Param["vev"]^2 (-s + Mass["Higgs"]^2 - mV^2)/s


MST33[s_, t_, mV_]:= Module[{u = -s - t + mV^2 + Mass["Higgs"]^2}, 4 * Param["vev"]^2 / s^2 * (mV^2 * (2 Mass["Higgs"]^2 - s) - 2 * t * u)]


(* ::Section:: *)
(*Spin-summed amplitude square*)


(* Matrix multiplication between the form-factors and interference matrix *)
ComputeIntPatternVH[s_, t_, mV_, X_, {\[Psi]1_[i_], \[Psi]2_[j_]}] := Module[{output, ffVector},
	ffVector = FormFactorVectorVH[s, t, X, {\[Psi]1[i], \[Psi]2[j]}];
	output = ConjugateTranspose@ffVector . InterferenceMatrixVH[s, t, mV] . ffVector;
	output = First@Flatten[output];
	Return[output]
];


SpinSumAmplitudeSqVH[s_, t_, {\[Psi]1_[i_], \[Psi]2_[j_]}] := Module[{mV, totalAmpSq, XX},
	(* Choose the gauge boson mass acording to the initial quarks *)
	mV = If[\[Psi]1 === \[Psi]2, Mass["ZBoson"], Mass["WBoson"]];
	
	(* Sum over all possible chiralities *)
	With[{ampSq = ComputeIntPatternVH[s, t, mV, XX, {\[Psi]1[i], \[Psi]2[j]}]},
		totalAmpSq = Sum[ampSq /. {XX-> X}, {X, {Left, Right}}]
	];
	
	(* differential partonic cross-section *)
	Return[1/(12 Param["vev"]^2) totalAmpSq]
];


(* ::Section:: *)
(*ExpandFormFactors*)


(* ::Subsection:: *)
(*Split FormFactor into regular and singular part*)


RegularFFVH::usage = "RegularFFVH denotes the entire regular part of a form-factor."


SingularFFVH::usage = "SingularFFVH denotes the entire singular part of a form-factor."


SplitFFVH::usage = "SplitFF returns the rule that splits the FormFactorVH into a RegularFFVH and SingularFFVH."


SplitFFVH = FormFactorVH[{lorentz_, index_}, s_, t_, X_, {i_, j_}] :> RegularFFVH[{lorentz, index}, s, t, X, {i, j}] + SingularFFVH[{lorentz, index}, s, t, X, {i, j}];


(* ::Subsection:: *)
(*Expand regular form factors*)


Options[ExpandRegularFFVH] = {OperatorDimension -> GetOperatorDimension[]};


ExpandRegularFFVH::dimension8 = "Expansion up to dimension-8 not available for VH production mode.";


(* Expands the regular form-factors up to dimension d = 6. *)
ExpandRegularFFVH[OptionsPattern[]] := Module[{rule = {}},
	(* d=8 not available at the moment *)
	If[OptionValue[OperatorDimension] === 8,
		Message[ExpandRegularFFVH::dimension8];
		Abort[]
	];
	
	If[$RunMode === "SMEFT",
		rule = {
			RegularFFVH[type_:Except[{Tensor, 1}], s_, t_, X_, {i_, j_}] :> ff[type, {"regular", {0, 0}}, X, {i, j}],
			RegularFFVH[{Tensor, 1}, s_, t_, X_, {i_, j_}] :> ff[{Tensor, 1}, {"regular", {0, 0}}, X, {i, j}] + (s / Param["vev"]^2) * ff[{Tensor, 1}, {"regular", {1, 0}}, X, {i, j}]
		}
		,
		rule = {RegularFFVH[___] :> 0}
	];
	
	Return[rule]
]


(* ::Subsection:: *)
(*Expand singular form factors*)


Options[ExpandSingularFFVH] := {OperatorDimension :> GetOperatorDimension[]};


ExpandSingularFFVH[OptionsPattern[]] := Module[{rule = {}},
	(* d=8 not available at the moment *)
	If[OptionValue[OperatorDimension] === 8,
		Message[ExpandRegularFFVH::dimension8];
		Abort[]
	];
	
	(* t- and u-channels not availables at the moment *)
	rule = {
		SingularFFVH[{lorentz_, index_}, s_, t_, X_, {i_, j_}] :> Plus[
			(* SM contribution *)
			If[MatchQ[lorentz, Vector] && index === 1, 
				SChannelSumVH[s, ff[{lorentz, index}, {"s", SM}, X, {i, j}]],
				0
			],
			SChannelSumVH[s, ff[{lorentz, index}, {"s", 0}, X, {i, j}]]
		]
	};	

	Return[rule]
]


(* ::Subsubsection::Closed:: *)
(*Channels sum*)


SChannelSumVH::usage = "SChannelSumVH[s, ff] denotes the sum of all s-channel mediators multipled by the corresponding FFs.";


(* If the FF vanishes the corresponding sum also vanishes *)
SChannelSumVH[_, 0] := 0


(* ::Subsubsection:: *)
(*Replace channel sums for VH production*)


ReplaceChannelSumsVH::usage = "ReplaceChannelSumsVH[channel] returns a replacement rule with which all SChannelSumVH can be replaced for a respective channel."


(* Quick solution. It would be good to changed it to have a better integration with the DY routines *)

(* Association with the mediators in each channel *)
mediatorsChannel = <|"WH" -> {"WBoson"}, "ZH" -> {"ZBoson"}|>;

(* List with the mediators predicted by the SM *)
mediatorsSM = {"ZBoson", "WBoson"};

(* Creates the replacement rule for the SM mediators *)
ReplaceChannelSumsVH[channel_:("WH" | "ZH")] := Module[{mediators, replacementRule},
	(* Mediators affecting the channel *)
	mediators = mediatorsChannel[channel];
	
	(* Contructs the replacement rule for the channel sums *)
	replacementRule = {
		SChannelSumVH[s_, ff[{lorentz_, index_}, {"s", ord_}, X_, {\[Psi]1_[i_], \[Psi]2_[j_]}]] :> Sum[
			Param["vev"]^2 * If[ord === SM, If[MemberQ[mediatorsSM, med], 1, 0] , 1] *
			FlavorDiagSMVH[med, ord, {i, j}] *
			LeftHandedCC[med, ord, X] *
			ff[{lorentz, index}, {med, ord}, X, {\[Psi]1[i], \[Psi]2[j]}] *
			Propagator[s, med]
		,
			{med, mediators}
		]
	};
	
	Return[replacementRule]
]


(* ::Subsubsection:: *)
(*SM properties*)


(* SM Z couplings are flavor diagonal *)
FlavorDiagSMVH[mediator_, ord_, {i_, j_}] := If[mediator === "ZBoson" && ord === SM, KroneckerDelta[i, j], 1];


(* SM CC is Left-Handed *)
LeftHandedCC[mediator_, order_, X_] := If[mediator === "WBoson" && order === SM, KroneckerDelta[X, Left], 1];


(* ::Subsection:: *)
(*Expand the full form factors*)


Options[ExpandFormFactorsVH] = {OperatorDimension :> GetOperatorDimension[]};


ExpandFormFactorsVH[arg_, OptionsPattern[]] := Module[
	{
		temp = arg,
		dim = OptionValue[OperatorDimension]
	},

	(* Splits FFs into regular and singular part *)
	temp = temp /. SplitFFVH;
	
	(* Expand regular part of the FF *)
	temp = temp /. ExpandRegularFFVH[OperatorDimension -> dim];
	
	(* Expand singular part of the FF *)
	temp = temp /. ExpandSingularFFVH[OperatorDimension -> dim];
	
	Return[
		Expand[ExpandConjugate[temp]]
	]
]
