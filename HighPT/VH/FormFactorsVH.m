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


(* ::Subsection:: *)
(*Exported*)


(* This has to be made PRIVATE later -- here only for testing implementation *)
PackageExport["FormFactorVH"]
PackageExport["SpinSumAmplitudeSqVH"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*FormFactorVH*)


(* ::Subsection:: *)
(*Usage*)


FormFactorVH::usage="FormFactorVH[{type, index}, s,t, X, {\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]] denotes the form-factor for operators of the given type \[Element] {Scalar, Vector, Tensor}. For each possible lorentz structures there can be more than one form-factor which is denoted by \[IAcute]ndex. The form factor depends on the partonic Mandelstam variables s and t. The chirality in the lepton current is X \[Element] {Left, Right} and in the quark current it is Y \[Element] {Left, Right}. The flavor indices are i, j for the quarks.";


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*Formatting*)


MakeBoxes[FormFactorVH[{type_, index_}, s_,t_, X_,{i_,j_}], TraditionalForm] := SubscriptBox[
	RowBox[{"[",SubsuperscriptBox["f", RowBox[{MakeBoxes[type,TraditionalForm], ", ", MakeBoxes[index, TraditionalForm]}], RowBox[{" ",MakeBoxes[X,TraditionalForm]}]],"(",ToString[s],",",ToString[t],")","]"}], RowBox[{ToString[i],ToString[j]}]]


(* ::Section:: *)
(*FormFactorVectorVH*)


FormFactorVectorVH::usage= "FormFactorVectorVH[s, t, X, {i, j}] returns the vector of all FormFactorsVH with: the partonic Mandestam variables s and t; the chirality X in the quark current and the quark flavor indices i,j.";


FormFactorVectorVH[s_, t_, X_, {i_, j_}] := Transpose[
	{{
		FormFactorVH[{Vector, 1}, s, t, X, {i, j}],
		FormFactorVH[{Vector, 2}, s, t, X, {i, j}],
		FormFactorVH[{Scalar, 1}, s, t, X, {i, j}],
		FormFactorVH[{Scalar, 2}, s, t, X, {i, j}],
		FormFactorVH[{Tensor, 1}, s, t, X, {i, j}]
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


MV22[s_, t_, mV_] := mV^2+ mV^4/(2 s) + s/2 +t -(mV^2 t)/s+ t^2/s-Mass["Higgs"]^2-(t Mass["Higgs"]^2)/s+Mass["Higgs"]^4/(2 s)


MST11[s_, t_, mV_]:=0


MST12[s_, t_, mV_]:=0


MST13[s_, t_, mV_]:=0


MST22[s_, t_, mV_]:=0


MST23[s_, t_, mV_]:=0


MST33[s_, t_, mV_]:=0


(* ::Section:: *)
(*Spin-summed amplitude square*)


(* Matrix multiplication between the form-factors and interference matrix *)
ComputeIntPatternVH[s_, t_, mV_, X_, {i_, j_}] := Module[{output, ffVector},
	ffVector = FormFactorVectorVH[s, t, X, {i, j}];
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
