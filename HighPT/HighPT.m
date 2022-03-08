(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["e"]
PackageExport["\[Nu]"]
PackageExport["u"]
PackageExport["d"]


PackageExport["Photon"]
PackageExport["ZBoson"]
PackageExport["WBoson"]


PackageExport["SelectTerms"]


PackageExport["SM"]


(* ::Text:: *)
(*Everything below needs to get usage messages when it is finalized*)


PackageExport["SetMode"]
PackageExport["GetMode"]


PackageExport["InitializeModel"]


PackageExport["AddMediator"]


PackageExport["GetMediators"]


PackageExport["Propagator"]


PackageExport["$ParallelHighPT"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["ReplaceChannelSums"]


PackageScope["ReplacePropagators"]


PackageScope["$RunMode"]


(* ::Chapter:: *)
(*Private:*)


(* ::Text:: *)
(*Some usage messages*)


e::usage= "e[\[Alpha]]
	Represents a charged lepton and \[Alpha] is the generation index: e[1] \[Rule] e, e[2] \[Rule] \[Mu], e[3] \[Rule] \[Tau].";


\[Nu]::usage= "\[Nu][\[Alpha]]
	Represents a neutrino and \[Alpha] is the generation index: \[Nu][1] \[Rule] \!\(\*SubscriptBox[\(\[Nu]\), \(e\)]\), \[Nu][2] \[Rule] \!\(\*SubscriptBox[\(\[Nu]\), \(\[Mu]\)]\), \[Nu][3] \[Rule] \!\(\*SubscriptBox[\(\[Nu]\), \(\[Tau]\)]\).
	Often \[Nu] without a specified generation index indicates the summation over all neutrino flavors.";


d::usage= "d[i]
	Represents a down-type quark and i is the generation index: d[1] \[Rule] d, d[2] \[Rule] s, d[3] \[Rule] b.";


u::usage= "u[i]
	Represents an up-type quark and i is the generation index: u[1] \[Rule] u, u[2] \[Rule] c, u[3] \[Rule] t.";


Photon::usage= "Photon
	Denotes the SM photon.";


ZBoson::usage= "ZBoson
	Denotes the SM Z-boson.";


WBoson::usage= "WBoson
	Denotes the SM W-boson.";


SM::usage= "SM
	Denotes a Standard Model form factor.
	Is an Option for EventYield specifying whether the pure Standard Model contribution should be included in the event count.";


(* ::Section:: *)
(*Parallelization*)


$ParallelHighPT=True;


(* ::Section:: *)
(*Defining the run mode*)


$RunMode= "SMEFT";


InitializeModel::usage= "InitializeModel[\"SMEFT\", d]
	Changes the run mode to the SMEFT at dimension d \[Element] {6,8}, where d is an optional argument with default value d=6.

InitializeModel[{mediator1, mediator2, ...}]
	Changes the run mode to an explicite NP model with the given set of mediators.
	Each mediator must be specifies as follows: mediator1={label, mass, width, \"channel\", \"current\", {type}}, 
	where \"channel\" \[Element] {\"s\",\"t\",\"u\",\"tu\"}, \"current\" \[Element] {\"NC\",\"CC\",\"All\"}, and type is one of (or a sequence of) Scalar|Vector|Tensor|DipoleL|DipoleQ.
	The SM mediators (\"\[Gamma]\",\"Z\",\"W\") are defined automatically and should not be specified again.
	All EFT contact interactions are turned of in this mode.
";


InitializeModel::invalidmediator= "The mediator `1` could not be defined."


(* ::Subsection:: *)
(*Initialize a specific NP model*)


InitializeModel[list_List]:= Module[
	{}
	,
	(* reset all mediators *)
	ResetMediators[];
	
	(* define the SM mediators *)
	DefineSM[];
	
	(* removes some unnecessary stuff *)
	SetEFTorder[6];
	
	(* set Model run mode *)
	$RunMode= "Model";
	
	(* define mediators of the given model *)
	Do[
		AddMediator[mediator];
		,
		{mediator,list}
	];
	
	Print["Run mode set to: model with the mediators:"];
	Print["  s-channel: ", GetMediators["s"]];
	Print["  t-channel: ", GetMediators["t"]];
	Print["  u-channel: ", GetMediators["u"]];
]


(* ::Subsection:: *)
(*Initialize the SMEFT*)


Options[InitializeModel]={EFTorder:> GetEFTorder[], OperatorDimension:> GetOperatorDimension[]}


InitializeModel["SMEFT", OptionsPattern[]]:= Module[
	{eftOrd= OptionValue[EFTorder], opDim= OptionValue[OperatorDimension]}
	,
	(* reset all mediators *)
	ResetMediators[];
	
	(* define the SM mediators *)
	DefineSM[];
	
	(* define EFT power counting *)
	SetEFTorder[eftOrd];
	SetOperatorDimension[opDim];
	
	(* set SMEFT run mode *)
	$RunMode= "SMEFT";
	
	Print["Using model: SMEFT"];
	Print["  Maximum operator mass dimension: ", opDim];
	Print["  EFT series truncation at: ", With[{x=ToString[-eftOrd]}, "\!\(\*SubsuperscriptBox[\(\[CapitalLambda]\), \(NP\), " <> x <> "]\)"]];
]


(* ::Subsection:: *)
(*DefineSM*)


(* define the SM mediators \[Gamma],Z,W *)
DefineSM[]:= Module[{param= GetParameters[]},
	(*Photon::uasge= "Photon ccc";*)
	AddMediator[Photon, 0, 0, "s", "NC", {Vector, DipoleL, DipoleQ}];
	AddMediator[ZBoson, param[Mass[ZBoson]], param[Width[ZBoson]], "s", "NC", {Vector, DipoleL, DipoleQ}];
	AddMediator[WBoson, param[Mass[WBoson]], param[Width[WBoson]], "s", "CC", {Vector, DipoleL, DipoleQ}];
	
	(* photons do not couple to neutrinos *)
	FF[_,{Photon,_},_,{OrderlessPatternSequence[_ \[Nu],___]}]:= 0;
	(*Photon/:Conjugate[Photon]= Photon;*) (* This might screw up IbP *)
]


Format[Photon,TraditionalForm]:= "\[Gamma]"
Format[ZBoson,TraditionalForm]:= "Z"
Format[WBoson,TraditionalForm]:= "W"


(* ::Subsubsection:: *)
(*Distinguish CC and NC*)


MakeNC[mediator_]:= Module[{},
	FF[_,{mediator,_},_,{OrderlessPatternSequence[_u,_d,___]}]:= 0;
]


MakeCC[mediator_]:= Module[{},
	FF[_,{mediator,_},_,{OrderlessPatternSequence[_u,_u,___]}]:= 0;
	FF[_,{mediator,_},_,{OrderlessPatternSequence[_d,_d,___]}]:= 0;
]


(* ::Subsection:: *)
(*Mediators*)


$Mediators= <||>;


$Channels["s"]= <||>;
$Channels["t"]= <||>;
$Channels["u"]= <||>;


(* ::Subsubsection:: *)
(*ResetMediators*)


(* resets all mediators *)
ResetMediators[]:= Module[{med= Keys[$Mediators]},
	$Mediators= <||>;
	$Channels["s"]= <||>;
	$Channels["t"]= <||>;
	$Channels["u"]= <||>;
	
	(* remove NC|CC definitions of the pevious mediators *)
	Do[
		Quiet[FF[_,{m,_},_,{OrderlessPatternSequence[_u,_d,___]}]=.];
		Quiet[FF[_,{m,_},_,{OrderlessPatternSequence[_u,_u,___]}]=.];
		Quiet[FF[_,{m,_},_,{OrderlessPatternSequence[_d,_d,___]}]=.];
		,
		{m,med}
	];
	Quiet[FF[_,{Photon,_},_,{OrderlessPatternSequence[_ \[Nu],___]}]=.];
]


(* ::Subsection:: *)
(*AddMediator*)


AddMediator::usage= "AddMediator[label, Mass, Width, channel, current, {type}]
	Defines a new mediator denoted by label. The arguments are its Mass, its Width, the channel \[Element] {'s','t','u','tu'} in which it appears, whether it is colored (current=\"CC\") or not (current=\"NC\"), and the type of Lorentz structure it generates for the four-fermion operators.";


AddMediator[{a___}]:= AddMediator[a]


AddMediator[l_, m_, w_, c:Alternatives["s","t","u","tu"], current:"NC"|"CC"|"All", lorentz_List]:= Module[
	{}
	,
	AppendTo[$Mediators, l->{m,w,c,current,lorentz}];
	Switch[c,
		"s", AppendTo[$Channels["s"], l->{m,w,c,current,lorentz}],
		"t", AppendTo[$Channels["t"], l->{m,w,c,current,lorentz}],
		"u", AppendTo[$Channels["u"], l->{m,w,c,current,lorentz}],
		"tu",AppendTo[$Channels["t"], l->{m,w,c,current,lorentz}]; AppendTo[$Channels["u"], l->{m,w,c,current,lorentz}]
	];
	Switch[current,
		"NC", MakeNC[l];,
		"CC", MakeCC[l];
	];
]


AddMediator[a___]:= Message[InitializeModel::invalidmediator, Flatten[{a}]]


(* ::Subsubsection:: *)
(*GetMediators*)


GetMediators::usage= "GetMediators[arg, type]
	Returns an association containing all defined mediators.
	The argument arg is optional and can be 's' | 't' | 'u' in which case only the mediators of the specified channel are returned.
	The second argument is also optional and can be used to filter by Lorentz structure, i.e. type \[Element] {Scalar,Vector,Tensor,DipoleL,DipoleQ}.";


(* return all mediators *)
GetMediators[]:= $Mediators


(* return all mediators of the specified channel *)
GetMediators[c:Alternatives["s","t","u"]]:= $Channels[c]


(* return all mediators of the specified channel with the specified Lorentz structure that contribute to the CC or NC current *)
(*
GetMediators[c:Alternatives["s","t","u"], current_, typ_]:= Module[
	{channel= $Channels[c]},
	(* loop over all mediators in this channel *)
	Do[
		(* drop if mediator does not have the correct Lorentz structure *)
		If[!MatchQ[Last[channel[key]], {OrderlessPatternSequence[typ,___]}],
			KeyDropFrom[channel,key],
			(* drop if mediator does not contribute to the CC|NC process *)
			If[!MatchQ[(channel[key])[[-2]], (current|"All")],
				KeyDropFrom[channel,key]
			]
		]
		,
		{key, Keys[channel]}
	];
	Return[channel]
]
*)


GetMediators[c:Alternatives["s","t","u"], typ_]:= Module[
	{channel= $Channels[c]},
	(* loop over all mediators in this channel *)
	Do[
		(* drop if mediator does not have the correct Lorentz structure *)
		If[!MatchQ[Last[channel[key]], {OrderlessPatternSequence[typ,___]}],
			KeyDropFrom[channel,key]
		]
		,
		{key, Keys[channel]}
	];
	Return[channel]
]


(* ::Subsection:: *)
(*[now done in init.m] Initialization in SMEFT d=6 mode*)


(* when loading the package initialize with SMEFT at d=6 with NP^2 contributions *)
(*InitializeModel["SMEFT", EFTorder-> 4, OperatorDimension-> 6];*)


(* ::Subsection:: *)
(*ReplaceChannelSums*)


ReplaceChannelSums::usage= "ReplaceChannelSums[]
	Returns a replacement rule with which all SChannelSum, TChannelSum, and UChannelSum can be replaced.";


(* returns the replacement rule to substitute all channel sums with the appropriate sums of propagators *)
ReplaceChannelSums[]:= {
	SChannelSum[s_,FF[type_,{"s",ord_},chirality_,indices_]]:> Sum[
		VEV^2 * 
		(* (* What is this doing? *)
		EFTcontribution[mediator,ord] *
		*)
		FlavorDiagonalSM[mediator, type, ord, indices] * 
		LeftHandedWSM[mediator, type, ord, chirality] * 
		FF[type,{mediator,ord},chirality,indices] * 
		Propagator[s,mediator],
		{mediator, Keys[GetMediators["s", type]]}
	]
	,
	TChannelSum[t_,FF[type_,{"t",0},chirality_,indices_]]:> Sum[
		VEV^2 * 
		FF[type, {mediator}, chirality, indices] * 
		Propagator[t,mediator],
		{mediator, Keys[GetMediators["t", type]]}
	]
	,
	UChannelSum[u_,FF[type_,{"u",0},chirality_,indices_]]:> Sum[
		VEV^2 * 
		FF[type, {mediator}, chirality, indices] * 
		Propagator[u,mediator],
		{mediator, Keys[GetMediators["u", type]]}
	]
}


(* checks whether the given process is a charged current or neutral current process *)
(*
CurrentType[{a_,b_,i_,j_}]:= If[Head[i]===Head[j],
	Return["NC"],
	Return["CC"]
]
*)


(* ::Text:: *)
(*??? remove this ???*)


(* returns 1 if marginal contribution, returns 0 if EFT contribution that is not associated to \[Gamma]|Z|W *)
EFTcontribution[mediator_,ord_]:= If[ord===0,
	Return[1]
	,
	If[MatchQ[mediator,Photon|ZBoson|WBoson],
		Return[1],
		Return[0]
	]
]


(* Ensures that \[Gamma]- and Z-couplings are always flavor diagonal *)
FlavorDiagonalSM[mediator_, type_, ord_, {a_,b_,i_,j_}]:= Module[
	{temp= 1}
	,
	If[ord===SM,
		Switch[{mediator, type},
			{Photon, Vector}, temp= KroneckerDelta[a,b] * KroneckerDelta[i,j],
			{ZBoson, Vector}, temp= KroneckerDelta[a,b] * KroneckerDelta[i,j],
			
			{Photon, DipoleL}, temp= KroneckerDelta[i,j],
			{ZBoson, DipoleL}, temp= KroneckerDelta[i,j],
			
			{Photon, DipoleQ}, temp= KroneckerDelta[a,b],
			{ZBoson, DipoleQ}, temp= KroneckerDelta[a,b]
			
			(* Whats with the W boson? It should be here as well. *)
		]
	];
	
	Return[temp]
]


(* make KroneckerDelta work properly *)
e/:KroneckerDelta[e[i_],a_]:= KroneckerDelta[i,a]
\[Nu]/:KroneckerDelta[\[Nu][i_],a_]:= KroneckerDelta[i,a]
u/:KroneckerDelta[u[i_],a_]:= KroneckerDelta[i,a]
d/:KroneckerDelta[d[i_],a_]:= KroneckerDelta[i,a]


(* ensures that W-couplings are left handed *)
LeftHandedWSM[mediator_, type_, ord_, {X_,Y_}]:= Module[
	{temp= 1}
	,
	If[ord===SM,
		Switch[{mediator, type},
			{WBoson, Vector}, temp= KroneckerDelta[X, Left]*KroneckerDelta[Y, Left],
			
			{WBoson, DipoleL}, temp= KroneckerDelta[Y, Left],
			{WBoson, DipoleQ}, temp= KroneckerDelta[X, Left]
		]
	];
	
	Return[temp/. KroneckerDelta[OrderlessPatternSequence[Right, Left]]->0]
]


(* ::Subsection:: *)
(*Propagator*)


Propagator::usage= "Propagator[\!\(\*SuperscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(A\)]\)] 
	Denotes the propagator of the particle \!\(\*SubscriptBox[\(\[Phi]\), \(A\)]\) with momentum p, i.e. we have Propagator[\!\(\*SuperscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(A\)]\)] = \!\(\*FractionBox[\(1\), \(\*SuperscriptBox[\(p\), \(2\)] - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\).";


(* ::Text:: *)
(*Propagators are real [no they are not !!!]*)


(*Propagator/:Conjugate[x_Propagator]:= x*)


Propagator/:Conjugate[Propagator[a_, b_]]:= Propagator[a, Conjugate[b]]


Format[Propagator[x_,Conjugate[f_]], TraditionalForm]:= Conjugate[1/(x-Mass[f]^2+I*Mass[f]*Width[f])]


Format[Propagator[x_,f_], TraditionalForm]:= 1/(x-Mass[f]^2+I*Mass[f]*Width[f])


(* ::Subsubsection:: *)
(*Replace Propagators*)


ReplacePropagators= {
	Propagator[x_, Conjugate[f_]]:> 1/(x - Mass[f]^2 - I*Mass[f]*Width[f]),
	Propagator[x_, f:Except[_Conjugate]]:> 1/(x - Mass[f]^2 + I*Mass[f]*Width[f])
};


(* ::Section:: *)
(*Selecting particular terms*)


SelectTerms::usage= "SelectTerms[arg, terms]
	Sets all form factors (FF) or Wilson Coefficients (WC), that do not match the expressions given in the list terms, in the argument arg to zero.
	The argument terms should match {_FF..} or {_WC..}.";


SelectTerms[arg_, terms:({_FF..} | {_WC..} | {___})]:= Module[
	{rule}
	,
	Switch[terms,
		{_FF..}, rule= (Except[Alternatives@@terms, _FF]:> 0),
		{_WC..}, rule= (Except[Alternatives@@terms, _WC]:> 0),
		{___}  , rule= {_WC -> 0, _FF -> 0}
	];
	Return[(arg/.rule)/.{0.->0,Complex[0.,0.]->0}]
]


(* ::Section:: *)
(*Main Options*)


PackageScope["OptionCheck"]


PackageExport["Coefficients"]


PackageExport["OutputFormat"]


PackageExport["EFTorder"]


PackageExport["OperatorDimension"]


PackageExport["Luminosity"]


PackageExport["PTcuts"]


PackageExport["MLLcuts"]


PTcuts::usage= "PTcuts -> {\!\(\*SubsuperscriptBox[\(p\), \(T\), \(min\)]\),\!\(\*SubsuperscriptBox[\(p\), \(T\), \(max\)]\)}
	Option that specifies the \!\(\*SubscriptBox[\(p\), \(T\)]\) cuts (in units of GeV) that should be used for the computation of cross sections.
	The OptionValues must satisfy 0 \[LessEqual] \!\(\*SubsuperscriptBox[\(p\), \(T\), \(min\)]\) < \!\(\*SubsuperscriptBox[\(p\), \(T\), \(max\)]\) \[LessEqual] \[Infinity].
	Can be used with: CrossSection, DifferentialCrossSection";


MLLcuts::usage= "MLLcuts -> {\!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(min\)]\),\!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(max\)]\)}
	Option that specifies the \!\(\*SubscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\)]\) cuts (in units of GeV) that should be used for the computation of cross sections.
	The OptionValues must satisfy 16 \[LessEqual] \!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(min\)]\) < \!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(max\)]\) \[LessEqual] 13000.
	Can be used with: CrossSection, DifferentialCrossSection";


EFTorder::usage= "EFTorder -> n
	Option specifying that the result of the computation should be truncated at order (\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\)\!\(\*SuperscriptBox[\()\), \(-n\)]\).
	The deault value is given by n=GetEFTorder[].
	This option can be used with: DifferentialCrossSection, CrossSection, EventYield, MatchToSMEFT.";


OperatorDimension::usage= "OperatorDimension -> d
	Option specifying that EFT operators up to mass dimension d should be considered.
	The deault value is given by d=GetOperatorDimension[].
	This option can be used with: DifferentialCrossSection, CrossSection, EventYield, MatchToSMEFT.";


OutputFormat::usage= "OutputFormat -> X
	Option specifying whether the result should be expressed in terms of:
		- form factors (default): X=FF
		- Wilson coefficients with NP scale \[CapitalLambda]: X={\"SMEFT\",\[CapitalLambda]}
		- coupling constants: [to be added]
	This option can be used with: DifferentialCrossSection, CrossSection, EventYield.";


Coefficients::usage= "Coefficients -> X
	Option specifying which form factors (FF) or Wilson coefficients (WC) should be considered.
	The default value is X=All, meaning all FF or WC should be used.
	If X is a list of either FF or WC only these are kept and all other coefficients are set to zero.	
	This option can be used with: DifferentialCrossSection, CrossSection, EventYield.";


Luminosity::usage= "Luminosity
	Option that determines the integrated Luminosity used for the event yield computation in units of \!\(\*SuperscriptBox[\(fb\), \(-1\)]\).
	The default value is 139.
	This option can be used with: EventYield.";


(* ::Subsection:: *)
(*Check allowed OptionValues*)


(* ::Text:: *)
(*Function to check if OptionValues are well defined*)


OptionCheck[opt_, optVal_]:= If[!MatchQ[optVal, $OptionValueAssociation[opt]],
	Message[OptionCheck::optionvalue, opt, optVal, $OptionValueAssociation[opt]];
	Abort[]
];


(* ::Text:: *)
(*List of well defined values for the Options*)


$OptionValueAssociation= <|
	OutputFormat -> FF | {"SMEFT",_},
	Coefficients -> All | {} | {_FF..} | {_WC..} (*| {Rule[_WC,_]..} | {Rule[_FF,_]..}*),
	EFTorder -> 0 | 2 | 4 | 6 | 8,
	OperatorDimension -> 4 | 6 | 8,
	Luminosity -> Default | _?NumericQ,
	PTcuts -> ({min_?NumericQ, max_?NumericQ}/;(0<=min<max)) | ({min_?NumericQ,\[Infinity]}/;0<=min),
	MLLcuts -> {min_?NumericQ, max_?NumericQ}/;(16<=min<max<=13000)
|>;


OptionCheck::optionvalue= "Invalid OptionValue specified: `1`->`2`, the allowed values for `1` must match `3`.";


(* ::Section:: *)
(*EchoTiming*)


(* ::Text:: *)
(*Include custom EchoTiming for v12.1 and older*)


(*
If[($VersionNumber==12 && $ReleaseNumber<=2) ||$VersionNumber<12,
	SetAttributes[HighPT`PackageScope`EchoTiming,{HoldFirst,SequenceHold}];
	HighPT`PackageScope`EchoTiming[x_]:= Evaluate@Module[{time,result},
		{time,result}=Timing[x];
		Echo[time, "time: "];
		Return[result]
	];
	HighPT`PackageScope`EchoTiming[x_,y_]:=Evaluate@Module[{time,result},
		{time,result}=Timing[x];
		Echo[time,"time: " <> ToString[y] <>": "];
		Return[result]
	];
];
*)
