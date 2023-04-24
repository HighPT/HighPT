(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`*)


(* ::Subtitle:: *)
(*General functions and definitions.*)


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


PackageExport["SelectTerms"]


PackageExport["InitializeModel"]


PackageExport["$ParallelHighPT"]


PackageExport["HighPTLogo"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["ReplaceChannelSums"]


PackageScope["Propagator"]


PackageScope["ReplacePropagators"]


PackageScope["$RunMode"]


PackageScope["$defaultMediatorProperties"]


PackageScope["$Verbose"]
PackageScope["MyTiming"]
PackageScope["MyEcho"]


PackageScope["ConditionalPrint"]


PackageScope["$AllowedMasses"]


PackageScope["MyExpand"]
PackageScope["ExpandConjugate"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section::Closed:: *)
(*Usage messages*)


e::usage= "e[\[Alpha]] represents a charged lepton and \[Alpha] is the generation index: e[1] \[Rule] e, e[2] \[Rule] \[Mu], e[3] \[Rule] \[Tau].";


\[Nu]::usage= "\[Nu][\[Alpha]] represents a neutrino and \[Alpha] is the generation index: \[Nu][1] \[Rule] \!\(\*SubscriptBox[\(\[Nu]\), \(e\)]\), \[Nu][2] \[Rule] \!\(\*SubscriptBox[\(\[Nu]\), \(\[Mu]\)]\), \[Nu][3] \[Rule] \!\(\*SubscriptBox[\(\[Nu]\), \(\[Tau]\)]\). Ususally \[Nu] without specifying a generation index indicates the summation over all neutrino flavors.";


d::usage= "d[\[ScriptI]] represents a down-type quark and \[ScriptI] is the generation index: d[1] \[Rule] d, d[2] \[Rule] s, d[3] \[Rule] b.";


u::usage= "u[\[ScriptI]] represents an up-type quark and \[ScriptI] is the generation index: u[1] \[Rule] u, u[2] \[Rule] c, u[3] \[Rule] t.";


(* ::Section::Closed:: *)
(*Parallelization*)


$ParallelHighPT::usage = "$ParallelHighPT is a boolean variable that indicates whether EventYield and ChiSquareLHC should be parallelized. The default is $ParallelHighPT=True.";


(* ::Text:: *)
(*By default parallelization is turned on*)


$ParallelHighPT=True;


(* ::Section::Closed:: *)
(*InitializeModel*)


$RunMode = "SMEFT";


InitializeModel::usage= 
"InitializeModel[\"SMEFT\"] activates the SMEFT run mode (default). No BSM mediators are considered. The allowed options and their default values are: EFTorder \[Rule] 4; OperatorDimension \[Rule] 6; EFTscale \[Rule] 1000.
InitializeModel[\"Mediators\"] activates the BSM mediator run mode. Heavy mediators can be specified using the Mediator Option as follows: Mediators \[Rule] {\"U1\"\[Rule]{2000,0},\"U3\"\[Rule]{3000,0},...},  which would define a \!\(\*SubscriptBox[\(U\), \(1\)]\) leptoquark with mass 2 TeV and zero width as well as a \!\(\*SubscriptBox[\(U\), \(3\)]\) leptowuark with mass 3 TeV and zero width. Notice that currently interference terms of two BSM mediator can only be computed on cross-section level, but they are set to zero for EventYield and ChiSquareLHC, due to missing Monte Carlo simulation. Furthermore, only a discrete set of masses is available. All EFT contact interactions are turned of in this mode.";


InitializeModel::invalidmediator= "The mediator `1` could not be defined."


(* ::Subsection::Closed:: *)
(*Initialize the SMEFT*)


Options[InitializeModel]={
	(* SMEFT *)
	EFTorder          -> 4,
	OperatorDimension -> 6,
	EFTscale          -> 1000,
	(* mediator *)
	Mediators         -> {}
}


InitializeModel["SMEFT", OptionsPattern[]]:= Module[
	{
		eftOrd= OptionValue[EFTorder],
		opDim=  OptionValue[OperatorDimension],
		\[CapitalLambda]NP=    OptionValue[EFTscale]
	}
	,
	(* reset all mediators *)
	ResetMediators[];
	
	(* define the SM mediators *)
	DefineSM[True];
	
	(* define EFT power counting *)
	SetEFTorder[eftOrd];
	SetOperatorDimension[opDim];
	SetEFTscale[\[CapitalLambda]NP];
	
	(* set SMEFT run mode *)
	$RunMode= "SMEFT";
	
	Print["Initialized SMEFT mode:"];
	Print["  Maximum operator dimension: ", opDim];
	Print["  EFT series truncation at: ", With[{x=ToString[-eftOrd]}, "\!\(\*SubsuperscriptBox[\(\[CapitalLambda]\), \(NP\), " <> x <> "]\)"]];
	Print["  EFT cutoff scale \!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\): ", N[\[CapitalLambda]NP/1000], " TeV"];
]


(* ::Subsection::Closed:: *)
(*Initialize a specific NP model*)


InitializeModel::undefmed= "The given mediator `1` is not in the list of predefined mediators: `2`"


InitializeModel::undefmass= "Mediator `1` of mass `2` GeV defined. Currently Monte Carlo data is only available for mediators of mass \!\(\*SubscriptBox[\(M\), \(BSM\)]\) \[Element] `3` GeV. For other masses you can only compute cross sections, but no event yields or likelihoods."


(* fix this number *)
InitializeModel::undefwidth= "In the current version only mediators with a fixed width of ??? GeV are supported."


InitializeModel::multimediator= "Multiple BSM mediator specified. Currently interference terms can only be computed on cross section level, but not for event yields or likelihoods due too missing Monte Carlo simulations."


$AllowedMasses = Alternatives[1000, 2000, 3000 (*, 4000, 5000*)]


InitializeModel["Mediators", OptionsPattern[]]:= Module[
	{
		mediators = OptionValue[Mediators]
	}
	,
	(* OPTION CHECK *)
	OptionCheck[#,OptionValue[#]]& /@ {Mediators};
	
	(* Show warning regarding interference terms if multiple mediators have been specified. *)
	If[Length[mediators]>1,
		Message[InitializeModel::multimediator]
	];
	
	(* make association *)
	mediators = Association[mediators];
	
	(* check that all mediator labels are known *)
	Do[
		If[!MatchQ[med, Alternatives@@Keys[$MediatorList]],
			Message[InitializeModel::undefmed, med, Keys@$MediatorList];
			Abort[]
		]
		,
		{med, Keys@mediators}
	];
	
	(* check mass and width *)
	Do[
		If[!MatchQ[First[mediators[mediator]], $AllowedMasses],
			Message[InitializeModel::undefmass, mediator, First[mediators[mediator]], List@@$AllowedMasses];
		];
		(* allow for all widths *)
		(*If[!MatchQ[Last[mediators[mediator]], 0],
			Message[InitializeModel::undefwidth];
		]*)
		,
		{mediator, Keys@KeyDrop[mediators,{"Photon","ZBoson","WBoson"}]}
	];
	
	(* reset all mediators *)
	ResetMediators[];
	
	(* define the SM mediators *)
	DefineSM[False];
	
	(* removes some unnecessary stuff *)
	SetEFTorder[0];
	SetOperatorDimension[4];
	
	(* set EFT contributions to SM mediators to zero *)
	FF[_,{"Photon",0},___]=0;
	FF[_,{"ZBoson",0},___]=0;
	FF[_,{"WBoson",0},___]=0;
	
	(* set Model run mode *)
	$RunMode= "Model";
	
	(* add all mediators to the model *)
	Do[
		AddMediator[med, First@mediators[med], Last@mediators[med], Sequence@@$MediatorList[med]];
		,
		{med, Keys[mediators]}
	];
	
	(* save default masses and widths for BSM mediators *)
	$defaultMediatorProperties = KeyDrop[mediators {"Photon","ZBoson","WBoson"}];
	
	(* print model info *)
	Print["Initialized mediator mode:"];
	Print["  s-channel: ", GetMediators["s"]];
	Print["  t-channel: ", GetMediators["t"]];
	Print["  u-channel: ", GetMediators["u"]];
]


$defaultMediatorProperties = <||>


(* ::Section::Closed:: *)
(*Auxiliary functions for defining and using a model*)


(* ::Subsection::Closed:: *)
(*Lists of currently used mediators*)


$Mediators= <||>;


$Channels["s"]= <||>;
$Channels["t"]= <||>;
$Channels["u"]= <||>;


(* ::Subsection::Closed:: *)
(*Make mediators only contribute to CC or NC processes if required.*)


MakeNC[mediator_]:= Module[{},
	FF[_,{mediator,_},_,{OrderlessPatternSequence[_u,_d,___]}]:= 0;
]


MakeCC[mediator_]:= Module[{},
	FF[_,{mediator,_},_,{OrderlessPatternSequence[_u,_u,___]}]:= 0;
	FF[_,{mediator,_},_,{OrderlessPatternSequence[_d,_d,___]}]:= 0;
]


(* ::Subsection::Closed:: *)
(*AddMediator*)


AddMediator::usage= "AddMediator[label, Mass, Width, channel, current, {type}] defines a new mediator denoted by label. The arguments are its Mass, its Width, the channel \[Element] {'s','t','u','tu'} in which it appears, whether it is colored (current=\"CC\") or not (current=\"NC\"), and the type of Lorentz structure it generates for the four-fermion operators.";


AddMediator[l_, m_, w_, c:{("s"|"t"|"u")..}, current:{("NC"|"CC")..}, lorentz:{(Scalar|Vector|Tensor|DipoleL|DipoleQ)..}]:= Module[
	{}
	,
	(* add mediator to list of currently used mediators *)
	AssociateTo[
		$Mediators,
		l -> <|
			Mass      -> m,
			Width     -> w,
			"channel" -> c,
			"current" -> current,
			"type"    -> lorentz
		|>
	];
	
	(* append mediator to all channels it contributes to *)
	Do[
		AppendTo[$Channels[chan], l -> lorentz]
		,
		{chan,c}
	];
	
	(* make the mediators NC or CC *)
	If[FreeQ[current,"NC"],
		MakeCC[l]
	];
	If[FreeQ[current,"CC"],
		MakeNC[l]
	];
	
	(* Make propagators of mediators with zero width mediators real *)
	If[w==0,
		Propagator[x_,Conjugate[l]] := Propagator[x,l]
	];
];


AddMediator[a___]:= Message[InitializeModel::invalidmediator, Flatten[{a}]]


(* ::Subsection::Closed:: *)
(*ResetMediators*)


(* resets all mediators *)
ResetMediators[]:= Module[{med= Keys[$Mediators]},
	$Mediators= <||>;
	$Channels["s"]= <||>;
	$Channels["t"]= <||>;
	$Channels["u"]= <||>;
	$defaultMasses = <||>;
	$defaultWidths = <||>;
	
	(* remove NC|CC definitions of the previously used mediators *)
	Do[
		Quiet[FF[_,{m,_},_,{OrderlessPatternSequence[_u,_d,___]}]=.];
		Quiet[FF[_,{m,_},_,{OrderlessPatternSequence[_u,_u,___]}]=.];
		Quiet[FF[_,{m,_},_,{OrderlessPatternSequence[_d,_d,___]}]=.];
		Quiet[Propagator[_,Conjugate[m]]=.];
		,
		{m,med}
	];
	Quiet[FF[_,{"Photon",_},_,{OrderlessPatternSequence[_\[Nu],___]}]=.];
	
	Quiet[FF[_,{"Photon",0},___]=.];
	Quiet[FF[_,{"ZBoson",0},___]=.];
	Quiet[FF[_,{"WBoson",0},___]=.];
]


(* ::Subsection::Closed:: *)
(*GetMediators*)


GetMediators::usage =
"GetMediators[] returns an Association of all currently defined mediators and their properties.
GetMediators[\[ScriptC]\[ScriptH]\[ScriptA]\[ScriptN]\[ScriptN]\[ScriptE]\[ScriptL]] returns an Association of all currently defined mediators propagating in the \[ScriptC]\[ScriptH]\[ScriptA]\[ScriptN]\[ScriptN]\[ScriptE]\[ScriptL] \[Element]{\"s\",\"t\",\"u\"}.
GetMediators[\[ScriptC]\[ScriptH]\[ScriptA]\[ScriptN]\[ScriptN]\[ScriptE]\[ScriptL], \[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]] returns an Association containing all defined mediators propagating in the \[ScriptC]\[ScriptH]\[ScriptA]\[ScriptN]\[ScriptN]\[ScriptE]\[ScriptL] \[Element]{\"s\",\"t\",\"u\"} with Lorentz structure \[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE] \[Element] {Scalar,Vector,Tensor,DipoleL,DipoleQ}.";


(* return all mediators *)
GetMediators[] := $Mediators


(* return all mediators of the specified channel *)
GetMediators[c:Alternatives["s","t","u"]] := $Channels[c]


(* return all mediators of the specified channel and Lorentz structure *)
GetMediators[c:Alternatives["s","t","u"], typ_]:= Module[
	{channel= $Channels[c]},
	(* loop over all mediators in this channel *)
	Do[
		(* drop if mediator does not have the correct Lorentz structure *)
		If[FreeQ[channel[key], typ],
			KeyDropFrom[channel,key]
		]
		,
		{key, Keys[channel]}
	];
	Return[channel]
]


(* ::Subsection::Closed:: *)
(*Modify mediator Mass and/or Width*)


Options[ModifyMediator]={
	Mediators -> <||>
}


(* allows to modify the mass and/or width of defined BSM mediators *)
ModifyMediator[OptionsPattern[]]:=Module[
	{
		mediators = OptionValue[Mediators]
	}
	,
	(* change masses and widths *)
	Do[
		If[KeyExistsQ[$Mediators, med],
			$Mediators[med][Mass]  = First[mediators[med]];
			$Mediators[med][Width] = Last[ mediators[med]];
		];
		,
		{med,Keys[mediators]}
	];
];


(* ::Subsection::Closed:: *)
(*DefineSM*)


(* define the SM mediators \[Gamma],Z,W *)
DefineSM[eft_]:= Module[
	{
		param = GetParameters[],
		lorentz
	},
	(* include dipoles in EFT scenario *)
	If[eft,
		lorentz = {Vector,DipoleL,DipoleQ},
		lorentz = {Vector}
	];
	
	(* add SM mediators *)
	AddMediator["Photon", 0, 0, {"s"}, {"NC"}, lorentz];
	AddMediator["ZBoson", param[Mass["ZBoson"]], param[Width["ZBoson"]], {"s"}, {"NC"}, lorentz];
	AddMediator["WBoson", param[Mass["WBoson"]], param[Width["WBoson"]], {"s"}, {"CC"}, lorentz];
	
	(* photons do not couple to neutrinos *)
	FF[_,{"Photon",_},_,{OrderlessPatternSequence[_\[Nu],___]}]:= 0;
];


(* ::Subsection::Closed:: *)
(*ReplaceChannelSums*)


ReplaceChannelSums::usage= "ReplaceChannelSums[] returns a replacement rule with which all SChannelSum, TChannelSum, and UChannelSum can be replaced.";


(* returns the replacement rule to substitute all channel sums with the appropriate sums of propagators *)
ReplaceChannelSums[]:= {
	SChannelSum[s_,FF[type_,{"s",ord_},chirality_,indices_]]:> Sum[
		Param["vev"]^2 * 
		FlavorDiagonalSM[mediator, type, ord, indices] * 
		LeftHandedWSM[mediator, type, ord, chirality] * 
		FF[type,{mediator,ord},chirality,indices] * 
		Propagator[s,mediator],
		{mediator, Keys[GetMediators["s", type]]}
	]
	,
	TChannelSum[t_,FF[type_,{"t",0},chirality_,indices_]]:> Sum[
		Param["vev"]^2 * 
		FF[type, {mediator,0}, chirality, indices] * 
		Propagator[t,mediator],
		{mediator, Keys[GetMediators["t", type]]}
	]
	,
	UChannelSum[u_,FF[type_,{"u",0},chirality_,indices_]]:> Sum[
		Param["vev"]^2 * 
		FF[type, {mediator,0}, chirality, indices] * 
		Propagator[u,mediator],
		{mediator, Keys[GetMediators["u", type]]}
	]
}


(* ::Subsubsection::Closed:: *)
(*SM properties*)


(* Ensures that \[Gamma]- and Z-couplings are always flavor diagonal *)
FlavorDiagonalSM[mediator_, type_, ord_, {a_,b_,i_,j_}]:= Module[
	{temp= 1}
	,
	If[ord===SM,
		Switch[{mediator, type},
			{"Photon", Vector}, temp= KroneckerDelta[a,b] * KroneckerDelta[i,j],
			{"ZBoson", Vector}, temp= KroneckerDelta[a,b] * KroneckerDelta[i,j],
			
			{"Photon", DipoleL}, temp= KroneckerDelta[i,j],
			{"ZBoson", DipoleL}, temp= KroneckerDelta[i,j],
			
			{"Photon", DipoleQ}, temp= KroneckerDelta[a,b],
			{"ZBoson", DipoleQ}, temp= KroneckerDelta[a,b]
		]
	];
	
	Return[temp]
]


(* ensures that W-couplings are left handed *)
LeftHandedWSM[mediator_, type_, ord_, {X_,Y_}]:= Module[
	{temp= 1}
	,
	If[ord===SM,
		Switch[{mediator, type},
			{"WBoson", Vector}, temp= KroneckerDelta[X, Left]*KroneckerDelta[Y, Left],
			
			{"WBoson", DipoleL}, temp= KroneckerDelta[Y, Left],
			{"WBoson", DipoleQ}, temp= KroneckerDelta[X, Left]
		]
	];
	
	Return[temp/. KroneckerDelta[OrderlessPatternSequence[Right, Left]]->0]
]


(* ::Subsubsection::Closed:: *)
(*KroneckerDelta*)


(* make KroneckerDelta work properly *)
e/:KroneckerDelta[e[i_],a_]:= KroneckerDelta[i,a]
\[Nu]/:KroneckerDelta[\[Nu][i_],a_]:= KroneckerDelta[i,a]
u/:KroneckerDelta[u[i_],a_]:= KroneckerDelta[i,a]
d/:KroneckerDelta[d[i_],a_]:= KroneckerDelta[i,a]


(* ::Subsection::Closed:: *)
(*Propagator*)


Propagator::usage= "Propagator[\!\(\*SuperscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(A\)]\)] denotes the propagator of the particle \!\(\*SubscriptBox[\(\[Phi]\), \(A\)]\) with momentum p, i.e. we have Propagator[\!\(\*SuperscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(A\)]\)] = \!\(\*FractionBox[\(1\), \(\*SuperscriptBox[\(p\), \(2\)] - \*SuperscriptBox[SubscriptBox[\(M\), \(A\)], \(2\)] + \*SubscriptBox[\(\[ImaginaryI]\[CapitalGamma]\), \(A\)] \*SubscriptBox[\(M\), \(A\)]\)]\).";


(* ::Subsubsection::Closed:: *)
(*Conjugated Propagators*)


Propagator/:Conjugate[Propagator[a_, b_]]:= Propagator[a, Conjugate[b]]


(* ::Subsubsection::Closed:: *)
(*Formatting*)


Format[Propagator[x_,Conjugate[f_]], TraditionalForm]:= Conjugate[1/(x-Mass[f]^2+I*Mass[f]*Width[f])]


Format[Propagator[x_,f_], TraditionalForm]:= 1/(x-Mass[f]^2+I*Mass[f]*Width[f])


(* ::Subsubsection::Closed:: *)
(*Replace Propagators*)


ReplacePropagators= {
	Propagator[x_, Conjugate[f_]]:> 1/(x - Mass[f]^2 - I*Mass[f]*Width[f]),
	Propagator[x_, f:Except[_Conjugate]]:> 1/(x - Mass[f]^2 + I*Mass[f]*Width[f])
};


(* ::Section:: *)
(*Selecting particular terms*)


SelectTerms::usage= "SelectTerms[\[ScriptA]\[ScriptR]\[ScriptG], \[ScriptT]\[ScriptE]\[ScriptR]\[ScriptM]\[ScriptS]] returns \[ScriptA]\[ScriptR]\[ScriptG] with all form factors (FF) or Wilson Coefficients (WC), that do not match the expressions given in the List \[ScriptT]\[ScriptE]\[ScriptR]\[ScriptM]\[ScriptS], set to zero.";


SelectTerms::conj="Conjugated coefficient given `1`. Probably the Hermitian conjugated version of `2` was specified. Keeping all instances of `2`.";


SelectTerms[arg_, termsIN:{(_FF | _WC | _Coupling | Conjugate[_FF] | Conjugate[_WC] | Conjugate[_Coupling])..}]:= Module[
	{ruleWC, ruleFF, ruleC, conj, terms=termsIN/.Conjugate[x_]:>x}
	,
	(* check for conjugated coeffs *)
	conj = Cases[termsIN, _Conjugate, All];
	Do[
		Message[SelectTerms::conj,TraditionalForm[con],TraditionalForm[con/.Conjugate[x_]:>x]]
		,
		{con,conj}
	];
	
	(* create replacement rule *)
	ruleFF = {Except[Alternatives@@Cases[terms, _FF, All], _FF] :> 0};
	ruleWC = {Except[Alternatives@@Cases[terms, _WC, All], _WC] :> 0};
	ruleC = {Except[Alternatives@@Cases[terms, _Coupling, All], _Coupling] :> 0};

	Return[(arg/.ruleFF/.ruleWC/.ruleC)/.{0.->0,Complex[0.,0.]->0}]
]


(* ::Section::Closed:: *)
(*Auxiliary function*)


SetAttributes[MyTiming, HoldFirst];
SetAttributes[MyEcho, HoldFirst];


$Verbose = 0


(* ::Subsection::Closed:: *)
(*Echo functions*)


MyTiming[arg_]:= If[$Verbose==0,
	Return[arg]
	,
	Return@EchoTiming[arg]
]


MyTiming[arg_, aux_]:= If[$Verbose==0,
	Return[arg]
	,
	Return@EchoTiming[arg, aux]
]


MyEcho[arg_]:= If[$Verbose<=1,
	Return[arg]
	,
	Return@Echo[arg]
]


MyEcho[arg_, aux_]:= If[$Verbose<=1,
	Return[arg]
	,
	Return@Echo[arg, aux]
]


MyEcho[arg_, aux_, f_]:= If[$Verbose<=1,
	Return[arg]
	,
	Echo[f[arg], aux];
	Return[arg]
]


(* ::Subsection::Closed:: *)
(*Printing*)


(* function that only prints if $PrintingProcessInfo is turned on *)
ConditionalPrint[arg_] := If[$PrintingProcessInfo, Print[arg]]


(* ::Subsection::Closed:: *)
(*ExpandConjugate*)


ExpandConjugate[arg_]:= (arg //. {Conjugate[x_Plus]:> Conjugate/@x, Conjugate[x_Times]:> Conjugate/@x})


(* ::Subsection::Closed:: *)
(*MyExpand*)


(* ::Text:: *)
(*Can be faster  for large sums*)


MyExpand[arg:(_Plus|_List)]:= Expand/@arg


MyExpand[arg:Except[_Plus|_List]]:= Expand[arg]


(* ::Section::Closed:: *)
(*HighPT logo for Plotting*)


HighPTLogo::usage = 
"HighPTLogo[] returns a Graphics object containing the HighPT logo that can be included in plots using the Inset[HighPTLogo[],...] routine.";


HighPTLogo[] := If[$VersionNumber < 13.0,
	First@ Import[FileNameJoin[{Global`$DirectoryHighPT, "Kernel", "HighPT_plot_logo.pdf"}]]
	,
	First@ Import[FileNameJoin[{Global`$DirectoryHighPT, "Kernel", "HighPT_plot_logo.pdf"}],"PageGraphics"]
]
