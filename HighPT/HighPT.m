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


(*PackageExport[""Photon""]*)
(*PackageExport[""ZBoson""]*)
(*PackageExport[""WBoson""]*)


PackageExport["SelectTerms"]


PackageExport["SM"]


(* ::Text:: *)
(*Everything below needs to get usage messages when it is finalized*)


PackageExport["InitializeModel"]


PackageExport["AddMediator"]


PackageExport["GetMediators"]


PackageScope["ModifyMediator"]


PackageExport["Propagator"]


PackageExport["$ParallelHighPT"]


PackageExport["HighPTLogo"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["ReplaceChannelSums"]


PackageScope["ReplacePropagators"]


PackageScope["$RunMode"]


PackageScope["$ModelMass"]


PackageScope["$Verbose"]
PackageScope["MyTiming"]
PackageScope["MyEcho"]


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


(*"Photon"::usage= ""Photon"
	Denotes the SM photon.";*)


(*"ZBoson"::usage= ""ZBoson"
	Denotes the SM Z-boson.";*)


(*"WBoson"::usage= ""WBoson"
	Denotes the SM W-boson.";*)


SM::usage= "SM
	Denotes a Standard Model form factor.
	Is an Option for EventYield specifying whether the pure Standard Model contribution should be included in the event count.";


(* ::Section:: *)
(*Parallelization*)


$ParallelHighPT=True;


(* ::Section:: *)
(*Defining the run mode*)


(* ::Text:: *)
(*By default the SMEFT modes is chosen with:*)
(*InitializeModel[*)
(*	"SMEFT",*)
(*	EFTorder :> 4*)
(*	OperatorDimension :> 6*)
(*]*)
(*this is done in the init.m*)


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
(*Initialize the SMEFT*)


Options[InitializeModel]={
	EFTorder          -> 4,
	OperatorDimension -> 6,
	EFTscale             -> 1000
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
	Print["  Maximum operator mass dimension: ", opDim];
	Print["  EFT series truncation at: ", With[{x=ToString[-eftOrd]}, "\!\(\*SubsuperscriptBox[\(\[CapitalLambda]\), \(NP\), " <> x <> "]\)"]];
	Print["  EFT cutoff scale \!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\): ", N[\[CapitalLambda]NP/1000], " TeV"];
]


(* ::Subsection:: *)
(*Initialize a specific NP model*)


InitializeModel::undefmed= "The given mediator `1` is not in the list of predefined mediators: `2`"


InitializeModel::undefmass= "In the current version only mediators with a fixed mass of 2000 GeV are supported."


InitializeModel::undefwidth= "In the current version only mediators with a fixed width of ??? GeV are supported."


(* ::Subsubsection:: *)
(*new*)


InitializeModel[{med_String, mass_, width_}]:= Module[
	{}
	,
	(* check that med corresponds to a defined mediator *)
	If[!MatchQ[med, Alternatives@@Keys[$MediatorList]],
		Message[InitializeModel::undefmed, med, Keys@$MediatorList];
		Abort[]
	];
	
	(* check mass and width *)
	If[mass=!=2000,
		Message[InitializeModel::undefmass];
		(*Abort[]*) (* uncommented for 5 TeV runs *)
	];
	(*If[width=!=_,
		Message[InitializeModel::undefwidth];
		Abort[]
	];*)
	
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
	$ModelMass= mass;
	
	(* add a mediators to the model *)
	AddMediator[med, mass, width, Sequence@@$MediatorList[med]];
	(* no support for multiple NP mediators as of now *)
	(*Do[
		AddMediator[mediator];
		,
		{mediator,list}
	];*)
	
	Print["Initialized mediator mode:"];
	Print["  s-channel: ", GetMediators["s"]];
	Print["  t-channel: ", GetMediators["t"]];
	Print["  u-channel: ", GetMediators["u"]];
]


(* ::Subsubsection:: *)
(*old*)


(*InitializeModel[list_List]:= Module[
	{}
	,
	(* reset all mediators *)
	ResetMediators[];
	
	(* define the SM mediators *)
	DefineSM[];
	
	(* removes some unnecessary stuff *)
	SetEFTorder[0];
	
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
]*)


(* ::Subsection:: *)
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
	(*"Photon"/:Conjugate["Photon"]= "Photon";*) (* This might screw up IbP *)
];


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


(* ::Subsection:: *)
(*AddMediator*)


AddMediator::usage= "AddMediator[label, Mass, Width, channel, current, {type}]
	Defines a new mediator denoted by label. The arguments are its Mass, its Width, the channel \[Element] {'s','t','u','tu'} in which it appears, whether it is colored (current=\"CC\") or not (current=\"NC\"), and the type of Lorentz structure it generates for the four-fermion operators.";


AddMediator[{a___}]:= AddMediator[a]


AddMediator[l_, m_, w_, c:{("s"|"t"|"u")..}, current:{("NC"|"CC")..}, lorentz:{(Scalar|Vector|Tensor|DipoleL|DipoleQ)..}]:= Module[
	{}
	,
	(* add mediator to list of currently used mediators *)
	AppendTo[$Mediators, l->{m,w,c,current,lorentz}];
	(* append mediator to all channels it contributes to *)
	Do[
		AppendTo[$Channels[chan], l->{m,w,c,current,lorentz}]
		,
		{chan,c}
	];
	
	(* this needs a rework *)
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


Options[ModifyMediator]={
	Mass  -> Default,
	Width -> Default
}


ModifyMediator[med_, OptionsPattern[]]:=Module[
	{temp  =$Mediators[med], mass, width},
	(* fix new mass *)
	If[OptionValue[Mass] === Default,
		mass = temp[[1]]
		,
		mass = OptionValue[Mass]
	];
	(* fix new width *)
	If[OptionValue[Width] === Default,
		width = temp[[2]]
		,
		width = OptionValue[Width]
	];
	temp = {mass,width,temp[[3]],temp[[4]],temp[[5]]};
	
	(* modify mediator in the list of currently used mediators *)
	AssociateTo[$Mediators, med->temp];
	(* modify mediator in all channels it contributes to *)
	Do[
		AssociateTo[$Channels[chan], med->temp]
		,
		{chan,temp[[3]]}
	];
]


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
(*ReplaceChannelSums*)


ReplaceChannelSums::usage= "ReplaceChannelSums[]
	Returns a replacement rule with which all SChannelSum, TChannelSum, and UChannelSum can be replaced.";


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
			{"WBoson", Vector}, temp= KroneckerDelta[X, Left]*KroneckerDelta[Y, Left],
			
			{"WBoson", DipoleL}, temp= KroneckerDelta[Y, Left],
			{"WBoson", DipoleQ}, temp= KroneckerDelta[X, Left]
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


SelectTerms::conj="Conjugated coefficient given `1`. Probably the Hermitian conjugated version of `2` was specified. Keeping all instances of `2`.";


SelectTerms[arg_, termsIN:{(_FF | _WC | _Coupling | Conjugate[_FF] | Conjugate[_WC] | Conjugate[_Coupling])..}]:= Module[
	{rule, conj, terms=termsIN/.Conjugate[x_]:>x}
	,
	(* check for conjugated coeffs *)
	conj = Cases[termsIN, _Conjugate, All];
	Do[
		Message[SelectTerms::conj,TraditionalForm[con],TraditionalForm[con/.Conjugate[x_]:>x]]
		,
		{con,conj}
	];
	
	(* create replacement rule *)
	rule = {Except[Alternatives@@terms, (_FF |\[NonBreakingSpace]_WC |\[NonBreakingSpace]_Coupling)] :> 0};
	(*
	Switch[terms,
		{_FF..}, rule= {Except[Alternatives@@terms, _FF] :> 0},
		{_WC..}, rule= {Except[Alternatives@@terms, _WC] :> 0},
		{_Coupling..}, rule= {Except[Alternatives@@terms, _Coupling] :> 0},
		{___}  , rule= {_WC -> 0, _FF -> 0, _Coupling ->0}
	];
	*)
	Return[(arg/.rule)/.{0.->0,Complex[0.,0.]->0}]
]


(* ::Section:: *)
(*Main Options*)


PackageScope["OptionCheck"]


PackageExport["Coefficients"]


(*PackageExport["OutputFormat"]*)


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


(*OutputFormat::usage= "OutputFormat -> X
	Option specifying whether the result should be expressed in terms of:
		- form factors (default): X=FF
		- Wilson coefficients with NP scale \[CapitalLambda]: X={\"SMEFT\",\[CapitalLambda]}
		- coupling constants: [to be added]
	This option can be used with: DifferentialCrossSection, CrossSection, EventYield.";*)


Coefficients::usage= "Coefficients -> X
	Option specifying which form factors (FF) or Wilson coefficients (WC) should be considered.
	The default value is X=All, meaning all FF or WC should be used.
	If X is a list of either FF or WC only these are kept and all other coefficients are set to zero.	
	This option can be used with: DifferentialCrossSection, CrossSection, EventYield.";


Luminosity::usage= "Luminosity
	Option that determines the integrated Luminosity used for computing event yields.
	The default value is Default corresponding to the actual luminosity of the specified search.
	This option can be used with: EventYield, Yield, ChiSquareLHC.";


RescaleError::usage= "RescaleError
	Option that determines whether the background uncertainty should be rescaled when changing the default luminosity.
	The default value is True, for which the error is scaled as \[CapitalDelta]BKG \[Rule] \!\(\*SqrtBox[\(\*SubscriptBox[\(L\), \(projection\)]/\*SubscriptBox[\(L\), \(search\)]\)]\) \[CapitalDelta]BKG.
	If set to False, the relative error for the background, i.e. \[CapitalDelta]BKG/BKG is kept constant.
	This option can be used with: ChiSquareLHC.";


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
	FF                -> True | False,
	Coefficients      -> All | {} | {(_FF|_WC|_Coupling)..} (*| {_WC..} | {_Coupling..}*) (*| {Rule[_WC,_]..} | {Rule[_FF,_]..}*),
	EFTorder          -> 0 | 2 | 4 (*| 8*),
	OperatorDimension -> 4 | 6 | 8,
	Luminosity        -> Default | _?NumericQ,
	RescaleError      -> True | False,
	PTcuts            -> ({min_?NumericQ, max_?NumericQ}/;(0<=min<max)) | ({min_?NumericQ,\[Infinity]}/;0<=min),
	MLLcuts           -> {min_?NumericQ, max_?NumericQ}/;(16<=min<max<=13000),
	EFTscale             -> _?NumericQ | _Symbol
|>;


OptionCheck::optionvalue= "Invalid OptionValue specified: `1`->`2`, the allowed values for `1` must match `3`.";


(* ::Section:: *)
(*Auxiliary function*)


SetAttributes[MyTiming, HoldFirst];
SetAttributes[MyEcho, HoldFirst];


$Verbose = 0


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


(* ::Section:: *)
(*HighPT logo for Plotting*)


HighPTLogo[] := If[$VersionNumber < 13.0,
	First@ Import[FileNameJoin[{Global`$DirectoryHighPT,"HighPT_plot_logo.pdf"}]]
	,
	First@ Import[FileNameJoin[{Global`$DirectoryHighPT,"HighPT_plot_logo.pdf"}],"PageGraphics"]
]
