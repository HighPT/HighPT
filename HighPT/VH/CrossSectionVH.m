(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`CrossSectionVH`*)


(* ::Subtitle:: *)
(*Implementation of the cross-section for VH production*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section::Closed:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["DifferentialCrossSectionVH"]
PackageExport["CrossSectionVH"]
PackageExport["MVHcuts"]
PackageExport["FinalBoson"]
PackageExport["Z"]
PackageExport["W"]
PackageExport["PartonicCrossSectionVH"]


(* ::Subsection:: *)
(*Internal	*)


PackageScope["HadronicDifferentialCrossSectionVH"]
(*PackageScope["PartonicCrossSectionVH"]*)
PackageScope["PDFConvCrossSection"]
PackageScope["PartonicCMEnergyIntegration"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section::Closed:: *)
(*Parton-level cross-section for VH production*)


PartonCrossSectionVH::usage="PartonCrossSectionVH[\!\(\*OverscriptBox[\(s\), \(^\)]\),{\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]
	Computes the parton-level cross-section for the process \!\(\*OverscriptBox[SubscriptBox[\(q\), \(1\)], \(_\)]\) \!\(\*SubscriptBox[\(q\), \(2\)]\) -> Vh.
	The options and their default values are: 
		PTcuts \[Rule] {0,\[Infinity]} [GeV],
		OperatorDimension \[RuleDelayed] GetOperatorDimension[].";


Options[PartonicCrossSectionVH]= {
	PTcuts            -> {0,\[Infinity]},
	OperatorDimension :> GetOperatorDimension[]
};


PartonicCrossSectionVH[s_, {\[Psi]1_[i_], \[Psi]2_[j_]}, OptionsPattern[]] := Module[
	{
		t, t1, t2, t3, t4, pTmin, pTminYh, pTmax, pTmaxYh, yHmin, yHmax, ampSqVH, intAmpSq, \[Lambda], mV, \[Sigma], finalStateV, \[Epsilon], subs,
		factor = 1 / (16 * \[Pi] * s^2)
	},
	(* t must be real *)
	t/:Conjugate[t] := t;
	
	(* Spin and color avg amplitude squared *)
	ampSqVH = SpinSumAmplitudeSqVH[s, t, {\[Psi]1[i], \[Psi]2[j]}];
	
	(* Expand the FormFactors *)
	ampSqVH = ExpandFormFactorsVH[ampSqVH, OperatorDimension -> OptionValue[OperatorDimension]];
	
	(* Phase-space integration over t -- does not handle t- and u-channel mediatiors at the moment *)
	finalStateV = If[\[Psi]1 === \[Psi]2, "ZBoson", "WBoson"];
	intAmpSq = IntegrateTVH[ampSqVH, t, finalStateV];
	
	(* The mass of the fnial gauge boson can be infered from the initial quarks, wheter we have a vanishing or non-vanishing overal charge. *)
	mV = Mass[finalStateV];
	\[Lambda] = \[Lambda]IntLimits[s, mV];
	
	(* User pT cuts *)
	{pTmin, pTmax} = OptionValue[PTcuts];
	
	(* Limits *)
	t1 = -(s/2) (1 - (mV^2 + Mass["H"]^2)/s + Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmin^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	t2 = -(s/2) (1 - (mV^2 + Mass["H"]^2)/s + Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmax^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	t3 = -(s/2) (1 - (mV^2 + Mass["H"]^2)/s - Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmax^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	t4 = -(s/2) (1 - (mV^2 + Mass["H"]^2)/s - Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmin^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	
	(* Appropriated boundaries for the integral *)
	If[t2 === t3,
		If[t1 === t4, 
			\[Sigma] = 0,
			\[Sigma] = (intAmpSq /. t -> t4) - (intAmpSq /. t -> t1)
		],
		\[Sigma] = (intAmpSq /. t -> t4) - (intAmpSq /. t -> t3) + (intAmpSq /. t -> t2) - (intAmpSq /. t -> t1)
	];
	
	Return @ Expand[factor * \[Sigma] ] (* GeV^-2*)
]


(* ::Subsubsection:: *)
(*Auxiliary lambda function for integration boundaries*)


\[Lambda]IntLimits::usage = "\[Lambda]IntLimits[s, t] denotes the \[Lambda] function that enters in the boundary limits of the phase-space integration";


\[Lambda]IntLimits[s_, mV_] := 1 - 2 (mV^2 + Mass["H"]^2)/s + (mV^2 - Mass["H"]^2)^2/s^2


(* ::Section::Closed:: *)
(*Phase-space integration*)


IntegrateTVH::usage = "IntegrateTVH[arg, t] performs the integration over the Mandelstam variable t for VH production";


IntegrateTVH::failed = "The phase-space integration failed. The remaining integrals are: `1`"


IntegrateTVH[arg_, t_, finalStateV_] := Module[
	{
		temp, channel
	},
	(* finds all t integrands, same as for the DY case *)
	temp = Integrand[arg, t];
	
	(* Thread conjugate over everything and assume t is real *)
	temp= ExpandConjugate[
		ExpandConjugate[temp]/.Conjugate[t]->t
	]/.Conjugate[t]->t;
	
	(* Replace the propagators *)	
	channel = If[finalStateV === "ZBoson", "ZH", "WH"];
	temp = temp /. ReplaceChannelSumsVH[channel];
	
	(* substitute the master integrals *)
	temp= temp/.ReplaceIntegralsVH[t];
	
	(* Throw an error if not all integrands were removed *)
	If[!FreeQ[temp,_Integrand], Message[IntegrateTVH::failed, Cases[temp,_Integrand,All]]; Abort[]];
	
	Return[MyExpand[temp]]
]


(* ::Subsubsection:: *)
(*ReplaceIntegrals*)


ReplaceIntegralsVH[t_] := {
	(* Polynomions *)
	Integrand[1, t] :> t,
	Integrand[t, t] :> 1/2 * t^2,
	Integrand[Power[t, n_/;(IntegerQ[n] && n > 0)], t] :> 1/(n + 1) * Power[t, n + 1]
	(* t- and u-channels to be done *)
	(* ... *)
}


(* ::Section:: *)
(*Hadron-level cross-section for VH production*)


(* ::Subsection::Closed:: *)
(*Integration over the partonic center of mass energy*)


PartonicCMEnergyIntegration::usage = "Performs the integration over the partonic CM energy";


PartonicCMEnergyIntegration[\[Sigma]HadFunc_, {smin_, smax_}, V_:(Z|W), arguments_] := Module[
	{
		\[Sigma], s, subs, \[Epsilon], MyMin, MyMax, sIntegralList,
		integralAssoc, dummyIntegral, nonRedundantIntegarlList = {}, integralAssocReverse, replaceMasses
	},
	(* Compute the hadronic cross-section  *)
	\[Sigma] = \[Sigma]HadFunc[s, V, arguments];
	
	(* Replace Min/Max with proxies to avoid OneIdentity issues *)
    \[Sigma] = \[Sigma] /. {Min -> MyMin, Max -> MyMax};
	
	(* List with mass replacements *)
	replaceMasses = Mass[#] -> GetParameters[][Mass[#]]& /@ {"H", "ZBoson", "WBoson"};
	
	(* 1. Build the integrand in s *)
    \[Sigma] = MyTiming[
        Integrand[\[Sigma], s]
        , "Integrand (s)"
    ];
	
	(* Collect all s integrals *)
    sIntegralList = DeleteDuplicates @ Cases[\[Sigma], _Integrand, All];
    
    (* Map each distinct integral to a dummy symbol *)
    integralAssoc = Association[(# -> dummyIntegral[Unique[]]) & /@ sIntegralList];
    
    (* Remove redundant conjugate integrals *)
    Do[
        If[! MemberQ[nonRedundantIntegarlList, int],
            With[
                {conjInt = Conjugate[int] //. {
                    Conjugate[Sqrt[arg_]] :> Sqrt[Conjugate @ arg],
                    Conjugate[x_MyMin] :> x,
                    Conjugate[x_MyMax] :> x
                }},
                If[MemberQ[nonRedundantIntegarlList, conjInt],
                    (* int is conjugate of an existing one *)
                    AssociateTo[integralAssoc, int -> Conjugate[integralAssoc[conjInt]]],
                    (* first time we see this pair *)
                    AppendTo[nonRedundantIntegarlList, int]
                ]
            ]
        ],
        {int, sIntegralList}
    ];
    
    (* Build rules for the unique integrals *)
    integralAssocReverse = Table[
        integralAssoc[int] -> int,
        {int, nonRedundantIntegarlList}
    ];
    
    integralAssocReverse = integralAssocReverse /. ReplacePropagators;
    integralAssocReverse = integralAssocReverse /. ReplaceConstants[] /. replaceMasses;
    integralAssocReverse = integralAssocReverse /. {MyMin -> Min, MyMax -> Max};
    MyEcho[Length[integralAssocReverse], "# Integrals"];
   
    integralAssocReverse = MyTiming[
        integralAssocReverse /. Integrand[arg_, x_] :> NIntegrate[arg, {x, smin, smax}],
        "NIntegrate"
    ];
    
	(* Substitute back in \[Sigma] *)
    \[Sigma] = \[Sigma] /. integralAssoc;
    \[Sigma] = \[Sigma] /. integralAssocReverse /. replaceMasses;
    
    (* Warn if something is left unintegrated *)
    If[! FreeQ[\[Sigma], _dummyIntegral],
        Message[CrossSection::inteval,
            Length @ DeleteDuplicates @ Cases[\[Sigma], _dummyIntegral, All]
        ]
    ];
	
	Return[\[Sigma]]
]


(* ::Subsection:: *)
(*Integrated VH cross-section*)


CrossSectionVH::usage = "Cross-section for VH production";


Options[CrossSectionVH] = {
	MVHcuts           -> {300, 13000},
	PTcuts            -> {0, \[Infinity]},
	FinalBoson        -> Z,
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	EFTscale          :> GetEFTScale[] 
}


CrossSectionVH[OptionsPattern[]] := Module[
	{
		\[Sigma], smin, smax, \[Sigma]Had, s, subs, \[Epsilon], MyMin, MyMax, sIntegralList,
		integralAssoc, dummyIntegral, nonRedundantIntegarlList = {}, integralAssocReverse
	},
	(* Check options -- TODO *)
	
	(* Min and Max CM energy squared *)
	{smin, smax} = OptionValue[MVHcuts]^2;
	
	(* Performs the integration over the partonic CM energy *)
	\[Sigma] = PartonicCMEnergyIntegration[
		HadronicDifferentialCrossSectionVH, 
		{smin, smax}, 
		OptionValue[FinalBoson],
		{PTcuts -> OptionValue[PTcuts], OperatorDimension -> OptionValue[OperatorDimension]}
	];
	
	(* Replace FFs by WCs if required - Here we may define it do be only in terms of SMEFT coefs *)
	If[!OptionValue[FF],
		\[Sigma] = SubstituteFFVH[
			\[Sigma],
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension],
			EFTscale          -> OptionValue[EFTscale]
		]
	];
	
	(* Set coefficients to zero *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]= SelectTerms[\[Sigma], OptionValue[Coefficients]]
	];
	
	\[Sigma] = MyExpand[\[Sigma]];
	
	Return[\[Sigma]]
];


(* ::Subsection:: *)
(*Hadronic differential VH cross-section (for internal use)*)


HadronicDifferentialCrossSectionVH::usage= "HadronicDifferentialCrossSectionVH[] computes the differential hadronic cross-section for the Higgs associated production.";


Options[HadronicDifferentialCrossSectionVH] = {
	PTcuts            -> {0, \[Infinity]},
	OperatorDimension :> GetOperatorDimension[]
}


HadronicDifferentialCrossSectionVH[s_, V_:(Z|W), OptionsPattern[]] := Module[
	{
		\[Sigma]PartonLevel, \[Sigma], \[Sigma]HadronDiff,
		GeV2toPB=(10^9)/(2.56819), \[Sigma]ConvFunc,
		fbar, f, i, j
	},
	
	(* Function to convolute the parton luminosity with the partonic cross-section *)
	\[Sigma]ConvFunc = PDFConvCrossSection[
		s, 
		V, 
		PTcuts -> OptionValue[PTcuts],
	    OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* Sums the contribution of all possible initial flavor that contributes to the process *)
	\[Sigma]HadronDiff = Plus @@ ( \[Sigma]ConvFunc /@ ListInitialQuarkFlavors[V] );
	
	(* Change units from GeV^-2 to pb *)
	\[Sigma]HadronDiff = GeV2toPB * \[Sigma]HadronDiff;
	
	(* Replace parton luminosity by the interpolated functions *)
	\[Sigma]HadronDiff = \[Sigma]HadronDiff /. PartonLuminosity -> PartonLuminosityFunction;
	
	(* in pb *)
	Return @ Expand[\[Sigma]HadronDiff] 
];


(* ::Subsubsection:: *)
(*Convoluted parton luminosity functions with the partonic cross-section for ZH and WH production*)


Options[PDFConvCrossSection] = {
	PTcuts            -> {0, \[Infinity]},
	OperatorDimension :> GetOperatorDimension[]
}


PDFConvCrossSection[s_, Z, OptionsPattern[]] := Module[
	{
		\[Sigma]Conv, \[Sigma]PartonLevel, f, i, j, dummyReplList
	},
	(* Parton level cross-section for ZH production - same fermion *)
	\[Sigma]PartonLevel = PartonicCrossSectionVH[
		s, 
	    {f[i], f[j]},
	    PTcuts -> OptionValue[PTcuts],
	    OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* PDF x Partonic x-section *)
	\[Sigma]Conv = \[Sigma]PartonLevel /. {f -> dummyReplList["type"], i -> dummyReplList["\[Psi]bar"], j -> dummyReplList["\[Psi]"]};
	\[Sigma]Conv = 1/s * PartonLuminosity[dummyReplList["PDF"]][Sqrt[s]] * \[Sigma]Conv;
	
	Return @ Function[x, \[Sigma]Conv /. dummyReplList -> x]
]


PDFConvCrossSection[s_, W, OptionsPattern[]] := Module[
	{
		\[Sigma]Conv, \[Sigma]PartonLevel, fbar, f, i, j, dummyReplList
	},
	(* Parton level cross-section for WH production *)
	\[Sigma]PartonLevel = PartonicCrossSectionVH[
		s, 
	    {fbar[i], f[j]},
	    PTcuts -> OptionValue[PTcuts],
	    OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* PDF x Partonic x-section *)
	\[Sigma]Conv = \[Sigma]PartonLevel /. {fbar -> dummyReplList["\[Psi]bar"], i -> dummyReplList["\[Psi]barI"], f -> dummyReplList["\[Psi]"], j -> dummyReplList["\[Psi]J"]};
	\[Sigma]Conv = 1/s * PartonLuminosity[dummyReplList["PDF"]][Sqrt[s]] * \[Sigma]Conv;
	
	Return @ Function[x, \[Sigma]Conv /. dummyReplList -> x]
]


(* ::Subsubsection:: *)
(*List of the different quark flavors and their Parton Luminosities for ZH and WH production*)


ListInitialQuarkFlavors::usage = "Creates a list with the initial quark flavors that must be considered in the computation of the hadronic diff. x-section"


ListInitialQuarkFlavors[Z] = {
	<|"PDF" -> "d_dbar", "type" -> d, "\[Psi]bar" -> 1, "\[Psi]" -> 1|>,
	<|"PDF" -> "d_sbar", "type" -> d, "\[Psi]bar" -> 2, "\[Psi]" -> 1|>,
	<|"PDF" -> "d_bbar", "type" -> d, "\[Psi]bar" -> 3, "\[Psi]" -> 1|>,
	<|"PDF" -> "s_dbar", "type" -> d, "\[Psi]bar" -> 1, "\[Psi]" -> 2|>,
	<|"PDF" -> "s_sbar", "type" -> d, "\[Psi]bar" -> 2, "\[Psi]" -> 2|>,
	<|"PDF" -> "s_bbar", "type" -> d, "\[Psi]bar" -> 3, "\[Psi]" -> 2|>,
	<|"PDF" -> "b_dbar", "type" -> d, "\[Psi]bar" -> 1, "\[Psi]" -> 3|>,
	<|"PDF" -> "b_sbar", "type" -> d, "\[Psi]bar" -> 2, "\[Psi]" -> 3|>,
	<|"PDF" -> "b_bbar", "type" -> d, "\[Psi]bar" -> 3, "\[Psi]" -> 3|>,
	<|"PDF" -> "u_ubar", "type" -> u, "\[Psi]bar" -> 1, "\[Psi]" -> 1|>,
	<|"PDF" -> "u_cbar", "type" -> u, "\[Psi]bar" -> 2, "\[Psi]" -> 1|>,
	<|"PDF" -> "c_ubar", "type" -> u, "\[Psi]bar" -> 1, "\[Psi]" -> 2|>,
	<|"PDF" -> "c_cbar", "type" -> u, "\[Psi]bar" -> 2, "\[Psi]" -> 2|>
};


ListInitialQuarkFlavors[W] = {
	<|"PDF" -> "d_ubar", "\[Psi]bar" -> u, "\[Psi]" -> d, "\[Psi]barI" -> 1, "\[Psi]J" -> 1|>,
	<|"PDF" -> "d_cbar", "\[Psi]bar" -> u, "\[Psi]" -> d, "\[Psi]barI" -> 2, "\[Psi]J" -> 1|>,
	<|"PDF" -> "s_ubar", "\[Psi]bar" -> u, "\[Psi]" -> d, "\[Psi]barI" -> 1, "\[Psi]J" -> 2|>,
	<|"PDF" -> "s_cbar", "\[Psi]bar" -> u, "\[Psi]" -> d, "\[Psi]barI" -> 2, "\[Psi]J" -> 2|>,
	<|"PDF" -> "b_ubar", "\[Psi]bar" -> u, "\[Psi]" -> d, "\[Psi]barI" -> 1, "\[Psi]J" -> 3|>,
	<|"PDF" -> "b_cbar", "\[Psi]bar" -> u, "\[Psi]" -> d, "\[Psi]barI" -> 2, "\[Psi]J" -> 3|>,
	<|"PDF" -> "u_dbar", "\[Psi]bar" -> d, "\[Psi]" -> u, "\[Psi]barI" -> 1, "\[Psi]J" -> 1|>,
	<|"PDF" -> "u_sbar", "\[Psi]bar" -> d, "\[Psi]" -> u, "\[Psi]barI" -> 2, "\[Psi]J" -> 1|>,
	<|"PDF" -> "u_bbar", "\[Psi]bar" -> d, "\[Psi]" -> u, "\[Psi]barI" -> 3, "\[Psi]J" -> 1|>,
	<|"PDF" -> "c_dbar", "\[Psi]bar" -> d, "\[Psi]" -> u, "\[Psi]barI" -> 1, "\[Psi]J" -> 2|>,
	<|"PDF" -> "c_sbar", "\[Psi]bar" -> d, "\[Psi]" -> u, "\[Psi]barI" -> 2, "\[Psi]J" -> 2|>,
	<|"PDF" -> "c_bbar", "\[Psi]bar" -> d, "\[Psi]" -> u, "\[Psi]barI" -> 3, "\[Psi]J" -> 2|>
};


(* ::Subsection:: *)
(*Hadronic differential VH cross-section (for external use)*)


DifferentialCrossSectionVH::usage = "Differential cross-section for VH production";


Options[DifferentialCrossSectionVH] = {
   FF                 -> False,
   Coefficients       -> All,
   FinalBoson        -> Z,
   EFTorder           :> GetEFTorder[],
   OperatorDimension  :> GetOperatorDimension[],
   PTcuts             -> {0, \[Infinity]},
   EFTscale           :> GetEFTscale[]
};


DifferentialCrossSectionVH[OptionsPattern[]] := Module[
	{\[Sigma], s, \[Epsilon], subs}
	,
	
	(* Check options - TODO *)
	(*OptionCheck[#, OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, PTcuts, EFTscale};*)
	
	(* Computes the hadronic cross-section (d\[Sigma]/ds) *)
	\[Sigma] = HadronicDifferentialCrossSectionVH[
		s, OptionValue[FinalBoson],
		PTcuts -> OptionValue[PTcuts],
		OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* Replace propagators and constants *)
	\[Sigma] = \[Sigma] /. ReplacePropagators ;
	\[Sigma] = \[Sigma] /. ReplaceConstants[];
	
	(* !!!!! REPLACE IT BY SubstituteFFVH WHICH STILL NEEDS TO BE IMPLEMENTED !!!!! *)
	subs = Join[SubstitutionRulesMediatorsVH["ZBoson"], SubstituteRulesSMEFTVH[\[Epsilon]]];
	\[Sigma] = \[Sigma] /. subs /. \[Epsilon] -> (Param["vev"]/ 1000)^2 /. ReplaceConstants[];
	(* !!!!!!!!!!!!!!!!!!!!!!!!!!! *)

	(* Set coefficients to zero *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]= SelectTerms[\[Sigma], OptionValue[Coefficients]]
	];

	\[Sigma] = MyExpand[\[Sigma]];
	
	With[{xSec=\[Sigma]},
		Return@ Function[x,Re[xSec]/.s->x]
	];
];
