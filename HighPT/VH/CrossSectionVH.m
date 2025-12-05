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


(* ::Subsection:: *)
(*Internal	*)


PackageScope["HadronicDifferentialCrossSectionVH"]
PackageScope["PartonicCrossSectionVH"]
PackageScope["PartonicCMEnergyIntegration"]


(* ::Chapter:: *)
(*Public:*)


(* ::Section::Closed:: *)
(*Parton-level cross-section for VH production*)


PartonCrossSectionVH::usage="PartonCrossSectionVH[\!\(\*OverscriptBox[\(s\), \(^\)]\),{\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]
	Computes the parton-level cross-section for the process \!\(\*OverscriptBox[SubscriptBox[\(q\), \(1\)], \(_\)]\) \!\(\*SubscriptBox[\(q\), \(2\)]\) -> Vh.
	The options and their default values are: 
		PTcuts \[Rule] {0,\[Infinity]} [GeV],
		OperatorDimension \[RuleDelayed] GetOperatorDimension[].";


Options[PartonicCrossSectionVH]= {
	PTcuts            -> {0,\[Infinity]},
	yHcuts            -> {0,\[Infinity]}, (* Cut on the absolute value of rapidity of the Higgs -- In the CM frame *)
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
	
	(* User cut on the absolute rapidity of the Higgs *)
	(* !!!In the CM frame!!! *)
	{yHmin, yHmax} = OptionValue[yHcuts];
	(* Equivalent pT cuts *)
	{pTminYh, pTmaxYh} = {ComputePTCutfromYH[yHmax, mV, s], ComputePTCutfromYH[yHmin, mV, s]};
	
	(* Updates the pT cuts if needed *)
	pTmax = If[pTmaxYh < pTmax, pTmaxYh, pTmax];
	pTmin = If[pTminYh > pTmin, pTminYh, pTmin];
	
	(* Limits *)
	t1 = -(s/2) (1 - (mV^2 + Mass["Higgs"]^2)/s + Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmin^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	t2 = -(s/2) (1 - (mV^2 + Mass["Higgs"]^2)/s + Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmax^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	t3 = -(s/2) (1 - (mV^2 + Mass["Higgs"]^2)/s - Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmax^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	t4 = -(s/2) (1 - (mV^2 + Mass["Higgs"]^2)/s - Sqrt[\[Lambda]] * Sqrt[1 - Min[1, 4 * pTmin^2 / (s * \[Lambda])]])/.{Sign[s] -> 1, Sign[\[Lambda]] -> 1};
	
	(* Appropriated boundaries for the integral *)
	If[t2 === t3,
		If[t1 === t4, 
			\[Sigma] = 0,
			\[Sigma] = (intAmpSq /. t -> t4) - (intAmpSq /. t -> t1)
		],
		\[Sigma] = (intAmpSq /. t -> t4) - (intAmpSq /. t -> t3) + (intAmpSq /. t -> t2) - (intAmpSq /. t -> t1)
	];
	
	(* !!!!!!!!! Test !!!!!!!!!! *)
	(* list with all replacements in the SMEFT *)
	(*subs = Join[SubstitutionRulesMediatorsVH[finalStateV], SubstituteRulesSMEFTVH[\[Epsilon]]];
	\[Sigma] = \[Sigma] /. subs /. ReplacePropagators /. \[Epsilon] -> (Param["vev"]/ 1000)^2;*)
	(* !!!!!!!!!!!!!!!!!!!!!!!!! *)
	
	Return @ Expand[factor * \[Sigma]] (* GeV^-2*)
]


(* ::Subsubsection::Closed:: *)
(*Auxiliary lambda function for integration boundaries*)


\[Lambda]IntLimits::usage = "\[Lambda]IntLimits[s, t] denotes the \[Lambda] function that enters in the boundary limits of the phase-space integration";


\[Lambda]IntLimits[s_, mV_] := 1 - 2 (mV^2 + Mass["Higgs"]^2)/s + (mV^2 - Mass["Higgs"]^2)^2/s^2


(* ::Subsubsection::Closed:: *)
(*Translates a cut on the rapidity to a cut on the pT*)


(* When no cut is included *)
trivialYhCuts = <|0 -> \[Infinity], \[Infinity] -> 0|>;


ComputePTCutfromYH[yH_, mV_, s_] := Module[{pTsq, pTCut},
	If[yH === 0 || yH === \[Infinity], 
		(* Trivial results *)
		pTCut = trivialYhCuts[yH]
		,
		(* Transverse momentum squared as a function of the rapidity *)
		pTsq = ((s + Mass["Higgs"]^2 - mV^2)^2 / (4 * s * Cosh[Abs[yH]]^2) - Mass["Higgs"]^2)/.GetParameters[];
		
		(* Cut must be applied only if the result is positive *)
		If[pTsq > 0, 
		  pTCut = Sqrt[pTsq]
		  ,
		  pTCut = 0
		]
	];
	
	Return[pTCut]
];


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


PartonicCMEnergyIntegration[\[Sigma]HadFunc_, {smin_, smax_}, arguments_] := Module[
	{
		\[Sigma], s, subs, \[Epsilon], MyMin, MyMax, sIntegralList,
		integralAssoc, dummyIntegral, nonRedundantIntegarlList = {}, integralAssocReverse
	},
	(* Compute the hadronic cross-section  *)
	\[Sigma] = \[Sigma]HadFunc[s, arguments];
	
	(* 3. Replace Min/Max with proxies to avoid OneIdentity issues *)
    \[Sigma] = \[Sigma] /. {Min -> MyMin, Max -> MyMax};
	
	(* 1. Build the integrand in s *)
    \[Sigma] = MyTiming[
        Integrand[\[Sigma], s]
        , "Integrand (s)"
    ];
	
	(* 4. Collect all s integrals *)
    sIntegralList = DeleteDuplicates @ Cases[\[Sigma], _Integrand, All];
    
    (* 5. Map each distinct integral to a dummy symbol *)
    integralAssoc = Association[(# -> dummyIntegral[Unique[]]) & /@ sIntegralList];
    
    (* 6. Remove redundant conjugate integrals *)
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
    
    (* 7. Build rules for the unique integrals *)
    integralAssocReverse = Table[
        integralAssoc[int] -> int,
        {int, nonRedundantIntegarlList}
    ];
    
    integralAssocReverse = integralAssocReverse /. ReplacePropagators;
    integralAssocReverse = integralAssocReverse /. ReplaceConstants[];
    integralAssocReverse = integralAssocReverse /. {MyMin -> Min, MyMax -> Max};
    MyEcho[Length[integralAssocReverse], "# Integrals"];
   
    integralAssocReverse = MyTiming[
        integralAssocReverse /. Integrand[arg_, x_] :> NIntegrate[arg, {x, smin, smax}],
        "NIntegrate"
    ];
    
	(* 9. Substitute back in \[Sigma] *)
    \[Sigma] = \[Sigma] /. integralAssoc;
    \[Sigma] = \[Sigma] /. integralAssocReverse;
    
    (* 10. Warn if something is left unintegrated *)
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
	(* !!! Only Zh !!! *)
	(* Check options -- TODO *)
	
	(* Min and Max CM energy squared *)
	{smin, smax} = OptionValue[MVHcuts]^2;
	
	(* Performs the integration over the partonic CM energy *)
	\[Sigma] = PartonicCMEnergyIntegration[
		HadronicDifferentialCrossSectionVH, 
		{smin, smax}, 
		{PTcuts -> OptionValue[PTcuts], OperatorDimension -> OptionValue[OperatorDimension]}
	];
	
	(* !!!!! REPLACE IT BY SubstituteFFVH WHICH STILL NEEDS TO BE IMPLEMENTED !!!!! *)
	subs = Join[SubstitutionRulesMediatorsVH["ZBoson"], SubstituteRulesSMEFTVH[\[Epsilon]]];
	\[Sigma] = \[Sigma] /. subs /. \[Epsilon] -> (Param["vev"]/ 1000)^2 /. ReplaceConstants[];
	(* !!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	
	(* Set coefficients to zero *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]= SelectTerms[\[Sigma], OptionValue[Coefficients]]
	];
	
	\[Sigma] = \[Sigma] /. ReplacePropagators;
    \[Sigma] = \[Sigma] /. ReplaceConstants[];
	
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


HadronicDifferentialCrossSectionVH[s_, OptionsPattern[]] := Module[
	{
		\[Sigma]PartonLevel, \[Sigma], \[Sigma]HadronDiff,
		GeV2toPB=(10^9)/(2.56819),
		f, i, j
	},
	(* !!!! Only Zh for the moment !!! *)
	
	(* Parton-level cross-section for arbitrary initial flavors *)
	\[Sigma]PartonLevel = PartonicCrossSectionVH[
		s, 
	    {f[i], f[j]},
	    PTcuts -> OptionValue[PTcuts],
	    OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* Convolute the partonic cross-sections with the parton-parton luminosities *)
	\[Sigma]["d_dbar"] = 1/s * PartonLuminosity["d_dbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 1, j -> 1};
	\[Sigma]["d_sbar"] = 1/s * PartonLuminosity["d_sbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 2, j -> 1};
	\[Sigma]["d_bbar"] = 1/s * PartonLuminosity["d_bbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 3, j -> 1};
	\[Sigma]["s_dbar"] = 1/s * PartonLuminosity["s_dbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 1, j -> 2};
	\[Sigma]["s_sbar"] = 1/s * PartonLuminosity["s_sbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 2, j -> 2};
	\[Sigma]["s_bbar"] = 1/s * PartonLuminosity["s_bbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 3, j -> 2};
	\[Sigma]["b_dbar"] = 1/s * PartonLuminosity["b_dbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 1, j -> 3};
	\[Sigma]["b_sbar"] = 1/s * PartonLuminosity["b_sbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 2, j -> 3};
	\[Sigma]["b_bbar"] = 1/s * PartonLuminosity["b_bbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> d, i -> 3, j -> 3};
	\[Sigma]["u_ubar"] = 1/s * PartonLuminosity["u_ubar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> u, i -> 1, j -> 1};
	\[Sigma]["u_cbar"] = 1/s * PartonLuminosity["u_cbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> u, i -> 2, j -> 1};
	\[Sigma]["c_ubar"] = 1/s * PartonLuminosity["c_ubar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> u, i -> 1, j -> 2};
	\[Sigma]["c_cbar"] = 1/s * PartonLuminosity["c_cbar"][Sqrt[s]] * \[Sigma]PartonLevel/.{f -> u, i -> 2, j -> 2};
	
	\[Sigma]HadronDiff = Plus[
		\[Sigma]["d_dbar"], \[Sigma]["d_sbar"], \[Sigma]["d_bbar"], \[Sigma]["s_dbar"], \[Sigma]["s_sbar"], \[Sigma]["s_bbar"], \[Sigma]["b_dbar"], 
		\[Sigma]["b_sbar"], \[Sigma]["b_bbar"], \[Sigma]["u_ubar"], \[Sigma]["u_cbar"], \[Sigma]["c_ubar"], \[Sigma]["c_cbar"]
	];
	
	(* Change units from GeV^-2 to pb *)
	\[Sigma]HadronDiff = GeV2toPB * \[Sigma]HadronDiff;
	
	(* Replace parton luminosity by the interpolated functions *)
	\[Sigma]HadronDiff = \[Sigma]HadronDiff /. PartonLuminosity -> PartonLuminosityFunction;
	
	(* in pb *)
	Return @ Expand[\[Sigma]HadronDiff] 
];


(* ::Subsection:: *)
(*Hadronic differential VH cross-section (for external use)*)


DifferentialCrossSectionVH::usage = "Differential cross-section for VH production";


Options[DifferentialCrossSectionVH] = {
   FF                 -> False,
   Coefficients       -> All,
   EFTorder           :> GetEFTorder[],
   OperatorDimension  :> GetOperatorDimension[],
   PTcuts             -> {0, \[Infinity]},
   EFTscale           :> GetEFTscale[]
};


DifferentialCrossSectionVH[OptionsPattern[]] := Module[
	{\[Sigma], s, \[Epsilon], subs}
	,
	(* !!!!!! Only for Zh at the moment !!!!!!! *)
	
	(* Check options *)
	OptionCheck[#, OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, PTcuts, EFTscale};
	
	(* Computes the hadronic cross-section (d\[Sigma]/ds) *)
	\[Sigma] = HadronicDifferentialCrossSectionVH[
		s, 
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
