(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`CrossSectionVH`*)


(* ::Subtitle:: *)
(*Implementation of the cross-section for VH production*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* This has to be made PRIVATE later -- here only for testing implementation *)
PackageExport["PartonicCrossSectionVH"]
PackageExport["yHcuts"]


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
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
	subs = Join[SubstitutionRulesMediatorsVH[finalStateV], SubstituteRulesSMEFTVH[\[Epsilon]]];
	\[Sigma] = \[Sigma] /. subs /. ReplacePropagators /. \[Epsilon] -> (Param["vev"]/ 1000)^2;
	(* !!!!!!!!!!!!!!!!!!!!!!!!! *)
	
	Return @ Expand[factor * \[Sigma]] (* GeV^-2*)
]


(* ::Subsubsection:: *)
(*Auxiliary lambda function for integration boundaries*)


\[Lambda]IntLimits::usage = "\[Lambda]IntLimits[s, t] denotes the \[Lambda] function that enters in the boundary limits of the phase-space integration";


\[Lambda]IntLimits[s_, mV_] := 1 - 2 (mV^2 + Mass["Higgs"]^2)/s + (mV^2 - Mass["Higgs"]^2)^2/s^2


(* ::Subsubsection:: *)
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


(* ::Section:: *)
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
