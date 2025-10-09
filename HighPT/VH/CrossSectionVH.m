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


(* ::Subsection::Closed:: *)
(*Exported*)


(* This has to be made PRIVATE later -- here only for testing implementation *)
PackageExport["PartonicCrossSectionVH"]


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
	OperatorDimension :> GetOperatorDimension[]
};


PartonicCrossSectionVH[s_, {\[Psi]1_[i_], \[Psi]2_[j_]}, OptionsPattern[]] := Module[
	{
		t, t1, t2, t3, t4, pTmin, pTmax, ampSqVH, intAmpSq, \[Lambda], mV, \[Sigma],
		factor = 1 / (16 * \[Pi] * s^2)
	},
	(* t must be real *)
	t/:Conjugate[t] := t;
	
	(* Spin and color avg amplitude squared *)
	ampSqVH = SpinSumAmplitudeSqVH[s, t, {\[Psi]1[i], \[Psi]2[j]}];
	
	(* Treat the form factors as independent of t --- ONLY FOR X-CHECK - TO BE REMOVED LATER *)
	ampSqVH = ampSqVH /. FormFactorVH[{ty_, ind_}, ss_, t_, ff___] :> FormFactorVH[{ty, ind}, ss, 0, ff];
	 
	(* Expand the FormFactors in terms of propagators and WCs *)
	(* TODO ... *)
	
	(* Phase-space integration over t -- does not handle t- and u-channel mediatiors at the moment *)
	intAmpSq = IntegrateTVH[ampSqVH, t];
	
	(* Integration limits *)
	mV = If[\[Psi]1 === \[Psi]2, Mass["ZBoson"], Mass["WBoson"]];
	\[Lambda] = \[Lambda]IntLimits[s, mV];
	{pTmin, pTmax} = OptionValue[PTcuts];
	
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

	Return @ Expand[factor * \[Sigma]]
]


(* ::Subsubsection:: *)
(*Auxiliary lambda function for integration boundaries*)


\[Lambda]IntLimits::usage = "\[Lambda]IntLimits[s, t] denotes the \[Lambda] function that enters in the boundary limits of the phase-space integration";


\[Lambda]IntLimits[s_, mV_] := 1 - 2 (mV^2 + Mass["Higgs"]^2)/s + (mV^2 - Mass["Higgs"]^2)^2/s^2


(* ::Section:: *)
(*Phase-space integration*)


IntegrateTVH::usage = "IntegrateTVH[arg, t] performs the integration over the Mandelstam variable t for VH production";


IntegrateTVH::failed = "The phase-space integration failed. The remaining integrals are: `1`"


IntegrateTVH[arg_, t_] := Module[
	{
		temp
	},
	(* finds all t integrands, same as for the DY case *)
	temp = Integrand[arg, t];
	
	(* Thread conjugate over everything and assume t is real *)
	temp= ExpandConjugate[
		ExpandConjugate[temp]/.Conjugate[t]->t
	]/.Conjugate[t]->t;
	
	(* TODO -- Important when we include mediators in the t- and u-channels *)
	
	(* replace propagators in the s-/t-/u-channels *)
	
	(* apply partial fractioning identities *)
	
	(* reduce integrals to the master-integrals - t- and u-channels *)
	
	(* substitute the master integrals *)
	temp= temp/.ReplaceIntegralsVH[t];
	
	(* Throw an error if not all integrands were removed *)
	If[!FreeQ[temp,_Integrand], Message[IntegrateTVH::failed, Cases[temp,_Integrand,All]]; Abort[]];
	
	Return[MyExpand[temp]]
]


(* ::Subsubsection::Closed:: *)
(*ReplaceIntegrals*)


ReplaceIntegralsVH[t_] := {
	(* Polynomions *)
	Integrand[1, t] :> t,
	Integrand[t, t] :> 1/2 * t^2,
	Integrand[Power[t, n_/;(IntegerQ[n] && n > 0)], t] :> 1/(n + 1) * Power[t, n + 1]
	(* t- and u-channels to be done *)
	(* ... *)
}
