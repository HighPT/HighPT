(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`CrossSection`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semileptonic processes pp -> ll and pp -> l\[Nu] at tree level.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["CrossSection"]


PackageExport["DifferentialCrossSection"]


PackageExport["SetPDF"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["HadronicDifferentialCrossSection"]
PackageScope["PartonCrossSection"]


PackageScope["Integrand"]
PackageScope["ReduceIntegrands"]
PackageScope["PartialFractioning"]
PackageScope["PartialFractioningSIntegrals"]


PackageScope["PartonLuminosityFunction"]
PackageScope["PartonLuminosity"]
PackageScope["$PDFsets"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section::Closed:: *)
(*Parton-level cross-section*)


PartonCrossSection::usage="PartonCrossSection[\!\(\*OverscriptBox[\(s\), \(^\)]\),{\!\(\*SubscriptBox[\(l\), \(1\)]\)[\[Alpha]],\!\(\*SubscriptBox[\(l\), \(2\)]\)[\[Beta]],\!\(\*SubscriptBox[\(q\), \(1\)]\)[i],\!\(\*SubscriptBox[\(q\), \(2\)]\)[j]}]
	Computes the parton-level cross-section for the process \!\(\*OverscriptBox[SubscriptBox[\(q\), \(1\)], \(_\)]\) \!\(\*SubscriptBox[\(q\), \(2\)]\) -> \!\(\*OverscriptBox[SubscriptBox[\(l\), \(1\)], \(_\)]\) \!\(\*SubscriptBox[\(l\), \(2\)]\) in units of \!\(\*SuperscriptBox[\(GeV\), \(-2\)]\).
	The options and their default values are: 
		PTcuts \[Rule] {0,\[Infinity]} [GeV],
		OperatorDimension \[RuleDelayed] GetOperatorDimension[].";


Options[PartonCrossSection]= {
	PTcuts            -> {0,\[Infinity]},
	OperatorDimension :> GetOperatorDimension[]
};


PartonCrossSection[s_,{\[Alpha]_,\[Beta]_,i_,j_}, OptionsPattern[]]:= Module[
	{
		temp, t, t1, t2, t3, t4, pTmin, pTmax, 
		factor= 1/(16*\[Pi]*s^2),
		\[Sigma]
	}
	,
	(* make t real *)
	t/:Conjugate[t]:= t;
	
	(* compute the spin sum-average of the modulo square of the amplitude *)
	temp= SpinSummedAmplitude2[s,t,{\[Alpha],\[Beta],i,j}];
	
	(* Expand the FormFactors *)
	temp= ExpandFormFactors[temp, OperatorDimension -> OptionValue[OperatorDimension]];
	
	(* Perform phase-space integration over t *)
	temp= IntegrateT[temp, t];
	
	(* Integration boundaries *)
	{pTmin, pTmax} = OptionValue[PTcuts];
	t1= -(s/2)(1+Sqrt[1-Min[1, 4*pTmin^2/s]])/.Sign[s]->1;
	t2= -(s/2)(1+Sqrt[1-Min[1, 4*pTmax^2/s]])/.Sign[s]->1;
	t3= -(s/2)(1-Sqrt[1-Min[1, 4*pTmax^2/s]])/.Sign[s]->1;
	t4= -(s/2)(1-Sqrt[1-Min[1, 4*pTmin^2/s]])/.Sign[s]->1;
	
	(* Use appropriate boundaries for the integral *)
	If[t2===t3,
		If[t1===t4,
			\[Sigma]= 0
			,
			\[Sigma]= (temp/.t->t4) - (temp/.t->t1)
		]
		,
		\[Sigma]= (temp/.t->t4) - (temp/.t->t3) + (temp/.t->t2) - (temp/.t->t1)
	];
	
	(* rescale the result *)
	Return@ Expand[factor * \[Sigma]] (* in GeV^-2 *)
]


(* ::Section::Closed:: *)
(*Phase-space integration*)


(* ::Subsubsection::Closed:: *)
(*IntegrateT*)


IntegrateT::usage= "IntegrateT[arg, t] performs the integraion over the partonic Mandelstam variable t, which is equivalent to the angular phase-space integration, or the pT integration.";


IntegrateT::failed= "The phase-space integration failed. The remaining integrals are: `1`";


IntegrateT[arg_, t_]:= Module[
	{temp}
	,
	(* find all t-integrands *)
	temp= Integrand[arg,t];
	
	(* substitute in propagators for s-/t-/u-channels *)
	temp= temp/.ReplaceChannelSums[];
	
	(* Thread conjugate over everything and assume t is real *)
	temp= ExpandConjugate[
		ExpandConjugate[temp]/.Conjugate[t]->t
	]/.Conjugate[t]->t;
	
	(* apply partial fractioning identities *)
	temp= temp/.PartialFractioning[t];
	
	(* reduce all integrals to the master-integrals *)
	temp= temp//.ReduceIntegrands[t];
	
	(* substitute the master integrals *)
	temp= temp/.ReplaceIntegrals[t];
	
	(* Throw an error if not all integrands were removed *)
	If[!FreeQ[temp,_Integrand], Message[IntegrateT::failed, Cases[temp,_Integrand,All]]; Abort[]];
	
	Return[MyExpand[temp]]
]


(* ::Subsubsection::Closed:: *)
(*Integrand*)


Integrand::usage= "Integrand[arg,t] threads over sums and collects in each term all dependence on the specified variable t.";


(* Thread Integrand over sums *)
Integrand[x_Plus,t_]:= Integrand[#,t]&/@x


(* Make Integrand expand its first argument *)
Integrand[Times[a___,sum_Plus],t_]:= Integrand[Times[a,#],t]&/@sum


(* Keep only dependence on t *)
Integrand[Times[a_,b___],t_]:= a*Integrand[Times[b],t]/;FreeQ[a,t]


Integrand[a_,t_]:= a*Integrand[1,t]/;(FreeQ[a,t] && !MatchQ[a,1])


(* ::Subsubsection::Closed:: *)
(*PartialFractioning*)


PartialFractioning::usage= "PartialFractioning[t] yields the replacement rule that can be used to apply the partial fractioning identities to two distinct t-channel propagators.";


PartialFractioning[t_]:= With[{mediators=GetMediators[]},{
	(* t x t *)
	Propagator[t,m1:Except[_Conjugate]] * Propagator[t, Conjugate[m2_]]:> (1/Propagator[0,Conjugate[m2]]-1/Propagator[0,m1])^(-1) * (Propagator[t,m1]-Propagator[t,Conjugate[m2]]) /; (mediators[m1][Mass]!=mediators[m2][Mass] || mediators[m1][Width]!=0 || mediators[m2][Width]!=0) (* (m1=!=m2 || GetMediators[][m1][[2]]!=0) *),
	Propagator[t,m1:Except[_Conjugate]] * Propagator[t, m2:Except[_Conjugate]]:> (1/Propagator[0,m2]-1/Propagator[0,m1])^(-1) * (Propagator[t,m1]-Propagator[t,m2]) /; (mediators[m1][Mass]!=mediators[m2][Mass]), (* only applies if m1 and m2 have width=0 *)
	(* u x u *)
	Propagator[-t-s_,m1:Except[_Conjugate]] * Propagator[-t-s_, Conjugate[m2_]]:> (1/Propagator[0,Conjugate[m2]]-1/Propagator[0,m1])^(-1) * (Propagator[-t-s,m1]-Propagator[-t-s,Conjugate[m2]]) /; (mediators[m1][Mass]!=mediators[m2][Mass] || mediators[m1][Width]!=0 || mediators[m2][Width]!=0) (* (m1=!=m2 || GetMediators[][m1][[2]]!=0) *) ,
	Propagator[-t-s_,m1:Except[_Conjugate]] * Propagator[-t-s_, m2:Except[_Conjugate]]:> (1/Propagator[0,m2]-1/Propagator[0,m1])^(-1) * (Propagator[-t-s,m1]-Propagator[-t-s,m2]) /; (mediators[m1][Mass]!=mediators[m2][Mass]) (* only applies if m1 and m2 have width=0 *) ,
	(* t x u *) (* the conditions m1=!=m2 can be removed since we have s+mass1+mass2>0 *)
	Propagator[t,m1:Except[_Conjugate]] * Propagator[-t-s_, Conjugate[m2_]]:> (s-1/Propagator[0,Conjugate[m2]]-1/Propagator[0,m1])^(-1) * (Propagator[t,m1]+Propagator[-t-s,Conjugate[m2]]) (*/; m1=!=m2*),
	Propagator[t,m1:Except[_Conjugate]] * Propagator[-t-s_, m2:Except[_Conjugate]]:> (s-1/Propagator[0,m2]-1/Propagator[0,m1])^(-1) * (Propagator[t,m1]+Propagator[-t-s,m2]) (* only applies if m1 and m2 have width=0 *),
	(* u x t *)
	Propagator[-t-s_,m1:Except[_Conjugate]] * Propagator[t, Conjugate[m2_]]:> (s-1/Propagator[0,Conjugate[m2]]-1/Propagator[0,m1])^(-1) * (Propagator[-t-s,m1]+Propagator[t,Conjugate[m2]]) (*/; m1=!=m2*),
	Propagator[-t-s_,m1:Except[_Conjugate]] * Propagator[t, m2:Except[_Conjugate]]:> (s-1/Propagator[0,m2]-1/Propagator[0,m1])^(-1) * (Propagator[-t-s,m1]+Propagator[t,m2]) (* only applies if m1 and m2 have width=0 *),
	
	(* for propagators of two particles with identical mass and width *)
	Propagator[t,m1:Except[_Conjugate]] * Propagator[t, m2:Except[_Conjugate]]:> Propagator[t,m1]^2 /; (mediators[m1][Mass]==mediators[m2][Mass] && mediators[m1][Width]==mediators[m2][Width]),
	Propagator[-t-s_,m1:Except[_Conjugate]] * Propagator[-t-s_, m2:Except[_Conjugate]]:> Propagator[-t-s,m1] /; (mediators[m1][Mass]==mediators[m2][Mass] && mediators[m1][Width]==mediators[m2][Width])
}]


(* ::Subsubsection::Closed:: *)
(*ReduceIntegrands*)


ReduceIntegrands::usage= "ReduceIntegrands[t] returns the replacement rules that can be used to simpify integrands by using the trick \!\(\*FractionBox[\(t\), \(t - X\)]\) = 1 + \!\(\*FractionBox[\(X\), \(t - X\)]\).";


ReduceIntegrands[t_]:= {
	(* t-channel *)
	(t * Propagator[t, mediator_]):> 1 - Propagator[t,mediator]/Propagator[0,mediator],
	(Power[t, pow_/;(IntegerQ[pow] && pow>0)] * Propagator[t, mediator_]):> t^(pow-1)*(1-Propagator[t,mediator]/Propagator[0,mediator]),
	(t * Propagator[t, mediator_]^2):> (1 - Propagator[t,mediator]/Propagator[0,mediator])*Propagator[t,mediator],
	(Power[t, pow_/;(IntegerQ[pow] && pow>0)] * Propagator[t, mediator_]^2):> t^(pow-1)*(1-Propagator[t,mediator]/Propagator[0,mediator])*Propagator[t,mediator],
	(* u-channel *)
	(t * Propagator[-t-s_, mediator_]):> -1 + Propagator[-t-s, mediator]/Propagator[-s,mediator],
	(Power[t, pow_/;(IntegerQ[pow] && pow>0)] * Propagator[-t-s_, mediator_]):> t^(pow-1)*(-1 + Propagator[-t-s, mediator]/Propagator[-s,mediator]),
	(t * Propagator[-t-s_, mediator_]^2):> (-1 + Propagator[-t-s, mediator]/Propagator[-s,mediator])*Propagator[-t-s, mediator],
	(Power[t, pow_/;(IntegerQ[pow] && pow>0)] * Propagator[-t-s_, mediator_]^2):> t^(pow-1)*(-1 + Propagator[-t-s, mediator]/Propagator[-s,mediator])*Propagator[-t-s, mediator]
}


(* ::Subsubsection::Closed:: *)
(*ReplaceIntegrals*)


ReplaceIntegrals::usage= "ReplaceIntegrals[t, tMin, tMax] returns the replacement rules required for all master integrals in t with lower and uper bounds tMin and tMax respectively.";


ReplaceIntegrals[t_]:= {
	(* polynomials *)
	Integrand[1,t]:> t,
	Integrand[t,t]:> 1/2*t^2,
	Integrand[Power[t,n_/;(IntegerQ[n] && n>0)],t]:> 1/(n+1) * Power[t,n+1],
	(* t-channels *)
	Integrand[Propagator[t, m1_],t]:> Log[1/Propagator[t, m1]],
	Integrand[Propagator[t, m1:Except[_Conjugate]] * Propagator[t, Conjugate[m1_]],t]:> If[GetMediators[][m1][[2]] == 0,
		- Propagator[t, m1], (* for zero width particles *)
		(*(ArcTan[(t-Mass[m1]^2)/(Mass[m1]*Width[m1])]/(Mass[m1]*Width[m1]))*) (* for no-zero width particles *)
		(Log[1/Propagator[t,m1]]-Log[1/Propagator[t,Conjugate[m1]]])/((-1/Propagator[0,m1])+(1/Propagator[0,Conjugate[m1]]))
	],
	Integrand[Propagator[t, m1_]^2,t]:> -Propagator[t, m1],
	(* u-channels *)
	Integrand[Propagator[-t-s_, m1_],t]:> - Log[-1/Propagator[-t-s, m1]],
	Integrand[Propagator[-t-s_, m1:Except[_Conjugate]] * Propagator[-t-s_, Conjugate[m1_]],t]:> If[GetMediators[][m1][[2]] == 0,
		Propagator[-t-s, m1], (* for zero width particles *)
		(*-(ArcTan[(Mass[m1]*Width[m1])/(t+s+Mass[m1]^2)]/(Mass[m1]*Width[m1]))*) (* for no-zero width particles *)
		(-Log[-1/Propagator[-t-s,m1]]+Log[-1/Propagator[-t-s,Conjugate[m1]]])/((-1/Propagator[0,m1])+(1/Propagator[0,Conjugate[m1]]))
	],
	Integrand[Propagator[-t-s_, m1_]^2,t]:> Propagator[-t-s, m1]
}


(* ::Subsubsection::Closed:: *)
(*SimplifySIntegrals*)


PartialFractioningSIntegrals[s_]:={
	Propagator[s,med_]/s :> Propagator[0,med](1/s - Propagator[s,med]) /; !MatchQ[GetMediators[][med/.Conjugate[m_]:>m][[1]], 0|0.],
	s^n_Integer*Propagator[s,med_] :> Propagator[0,med](s^n - s^(n+1)*Propagator[s,med]) /; (n<-1 && (!MatchQ[GetMediators[][med/.Conjugate[m_]:>m][[1]], 0|0.]))
}


(* ::Section:: *)
(*Hadron-level cross-section*)


(* ::Subsection:: *)
(*Integrated CrossSection*)


CrossSection::usage= "CrossSection[{\!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\)[\[Alpha]],\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)[\[Beta]]}] computes the total hadronic cross section \[Sigma]=\[Integral]\!\(\*FractionBox[\(d\[Sigma]\), \(ds\)]\)\[DifferentialD]s for the process p p -> \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\) \!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(2\)], \(_\)]\) in units of picobarn. The final state consists of a lepton \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\) and an anti-lepton \!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(2\)], \(_\)]\), i.e. \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\),\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)\[Element]{e,\[Nu]} with flavor indices \[Alpha],\[Beta]\[Element]{1,2,3}. The cross section is obtained by integrating over a specific bin in the dilepton invariant mass and in the transverse momentum. The options and their default values are: MLLcuts \[Rule] {50,13000} [GeV]; PTcuts \[Rule] {0,\[Infinity]} [GeV]; FF \[Rule] False; Coefficients \[Rule] All; EFTorder \[RuleDelayed] GetEFTorder[]; OperatorDimension \[RuleDelayed] GetOperatorDimension[], EFTscale \[RuleDelayed] GetEFTscale[], Efficiency \[Rule] False.
CrossSection[{e[\[Alpha]],\[Nu]}] computes the total hadronic cross section while summing the contributions of all anti-neutrino flavors.
CrossSection[{\[Nu],e[\[Beta]]}] computes the total hadronic cross section while summing the contributions of all neutrino flavors.";


CrossSection::inteval= "Not all \!\(\*OverscriptBox[\(s\), \(^\)]\) integrals have been evaluated. Number of unevaluated integrals: `1`.";


Options[CrossSection]= {
	MLLcuts           -> {50,13000},
	PTcuts            -> {0,\[Infinity]},
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	Efficiency        -> False,
	EFTscale          :> GetEFTscale[]
};


(* summing over \[Nu] flavors *)
CrossSection[{e[a_],\[Nu]}, OptionsPattern[]]:= Module[
	{b, \[Sigma]}
	,
	(* compute cross section *)
	\[Sigma]= CrossSection[
		{e[a],\[Nu][b]},
		MLLcuts           -> OptionValue[MLLcuts],
		PTcuts            -> OptionValue[PTcuts],
		FF                -> OptionValue[FF],
		Coefficients      -> OptionValue[Coefficients],
		EFTorder          -> OptionValue[EFTorder],
		OperatorDimension -> OptionValue[OperatorDimension],
		Efficiency        -> OptionValue[Efficiency],
		EFTscale          -> OptionValue[EFTscale]
	];
	(* sum over \[Nu] flavors *)
	\[Sigma]= (\[Sigma]/.b->1) + (\[Sigma]/.b->2) + (\[Sigma]/.b->3);
	
	Return[\[Sigma]]
]


(* summing over \[Nu] flavors *)
CrossSection[{\[Nu],e[b_]}, OptionsPattern[]]:= Module[
	{a, \[Sigma]}
	,
	(* compute cross section *)
	\[Sigma]= CrossSection[
		{\[Nu][a],e[b]},
		MLLcuts           -> OptionValue[MLLcuts],
		PTcuts            -> OptionValue[PTcuts],
		FF                -> OptionValue[FF],
		Coefficients      -> OptionValue[Coefficients],
		EFTorder          -> OptionValue[EFTorder],
		OperatorDimension -> OptionValue[OperatorDimension],
		Efficiency        -> OptionValue[Efficiency],
		EFTscale          -> OptionValue[EFTscale]
	];
	(* sum over \[Nu] flavors *)
	\[Sigma]= (\[Sigma]/.a->1) + (\[Sigma]/.a->2) + (\[Sigma]/.a->3);
	
	Return[\[Sigma]]
]


(* for NC and CC with specific \[Nu] flavor *)
CrossSection[{\[Alpha]:(e[_]|\[Nu][_]), \[Beta]:(e[_]|\[Nu][_])}, OptionsPattern[]]:= Module[
	{
		\[Sigma],s,sMin,sMax,ptCuts,
		sIntegralList,
		dummyIntegral, 
		integralAssoc, integralAssocReverse, 
		nonRedundantIntegarlList={},
		MyMin, MyMax (* these are proxies for Min and Max are OneIdentity which breaks patternmatching below *)
	}
	,
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, PTcuts, MLLcuts, Efficiency, EFTscale};
	
	(*
	(* slows down computations and is unnecessary *)
	s/:Conjugate[s]:= s;
	*)
	
	MyMin/:Conjugate[MyMin]:= MyMin;
	MyMax/:Conjugate[MyMax]:= MyMax;
	
	(* define the cuts *)
	{sMin,sMax}= OptionValue[MLLcuts]^2;
	ptCuts= OptionValue[PTcuts];
	
	(* compute differential hadronic cross section *)
	\[Sigma]= HadronicDifferentialCrossSection[s, {\[Alpha],\[Beta]}, 
		PTcuts            -> ptCuts,
		Efficiency        -> OptionValue[Efficiency],
		OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* Find and collect all s-integrals *)
	\[Sigma]= MyTiming[
	Integrand[\[Sigma],s]
	, "Integrand (s)"
	];
	
	(* simplify integrals *)
	\[Sigma]= \[Sigma]/.PartialFractioning[s];
	\[Sigma]= \[Sigma]//.ReduceIntegrands[s];
	\[Sigma]= \[Sigma]//.PartialFractioningSIntegrals[s];
	
	(* Min and Max are OneIdentity which breaks patternmatching below *)
	\[Sigma]= \[Sigma] /. {Min->MyMin, Max->MyMax};
	
	(* find list of all non-equivalent integrals *)
	sIntegralList= DeleteDuplicates@Cases[\[Sigma], _Integrand, All];
	
	(* built association with unique symbols *)
	integralAssoc= Association[(# -> dummyIntegral[Unique[]])& /@ sIntegralList];
	
	(* find self conjugate and complex conjugated integrals *)
	Do[
		(* check if int is already in list *)
		If[!MemberQ[nonRedundantIntegarlList,int],
			With[{conjInt=Conjugate[int]//.{Conjugate[Sqrt[arg_]]:>Sqrt[Conjugate@arg], Conjugate[x_MyMin]:>x, Conjugate[x_MyMax]:>x}},
				If[MemberQ[nonRedundantIntegarlList,conjInt],
					(* if Conjugate[int] is already in the list *)
					AssociateTo[integralAssoc, int -> Conjugate[integralAssoc[conjInt]]],
					(* if neither int nor Conjugate[int] is already in the list *)
					AppendTo[nonRedundantIntegarlList, int]
				]
			]
		]
		,
		{int,sIntegralList}
	];
	
	(* compute necessary integrals and store them as an association*)
	integralAssocReverse= Table[
		integralAssoc[int] -> int
		,
		{int, nonRedundantIntegarlList}
	];
	
	(* built the replacement rules *)
	integralAssocReverse= integralAssocReverse/.ReplacePropagators;
	integralAssocReverse= integralAssocReverse/.ReplaceConstants[];
	integralAssocReverse= integralAssocReverse/.{MyMin->Min, MyMax->Max};
	MyEcho[Length[integralAssocReverse], "# Integrals"];
	
	(* if required print all integrals *)
	(*
	Do[
		Print[int]
		,
		{int, DeleteDuplicates@Cases[integralAssocReverse,ig_Integrand :> (ig/.{s->Global`s, _InterpolatingFunction->Global`IF}), All]}
	];
	*)
	
	integralAssocReverse= MyTiming[integralAssocReverse/.Integrand[arg_,x_]:> NIntegrate[arg,{x,sMin,sMax}], "NIntegrate"];
	(*integralAssocReverse= MyTiming[integralAssocReverse/.Integrand[arg_,x_]:> CachedIntegrals[arg,{x,sMin,sMax}], "NIntegrate"]; *)
	integralAssocReverse= Association[integralAssocReverse];
	
	(* substitute in cross section *)
	\[Sigma]= \[Sigma] /. integralAssoc;
	\[Sigma]= \[Sigma] /. integralAssocReverse;
	
	(* warning if some integrals have not been computed *)
	If[!FreeQ[\[Sigma], _dummyIntegral],
		Message[CrossSection::inteval, Length@DeleteDuplicates@Cases[\[Sigma], _dummyIntegral, All]]
	];

	(* replace remaining propagators and constants outside integrands *)
	\[Sigma]= \[Sigma]/.ReplacePropagators;
	\[Sigma]= \[Sigma]/.ReplaceConstants[];
	
	(* select specified FF if required *)
	If[!FreeQ[OptionValue[Coefficients],_FF],
		\[Sigma]= SelectTerms[\[Sigma], Cases[OptionValue[Coefficients], _FF, All]]
	];
	
	(* Substitute in WC if recuired *)
	If[!OptionValue[FF],
		\[Sigma]= SubstituteFF[
			\[Sigma],
			EFTscale          -> OptionValue[EFTscale],
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension]
		]
	];
	
	(*
	(* Set coefficients to zero if required *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]= SelectTerms[\[Sigma], OptionValue[Coefficients]]
	];
	*)
	
	(* remove WC and/or Coupling *)
	If[!FreeQ[OptionValue[Coefficients], _WC],
		\[Sigma] = SelectTerms[\[Sigma], Cases[OptionValue[Coefficients], _WC, All]]
	];
	If[!FreeQ[OptionValue[Coefficients], _Coupling],
		\[Sigma] = SelectTerms[\[Sigma], Cases[OptionValue[Coefficients], _Coupling, All]]
	];
	
	\[Sigma]= MyExpand[\[Sigma]];
	
	Return[\[Sigma]/.{Complex[a_,0.`]:> a, Complex[b_,0]:> b}/.{0.`->0}] (* in pb *)
]


(* ::Subsubsection::Closed:: *)
(*Cached numeric integrals [not in use currently]*)


$NumericIntegrals= FileNameJoin[{Global`$DirectoryHighPT,"NumericIntegrals"}];


CachedIntegrals[integrand_,{s_,sMin_,sMax_}] := Module[
	{
		int,
		solution,
		file = FileNameJoin[{$NumericIntegrals, $CurrentSearch <> ".m"}],
		str
	}
	,
	int = NIntegrate[integrand,{s,sMin,sMax}];
	solution = <|(Integrand[integrand,{s,sMin,sMax}]/.s->Pattern[Global`s,Blank[]]) -> int|>;
	
	str = OpenAppend[file];
	Write[str, solution];
	Close[str];
	
	Return[int]
]


(* ::Subsection::Closed:: *)
(*Differential cross-section in s (for internal use)*)


HadronicDifferentialCrossSection::usage= "HadronicDifferentialCrossSection[] computes the differential hadronic cross-section.";


Options[HadronicDifferentialCrossSection]= {
	PTcuts             -> {0,\[Infinity]},
	Efficiency         -> False,
	OperatorDimension  :> GetOperatorDimension[],
	"PartonLuminosity" -> True
};


HadronicDifferentialCrossSection[s_, {\[Alpha]_,\[Beta]_}, OptionsPattern[]]:= MyTiming[
Module[
	{
		\[Sigma]General, \[Sigma], \[Sigma]HadronDifferential,
		a,b,i,j,
		GeV2toPB=(10^9)/(2.56819)
	}
	,
	(* derive parton-level cross-section with generic flavor indices *)
	\[Sigma]General= PartonCrossSection[s, {\[Alpha],\[Beta],i,j}, 
		PTcuts            -> OptionValue[PTcuts],
		OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* distinguish charged-current from neutral-current *)
	Switch[{Head[\[Alpha]],Head[\[Beta]]},
		{e,e},(* NC *)
			\[Sigma]["d_dbar"]= 1/s * PartonLuminosity["d_dbar"][Sqrt[s]] * \[Sigma]General/.{i->d[1], j->d[1]};
			\[Sigma]["d_sbar"]= 1/s * PartonLuminosity["d_sbar"][Sqrt[s]] * \[Sigma]General/.{i->d[2], j->d[1]};
			\[Sigma]["d_bbar"]= 1/s * PartonLuminosity["d_bbar"][Sqrt[s]] * \[Sigma]General/.{i->d[3], j->d[1]};
			\[Sigma]["s_dbar"]= 1/s * PartonLuminosity["s_dbar"][Sqrt[s]] * \[Sigma]General/.{i->d[1], j->d[2]};
			\[Sigma]["s_sbar"]= 1/s * PartonLuminosity["s_sbar"][Sqrt[s]] * \[Sigma]General/.{i->d[2], j->d[2]};
			\[Sigma]["s_bbar"]= 1/s * PartonLuminosity["s_bbar"][Sqrt[s]] * \[Sigma]General/.{i->d[3], j->d[2]};
			\[Sigma]["b_dbar"]= 1/s * PartonLuminosity["b_dbar"][Sqrt[s]] * \[Sigma]General/.{i->d[1], j->d[3]};
			\[Sigma]["b_sbar"]= 1/s * PartonLuminosity["b_sbar"][Sqrt[s]] * \[Sigma]General/.{i->d[2], j->d[3]};
			\[Sigma]["b_bbar"]= 1/s * PartonLuminosity["b_bbar"][Sqrt[s]] * \[Sigma]General/.{i->d[3], j->d[3]};
			\[Sigma]["u_ubar"]= 1/s * PartonLuminosity["u_ubar"][Sqrt[s]] * \[Sigma]General/.{i->u[1], j->u[1]};
			\[Sigma]["u_cbar"]= 1/s * PartonLuminosity["u_cbar"][Sqrt[s]] * \[Sigma]General/.{i->u[2], j->u[1]};
			\[Sigma]["c_ubar"]= 1/s * PartonLuminosity["c_ubar"][Sqrt[s]] * \[Sigma]General/.{i->u[1], j->u[2]};
			\[Sigma]["c_cbar"]= 1/s * PartonLuminosity["c_cbar"][Sqrt[s]] * \[Sigma]General/.{i->u[2], j->u[2]};
			\[Sigma]HadronDifferential= Plus[
				\[Sigma]["d_dbar"], \[Sigma]["d_sbar"], \[Sigma]["d_bbar"], \[Sigma]["s_dbar"], \[Sigma]["s_sbar"], \[Sigma]["s_bbar"],
				\[Sigma]["b_dbar"], \[Sigma]["b_sbar"], \[Sigma]["b_bbar"], \[Sigma]["u_ubar"], \[Sigma]["u_cbar"], \[Sigma]["c_ubar"], \[Sigma]["c_cbar"]
			]
		,
		{e,\[Nu]},(* CC *)
			\[Sigma]["d_ubar"]= 1/s * PartonLuminosity["d_ubar"][Sqrt[s]] * \[Sigma]General/.{i->u[1], j->d[1]};
			\[Sigma]["d_cbar"]= 1/s * PartonLuminosity["d_cbar"][Sqrt[s]] * \[Sigma]General/.{i->u[2], j->d[1]};
			\[Sigma]["s_ubar"]= 1/s * PartonLuminosity["s_ubar"][Sqrt[s]] * \[Sigma]General/.{i->u[1], j->d[2]};
			\[Sigma]["s_cbar"]= 1/s * PartonLuminosity["s_cbar"][Sqrt[s]] * \[Sigma]General/.{i->u[2], j->d[2]};
			\[Sigma]["b_ubar"]= 1/s * PartonLuminosity["b_ubar"][Sqrt[s]] * \[Sigma]General/.{i->u[1], j->d[3]};
			\[Sigma]["b_cbar"]= 1/s * PartonLuminosity["b_cbar"][Sqrt[s]] * \[Sigma]General/.{i->u[2], j->d[3]};
			\[Sigma]HadronDifferential= Plus[
				\[Sigma]["d_ubar"], \[Sigma]["d_cbar"], \[Sigma]["s_ubar"], \[Sigma]["s_cbar"], \[Sigma]["b_ubar"], \[Sigma]["b_cbar"]
			]
		,
		{\[Nu],e},(* CC *)
			\[Sigma]["u_dbar"]= 1/s * PartonLuminosity["u_dbar"][Sqrt[s]] * \[Sigma]General/.{i->d[1], j->u[1]};
			\[Sigma]["u_sbar"]= 1/s * PartonLuminosity["u_sbar"][Sqrt[s]] * \[Sigma]General/.{i->d[2], j->u[1]};
			\[Sigma]["u_bbar"]= 1/s * PartonLuminosity["u_bbar"][Sqrt[s]] * \[Sigma]General/.{i->d[3], j->u[1]};
			\[Sigma]["c_dbar"]= 1/s * PartonLuminosity["c_dbar"][Sqrt[s]] * \[Sigma]General/.{i->d[1], j->u[2]};
			\[Sigma]["c_sbar"]= 1/s * PartonLuminosity["c_sbar"][Sqrt[s]] * \[Sigma]General/.{i->d[2], j->u[2]};
			\[Sigma]["c_bbar"]= 1/s * PartonLuminosity["c_bbar"][Sqrt[s]] * \[Sigma]General/.{i->d[3], j->u[2]};
			\[Sigma]HadronDifferential= Plus[
				\[Sigma]["u_dbar"], \[Sigma]["u_sbar"], \[Sigma]["u_bbar"], \[Sigma]["c_dbar"], \[Sigma]["c_sbar"], \[Sigma]["c_bbar"]
			]
		,
		{\[Nu],\[Nu]}, (* NC \[Rule] invisible *)
			\[Sigma]HadronDifferential= 0
	];
	
	(* include efficiencies if necessary *)
	If[OptionValue[Efficiency],
		\[Sigma]HadronDifferential= IncludeEfficiencies[\[Sigma]HadronDifferential]
	];
	
	(* rotate from mass to weak basis for quark flavor indices *)
	\[Sigma]HadronDifferential= RotateMassToWeakBasis[\[Sigma]HadronDifferential];
	(* change units from GeV^-2 to pb *)
	\[Sigma]HadronDifferential= GeV2toPB * \[Sigma]HadronDifferential;
	
	(* substitute in interpolated functions for PartonLuminosity *)
	If[OptionValue["PartonLuminosity"],
		\[Sigma]HadronDifferential= \[Sigma]HadronDifferential /. PartonLuminosity -> PartonLuminosityFunction
	];
	
	(*MyEcho[\[Sigma]HadronDifferential, "\[Sigma]", TraditionalForm];*)
	Return@ Expand[\[Sigma]HadronDifferential] (* in pb *)
]
,
"HadronicDifferentialCrossSection"
]


(* ::Subsection::Closed:: *)
(*Differential cross section (for external use)*)


DifferentialCrossSection::usage= "DifferentialCrossSection[{\!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\)[\[Alpha]],\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)[\[Beta]]}] computes the differential hadronic cross section \!\(\*FractionBox[\(d\[Sigma]\), \(ds\)]\) for the process p p -> \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\) \!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(2\)], \(_\)]\) in units of picobarn. The final state consists of a lepton \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\) and an anti-lepton \!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(2\)], \(_\)]\), i.e. \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\),\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)\[Element]{e,\[Nu]} with flavor indices \[Alpha],\[Beta]\[Element]{1,2,3}. The result is returned as a function of the partonic center of mass energy \!\(\*OverscriptBox[\(s\), \(^\)]\). Usage: if \[Sigma]=DifferentialCrossSection[{\!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\)[\[Alpha]],\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)[\[Beta]]}], then \[Sigma][\!\(\*OverscriptBox[\(s\), \(^\)]\)] can be used to evaluate the differential cross section at any value of \!\(\*OverscriptBox[\(s\), \(^\)]\). The options and their default values are: PTcuts \[Rule] {0,\[Infinity]} [GeV]; FF \[Rule] False; Coefficients \[Rule] All; EFTorder \[RuleDelayed] GetEFTorder[]; OperatorDimension \[RuleDelayed] GetOperatorDimension[]; EFTscale \[RuleDelayed] GetEFTscale[].";


Options[DifferentialCrossSection]= {
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	PTcuts            -> {0,\[Infinity]},
	EFTscale          :> GetEFTscale[]
};


DifferentialCrossSection[{\[Alpha]:(e[_]|\[Nu][_]),\[Beta]:(e[_]|\[Nu][_])}, OptionsPattern[]]:= Module[
	{\[Sigma],s}
	,
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, PTcuts, EFTscale};
	
	(* compute d\[Sigma]/ds *)
	\[Sigma]= HadronicDifferentialCrossSection[s, {\[Alpha],\[Beta]}, 
		PTcuts            -> OptionValue[PTcuts],
		OperatorDimension -> OptionValue[OperatorDimension]
	];
	
	(* replace constants *)
	\[Sigma]= \[Sigma]/.ReplacePropagators;
	\[Sigma]= \[Sigma]/.ReplaceConstants[];
	
	\[Sigma]= MyExpand[\[Sigma]];
	
	(* select specified FF if required *)
	If[!FreeQ[OptionValue[Coefficients],_FF],
		\[Sigma]= SelectTerms[\[Sigma], Cases[OptionValue[Coefficients], _FF, All]]
	];
	
	(* Substitute in WC *)
	If[!OptionValue[FF],
		\[Sigma]= SubstituteFF[
			\[Sigma],
			EFTscale          -> OptionValue[EFTscale],
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension]
		]
	];
	
	(* Set coefficients to zero *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]= SelectTerms[\[Sigma], OptionValue[Coefficients]]
	];
	
	\[Sigma]= MyExpand[\[Sigma]];
	
	With[{xSec=\[Sigma]},
		Return@ Function[x,Re[xSec]/.s->x]
	];	
]


(* ::Section::Closed:: *)
(*Parton luminosity functions*)


PartonLuminosityFunction::usage= "PartonLuminosityFunction['q_qbar'][\[Tau]] is the parton luminosity function for the quark flavor combination given by the string 'q_qbar'. The u\!\(\*OverscriptBox[\(d\), \(_\)]\) parton luminosity is for example given by PartonLuminosityFunction['u_dbar']. The parton luminosity functions can be evaluated at a given energy scale \!\(\*SqrtBox[\(s\)]\) by using: PartonLuminosityFunction['q_qbar'][\!\(\*SqrtBox[\(s\)]\)].";


(* ::Subsection::Closed:: *)
(*List of all parton luminosity functions*)


(* List of all parton luminosities to be imported *)
$PartonLuminosityFiles= {
	(* NC *)
	"d_dbar.dat","s_sbar.dat","b_bbar.dat","u_ubar.dat","c_cbar.dat",
	"d_sbar.dat","s_dbar.dat",
	"d_bbar.dat","b_dbar.dat",
	"s_bbar.dat","b_sbar.dat",
	"u_cbar.dat","c_ubar.dat",
	(* CC *)
	"u_dbar.dat","d_ubar.dat",
	"u_sbar.dat","s_ubar.dat",
	"u_bbar.dat","b_ubar.dat",
	"c_dbar.dat","d_cbar.dat",
	"c_sbar.dat","s_cbar.dat",
	"c_bbar.dat","b_cbar.dat"
};


(* ::Subsection::Closed:: *)
(*Directory of PartonLuminosityFunctions*)


(* relative path to the directory of the parton luminosity functions *)
$DirectoryPartonLuminosityFiles= FileNameJoin[
	{Global`$DirectoryHighPT, "PartonLuminosities", "PDF4LHC15"}
];


(* ::Subsection::Closed:: *)
(*Initialize parton luminosities*)


(* Function to load a single parton luminosity function *)
LoadLuminosityFunction[fname_String]:= Module[
	{
		(* import the file *)
		file= Import@ FileNameJoin[{$DirectoryPartonLuminosityFiles,fname}],
		name= FileBaseName[fname],
		mInv, values
	}
	,
	(* extract masses and values *)
	{mInv, values}= Transpose[file];
	(* interpolate *)
	PartonLuminosityFunction[name]= Interpolation[MapThread[List,{mInv, values}]];
]


(* available PDF names *)
$PDFsets  = {
	"PDF4LHC15",
	"PDF4LHC21",
	"NNPDF40",
	"NNPDF31",
	"CT18NNLO"
};


SetPDF::usage = "SetPDF[\"pdf\"] changes the PDF set used for cross section computations to the set labeled by \"pdf\".
SetPDF[] list all available PDF sets.";


(* function to laod a new set of PDFs *)
SetPDF[pdf_:Alternatives@@$PDFsets]:=Module[
	{}
	,
	(* set directory for pdfs *)
	$DirectoryPartonLuminosityFiles = FileNameJoin[{
		Global`$DirectoryHighPT,
		"PartonLuminosities",
		pdf
	}];
	
	(* load all luminosities in that directory *)
	Do[
		LoadLuminosityFunction[file];
		,
		{file, $PartonLuminosityFiles}
	];
];


(* list all available PDF sets *)
SetPDF[] := ($PDFsets)


(* throw a warning if PDF is unknown *)
SetPDF::unknownpdf = "The PDF label `1` is unknown. The available PDF sets are: `2`";
SetPDF[arg:Except[Alternatives@@$PDFsets]] := Message[SetPDF::unknownpdf, arg, $PDFsets]


(* initialization *)
SetPDF["PDF4LHC15"];
