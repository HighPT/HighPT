(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`EventYield`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["Yield"]


(* ::Chapter:: *)
(*Private:*)


PackageScope["NIntegrand"]


(* ::Section:: *)
(*Yield*)


Yield::usage= "EventYield[\"proc\"]
	Computes the expected number of events for all bins of the observables in the search specified by the argument \"proc\".
	The final state consists of a lepton \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\) and an anti-lepton \!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(2\)], \(_\)]\), i.e. \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\),\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)\[Element]{e,\[Nu]} with flavor indices \[Alpha],\[Beta]\[Element]{1,2,3}.
	For neutrinos no flavor index can be specified in which case a summation of all \[Nu] flavors is implicit.
	The options and their default values are: 
		OutputFormat \[Rule] FF,
		Coefficients \[Rule] All,
		EFTorder \[RuleDelayed] GetEFTorder[],
		OperatorDimension \[RuleDelayed] GetOperatorDimension[],
		SM \[Rule] True, [include (True) or exclude (False) Standard Model contribution],
		Luminosity \[Rule] Default.";


Yield::missingeff= "Not all required efficincies have been given. The missing efficiencies are set to zero, these include: `1`.";


Yield::invalidfinalstate= "The argument `1` is not a valid final state particle. Allowed values are e[1|2|3] and \[Nu]. Asummation over \[Nu] flavors is always implicite."


Options[Yield]= {
	SM                -> True,
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	Scale             :> GetScale[],
	Luminosity        -> Default
};


Yield::binning= "Binning in both \!\(\*SubscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\)]\) and \!\(\*SubscriptBox[\(p\), \(T\)]\) detected. Currently 2d binning is not supported.";


Yield::undefinedsearch= "The LHC search `1` is not defined; defined searches are `2`.";


Yield[proc_String, OptionsPattern[]]:= Module[
	{
		coeff,
		finalstate, \[Nu]flav, ptBins, sBins, lumi,
		\[Sigma]full=0, \[Sigma]temp, \[Sigma]Binned, pTmin, pTmax, s, aux,
		temp,
		efficiencies,
		MyMin, MyMax,
		NObserved
	}
	,
	(*** CHECKS ***)
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, Scale , Luminosity};
	coeff = OptionValue[Coefficients];
	(* Check that proc corresponds to a specified search *)
	If[!KeyExistsQ[LHCSearch[], proc],
		Message[EventYield::undefinedsearch, proc, LHCSearch[]];
		Abort[]
	];
	
	(*** extract and print process info ***)
	{finalstate, ptBins, sBins, lumi} = ExtractProcessInfo[proc];
	(* for mll bining fix pTmin/pTmax*)
	If[Length[ptBins]==1, 
		pTmin= ptBins[[1,1]];
		pTmax= ptBins[[1,2]];
	];
	(* modify the luminosity if required *)
	If[!MatchQ[OptionValue[Luminosity],Default],
		lumi = OptionValue[Luminosity];
	];
	
	(*** loop over all final states of the search do prepare the respective differential cross section ***)
	Do[
		(* compute differential hadronic cross-section - give neutrinos a flavor index*)
		\[Sigma]temp = HadronicDifferentialCrossSection[s, fstate/.\[Nu]->\[Nu][\[Nu]flav], 
			PTcuts             -> {pTmin,pTmax},
			Efficiency         -> True,
			OperatorDimension  -> OptionValue[OperatorDimension],
			"PartonLuminosity" -> False
		];
		
		(* remove pure SM contribution if required *)
		If[!OptionValue[SM],
			\[Sigma]temp = DropSMFF[\[Sigma]temp]
		];
		
		(* if final state contains \[Nu] w/o specified flavor index, then sum over all flavors *)
		If[MatchQ[fstate, {OrderlessPatternSequence[e[_],\[Nu]]}],
			\[Sigma]temp = (\[Sigma]temp/.\[Nu]flav->1) + (\[Sigma]temp/.\[Nu]flav->2) + (\[Sigma]temp/.\[Nu]flav->3)
		];
		
		(* remove FF *)
		If[!FreeQ[coeff, _FF],
			\[Sigma]temp = SelectTerms[\[Sigma]temp, Cases[coeff, _FF, All]]
		];
		
		(* Substitute FF by WC and/or Coupling *)
		If[!OptionValue[FF],
			\[Sigma]temp = SubstituteFF[\[Sigma]temp,
				OperatorDimension -> OptionValue[OperatorDimension],
				EFTorder          -> OptionValue[EFTorder],
				Scale             -> OptionValue[Scale]
			]
		];
		
		(* remove WC and/or Coupling *)
		If[!FreeQ[coeff, _WC],
			\[Sigma]temp = SelectTerms[\[Sigma]temp, Cases[coeff, _WC, All]]
		];
		If[!FreeQ[coeff, _Coupling],
			\[Sigma]temp = SelectTerms[\[Sigma]temp, Cases[coeff, _Coupling, All]]
		];
		
		(* collect integrands *)
		\[Sigma]temp = CollectIntegrals[\[Sigma]temp,s];
		
		(* add the cross section to the full cross section *)
		\[Sigma]full = \[Sigma]full + \[Sigma]temp;
		,
		{fstate,finalstate}
	];
	
	(* hold the expression to avoid constant checking of up/down-values *)
	With[{\[Sigma]Aux=\[Sigma]full},
		\[Sigma]full= Hold[\[Sigma]Aux]
	];
	
	(*** bin the cross section ***)
	MyTiming[
	Switch[{Length[ptBins],Length[sBins]},
		(* mll binning *)
		{1,n_/;n>1}, 
			\[Sigma]Binned = Table[
				With[{sMin= (First[bin])^2, sMax= (Last[bin])^2},
					\[Sigma]full /. (Integrand[arg_,s_] :> NIntegrand[arg,{s,sMin,sMax}])
				]
				,
				{bin,sBins}
			];
		,
		(* pT binning *)
		{n_/;n>1,1}, 
			\[Sigma]Binned = Table[
				With[{PTmin=First[bin],PTmax=Last[bin]},
					\[Sigma]full /. {pTmin->PTmin,pTmax->PTmax}
				]
				,
				{bin,ptBins}
			];
			\[Sigma]Binned = \[Sigma]Binned /. (Integrand[arg_,s_] :> NIntegrand[arg,{s, (sBins[[1,1]])^2, (sBins[[1,2]])^2}]);
		,
		(* throw error for 2d binning *)
		_,
			Message[Yield::binning];
			Abort[]
	];
	,
	"binning the cross section"
	];
	
	(* adapt efficienies to bins *)
	\[Sigma]Binned= Table[
		With[{count=counter},
			\[Sigma]Binned[[count]] /. Efficiency[arg___] :> Efficiency[arg, count]
		]
		,
		{counter, Length@\[Sigma]Binned}
	 ];
	
	(* release the hold *)
	\[Sigma]Binned = ReleaseHold[\[Sigma]Binned];
	
	(* S-integration by identifying complex conjugated integrals *)
	\[Sigma]Binned= MyTiming[SIntegrate[\[Sigma]Binned], "NIntegrate"];
	
	(* Substitute efficiency kernels *)
	MyTiming[
	NObserved= SubstituteEfficiencyKernels[\[Sigma]Binned, proc];
	,
	"Substituting efficiency kernels"
	];
	
	(* multiply by luminosity [lumi]=fb^-1 *)
	MyTiming[
	NObserved= Expand[(1000 * lumi) * NObserved];
	,
	"Final Expand in Yield"
	];
	
	Return[Chop[NObserved, 10^-5](*/.{Complex[0.,0.]-> 0, 0.-> 0}*)]
]


(* ::Subsection:: *)
(*Extracting search details*)


ExtractProcessInfo[proc_]:= Module[
	{searchData,expInfo,finalstate,sBins,ptBins,lumi,printState}
	,
	(* Load all experimental data for this search *)	
	searchData= LHCSearch[proc];
	expInfo= searchData["INFO"];
	finalstate= ToExpression[expInfo["FINALSTATE"]];
	sBins= expInfo["BINS"]["MLL"];
	ptBins= expInfo["BINS"]["PT"];
	lumi= expInfo["LUMINOSITY"];
	
	(* prepare printing labels *)
	If[MatchQ[finalstate,_Alternatives],
		printState= Table[
			"pp \[Rule] " <> (First[state]/.PrintRuleLepton) <> (Last[state]/.PrintRuleAntiLepton) <> " | "
			,
			{state, List@@finalstate}
		];
		printState= StringDrop[printState,-3]
		,
		printState= "pp \[Rule] " <> (First[finalstate]/.PrintRuleLepton) <> (Last[finalstate]/.PrintRuleAntiLepton)
	];
	
	(* print info *)
	Print@TableForm[{
	{"PROCESS",           ":", printState },
	{"EXPERIMENT",        ":", expInfo["EXPERIMENT"]},
	{"ARXIV",             ":", expInfo["ARXIV"]},
	{"SOURCE",            ":", expInfo["SOURCE"]},
	{"OBSERVABLE",        ":", expInfo["OBSERVABLE"]},
	{"BINNING " <> expInfo["OBSERVABLE"] <> " [GeV]", ":", TraditionalForm[expInfo["BINS"]["OBSERVABLE"]]},
	{"EVENTS OBSERVED",   ":", ToString@searchData["DATA"]},
	{"LUMINOSITY [\!\(\*SuperscriptBox[\(fb\), \(-1\)]\)]", ":", lumi},
	(* for internal computation *)
	{"BINNING \!\(\*SqrtBox[OverscriptBox[\(s\), \(^\)]]\) [GeV]", ":", TraditionalForm[sBins]},
	{"BINNING \!\(\*SubscriptBox[\(p\), \(T\)]\) [GeV]", ":", TraditionalForm[ptBins]}
	}];
	
	(* create list of mll bins *)
	sBins= Table[
		{sBins[[n]],sBins[[n+1]]}
		,
		{n, Length[sBins]-1}
	];
	(* create list of pt bins *)
	ptBins= Table[
		{ptBins[[n]],ptBins[[n+1]]}
		,
		{n, Length[ptBins]-1}
	];
	
	finalstate= Switch[Head[finalstate],
		Alternatives, List@@finalstate,
		List, {finalstate}
	];
	
	(* return info required by Yield *)
	Return[{finalstate, ptBins, sBins, lumi}]
]


(* ::Text:: *)
(*Auxiliary functions for printing*)


PrintRuleLepton= {
	e[3] -> "\!\(\*SuperscriptBox[\(\[Tau]\), \(-\)]\)",
	e[2] -> "\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)",
	e[1] -> "\!\(\*SuperscriptBox[\(e\), \(-\)]\)",
	\[Nu]    -> "\[Nu]"
};
PrintRuleAntiLepton= {
	e[3] -> "\!\(\*SuperscriptBox[\(\[Tau]\), \(+\)]\)",
	e[2] -> "\!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)",
	e[1] -> "\!\(\*SuperscriptBox[\(e\), \(+\)]\)",
	\[Nu]    -> "\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\)"
};


(* ::Subsection::Closed:: *)
(*Collect integrals*)


(* ::Text:: *)
(*Function that collects and simplifies all integrals*)


CollectIntegrals[arg_,s_] := MyTiming[
Module[
	{\[Sigma]=arg}
	,
	(* collect all integrals *)
	\[Sigma] = Integrand[\[Sigma],s];
	
	(* partial fraction identities *)
	\[Sigma] = \[Sigma]/.PartialFractioning[s];
	
	(* integral reduction identities *)
	\[Sigma] = \[Sigma]//.ReduceIntegrands[s];
	
	(* special partial fractioning for s-integration *)
	\[Sigma] = \[Sigma]//.PartialFractioningSIntegrals[s];
	
	Return[\[Sigma]]
]
,
"CollectIntegrals"
]


(* ::Subsection::Closed:: *)
(*Remove SM*)


DropSMFF[arg_] := MyTiming[
Module[
	{\[Epsilon]sm, temp=arg}
	,
	\[Epsilon]sm/:Conjugate[\[Epsilon]sm]:= \[Epsilon]sm;
	temp= temp/.(a:FF[_,{_,SM},___]:>\[Epsilon]sm*a);
	temp= MyExpand/@temp;
	temp= temp/.\[Epsilon]sm^2->0;
	temp= temp/.\[Epsilon]sm->1;
	Return[temp]
]
,
"DropSM"
]


(* ::Subsection::Closed:: *)
(*perform the s-integrate*)


Yield::unevaluatedIntegrals = "There are `1` unevaluated s-integrals.";


(* ::Text:: *)
(*Perform all s-integrals given in the argument, by identifying complex conjugated integrals*)


SIntegrate[expr_] := Module[
	{
		\[Sigma]= expr,
		MyMin, MyMax,
		dummyIntegral,
		sIntegralList, sIntegralListMinimal= {},
		integralAssoc, integralAssocReverse
	}
	,
	(* Min and Max are OneIdentity which breaks pattern matching below *)
	\[Sigma]= \[Sigma] /. {Min->MyMin, Max->MyMax};
	
	(* find list of all non-equivalent integrals *)
	sIntegralList= DeleteDuplicates@Cases[\[Sigma], _NIntegrand, All];
	
	(* built association with unique symbols *)
	integralAssoc= Association[(# -> dummyIntegral[Unique[]])& /@ sIntegralList];
	
	(* find self conjugate and complex conjugated integrals *)
	(* loop over all integrals *)
	Do[
		If[!MemberQ[sIntegralListMinimal,int],
			(* if integral (int) is not yet in the minimal list compute its conjugate *)
			With[{conjInt=Conjugate[int]//.{Conjugate[Sqrt[arg_]]:>Sqrt[Conjugate@arg], Conjugate[x_MyMin]:>x, Conjugate[x_MyMax]:>x}},
				If[MemberQ[sIntegralListMinimal,conjInt],
					(* if Conjugate[int] is already in the list associate to this *)
					AssociateTo[integralAssoc, int -> Conjugate[integralAssoc[conjInt]]],
					(* if neither int nor Conjugate[int] is already in the list append int *)
					AppendTo[sIntegralListMinimal, int]
				]
			]
		]
		,
		{int, sIntegralList}
	];
	(*
	sIntegralListMinimal is minimal set of all integrals to compute.
	integralAssoc now only points to members of sIntegralListMinimal.
	*)
	
	(* invert the integral association *)
	integralAssocReverse= Table[
		integralAssoc[int] -> int
		,
		{int, sIntegralListMinimal}
	];
	
	(* replace all propagators and constants in the integals and reintroduce Min/Max *)
	integralAssocReverse= integralAssocReverse/.ReplacePropagators;
	integralAssocReverse= integralAssocReverse/.ReplaceConstants[];
	integralAssocReverse= integralAssocReverse/.{MyMin->Min, MyMax->Max};
	
	(* This can be used to print all integrals: *)
	MyEcho[Length[integralAssocReverse], "# Integrals"];
	(*
	Do[
		Print[int]
		,
		{int, DeleteDuplicates@Cases[integralAssocReverse,ig_NIntegrand :> (ig/.{s->Global`s}), All]}
	];
	*)
	
	(* substitute parton luminosity functions by interpolated functions*)
	integralAssocReverse = MyTiming[integralAssocReverse /. PartonLuminosity -> PartonLuminosityFunction, "Substitute parton luminosities"];
	
	(* perform the numeric integrals *)
	(*integralAssocReverse= integralAssocReverse/.NIntegrand[arg_,x_]:> NIntegrate[arg,x(*, AccuracyGoal\[Rule]4*)]; (* modify accuracy goal ? *)*)
	(* parallelize this step *)
	(* ADD SOME CHECK FOR THE NUMBER OF INTEGRALS, FOR LESS THAN A FEW A SINGLE KERNEL MIGHT BE FASTER *)
	If[$ParallelHighPT,
		integralAssocReverse= ParallelMap[
			(#/.NIntegrand->NIntegrate)&,
			integralAssocReverse
		];
		,
		integralAssocReverse= integralAssocReverse/.NIntegrand->NIntegrate;
	];
	integralAssocReverse= Association[integralAssocReverse];
	
	(*Print@Values@integralAssocReverse;*)
	
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
	
	(* return *)
	Return[\[Sigma]]
]


(* ::Subsection::Closed:: *)
(*substitute efficiency kernels*)


SubstituteEfficiencyKernels[xSec_, proc_String]:= Module[
	{
		efficiencies,
		\[Sigma]Binned= xSec,
		temp,
		NObserved
	}
	,
	(* load efficiencies *)
	efficiencies= LoadEfficiencies[proc];
	
	(* (* ? is this necessary ? *)
	(* expand all bins *)
	\[Sigma]Binned= MyTiming[MyExpand/@\[Sigma]Binned, "Expand in EventYield"];
	*)
	
	(* inactivate all the weird Plus, Times, Power, ... behaviour of List *)
	\[Sigma]Binned= Hold/@ \[Sigma]Binned;
	
	(* group bins and efficiencies *)
	temp= Transpose[{\[Sigma]Binned, Dispatch[efficiencies]}];
	
	(* substitute efficiencies *)
	(* The parallelized version is very slow ... *)
	(*
	MyTiming[
	(* Make the parallelization depend on the number of bins? *)
	If[$ParallelHighPT,
		NObserved= ParallelMap[(First[#]/.Last[#])&, temp];
		,
		NObserved= Map[(First[#]/.Last[#])&, temp];
	];
	,
	"Substituting Efficiencies"
	];
	*)
	NObserved= Map[(First[#]/.Last[#])&, temp];
	
	(* check if there are efficiencies remaining and set them to zero *)
	If[!FreeQ[NObserved,_Efficiency],
		Print/@DeleteDuplicates@ Cases[NObserved, eff_Efficiency(*:> Drop[eff,-1]*), All]; (* explicit printing *)
		Message[
			Yield::missingeff,
			DeleteDuplicates@ Cases[NObserved, eff_Efficiency:> Drop[eff,-1], All]
		];
		(* set remaining efficiencies to zero *)
		NObserved= NObserved/. Efficiency[___] -> Table[0, Length[efficiencies[[1,1,2]]]] (* must be set to zero vector *)
	];
	
	(* activate Plus, Times, Power, ... behaviour of List again *)
	MyTiming[
	NObserved= ReleaseHold[NObserved];
	,
	"ReleaseHold for Efficiencies"
	];
	
	(* sum contribution to each bin *)
	MyTiming[
	NObserved= Plus@@ NObserved;
	,
	"Sum Efficiencies"
	];
	
	(* Return *)
	Return[NObserved]
]
