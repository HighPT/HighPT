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
	Scale             :> GetScale[]
};


Yield::binning2d= "Binning in both \!\(\*SubscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\)]\) and \!\(\*SubscriptBox[\(p\), \(T\)]\) detected. Currently 2d binning is not supported.";


Yield::undefinedsearch= "The LHC search `1` is not defined; defined searches are `2`.";


Yield[proc_String, OptionsPattern[]]:= Module[
	{
		coeff,
		finalstate, ptBins, sBins,
		\[Sigma], pTmin, pTmax, s, aux,
		temp
	}
	,
	(*** CHECKS ***)
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, Scale (*, Luminosity*)};
	coeff = OptionValue[Coefficients];
	(* Check that proc corresponds to a specified search *)
	If[KeyExistsQ[LHCSearch[], proc],
		Print["Computing observable for ", proc, " search: ", LHCSearch[][proc]]
		,
		Message[EventYield::undefinedsearch, proc, LHCSearch[]];
		Abort[]
	];
	
	(*** extract and print process info ***)
	{finalstate, ptBins, sBins} = Echo@ExtractProcessInfo[proc];
	(* for mll bining fix pTmin/pTmax*)
	If[Length[ptBins]==1, 
		pTmin= ptBins[[1,1]];
		pTmax= ptBins[[1,2]];
	];
	
	(*** loop over all final states of the search ***)
	Do[
		(* compute differential hadronic cross-section *)
		\[Sigma] = HadronicDifferentialCrossSection[s, fstate/.\[Nu]->\[Nu][i], 
			(*PTcuts            -> {pTmin,pTmax},*)
			Efficiency        -> True,
			OperatorDimension -> OptionValue[OperatorDimension]
		];
		
		(* remove pure SM contribution if required *)
		If[!OptionValue[SM],
			\[Sigma] = DropSMFF[\[Sigma]]
		];
		
		(* remove FF *)
		If[!FreeQ[coeff, _FF],
			\[Sigma] = SelectTerms[\[Sigma], Cases[coeff, _FF, All]]
		];
		
		(* Substitute FF by WC and/or Coupling *)
		If[!OptionValue[FF],
			(*aux=Table[
				pol -> Unique[]
				,
				{pol, DeleteDuplicates@Cases[\[Sigma], _InterpolatingFunction, All]}
			];
			\[Sigma] = \[Sigma] /. aux;*)
			\[Sigma] = SubstituteFF[\[Sigma],
				OperatorDimension -> OptionValue[OperatorDimension],
				EFTorder          -> OptionValue[EFTorder],
				Scale             -> OptionValue[Scale]
			];
			(*aux = aux/.(Rule[a_,b_] :> Rule[b,a]);
			\[Sigma] = \[Sigma] /. aux*)
		];
		
		(* remove WC and/or Coupling *)
		If[!FreeQ[coeff, _WC],
			\[Sigma] = SelectTerms[\[Sigma], Cases[coeff, _WC, All]]
		];
		If[!FreeQ[coeff, _Coupling],
			\[Sigma] = SelectTerms[\[Sigma], Cases[coeff, _Coupling, All]]
		];
		
		(* collect integrands *)
		\[Sigma] = CollectIntegrals[\[Sigma],s];
		
		,
		{fstate,finalstate}
	];
	
(*******************************************************************)	
	
	(* load efficiencies *)
	
	(* parallelization *)
		(* perform  numerical integration *)
		(* substitute efficiencies *)
	
	(* sum all computed bins to obtain experimental bins *)
	Return[\[Sigma]]
]


(* ::Subsection::Closed:: *)
(*Extracting search details*)


ExtractProcessInfo[proc_]:= Module[
	{searchData,expInfo,finalstate,sBins,ptBins,lumi,printState}
	,
	(* Load all experimental data for this search *)	
	searchData= LHCSearch[proc];
	expInfo= searchData["Info"];
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
	{"EVENTS OBSERVED",   ":", ToString@searchData["Observed"]},
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
	Return[{finalstate, ptBins, sBins}]
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
	\[Sigma]= \[Sigma]/.PartialFractioning[s];
	
	(* integral reduction identities *)
	\[Sigma]= \[Sigma]//.ReduceIntegrands[s];
	
	(* special partial fractioning for s-integration *)
	\[Sigma]= \[Sigma]//.PartialFractioningSIntegrals[s];
	
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
