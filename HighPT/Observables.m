(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`Observables`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["EventYield"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["BinnedCrossSection"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*BinnedCrossSection*)


Options[BinnedCrossSection]= {
	OutputFormat      -> FF,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	Efficiency        -> False
};


(* if serveral final states are specified sum there cross-sections *)
BinnedCrossSection[a_Alternatives, ptBinList_List, mllBinList_List, OptionsPattern[]]:= Sum[
	PrintTemporary["Computing cross sections for finalstate: ", finalstate];
	BinnedCrossSection[finalstate, ptBinList, mllBinList,
		OutputFormat      -> OptionValue[OutputFormat],
		Coefficients      -> OptionValue[Coefficients],
		EFTorder          -> OptionValue[EFTorder],
		OperatorDimension -> OptionValue[OperatorDimension],
		Efficiency        -> OptionValue[Efficiency]
	]
	,
	{finalstate, List@@a}
]


BinnedCrossSection[{\[Alpha]_,\[Beta]_}, ptBinList_List, mllBinList_List, OptionsPattern[]]:= Module[
	{mllBins, ptBins, obs, \[Sigma]Bins, counter=0, $CrossSection}
	,
	(* create list of mll bins *)
	mllBins= Table[
		{mllBinList[[n]],mllBinList[[n+1]]}
		,
		{n, Length[mllBinList]-1}
	];
	(* create list of pt bins *)
	ptBins= Table[
		{ptBinList[[n]],ptBinList[[n+1]]}
		,
		{n, Length[ptBinList]-1}
	];
	
	(* compute \[Sigma] for all bins *)
	\[Sigma]Bins= Module[{mll, \[Sigma]aux, \[Sigma]result},
		(* compute pt bins *)
		\[Sigma]aux= $CrossSection[{\[Alpha],\[Beta]},
			PTcuts            -> #,
			MLLcuts           -> mll,
			OutputFormat      -> OptionValue[OutputFormat],
			Coefficients      -> OptionValue[Coefficients],
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension],
			Efficiency        -> OptionValue[Efficiency]
		]& /@ ptBins;
		(* compute mll bins *)
		\[Sigma]result= (\[Sigma]aux/.mll->#)& /@ mllBins;
		
		\[Sigma]result= Flatten[\[Sigma]result];
		
		(* start computation *)
		If[$ParallelHighPT,
			(* parallelized *)
			With[{$xSec=$CrossSection(*, xSec=CrossSection, list= \[Sigma]result*)},
				\[Sigma]result= ParallelMap[
					(# /. $xSec->CrossSection)&,
					\[Sigma]result,
					DistributedContexts -> {"HighPT`"},
					ProgressReporting   -> True
				];
			];
			,
			(* single core *)
			PrintTemporary["Computing bin: ", Dynamic[counter], "/", Length[ptBins]*Length[mllBins]];
			\[Sigma]result= (\[Sigma]result/.$CrossSection[args___]:>(counter++; CrossSection[args]));
		];

		\[Sigma]result
	];
	
	Return[\[Sigma]Bins]
]


(* ::Section:: *)
(*EventYield*)


EventYield::usage= "EventYield[{\!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\)[\[Alpha]],\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)[\[Beta]]}]
	Computes the expected number of events for the process p p -> \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\) \!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(2\)], \(_\)]\) in each bin of the observable used for the specific process.
	The final state consists of a lepton \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\) and an anti-lepton \!\(\*OverscriptBox[SubscriptBox[\(\[ScriptL]\), \(2\)], \(_\)]\), i.e. \!\(\*SubscriptBox[\(\[ScriptL]\), \(1\)]\),\!\(\*SubscriptBox[\(\[ScriptL]\), \(2\)]\)\[Element]{e,\[Nu]} with flavor indices \[Alpha],\[Beta]\[Element]{1,2,3}.
	For neutrinos no flavor index can be specified and a summation of all \[Nu] flavor is implicit.
	The options and their default values are: 
		OutputFormat \[Rule] FF,
		Coefficients \[Rule] All,
		EFTorder \[RuleDelayed] GetEFTorder[],
		OperatorDimension \[RuleDelayed] GetOperatorDimension[],
		SM \[Rule] True, [include (True) or exclude (False) Standard Model contribution],
		Luminosity \[Rule] Default.";


EventYield::missingeff= "Not all required efficincies have been given. The missing efficiencies are set to zero, these include: `1`.";


EventYield::invalidfinalstate= "The argument `1` is not a valid final state particle. Allowed values are e[1|2|3] and \[Nu]. Asummation over \[Nu] flavors is always implicite."


Options[EventYield]= {
	OutputFormat      -> FF,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	SM                -> True
	(*,
	Luminosity        -> Default*)
};


EventYield::binning2d= "Binning in both \!\(\*SubscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\)]\) and \!\(\*SubscriptBox[\(p\), \(T\)]\) detected. Currently 2d binning is not supported.";


(*EventYield[{\[Alpha]_,\[Beta]_}, OptionsPattern[]]:= Module[
	{
		\[Sigma]Binned,
		efficiencies,
		\[Sigma]Observable,
		lumi= OptionValue[Luminosity] (* in fb^-1 *),
		nEvents,
		sBins,
		ptBins,
		\[Epsilon]sm,
		expInfo
	}
	,
	(* Check arguments *)
	If[!MatchQ[\[Alpha], (e[1]|e[2]|e[3]|\[Nu])],
		Message[EventYield::invalidfinalstate, \[Alpha]];
		Abort[]
	];
	If[!MatchQ[\[Beta], (e[1]|e[2]|e[3]|\[Nu])],
		Message[EventYield::invalidfinalstate, \[Beta]];
		Abort[]
	];
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {OutputFormat, Coefficients, EFTorder, OperatorDimension, Luminosity};
	
	(* get experimental info -> postponed to below *)
	(*expInfo= ExperimentInfo[{\[Alpha],\[Beta]}];*)
	
	(* binning info for all observables *) (* ? MOVE THIS SWITCH TO Experiments.m ? *)
	Switch[{\[Alpha],\[Beta]},
		{e[3],e[3]},
			sBins= {150, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000, 1150, 1500}; (* check the last bin *)
			ptBins= {0, \[Infinity]};
			expInfo= ExperimentInfo["tata"];
			lumi = expInfo["LUMINOSITY"],
		_,
			Abort[]
	];
	
	(* modify luminosity is required *)
	If[OptionValue[Luminosity]=!=Default,
		lumi= OptionValue[Luminosity]
	];
	
	(* print info *)
	Print@TableForm[{
	{"PROCESS",           ":", ("pp \[Rule] " <> (\[Alpha]/.PrintRuleLepton) <> (\[Beta]/.PrintRuleAntiLepton) <> " | " <> expInfo["PROCESS"])},
	{"EXPERIMENT",        ":", expInfo["EXPERIMENT"]},
	{"ARXIV",             ":", expInfo["ARXIV"]},
	{"OBSERVABLE",        ":", expInfo["OBSERVABLE"]},
	{"BINNING " <> expInfo["OBSERVABLE"] <> " [GeV]", ":", TraditionalForm[expInfo["BINNING"]]},
	{"LUMINOSITY [\!\(\*SuperscriptBox[\(fb\), \(-1\)]\)]", ":", lumi},
	(* for internal computation *)
	{"BINNING \!\(\*SqrtBox[OverscriptBox[\(s\), \(^\)]]\) [GeV]",  ":", TraditionalForm[sBins]},
	{"BINNING \!\(\*SubscriptBox[\(p\), \(T\)]\) [GeV]",  ":", TraditionalForm[ptBins]}
	}];
	
	(* compute binned cross section *)
	\[Sigma]Binned= EchoTiming[
		BinnedCrossSection[
			{\[Alpha],\[Beta]}, ptBins, sBins,
			(* due to the way efficiencies are included this has to be done afterwards. *)
			(*
			OutputFormat -> OptionValue[OutputFormat],
			Coefficients -> OptionValue[Coefficients],
			*)
			OutputFormat      -> FF,
			Coefficients      -> All,
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension]
		],
		"\[Sigma] computation: "
	];
	
	(* check that binning is 1d *)
	If[!MatchQ[Dimensions[\[Sigma]Binned], {OrderlessPatternSequence[_,1]}], 
		Message[EventYield::binning2d];
		Abort[]
	];
	
	(* flatten \[Sigma] *)
	\[Sigma]Binned= Flatten[\[Sigma]Binned];
	
	(* remove SM contribution if required *)
	\[Epsilon]sm/:Conjugate[\[Epsilon]sm]:= \[Epsilon]sm;
	If[!OptionValue[SM],
		\[Sigma]Binned= \[Sigma]Binned/.(a:FF[_,{_,SM},___]:>\[Epsilon]sm*a);
		\[Sigma]Binned= MyExpand/@\[Sigma]Binned;
		\[Sigma]Binned= \[Sigma]Binned/.\[Epsilon]sm^2->0;
		\[Sigma]Binned= \[Sigma]Binned/.\[Epsilon]sm->1;
	];
	
	(* include efficiencies *)
	Do[
		\[Sigma]Binned[[i]]= IncludeEfficiencies[\[Sigma]Binned[[i]], i]
		,
		{i, Length[\[Sigma]Binned]}
	];
	
	(* load efficiencies *)
	efficiencies= LoadEfficiencies[{\[Alpha],\[Beta]}];
	
	(* expand all bins *)
	\[Sigma]Binned= MyExpand/@\[Sigma]Binned;
	
	(* inactivate all the weird Plus, Times, Power, ... behaviour of List *)
	\[Sigma]Binned= Hold/@ \[Sigma]Binned;
	
	(* transform from mll or pt bins to bins in the observables *)
	\[Sigma]Observable= EchoTiming[\[Sigma]Binned/.efficiencies, "\[Epsilon] substitution: "];
	(*efficiencies= EchoTiming[Dispatch[efficiencies], "Dispatch: "];*) (* no improvement *)
	
	(* check if there are efficiencies remaining and set them to zero *)
	If[!FreeQ[\[Sigma]Observable,_Efficiency],
		Message[
			EventYield::missingeff,
			DeleteDuplicates@ Cases[\[Sigma]Observable, eff_Efficiency:> Drop[eff,-1], All]
		];
		(* set remaining efficiencies to zero *)
		\[Sigma]Observable= \[Sigma]Observable/. Efficiency[___] -> Table[0, Length[efficiencies[[1,2]]]]
	];
	
	(* activate Plus, Times, Power, ... behaviour of List again *)
	\[Sigma]Observable= ReleaseHold[\[Sigma]Observable];
	
	(* sum contribution to each bin *)
	\[Sigma]Observable= Plus@@ \[Sigma]Observable;
	
	(* Substitute in WC if recuired *)
	If[MatchQ[OptionValue[OutputFormat],{"SMEFT",_}],
		\[Sigma]Observable= MatchToSMEFT[
			#,
			OptionValue[OutputFormat][[2]],
			EFTorder-> OptionValue[EFTorder],
			OperatorDimension-> OptionValue[OperatorDimension]
		]& /@ \[Sigma]Observable
	];
	
	(* Set coefficients to zero if required *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]Observable= SelectTerms[#, OptionValue[Coefficients]]& /@ \[Sigma]Observable
	];
	
	(* multiply by luminosity [lumi]=fb^-1 *)
	nEvents= (1000 * lumi) * \[Sigma]Observable;
	
	Return[nEvents/.{Complex[0.,0.]-> 0, 0.-> 0}]
]*)


EventYield::undefinedsearch= "The LHC search `1` is not defined; defined searches are `2`.";


EventYield[proc_String, OptionsPattern[]]:= Module[
	{
		\[Sigma]Binned,
		efficiencies,
		\[Sigma]Observable,
		lumi(*= OptionValue[Luminosity] (* in fb^-1 *)*),
		nEvents,
		sBins,
		ptBins,
		\[Epsilon]sm,
		expInfo,
		searchData,
		finalstate,
		printState
	}
	,
	(* Check that proc corresponds to a specified search *)
	If[KeyExistsQ[LHCSearch[],proc],
		Print["Computing observable for ", proc, " search: ", LHCSearch[][proc]]
		,
		Message[EventYield::undefinedsearch, proc, LHCSearch[]];
		Abort[]
	];
	
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {OutputFormat, Coefficients, EFTorder, OperatorDimension(*, Luminosity*)};
	
	(* Load all experimental data for this search *)	
	searchData= LHCSearch[proc];
	expInfo= searchData["Info"];
	finalstate= ToExpression[expInfo["FINALSTATE"]];
	sBins= expInfo["BINS"]["MLL"];
	ptBins= expInfo["BINS"]["PT"];
	lumi= expInfo["LUMINOSITY"];
	
	(* modify luminosity is required *)
	(*If[OptionValue[Luminosity]=!=Default,
		lumi= OptionValue[Luminosity]
	];*)
	
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
	{"PROCESS",           ":", printState (*("pp \[Rule] " <> (First[finalstate]/.PrintRuleLepton) <> (Last[finalstate]/.PrintRuleAntiLepton))*)},
	{"EXPERIMENT",        ":", expInfo["EXPERIMENT"]},
	{"ARXIV",             ":", expInfo["ARXIV"]},
	{"SOURCE",            ":", expInfo["SOURCE"]},
	{"OBSERVABLE",        ":", expInfo["OBSERVABLE"]},
	{"BINNING " <> expInfo["OBSERVABLE"] <> " [GeV]", ":", TraditionalForm[expInfo["BINS"]["OBSERVABLE"]]},
	{"LUMINOSITY [\!\(\*SuperscriptBox[\(fb\), \(-1\)]\)]", ":", lumi},
	(* for internal computation *)
	{"BINNING \!\(\*SqrtBox[OverscriptBox[\(s\), \(^\)]]\) [GeV]", ":", TraditionalForm[sBins]},
	{"BINNING \!\(\*SubscriptBox[\(p\), \(T\)]\) [GeV]", ":", TraditionalForm[ptBins]}
	}];
	
	(* compute binned cross section *)
	\[Sigma]Binned= (*EchoTiming[*)
		BinnedCrossSection[
			finalstate, ptBins, sBins,
			(* due to the way efficiencies are included this has to be done afterwards. *)
			(*
			OutputFormat -> OptionValue[OutputFormat],
			Coefficients -> OptionValue[Coefficients],
			*)
			OutputFormat      -> FF,
			Coefficients      -> All,
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension],
			Efficiency        -> True
		(*],
		"\[Sigma] computation: "*)
	];

	(* This is no longer possible since BinnedCrossSection always returns 1d Lists now *)
	(*	
	(* check that binning is 1d *)
	If[!MatchQ[Dimensions[\[Sigma]Binned], {OrderlessPatternSequence[_,1]}], 
		Message[EventYield::binning2d];
		Abort[]
	];
	(* flatten \[Sigma] *)
	\[Sigma]Binned= Flatten[\[Sigma]Binned];
	*)
	
	(* check that binning is 1d *)
	If[!MatchQ[Length@Dimensions[\[Sigma]Binned], 1], 
		Message[EventYield::binning2d];
		Abort[]
	];
	
	(* This has to be done before rotating to the weak basis *)
	(* include efficiencies *)
	Do[
		\[Sigma]Binned[[i]]= \[Sigma]Binned[[i]] /. Efficiency[a___]:>Efficiency[a,i]
		,
		{i, Length[\[Sigma]Binned]}
	];
	
	(* remove SM contribution if required *)
	\[Epsilon]sm/:Conjugate[\[Epsilon]sm]:= \[Epsilon]sm;
	If[!OptionValue[SM],
		\[Sigma]Binned= \[Sigma]Binned/.(a:FF[_,{_,SM},___]:>\[Epsilon]sm*a);
		\[Sigma]Binned= MyExpand/@\[Sigma]Binned;
		\[Sigma]Binned= \[Sigma]Binned/.\[Epsilon]sm^2->0;
		\[Sigma]Binned= \[Sigma]Binned/.\[Epsilon]sm->1;
	];
	
	(* load efficiencies *)
	efficiencies= LoadEfficiencies[proc];
	
	(* expand all bins *)
	\[Sigma]Binned= MyExpand/@\[Sigma]Binned;
	
	(* inactivate all the weird Plus, Times, Power, ... behaviour of List *)
	\[Sigma]Binned= Hold/@ \[Sigma]Binned;
	
	(* convolution with efficiency kernels *)
	If[$ParallelHighPT,
		(* parallel substitute *)
		(*EchoTiming[*)With[{eff= Dispatch[efficiencies]},
			\[Sigma]Observable= ParallelMap[(#/.eff)&, \[Sigma]Binned];
		](*, "\[Epsilon] substitution: "]*);
		,
		(* standard substitution *)
		\[Sigma]Observable= (*EchoTiming[*)\[Sigma]Binned/.efficiencies(*, "\[Epsilon] substitution: "]*);
	];
	
	(* check if there are efficiencies remaining and set them to zero *)
	If[!FreeQ[\[Sigma]Observable,_Efficiency],
		Message[
			EventYield::missingeff,
			DeleteDuplicates@ Cases[\[Sigma]Observable, eff_Efficiency:> Drop[eff,-1], All]
		];
		(*Print[DeleteDuplicates@ Cases[\[Sigma]Observable, eff_Efficiency:> Drop[eff,-1], All]];*)
		(* set remaining efficiencies to zero *)
		\[Sigma]Observable= \[Sigma]Observable/. Efficiency[___] -> Table[0, Length[efficiencies[[1,2]]]]
	];
	
	(* activate Plus, Times, Power, ... behaviour of List again *)
	\[Sigma]Observable= ReleaseHold[\[Sigma]Observable];
	
	(* sum contribution to each bin *)
	\[Sigma]Observable= Plus@@ \[Sigma]Observable;
	
	(* Substitute in WC if recuired *)
	If[MatchQ[OptionValue[OutputFormat],{"SMEFT",_}],
		\[Sigma]Observable= MatchToSMEFT[
			#,
			OptionValue[OutputFormat][[2]],
			EFTorder-> OptionValue[EFTorder],
			OperatorDimension-> OptionValue[OperatorDimension]
		]& /@ \[Sigma]Observable
	];
	
	(* Set coefficients to zero if required *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]Observable= SelectTerms[#, OptionValue[Coefficients]]& /@ \[Sigma]Observable
	];
	
	(* multiply by luminosity [lumi]=fb^-1 *)
	nEvents= (1000 * lumi) * \[Sigma]Observable;
	
	Return[nEvents/.{Complex[0.,0.]-> 0, 0.-> 0}]
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
