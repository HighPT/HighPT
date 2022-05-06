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
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	Efficiency        -> False
};


(* if serveral final states are specified sum there cross-sections *)
BinnedCrossSection[a_Alternatives, ptBinList_List, mllBinList_List, OptionsPattern[]]:= Sum[
	PrintTemporary["Computing cross sections for finalstate: ", finalstate];
	BinnedCrossSection[finalstate, ptBinList, mllBinList,
		FF                -> OptionValue[FF],
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
			FF                -> OptionValue[FF],
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
					DistributedContexts -> {"HighPT`"}
					(*,ProgressReporting   -> True*)
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


EventYield::usage= "EventYield[\"proc\"]
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


EventYield::missingeff= "Not all required efficincies have been given. The missing efficiencies are set to zero, these include: `1`.";


EventYield::invalidfinalstate= "The argument `1` is not a valid final state particle. Allowed values are e[1|2|3] and \[Nu]. Asummation over \[Nu] flavors is always implicite."


Options[EventYield]= {
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	SM                -> True,
	Scale             :> GetScale[]
	(*,
	Luminosity        -> Default*)
};


EventYield::binning2d= "Binning in both \!\(\*SubscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\)]\) and \!\(\*SubscriptBox[\(p\), \(T\)]\) detected. Currently 2d binning is not supported.";


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
		printState,
		temp
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
	OptionCheck[#,OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, Scale (*, Luminosity*)};
	
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
	{"EVENTS OBSERVED",   ":", ToString@searchData["Observed"]},
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
			FF                -> True,
			Coefficients      -> All,
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension],
			Efficiency        -> True
		](*,
		"\[Sigma] computation: "
	]*);

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
	\[Sigma]Binned= MyTiming[MyExpand/@\[Sigma]Binned, "Expand in EventYield"];
	
	(* inactivate all the weird Plus, Times, Power, ... behaviour of List *)
	\[Sigma]Binned= Hold/@ \[Sigma]Binned;
	
	(* group bins and efficiencies *)
	temp= Transpose[{\[Sigma]Binned,Dispatch[efficiencies]}];
	
	(* convolution with efficiency kernels *)
	(* with the new code it is more efficient to only use a single kernel for the substitutions *)
	(*
	If[$ParallelHighPT,
		(* parallel substitute *)
		With[{eff= temp},
			\[Sigma]Observable= ParallelMap[MyTiming[First[#]/.Last[#],"Substituting Efficiencies"]&, temp]
			(*\[Sigma]Observable= ParallelMap[MyTiming[(#/.eff),"Substituting Efficiencies"]&, \[Sigma]Binned];*)
		]
		,
		(* standard substitution *)
		\[Sigma]Observable= Map[MyTiming[First[#]/.Last[#],"Substituting Efficiencies"]&, temp]
		(*\[Sigma]Observable= (*EchoTiming[*)\[Sigma]Binned/.efficiencies(*, "\[Epsilon] substitution: "]*);*)
	];
	*)
	MyTiming[
	\[Sigma]Observable= Map[(First[#]/.Last[#])&, temp];
	,
	"Substituting Efficiencies"
	];
	
	(* check if there are efficiencies remaining and set them to zero *)
	If[!FreeQ[\[Sigma]Observable,_Efficiency],
		Print/@DeleteDuplicates@ Cases[\[Sigma]Observable, eff_Efficiency:> Drop[eff,-1], All];
		Message[
			EventYield::missingeff,
			DeleteDuplicates@ Cases[\[Sigma]Observable, eff_Efficiency:> Drop[eff,-1], All]
		];
		(*Print[DeleteDuplicates@ Cases[\[Sigma]Observable, eff_Efficiency:> Drop[eff,-1], All]];*)
		(* set remaining efficiencies to zero *)
		\[Sigma]Observable= \[Sigma]Observable/. Efficiency[___] -> Table[0, Length[efficiencies[[1,2]]]]
	];
	
	(* activate Plus, Times, Power, ... behaviour of List again *)
	MyTiming[
	\[Sigma]Observable= ReleaseHold[\[Sigma]Observable];
	,
	"ReleaseHold for Efficiencies"
	];
	
	(* sum contribution to each bin *)
	MyTiming[
	\[Sigma]Observable= Plus@@ \[Sigma]Observable;
	,
	"Sum Efficiencies"
	];
	
	(* Substitute in WC if required *)
	If[!OptionValue[FF],
		\[Sigma]Observable= SubstituteFF[
			#,
			Scale             -> OptionValue[Scale],
			EFTorder          -> OptionValue[EFTorder],
			OperatorDimension -> OptionValue[OperatorDimension]
		]& /@ \[Sigma]Observable
	];
	
	(* Set coefficients to zero if required *)
	If[!MatchQ[OptionValue[Coefficients], All],
		\[Sigma]Observable= SelectTerms[#, OptionValue[Coefficients]]& /@ \[Sigma]Observable
	];
	
	(* multiply by luminosity [lumi]=fb^-1 *)
	MyTiming[
	nEvents= Expand[(1000 * lumi) * \[Sigma]Observable];
	,
	"Final Expand in EventYield"
	];
	
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
