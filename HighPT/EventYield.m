(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`EventYield`*)


(* ::Subtitle:: *)
(*Computation of event yields*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["EventYield"]


PackageExport["$PrintingProcessInfo"]


(* ::Subsection:: *)
(*Private*)


PackageScope["$\[Sigma]Parallel"]


PackageScope["NIntegrand"]


PackageScope["$IntegralCaching"]


(* ::Chapter:: *)
(*Private:*)


$PrintingProcessInfo::usage= "$PrintingProcessInfo is a boolean value that specifies whether information about the specified process should be printed by EventYield and ChiSquarLHC, or not. The default choice is $PrintingProcessInfo=True."


(* turn on printing of process information *)
$PrintingProcessInfo = True


$\[Sigma]Parallel::usage = "$\[Sigma]Parallel is a variable storing the expression for the cross section of a ganeric bin, that is shared between all subkernels. Reduces the overload of the parallel evaluation."


NIntegrand::usage = "NIntegrand[int,{x,xmin,xmax}] denotes an unevaluated numerical integral. Can be evaluated by the replacement NIntegrand->NIntegrate."


$IntegralCaching::usage = "$IntegralCaching boolean variable, that determines whether the result of numeric integrals should be saved (True) or not (False), where the latter is the dafault."


(* turn off integral caching *)
$IntegralCaching = False


(* ::Section:: *)
(*EventYield*)


EventYield::usage= "EventYield[\"proc\"] computes the expected number of events for the search specified by the argument \"proc\" as a function of form factors, Wilson coefficients or NP coupling constants. Returned is a list where the elements correspond to the number of events in each bin of the experimental observable. The options and their default values are: SM \[Rule] False; FF \[Rule] False; Coefficients \[Rule] All; EFTorder \[RuleDelayed] GetEFTorder[]; OperatorDimension \[RuleDelayed] GetOperatorDimension[]; EFTscale \[RuleDelayed] GetEFTscale[]; Luminosity \[Rule] Default.";


EventYield::missingeff= "Not all required efficincies are known. The missing efficiencies are set to zero. This is possibly caused by using a madiator mass that is not supported, or if you compute the interference of two BSM mediators.";


EventYield::undefinedsearch= "The LHC search `1` is not defined. The defined searches are `2`.";


EventYield::zerocrosssection= "Zero cross section obtained for search `1`. A possible issue might be that non of the form factors, Wilson coefficients, or couplings that are turned on contribute to the given process.";


EventYield::unevaluatedIntegrals = "There are `1` unevaluated s-integrals.";


Options[EventYield]= {
	SM                -> False,
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	EFTscale          :> GetEFTscale[],
	Luminosity        -> Default
};


EventYield[proc_String, OptionsPattern[]]:= Module[
	{
		coeff,
		finalstate, \[Nu]flav, ptBins, sBins, lumi,
		\[Sigma]full=0, \[Sigma]temp, \[Sigma]Binned, pTmin, pTmax, s, binType, MyBins, NObserved,
		$ptMin, $ptMax, $sMin, $sMax
	}
	,
	(*** CHECKS ***)
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, EFTscale , Luminosity, SM};
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
				EFTscale             -> OptionValue[EFTscale]
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
		(*s/:Conjugate[s]:=s;*)
		\[Sigma]temp = CollectIntegrals[\[Sigma]temp,s];
		
		(* add the cross section to the full cross section *)
		\[Sigma]full = \[Sigma]full + \[Sigma]temp;
		,
		{fstate,finalstate}
	];

	(* prepare \[Sigma] for parallel evaluation of bins *)
	(* prepare different types of binning *)
	If[Length[ptBins]!=1,
		binType = "PT";
		\[Sigma]full = \[Sigma]full /. {pTmin -> $ptMin, pTmax -> $ptMax};
		sBins = Table[First[sBins],{n,Length[ptBins]}]; (* make ptBins and sBins of same length *)
		,
		binType = "MLL";
		ptBins = Table[First[ptBins],{n,Length[sBins]}]; (* make ptBins and sBins of same length *)
	];
	
	(* if zero cross section is found return a warning and return zero *)
	If[\[Sigma]full===0,
		Message[EventYield::zerocrosssection, proc];
		Return[Table[0,{i,Length[LHCSearch[proc]["DATA"]]}]]
	];
	
	(* replace integrals *)
	\[Sigma]full = \[Sigma]full /. (Integrand[arg_,s_] :> NIntegrand[arg,{s,$sMin,$sMax}]);
	
	(* create all bins to compute *)
	MyBins = Table[
		{(*\[Sigma]temp,*) i, binType, {{$sMin,$sMax},sBins[[i]]}, {{$ptMin,$ptMax},ptBins[[i]]}, proc}
		,
		{i, Length[sBins]}
	];
	
	(* Launch parallel Kernels *)
	$\[Sigma]Parallel=\[Sigma]full;
	Quiet[LaunchKernels[],LaunchKernels::nodef];
	SetSharedVariable[$\[Sigma]Parallel]; (* it is much faster if \[Sigma] is not copied to each Kernel *)
	
	(* parallel evaluation of bins *)
	\[Sigma]Binned = ParallelMap[
		NEventsBin[#]&
		,
		MyBins
		,
		DistributedContexts->{
			"Global`",
			"HighPT`"
		}
	];
	CloseKernels[];
	
	(* sum contributions of all bins *)
	NObserved = Plus@@\[Sigma]Binned;
	
	(* multiply by luminosity [lumi]=fb^-1 *)
	NObserved= Expand[(1000 * lumi) * NObserved];
	
	Return[NObserved]
]


(* ::Subsection::Closed:: *)
(*Evaluation of a single bin*)


NEventsBin[{binNumber_Integer, binType:("MLL"|"PT"), {{$sMin_,$sMax_},{mllLOW_,mllHIGH_}}, {{$ptMin_,$ptMax_},{ptLOW_,ptHIGH_}}, proc_}] := Module[
	{
		\[Sigma]temp,
		NObserved
	}
	,
	(* substitute correct bin boundaries *)
	Switch[binType,
		"MLL", \[Sigma]temp = $\[Sigma]Parallel /. {$sMin -> mllLOW^2, $sMax -> mllHIGH^2},
		"PT",  \[Sigma]temp = $\[Sigma]Parallel /. {$ptMin -> ptLOW, $ptMax -> ptHIGH, $sMin -> Max[(2*ptLOW)^2,mllLOW^2], $sMax -> mllHIGH^2}
	];
	
	(* adapt efficienies to bin *)
	\[Sigma]temp = \[Sigma]temp /. Efficiency[arg___] :> Efficiency[arg, binNumber];
	
	(* s-integration by identifying complex conjugated integrals *)
	\[Sigma]temp= MyTiming[
		SIntegrate[\[Sigma]temp, {proc, binNumber}]
		,
		"Integrate bin " <> ToString[binNumber]
	];
	
	(* Substitute efficiency kernels *)
	NObserved= MyTiming[
		SubstituteEfficiencyKernels[\[Sigma]temp, {proc, binNumber}]
		,
		"Efficiency subtitution bin " <> ToString[binNumber]
	];
	
	Return[NObserved]
]


(* ::Subsection::Closed:: *)
(*Extracting search details*)


ExtractProcessInfo[proc_]:= Module[
	{searchData, expInfo, finalstate, sBins, ptBins, lumi, printState}
	,
	(* Load all experimental data for this search *)	
	searchData = LHCSearch[proc];
	expInfo    = searchData["INFO"];
	finalstate = ToExpression[expInfo["FINALSTATE"]];
	sBins      = expInfo["BINS"]["MLL"];
	ptBins     = expInfo["BINS"]["PT"];
	lumi       = expInfo["LUMINOSITY"];
	
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
	ConditionalPrint@TableForm[{
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
	(*temp= MyExpand/@temp;*)
	temp= Expand@ExpandConjugate[temp];
	(*Print[temp/.FF[_,{_,Except[SM]},___]->0];*)
	temp= temp/.\[Epsilon]sm^2->0;
	temp= temp/.\[Epsilon]sm->1;
	Return[temp]
]
,
"DropSM"
]


(* ::Subsection::Closed:: *)
(*perform the s-integrate*)


(* ::Text:: *)
(*Perform all s-integrals given in the argument, by identifying complex conjugated integrals*)


SIntegrate[expr_, {proc_,bin_}] := Module[
	{
		\[Sigma]= expr,
		MyMin, MyMax,
		dummyIntegral,
		sIntegralList, sIntegralListMinimal= {},
		integralAssoc, integralAssocReverse,
		temp
	}
	,
	(* Min and Max are OneIdentity which breaks pattern matching below *)
	\[Sigma]= \[Sigma] /. {Min->MyMin, Max->MyMax};
	
	(* substitution rules for all integrals *)
	{integralAssoc,integralAssocReverse} = MinimalIntegralList[\[Sigma], dummyIntegral];
	
	(* replace all propagators and constants in the integals and reintroduce Min/Max *)
	integralAssocReverse= integralAssocReverse/.ReplacePropagators;
	integralAssocReverse= integralAssocReverse/.ReplaceConstants[];
	integralAssocReverse= integralAssocReverse/.{MyMin->Min, MyMax->Max};
	
	(* Cache integrals *)
	If[$IntegralCaching,
		CacheIntegrals[Values@integralAssocReverse, {proc, bin}];
	];
	
	(* substitute cashed integrals if available *)
	integralAssocReverse = integralAssocReverse /. Dispatch@Quiet@Check[
		Import[
			FileNameJoin[{
				Global`$DirectoryHighPT,
				"NumericIntegrals",
				$RunMode,
				proc,
				"integrals_"<>ToString[bin]<>".dat"
			}],
			"WL"
		]
		,
		{}
	];
	
	(* substitute parton luminosity functions by interpolated functions*)
	integralAssocReverse = integralAssocReverse /. PartonLuminosity -> PartonLuminosityFunction;
	
	(* perform remaining integrals if necessary *)
	integralAssocReverse= integralAssocReverse/.NIntegrand->NIntegrate;
	
	(* built association *)
	integralAssocReverse= Association[integralAssocReverse];

	(* substitute in cross section *)
	\[Sigma]= \[Sigma] /. Dispatch@integralAssoc;
	\[Sigma]= \[Sigma] /. Dispatch@integralAssocReverse;

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


(* ::Subsubsection::Closed:: *)
(*Find minimal list of integrals*)


MinimalIntegralList[arg_, dummyIntegral_] := Module[
	{
		intList,
		intListMin = {},
		intAssoc,
		intAssocInverse
	}
	,
	(* find all integrals *)
	intList = DeleteDuplicates@Cases[arg, _NIntegrand, All];
	
	(*
	(* remove unnecessary digits *)
	intList = Chop[intList];
	*)
	
	(* built association with unique symbols *)
	intAssoc= Association[(# -> dummyIntegral[Unique[]])& /@ intList];
	
	(* find complex conjugated integrals *)
	
	Do[
		If[!MemberQ[intListMin,int],
			(* if integral (int) is not yet in the minimal list compute its conjugate *)
			With[
				{
					conjInt=(int /. prop_Propagator :> Conjugate[prop])
				},
				If[MemberQ[intListMin,conjInt],
					(* if Conjugate[int] is already in the list associate to this *)
					AssociateTo[intAssoc, int -> Conjugate[intAssoc[conjInt]]],
					(* if neither int nor Conjugate[int] is already in the list append int *)
					AppendTo[intListMin, int]
				]
			]
		]
		,
		{int, intList}
	];
	(*Echo[Length@intListMin, "min. # of integrals"];*)
	
	(* invert the integral association *)
	intAssocInverse= Table[
		intAssoc[int] -> int
		,
		{int, intList(*intListMin*)}
	];
	
	Return[{intAssoc,intAssocInverse}]
]


(* ::Subsubsection::Closed:: *)
(*caching integrals*)


CacheIntegrals[integralList_, {proc_,bin_}] := Module[
	{
		integrals = integralList,
		integralReplacements = {},
		fileName = FileNameJoin[{
			Global`$DirectoryHighPT,
			"NumericIntegrals",
			$RunMode,
			proc,
			"integrals_"<>ToString[bin]<>".dat"
		}],
		knownIntegrals
	}
	,
	(* load known integrals if available *)
	knownIntegrals = Quiet@Check[Import[fileName,"WL"], {}];
	
	(* remove all known integrals from the list of integrals *)
	integrals = integrals /. Cases[knownIntegrals, (Rule[a_,_]):>(a->Nothing), All];
	
	(* stop if all integrals are known *)
	If[FreeQ[integrals, _NIntegrand, All],
		Return[],
		Print["caching integrals..."]
	];
	
	(* compute all unknown integrals *)
	integralReplacements = Map[
		Module[
			{pattern,temp}
			,
			(* create pattern *)
			pattern = # /. NIntegrand[arg_,{var_,min_,max_}] :> (NIntegrand[arg,{var,min,max}]/.var->Pattern[Global`s,_]);
			(* compute integrals *)
			temp = # /. PartonLuminosity -> PartonLuminosityFunction;
			temp = temp /. NIntegrand -> NIntegrate;
			(* return solution as rule *)
			pattern -> temp
			
		]&
		,
		integrals
	];
	
	(* combine with previous integrals *)
	integralReplacements = Join[knownIntegrals,integralReplacements];
	
	Export[fileName,integralReplacements,"WL"] (* export as Wolfram Language *)
];


(* ::Subsection::Closed:: *)
(*substitute efficiency kernels*)


SubstituteEfficiencyKernels[xSec_, {proc_String, bin_}]:= Module[
	{
		efficiencies,
		\[Sigma]Bin= Hold[xSec], (* inactivate all the Plus, Times, Power, ... behaviour of List *)
		NObserved
	}
	,
	(* load efficiencies *)
	efficiencies = (LoadEfficiencies[proc])[[bin]];
	
	(* substitute efficiencies *)
	NObserved=  \[Sigma]Bin /. efficiencies;
	
	(* check if there are efficiencies remaining and set them to zero *)
	If[!FreeQ[NObserved,_Efficiency],
		(*Print/@DeleteDuplicates@ Cases[NObserved, eff_Efficiency(*:> Drop[eff,-1]*), All];*) (* explicit printing *)
		Message[
			EventYield::missingeff
			(*,DeleteDuplicates@ Cases[NObserved, eff_Efficiency:> Drop[eff,-1], All]*)
		];
		(* set remaining efficiencies to zero *)
		NObserved= NObserved/. Efficiency[___] -> Table[0, Length[efficiencies[[1,2]]]] (* must be set to zero vector *)
	];
	
	NObserved= ReleaseHold[NObserved];
	
	(* Return *)
	Return[NObserved]
]
