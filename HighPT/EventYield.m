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
(*EventYield*)


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
		s,sMin,sMax,ptCuts,
		\[Sigma]Dif,
		temp
	}
	,
	(* Check options *)
	OptionCheck[#,OptionValue[#]]& /@ {FF, Coefficients, EFTorder, OperatorDimension, Scale (*, Luminosity*)};
	
	(* Check that proc corresponds to a specified search *)
	If[KeyExistsQ[LHCSearch[], proc],
		Print["Computing observable for ", proc, " search: ", LHCSearch[][proc]]
		,
		Message[EventYield::undefinedsearch, proc, LHCSearch[]];
		Abort[]
	];
	
	(* figure out which cross sections to compute *)
	
	
	(* compute differential hadronic cross-section *)
	(*\[Sigma]Dif = HadronicDifferentialCrossSection[s, {\[Alpha],\[Beta]}, 
		PTcuts            -> ptCuts,
		Efficiency        -> OptionValue[Efficiency],
		OperatorDimension -> OptionValue[OperatorDimension]
	];*)
	
	(* perform find all s-integrals *)
	
	(* remove SM contribution *)
	
	(* remove FF if required *)
	
	(* substitute remaining FF if required *)
	
	(* remove WC if required *)
	
	(* load efficiencies *)
	
	(* parallelization *)
		(* perform  numerical integration *)
		(* substitute efficiencies *)
	
	(* sum all computed bins to obtain experimental bins *)
	Return[temp]
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
