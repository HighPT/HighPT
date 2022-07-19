(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Options`*)


(* ::Subtitle:: *)
(*Provides definition of all Options.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["Coefficients"]


PackageExport["EFTorder"]


PackageExport["EFTscale"]


PackageExport["OperatorDimension"]


PackageExport["Luminosity"]


PackageExport["PTcuts"]


PackageExport["MLLcuts"]


PackageExport["Mediators"]


PackageExport["CombineBins"]


PackageExport["RescaleError"]


PackageExport["SM"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["OptionCheck"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Usage*)


PTcuts::usage= "PTcuts \[Rule] {\!\(\*SubsuperscriptBox[\(p\), \(T\), \(min\)]\),\!\(\*SubsuperscriptBox[\(p\), \(T\), \(max\)]\)} is an option that specifies the \!\(\*SubscriptBox[\(p\), \(T\)]\) cuts (in units of GeV) that should be used for the computation of cross sections. The OptionValues must satisfy 0 \[LessEqual] \!\(\*SubsuperscriptBox[\(p\), \(T\), \(min\)]\) < \!\(\*SubsuperscriptBox[\(p\), \(T\), \(max\)]\) \[LessEqual] \[Infinity]. Can be used with: CrossSection, DifferentialCrossSection";


MLLcuts::usage= "MLLcuts \[Rule] {\!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(min\)]\),\!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(max\)]\)} is an option that specifies the \!\(\*SubscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\)]\) cuts (in units of GeV) that should be used for the computation of cross sections. The OptionValues must satisfy 16 \[LessEqual] \!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(min\)]\) < \!\(\*SubsuperscriptBox[\(m\), \(\[ScriptL]\[ScriptL]\), \(max\)]\) \[LessEqual] 13000. Can be used with: CrossSection.";


EFTorder::usage= "EFTorder \[Rule] \[ScriptN] is an option specifying that the result of the computation should be truncated at order (\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(NP\)]\)\!\(\*SuperscriptBox[\()\), \(-\[ScriptN]\)]\). The deault value is determined by the last call of InitializeModel. This option can be used with: InitializeModel, DifferentialCrossSection, CrossSection, EventYield, ChiSquareLHC, SubstituteFF, and is omitted in the mediator mode.";


EFTscale::usage= "EFTscale \[Rule] \[CapitalLambda] is an option specifying the EFT cutoff scale \[CapitalLambda]. The deault value is determined by the last call of InitializeModel. This option can be used with: InitializeModel, DifferentialCrossSection, CrossSection, EventYield, ChiSquareLHC, SubstituteFF, and is omitted in the mediator mode.";


OperatorDimension::usage= "OperatorDimension \[Rule] \[ScriptD] is an option specifying that EFT operators up to mass dimension \[ScriptD] should be considered. The deault value is determied by the last call InitializeModel. This option can be used with: InitializeModel, DifferentialCrossSection, CrossSection, EventYield, ChiSquareLHC, SubstituteFF.";


Coefficients::usage= "Coefficients \[Rule] {\[ScriptC]\[ScriptO]\[ScriptE]\[ScriptF]1, \[ScriptC]\[ScriptO]\[ScriptE]\[ScriptF]2, ...} is an option specifying which form factors (FF), Wilson coefficients (WC) or coupling constants (Couling), that must be given as \[ScriptC]\[ScriptO]\[ScriptE]\[ScriptF]n, should be considered. All coefficients not matching any of the \[ScriptC]\[ScriptO]\[ScriptE]\[ScriptF]n are set to zero. The default value is Coefficients\[Rule]All, coefficients should be kept. This option can be used with: DifferentialCrossSection, CrossSection, EventYield, ChiSquareLHC, SubstituteFF.";


Luminosity::usage= "Luminosity \[Rule] \[ScriptL]\[ScriptU]\[ScriptM] is an option, that can be used for projections, that determines the integrated Luminosity (in \!\(\*SuperscriptBox[\(fb\), \(-1\)]\)) used for computing EventYield and ChiSquareLHC. The default value is \[ScriptL]\[ScriptU]\[ScriptM]=Default corresponding to the actual experimental luminosity of the specified search. This option can be used with: EventYield, ChiSquareLHC.";


RescaleError::usage= "RescaleError \[Rule] \[ScriptB]\[ScriptO]\[ScriptO]\[ScriptL] is an option that determines whether the background uncertainty should be rescaled when changing the default luminosity. The default value is \[ScriptB]\[ScriptO]\[ScriptO]\[ScriptL]=True, for which the error is scaled as \[CapitalDelta]BKG \[Rule] \!\(\*SqrtBox[\(\*SubscriptBox[\(L\), \(projection\)]/\*SubscriptBox[\(L\), \(search\)]\)]\) \[CapitalDelta]BKG. If \[ScriptB]\[ScriptO]\[ScriptO]\[ScriptL]=False, the relative error for the background, i.e. \[CapitalDelta]BKG/BKG, is kept constant. This option can be used with: ChiSquareLHC.";


CombineBins::usage= "CombineBins is an Option of ChiSquareLHC that allows to combine several bins of the exerpimental search. The bins used in a given \"\[ScriptS]\[ScriptE]\[ScriptA]\[ScriptR]\[ScriptC]\[ScriptH]\" and the corresponding event count per bin can be displayed using LHCSearch[\"\[ScriptS]\[ScriptE]\[ScriptA]\[ScriptR]\[ScriptC]\[ScriptH]\"]. For example to combine the 2nd and 3rd bin as well as the bins 7 to 9 one should specify CombineBins->{{2,3},{7,8,9}}. By default bins are merged such that at least 10 events are in all resulting bins."


SM::usage= "SM denotes a Standard Model form factor.
SM \[Rule] True | False is an Option for EventYield specifying whether the pure Standard Model contribution should be included in the event count.";


Mediators::usage = "Mediators \[Rule] {\"\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\)\"\[Rule]{\!\(\*SubscriptBox[\(M\), SubscriptBox[\(\[Phi]\), \(1\)]]\),\!\(\*SubscriptBox[\(\[CapitalGamma]\), SubscriptBox[\(\[Phi]\), \(1\)]]\)}, ...} is an option of InitializeModel and specifies which BSM mediators \!\(\*SubscriptBox[\(\[Phi]\), \(n\)]\), with masses \!\(\*SubscriptBox[\(M\), SubscriptBox[\(\[Phi]\), \(n\)]]\) and widths \!\(\*SubscriptBox[\(\[CapitalGamma]\), SubscriptBox[\(\[Phi]\), \(n\)]]\), should be included. Similarily, Mediators can also be used with DefineParameters to modify the mass and width of previously defined mediators. This option is ignored in the SMEFT run mode.";


(* ::Section:: *)
(*Check allowed OptionValues*)


(* ::Text:: *)
(*Function to check if OptionValues are well defined*)


OptionCheck[opt_, optVal_]:= If[!MatchQ[optVal, $OptionValueAssociation[opt]],
	Message[OptionCheck::optionvalue, opt, optVal, $OptionValueAssociation[opt]];
	Abort[]
];


(* ::Text:: *)
(*List of well defined values for the Options*)


$OptionValueAssociation= <|
	FF                -> True | False,
	SM                -> True | False,
	Efficiency        -> True | False,
	Coefficients      -> All | {} | {(_FF|_WC|_Coupling)..} (*| {_WC..} | {_Coupling..}*) (*| {Rule[_WC,_]..} | {Rule[_FF,_]..}*),
	EFTorder          -> 0 | 2 | 4 (*| 8*),
	OperatorDimension -> 4 | 6 | 8,
	CombineBins       -> Default | {{_?IntegerQ..}..} | {} | None ,
	Luminosity        -> Default | _?((NumericQ[#]&&Positive[#])&),
	RescaleError      -> True | False,
	PTcuts            -> ({min_?NumericQ, max_?NumericQ}/;(0<=min<max)) | ({min_?NumericQ,\[Infinity]}/;0<=min),
	MLLcuts           -> {min_?NumericQ, max_?NumericQ}/;(16<=min<max<=13000),
	EFTscale          -> _?((NumericQ[#]&&Positive[#])&) | _Symbol,
	Mediators         -> {} | <||> | {Rule[_String,{_?NumericQ,_?NumericQ}]..} | <|Rule[_String,{_?NumericQ,_?NumericQ}]..|>,
	"\[Alpha]EM"             -> _?NumericQ | Default,
	"GF"              -> _?NumericQ | Default,
	"mZ"              -> _?NumericQ | Default,
	"\[CapitalGamma]Z"              -> _?NumericQ | Default,
	"\[CapitalGamma]W"              -> _?NumericQ | Default,
	"Wolfenstein"     -> {_?NumericQ | Default, _?NumericQ | Default, _?NumericQ | Default, _?NumericQ | Default}
|>;


OptionCheck::optionvalue= "Invalid OptionValue specified: `1`->`2`, the allowed values for `1` must match `3`.";
