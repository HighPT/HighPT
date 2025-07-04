(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Experiments`*)


(* ::Subtitle:: *)
(*Provides the experimental information for the different LHC searches.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*External*)


PackageExport["LHCSearch"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["$SearchDirectories"]


PackageScope["$DefaultCombinedBins"]


(* ::Chapter:: *)
(*Private:*)


$NEventsObserved::usage= "$NEventsObserved[\"xxx\"] gives the number of events observed by the experiment for the \"xxx\" final state."


$NEventsPredicted::usage= "$NEventsPredicted[\"xxx\"] gives SM expectation value for the observed number of events for the \"xxx\" final state as reported by the experiment."


$NEventsUncertainty::usage= "$NEventsUncertainty[\"xxx\"] ";


(* ::Section::Closed:: *)
(*Loading the experimental results*)


LoadExperimentalResults::usage= "LoadExperimentalResults[\"proc\"] loads all experiemntal measurements for the search \"proc\".";


LoadExperimentalResults[str_String]:= Get@FileNameJoin[{Global`$DirectoryHighPT, "LHC_searches", str, str<>".dat"}];


LoadExperimentalResultsCSV[str_String]:= Import[
	FileNameJoin[{Global`$DirectoryHighPT, "LHC_searches", str, str<>".csv"}],
	"CSV", 
	(* 
	The Option MissingValuePattern is required for Mathematica v14.2 and later since the default behavior for importing CSV files changed in this version. 
	Of course, backwards compatibility of this Option is entirely unclear and undocumented... (at least it works on v14.0)
	*)
	MissingValuePattern -> None
]/.{
	""       -> Nothing,
	s_String :> StringReplace[s,{
		"\\[Mu]"->"\[Mu]",
		"\\[Tau]"->"\[Tau]",
		"\\[Nu]"->"\[Nu]",
		"\\("->"\(",
		"\\)"->"\)",
		"\\!"->"\!",
		"\\*"->"\*"
	}
	]
};


(* ::Section::Closed:: *)
(*LHCSearch*)


LHCSearch::usage= "LHCSearch[\"proc\"] loads and returns all available data for the LHC search specified by the string \"proc\" as an association.
LHCSearch[] lists all LHC searches that are currently available in HighPT`."


LHCSearch::undefinedsearch= "The LHC search `1` is not defined. The defined searches are `2`.";


LHCSearch[str_String]:= Module[
	{
		proc=str,
		temp,
		csv,
		info,
		data,
		bkg,
		\[CapitalDelta]bkg
	},
	(* Check that proc corresponds to a specified search *)
	If[!KeyExistsQ[LHCSearch[], str],
		Message[LHCSearch::undefinedsearch, str, LHCSearch[]];
		Abort[]
	];
	
	(* use alias if available *)
	If[KeyExistsQ[$Searches, str], proc= $SearchDirectories[str]];
	
	(* Load experimental results *)
	csv = LoadExperimentalResultsCSV[proc];
	
	(* extract info *)
	info = <|
		"ARXIV"       -> Hyperlink[ToString[csv[[1,2]]],csv[[1,3]]],
		"SOURCE"      -> Hyperlink[ToString[csv[[2,2]]],csv[[2,3]]],
		"DESCRIPTION" -> csv[[3,2]],
		"EXPERIMENT"  -> csv[[4,2]],
		"PROCESS"     -> csv[[5,2]],
		"FINALSTATE"  -> csv[[6,2]],		
		"OBSERVABLE"  -> csv[[7,2]],
		"LUMINOSITY"  -> csv[[8,2]],
		"CMENERGY"    -> csv[[9,2]],
		"BINS"        -> <|
			"OBSERVABLE" -> PrependTo[csv[[15;;,2]],csv[[15,1]]],
			"MLL"        -> csv[[11,2;;]],
			"PT"         -> csv[[12,2;;]]/."Infinity"->\[Infinity]
		|>
	|>;
	
	(* extract data, bkg and error *)
	data = csv[[15;;,3]];
	bkg = csv[[15;;,5]];
	\[CapitalDelta]bkg = csv[[15;;,6]];
	
	(* set experimental data *)
	temp = <|
		"INFO"           -> info,
		"DATA"           -> data,
		"BACKGROUND"     -> bkg,
		"ERROR-BKG"      -> \[CapitalDelta]bkg,
		"DEFAULT-BINNING" -> $DefaultCombinedBins[str]
	|>;
	
	Return[temp]
]


LHCSearch[]:= $Searches


(* ::Subsubsection::Closed:: *)
(*Internal module for comparisons [no used]*)


(*PackageScope["CompareData"]*)


CompareData[str_String]:= Module[{proc=str, temp, aux},
	(* use alias if available *)
	If[KeyExistsQ[$Searches, str], proc= $SearchDirectories[str]];
	(* Load experimental results *)
	LoadExperimentalResults[proc];
	(* set experimental data *)
	(*LHCSearch[str]*)temp= <|
		"Info"                  -> $Info[proc],
		"Observed"              -> $NEventsObserved[proc],
		"Expected"              -> $NEventsPredicted[proc],
		"Error"                 -> $NEventsUncertainty[proc],
		"DefaultBinCombination" -> $DefaultCombinedBins[str]
	|>;
	
	aux=LoadExperimentalResultsCSV[proc];
	Print["DATA: ", Chop[(aux[[15;;,3]]-$NEventsObserved[proc])/($NEventsObserved[proc]+1),10^-2]];
	Print["BKG:  ", Chop[(aux[[15;;,5]]-$NEventsPredicted[proc])/($NEventsPredicted[proc]+1),10^-2]];
	Print["\[CapitalDelta]BKG: ", Chop[(aux[[15;;,7]]-$NEventsUncertainty[proc])/($NEventsUncertainty[proc]+1),10^-2]];
	
	Return[temp]
]


(* ::Section:: *)
(*Searches*)


(* ::Subsection::Closed:: *)
(*List of all available searches*)


$Searches= <|
	"di-tau-ATLAS"        -> Hyperlink["arXiv:2002.12223", "https://arxiv.org/abs/2002.12223"],
	"di-muon-CMS"         -> Hyperlink["arXiv:2103.02708", "https://arxiv.org/abs/2103.02708"],
	"di-electron-CMS"     -> Hyperlink["arXiv:2103.02708", "https://arxiv.org/abs/2103.02708"],
	
	"mono-tau-ATLAS"      -> Hyperlink["ATLAS-CONF-2021-025", "https://cds.cern.ch/record/2773301/"],
	"mono-muon-ATLAS"     -> Hyperlink["arXiv:1906.05609", "https://arxiv.org/abs/1906.05609"],
	"mono-electron-ATLAS" -> Hyperlink["arXiv:1906.05609", "https://arxiv.org/abs/1906.05609"],
	
	"muon-tau-CMS"        -> Hyperlink["arXiv:2205.06709", "https://arxiv.org/abs/2205.06709"],
	"electron-tau-CMS"    -> Hyperlink["arXiv:2205.06709", "https://arxiv.org/abs/2205.06709"],
	"electron-muon-CMS"   -> Hyperlink["arXiv:2205.06709", "https://arxiv.org/abs/2205.06709"]
|>


(* ::Subsection::Closed:: *)
(*Internal labeling of the searches*)


$SearchDirectories= <|
	"di-tau-ATLAS"        -> "di-tau_ATLAS_2002_12223",
	"di-muon-CMS"         -> "di-muon_CMS_2103_02708",
	"di-electron-CMS"     -> "di-electron_CMS_2103_02708",
	
	"mono-tau-ATLAS"      -> "mono-tau_ATLAS-CONF-2021-025",
	"mono-muon-ATLAS"     -> "mono-muon_ATLAS_1906_05609",
	"mono-electron-ATLAS" -> "mono-electron_ATLAS_1906_05609",
	
	"muon-tau-CMS"        -> "muon-tau_CMS_2205_06709",
	"electron-tau-CMS"    -> "electron-tau_CMS_2205_06709",
	"electron-muon-CMS"   -> "electron-muon_CMS_2205_06709"
|>


(* ::Subsection::Closed:: *)
(*Default combined bins*)


(*
If the default binning is changed, the reading of efficiency matrices must probably also be changed!
*)
$DefaultCombinedBins= <|
	"di-tau-ATLAS"        -> {},
	"di-muon-CMS"         -> {Range[29,30],Range[31,41]},
	"di-electron-CMS"     -> {Range[43,44], Range[45,46], Range[47,48], Range[49,51], Range[52,94]},
	
	"mono-tau-ATLAS"      -> {Range[10,14]}(*{Range[12,16]}*),
	"mono-muon-ATLAS"     -> {Range[19,20], Range[21,38]}(*{Range[27,28], Range[29,46]}*),
	"mono-electron-ATLAS" -> {Range[23,24],Range[25,48]}(*{Range[33,34],Range[35,58]}*),
	
	"muon-tau-CMS"        -> {Range[16,17], Range[18,25]},
	"electron-tau-CMS"    -> {Range[19,25]},
	"electron-muon-CMS"   -> {Range[19,25]}
|>
