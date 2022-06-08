(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`ChiSquare`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ChiSquareLHC"]


PackageExport["MinimizeChiSquare"]


PackageScope["ConfidenceRegion"]


PackageScope["PlotConfidenceRegion"]


PackageScope["PlotConfidenceIntervals"]


PackageExport["CombineBins"]


PackageExport["RescaleError"]


(* ::Text:: *)
(*rm*)


(*PackageExport["PlotTwoParameterFit"]*)


(*PackageExport["PlotOneParameterFit"]*)


(* ::Chapter:: *)
(*Private:*)


CombineBins::usage= "CombineBins
	is an Option of ChiSquareLHC[...] that allows to combine several bins of the exerpimental search. 
	The bins used in a given search \"xxx\" and the corresponding event count per bin can be displayed using LHCSearch[\"xxx\"].
	Example: for combining the 2nd and 3rd bin as well as the bins 7 to 9 one should specify CombineBins->{{2,3},{7,8,9}}.
	By default no bins are merged."


(* ::Section:: *)
(*\[Chi]^2 construction*)


ChiSquareLHC::usage="ChiSquareLHC[\"proc\"]
	Computes the \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) for the process specified by the string \"proc\".
	The \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) is computed for each bin of the experimental search individually.
	Returned is a list of the \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) for all the experimental bins.
	To combine all bins into a single \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) one can use:  Plus@@ChiSquareLHC[\"proc\"]
	The bins and all other relevant information is printed on the screen when running ChiSquareLHC.
	The options and their default values are: 
		OutputFormat \[Rule] FF,
		Coefficients \[Rule] All,
		EFTorder \[RuleDelayed] GetEFTorder[],
		OperatorDimension \[RuleDelayed] GetOperatorDimension[],
		CombineBins -> Default.";


Options[ChiSquareLHC]= {
	FF                -> False,
	Coefficients      -> All,
	EFTorder          :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	CombineBins       -> Default,
	Scale             :> GetScale[],
	Luminosity        -> Default,
	RescaleError      -> True
};


ChiSquareLHC[proc_String, OptionsPattern[]]:= Module[
	{
		\[CapitalDelta]Events, \[Delta]tot, chi2, expData, NObserved, NPredicted, \[Sigma]N, searchLumi, rescale=1,
		\[Sigma]Predicted, mybins, poissonError
	}
	,	
	(* compute event yield for all bins subtracting SM prediction*)
	\[Sigma]Predicted= EventYield[
		proc,
		FF                -> OptionValue[FF],
		Coefficients      -> OptionValue[Coefficients],
		EFTorder          -> OptionValue[EFTorder],
		OperatorDimension -> OptionValue[OperatorDimension],
		SM                -> False,
		Scale             -> OptionValue[Scale],
		Luminosity        -> OptionValue[Luminosity]
	];
	
	(* prepare experimental data *)
	expData    = LHCSearch[proc];
	
	(* resacling of luminosity for projections *)
	searchLumi = expData["INFO"]["LUMINOSITY"];
	If[!MatchQ[OptionValue[Luminosity],Default],
		rescale = OptionValue[Luminosity]/searchLumi;
	];
	
	NObserved  = rescale * expData["DATA"];
	NPredicted = rescale * expData["BACKGROUND"];
	
	If[!MatchQ[OptionValue[Luminosity],Default],
		If[MatchQ[OptionValue[RescaleError],True],
			\[Sigma]N = Sqrt[rescale] * expData["ERROR-BKG"]
			,
			\[Sigma]N = NPredicted * expData["ERROR-BKG"]/expData["BACKGROUND"];
		],
		\[Sigma]N = expData["ERROR-BKG"];
	];
	
	(* merging bins if requested *)
	Switch[OptionValue[CombineBins],
		Default, mybins = $DefaultCombinedBins[proc],
		None,    mybins = {},
		_,       mybins = OptionValue[CombineBins]
	];
	If[mybins=!={},
		{\[Sigma]Predicted,NObserved,NPredicted}= MergeBins[{\[Sigma]Predicted,NObserved,NPredicted}, mybins];
		{\[Sigma]N}= MergeBinsSquared[{\[Sigma]N}, mybins];
		ConditionalPrint["# Events per bin after merging: ",NObserved];
	];
	
	(* add Poisson error for data *)
	poissonError = NObserved /. (0 -> 1);
	\[Sigma]N = Sqrt[\[Sigma]N^2 + poissonError]; (* if a bin contains 0 events the Poisson error is 1*)
	
	(* # events differences *)
	\[CapitalDelta]Events= NObserved - NPredicted;
	
	(* chi^2 per bin *)
	chi2= ((\[CapitalDelta]Events-\[Sigma]Predicted)/\[Sigma]N)^2;
	
	(* * *)
	Return[chi2]
]


(* combines the desired bins *)
MergeBins[lists_List, merge_List]:= Module[
	{
		combinedBins,
		allBins
	}
	,
	(* auxiliary list of all bins *)
	allBins = Table[{i},{i,Length@First[lists]}];
	
	(* sort the bins *)
	combinedBins= SortBy[
		(* create a list of all bins where the ones that should be grouped are combined *)
		Join[
			merge,
			Complement[
				allBins,
				Table[{n},{n,Flatten[merge]}]
			]
		],
		First
	];
	
	ConditionalPrint["Merging bins as: ", combinedBins];
	
	Table[
		(* sum the bins *)
		Table[
			Plus@@list[[bin]]
			,
			{bin, combinedBins}
		]
		,
		{list,lists}
	]
]


(* combines the desired bins by adding their squares and taking the sqrt of the result*)
MergeBinsSquared[lists_List, merge_List]:= Module[
	{
		combinedBins,
		allBins
	}
	,
	(* auxiliary list of all bins *)
	allBins = Table[{i},{i,Length@First[lists]}];
	
	(* sort the bins *)
	combinedBins= SortBy[
		(* create a list of all bins where the ones that should be grouped are combined *)
		Join[
			merge,
			Complement[
				allBins,
				Table[{n},{n,Flatten[merge]}]
			]
		],
		First
	];
	
	(*Print["Merging bins as: ", combinedBins];*)
	
	Table[
		(* sum the bins *)
		Table[
			Sqrt[Plus@@Power[list[[bin]],2]]
			,
			{bin, combinedBins}
		]
		,
		{list,lists}
	]
]


(* ::Section::Closed:: *)
(*\[Chi]^2 minimization*)


MinimizeChiSquare::usage= "MinimizeChiSquare[\[Chi]Sq, params, relations]
	Minimizes the given \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) denoted \[Chi]Sq with respect to the real parameters specified in the list params.
	The parameters can be Wilson coefficients, form factos, or any other symbol.
	Relations of the Wilson coefficents or form factors to the given parameters can be specified as rules in the list relations.
	The minimization of the \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) is performed using Mathematica's NMinimize, thus all Options of NMinimize can also be passed to MinimizeChiSquare.
	MinimizeChiSquare returns the following list:
	{\!\(\*SubsuperscriptBox[\(\[Chi]\), \(min\), \(2\)]\), best fit values of params, \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) containing only the relevant params}";


(* make MinimizeChiSquare take same options and default values as NMinimize *)
Options[MinimizeChiSquare]= Options[NMinimize];


MinimizeChiSquare[\[Chi]_, parameters_List, relations:({(_WC->_)..} | {(_FF->_)..} | {}), OptionsPattern[]]:= Module[
	{c, wcRule={}, cRule={}, cList={}, min, wcList, \[Chi]temp=\[Chi]}
	,
	(* make auxiliary variable real *)
	c/:Conjugate@c[i_]:= c[i];
	
	If[relations=!={},
		\[Chi]temp= \[Chi]temp/.relations
	];
	
	(* set all irrelevant terms to zero *)
	\[Chi]temp= SelectTerms[\[Chi]temp, parameters];
	
	Do[
		If[FreeQ[\[Chi]temp,param],
			Print["Warning: The given \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) does not depend on the specified parameter: ", param];
		];
		,
		{param, parameters}
	];
	
	(* map variables *)
	Do[
		With[{$c= c[Unique[]]},
			AppendTo[cList, $c];
			AppendTo[wcRule, param->$c];
			AppendTo[cRule, $c->param]
		]
		,
		{param, parameters}
	];
	
	(* minimize chi2 *)
	min= NMinimize[
		\[Chi]temp/.wcRule,
		cList
		,
		(* apply all options *)
		Sequence@@Table[
			opt -> OptionValue[opt]
			,
			{opt, Options[NMinimize][[;;,1]]}
		]
	]/.cRule;
	
	(* return: {\[Chi]^2_min, {best fit values}, \[Chi]^2} *)
	Return@Join[min, {\[Chi]temp}]
]


(* ::Section::Closed:: *)
(*ConfidenceRegion*)


ConfidenceRegion::usage= "ConfidenceRegion[\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\), params, relations, CL]
	Minimizes the given \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) with respect to the real parameters specified in the list params.
	The parameters can be Wilson coefficients, form factos, or any other symbol.
	Relations of the Wilson coefficents or form factors to the given parameters can be specified as rules in the list relations.
	The right hand side of these rules should only depend on parameters listed in params.
	The minimization of the \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) is performed using Mathematica's NMinimize, thus all Options of NMinimize can also be passed to ConfidenceRegion.
	ConfidenceRegion then returns an ImpliciteRegion corresponding to the confidence region for the specified params with confidence level CL.";


(* make MinimizeChiSquare take same options and default values as NMinimize *)
Options[ConfidenceRegion]= Options[NMinimize];


ConfidenceRegion[\[Chi]_, parameters_List, relations:({(_WC->_)..} | {(_FF->_)..} | {}), cl_, OptionsPattern[]]:= Module[
	{minimized, region}
	,
	(* minimize the \[Chi]^2 *)
	minimized= MinimizeChiSquare[
		\[Chi],
		parameters,
		relations,
		(* apply all options *)
		Sequence@@Table[
			opt -> OptionValue[opt]
			,
			{opt, Options[NMinimize][[;;,1]]}
		]
	];
	
	(* find the confidence region *)
	With[{param= parameters, \[Chi]min= First[minimized], \[Chi]mod= Last[minimized], \[CapitalDelta]\[Chi]= \[CapitalDelta]\[Chi]2[Length[parameters],cl]},
		region= ImplicitRegion[\[Chi]mod <= \[Chi]min + \[CapitalDelta]\[Chi], param]
	];
	
	Return[region]
]


(* ::Section::Closed:: *)
(*PlotConfidenceRegion*)


PlotConfidenceRegion::usage= "PlotConfidenceRegion[{reg1,reg2,...}, {\!\(\*SubscriptBox[\(x\), \(min\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\)}, {\!\(\*SubscriptBox[\(y\), \(min\)]\),\!\(\*SubscriptBox[\(y\), \(max\)]\)}]
	Creates a two dimensional plot of all regions regn in the parameter range x\[Element]{\!\(\*SubscriptBox[\(x\), \(min\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\)} and y\[Element]{\!\(\*SubscriptBox[\(y\), \(min\)]\),\!\(\*SubscriptBox[\(y\), \(max\)]\)}.
	The regions regn must be of type ImplicitRegion and two dimensional as returned by ConfidenceRegion.
	PlotConfidenceRegion uses RegionPlot and takes all arguments of the latter.";


(* combine Options of Plot and RegionPlot*)
Options[PlotConfidenceRegion]= Options[RegionPlot]


(* https://mathematica.stackexchange.com/questions/206173/increasing-the-length-of-frame-ticks *)
tickFunc=Charting`ScaledTicks[{Identity,Identity},TicksLength->{.018,.01}][##]&;


SetOptions[PlotConfidenceRegion, #]&/@{
	Background      -> White,
	GridLines       -> Automatic,
	PlotPoints      -> 50,
	ImageSize       -> {1000,1000},
	GridLines       -> {{0},{0}},
	GridLinesStyle  -> Directive[Black, Dashing[0.012,2.05]],
	FrameTicksStyle -> Directive[Black, 28, FontFamily->"Times"],
	FrameTicks      -> {{tickFunc,Automatic},{tickFunc,Automatic}}
	(*,PlotStyle->ColorData[1]*)
 };


PlotConfidenceRegion[regions:{ImplicitRegion[_,{_,_}]..}, {xMin_,xMax_}, {yMin_,yMax_}, OptionsPattern[]]:= Module[
	{plt, $RegionPlot, x, y, plotOpt= Options[RegionPlot]}
	,
	(* prepare the plot function *)
	plt= $RegionPlot[
		Table[
			{x,y}\[Element]reg
			,
			{reg,regions}
		]
		,
		(* x- and y-range *)
		{x, xMin, xMax}, {y, yMin, yMax}
		,
		(* apply all options *)
		Sequence@@Table[
			opt -> OptionValue[opt]
			,
			{opt, Options[RegionPlot][[;;,1]]}
		]
	];
	
	Return[plt/.$RegionPlot->RegionPlot]
]


(* ::Section::Closed:: *)
(*PlotConfidenceIntervals*)


PlotConfidenceIntervals::usage= "PlotConfidenceIntervals[{int1,int2,...}]
	Combines the given confidence intervals int1, int2, ... in one plot.
	The confidence intervals intn must be of type ImpliciteRegion and one dimensional, as for example obtainde by ConfidenceRegion.
	PlotConfidenceIntervals uses Plot and takes all arguments of the latter.";


(* inherit Plot options *)
Options[PlotConfidenceIntervals]= Options[Plot]


(* set custom default OptionValues *)
SetOptions[PlotConfidenceIntervals, #]&/@{
	PlotStyle->Directive[Thickness[0.008], RGBColor[0.,0.24,0.6]],
	PlotRange->All,
	Axes->{False,True},
	AxesStyle->{None, Directive[Thickness[0.0025],Black]},
	Frame->True,
	FrameTicks->{Automatic, None},
	Ticks->{Automatic, None},
	AxesOrigin->{0,0},
	GridLines->{Automatic, None},
	Background->White,
	PlotLabels->Automatic,
	PerformanceGoal->"Quality",
	PlotPoints->200
};


(* returns returnValue if x\[Element]regrion and Nothing otherwise *)
InRegrion[x_, region_, returnValue_]:= Module[
	{var= region[[-1,-1]], reg}
	,
	reg= region/.var->x;
	If[{x} \[Element] reg, returnValue, Nothing]
]


PlotConfidenceIntervals[regions:{ImplicitRegion[_,{_}]..}, OptionsPattern[]]:= Module[
	{plt, $Plot, x, len= Length[regions], labels= OptionValue[PlotLabels], boundaries, xMin, xMax}
	,
	(* define default lables *)
	If[labels===Automatic,
		labels= Placed[TraditionalForm[Style[#,Large]], Before]&/@(regions[[;;,-1,1]]);
	];
	
	(* Write out the confidence intervals *)
	boundaries= {#[[-1,1]], RegionBounds@Region[#]}&/@ regions;
	Print[TraditionalForm[First[#]], " : ", Last[#]]&/@ boundaries;
	
	boundaries= Flatten[boundaries[[;;,-1]]];
	xMin= Min[boundaries]-0.2;
	xMax= Max[boundaries]+0.2;
	
	(* create plot *)
	plt= $Plot[
		Table[
			InRegrion[x, regions[[n]], len+1-n]
			,
			{n, len}
		]
		,
		{x,xMin,xMax}
		
		,
		(* apply all options *)
		Sequence@@Table[
			If[opt===PlotLabels,
				opt -> labels,
				opt -> OptionValue[opt]
			]
			,
			{opt, Options[Plot][[;;,1]]}
		]
	];
	
	Return[plt/.$Plot->Plot]
]


(* ::Section::Closed:: *)
(*Delta \[Chi]^2 value for a given # parameters and a given CL*)


(*
m: number of fitted parameters
p: probability to contain true value
*)
\[CapitalDelta]\[Chi]2[m_Integer,p_?NumberQ]:= InverseCDF[ChiSquareDistribution[m],p];


(* ::Section::Closed:: *)
(*Plotting [ ? remove ? ]*)


(* ::Subsection::Closed:: *)
(*Plot confidence regions for 2 parameter fit*)


(*PlotTwoParameterFit::usage= "PlotTwoParameterFit[Minimized\[Chi], {\!\(\*SubscriptBox[\(x\), \(min\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\)}, {\!\(\*SubscriptBox[\(y\), \(min\)]\),\!\(\*SubscriptBox[\(y\), \(max\)]\)}, CL]
	Plots the confidence region with a conficence level CL for two parameters.
	The two parameter fit should be obtained by MinimizeChiSquare. 
	Its return value should be used as Minimized\[Chi]={\!\(\*SubsuperscriptBox[\(\[Chi]\), \(min\), \(2\)]\), best fit values of params, \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) containing only the relevant params}.
	The second and third argument specify which regions in x- and y-direction are printed.
	PlotTwoParameterFit uses RegionPlot and all the Options of the latter can be passed to the former as well.
";*)


(*Options[PlotTwoParameterFit]= Options[RegionPlot];*)


(*PlotTwoParameterFit[{chi2min_, centralValue:{_,_}, chi2_}, {xMin_,xMax_}, {yMin_,yMax_}, cl_, OptionsPattern[]]:= Module[
	{}
	,
	With[{cv1=centralValue[[1,1]],cv2=centralValue[[2,1]]},
		RegionPlot[
			chi2 <= chi2min + \[CapitalDelta]\[Chi]2[2,cl]
			,
			{cv1,xMin,xMax},{cv2,yMin,yMax}
			,
			(* apply all options *)
			Evaluate[
				Sequence@@Table[
					opt -> OptionValue[opt]
					,
					{opt, Options[RegionPlot][[;;,1]]}
				]
			]
		]
	]
]*)


(* ::Subsection::Closed:: *)
(*Plot confidence intervals for 1 parameter at a time fit*)


(*PlotOneParameterFit::usage= "PlotOneParameterFit[\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\), params, CL]
	Performs a one parameters at a time fit for the given \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) and all the Wilson coefficients or form factors specified in the list params.
	Returned are the confidence intervals with a confidence level CL.";*)


(*PlotOneParameterFit[\[Chi]_, parameters:({_WC..} | {_FF..}), cl_]:= Module[
	{mins, \[CapitalDelta]ChiSq= \[CapitalDelta]\[Chi]2[1,cl], list={}}
	,
	(* minimize chi^2 for each parameter individually *)
	mins= MinimizeChiSquare[\[Chi],{#},{}]&/@parameters;
	Print["Confidence intervals: (", cl*100, "% CL)"];
	Do[
		AppendTo[list, FindCL[parameters[[i]], mins[[i]], \[CapitalDelta]ChiSq, i]]
		,
		{i, Length[parameters]}
	];
	Show@(Sequence@@Reverse[list[[;;,-1]]])
]*)


(*FindCL[param_, chimin_, \[CapitalDelta]ChiSq_, counter_]:= Module[
	{minmax, best= chimin[[2]], plt, zz}
	,
	
	(* find region boundaries *)
	minmax= RegionBounds@ Region@ ImplicitRegion[chimin[[-1]]<=chimin[[1]] + \[CapitalDelta]ChiSq, {param}];
	Print[TraditionalForm[param], " : ", minmax];
	
	plt= Plot[counter,{zz,-5,+5},
		RegionFunction->Function[{x}, (chimin[[-1]]/.param->x) <= chimin[[1]] + \[CapitalDelta]ChiSq],
		PlotStyle->Thickness[0.01],
		PlotRange->All,
		Background->White,
		PlotLabels->Placed[TraditionalForm[param], Before],
		Ticks->{Automatic, None},
		AxesOrigin->{0,0},
		GridLines->{Automatic, None}
	];
	Return[{best, minmax, plt}]
]*)
