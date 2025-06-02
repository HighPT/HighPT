(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`BigChiSquare`*)


(* ::Subtitle:: *)
(*Comprehensive ChiSquare function, may want to rename the file to just ChiSquare*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ChiSquare"]


PackageExport["\[Mu]EW"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["EFTTruncate"]
PackageScope["ExpandComplex"]


(* ::Chapter:: *)
(*Private:*)


\[Mu]EW = DsixTools`EWSCALE;


(* ::Section:: *)
(*ChiSquare function*)


Options[ChiSquare] = {
	FF                -> False,
	Observables -> All,
	EFTscale :> GetEFTscale[],
	Coefficients -> All,
	EFTorder :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[],
	CombineBins       -> Default,
	Luminosity -> Default,
	RescaleError -> True,
	DimensionlessCoefficients -> True
}


ChiSquare::invalidwc="The expression `1` is not a valid argument for the option WC (Wilson Coefficients).";
ChiSquare::nomodel="Model run mode is not implemented for anything else than Drell-Yan. Results will be in a hybrid SMEFT/model notation"
ChiSquare::undefinedmode="Run mode undefined. Something went very wrong..."
ChiSquare::emptyobs="No observables selected"
ChiSquare::invalidinput="Invalid input for Observables. Input must be a list of defined labels (strings)."
ChiSquare::invalidobs="The observables `1` are not implemented or don't exist"


ChiSquare[OptionsPattern[]] := Module[
	{
	wilson,
	observables,
	nondyobs, dyobs,
	obsvector,
	covmatrix, invcovmatrix,
	lhcchi2,
	chi2
	}
	,
	(* ####################### input checks and so on ####################### *)
	
	
	(* Check run mode, yield warning for model case *)
	Switch[$RunMode,
		"SMEFT",
		Switch[OptionValue[Coefficients],
				All,
					wilson = Except[Alternatives@@(WC[#,_]&/@GetAllWC),WC[___]]->0,
				{Rule[_,_]..},
					wilson = Append[OptionValue[Coefficients],WC[___]->0],
				{WC[___]..},
					wilson = Except[Alternatives@@OptionValue[Coefficients],WC[___]]->0,
				{},
					wilson = {WC[___]->0},
				__,
					Message[Chi2Flavor::invalidwc,OptionValue[Coefficients]];Abort[]	
			],
		"Model",
		Message[ChiSquare::nomodel],
		_,
		Message[ChiSquare::undefinedmode];Abort[]
	];
	
	(* Check that standard options make sense *)
	OptionCheck[#,OptionValue[#]]& /@ {Coefficients, EFTorder, OperatorDimension, EFTscale , Luminosity, RescaleError};
	
	(* Check that the input for Observables makes sense *)
	Switch[OptionValue[Observables],
		All,
			observables = ObservableList[]//Flatten,
		{},
			Message[ChiSquare::emptyobs];Abort[],
		__,
			If[ListQ[OptionValue[Observables]],
				If[SubsetQ[Flatten[ObservableList[]],OptionValue[Observables]],
					observables=OptionValue[Observables],
					Message[ChiSquare::invalidobs,Complement[OptionValue[Observables],Flatten[ObservableList[]]]];Abort[]
				],
				Message[ChiSquare::invalidinput];Abort[]
			]
	];
	
	(* ####################### building individual \[Chi]^2 contributions ####################### *)
	
	
	(* separate Drell-Yan from other observables *)
	nondyobs = Complement[observables,Keys[LHCSearch[]]];
	dyobs = Intersection[observables,Keys[LHCSearch[]]];
	
	(* compute individual contribution to the \[Chi]^2 for non Drell-Yan *)
	If[MatchQ[nondyobs,{}],
		chi2 = 0,
		obsvector = ChiSquarePiece[#, EFTscale -> OptionValue[EFTscale]]& /@Obs/@nondyobs
	];
	
	(* ####################### build covariance matrix and invert ####################### *)
	If[!MatchQ[nondyobs,{}],
		covmatrix = (Table[Obs[i]["Exp"]["Uncertainty"]*ExpCorrelation[i,j]*Obs[j]["Exp"]["Uncertainty"],{i,nondyobs},{j,nondyobs}]+Table[Obs[i]["SM"]["Uncertainty"]*THCorrelation[i,j]*Obs[j]["SM"]["Uncertainty"],{i,nondyobs},{j,nondyobs}])/.null->0;
		invcovmatrix = Inverse[covmatrix]
	];
	
	(* ####################### build likelihood ####################### *)
	
	
	(* Compute relevant LHC likelihoods *)
	Switch[dyobs,
		{},
			lhcchi2 = 0,
		{_},
			lhcchi2 = ChiSquareLHC[dyobs[[1]],
				Coefficients -> OptionValue[Coefficients],
				EFTorder -> OptionValue[EFTorder],
				OperatorDimension -> OptionValue[OperatorDimension],
				EFTscale -> OptionValue[EFTscale],
				Luminosity -> OptionValue[Luminosity],
				RescaleError -> OptionValue[RescaleError],
				FF -> OptionValue[FF],
				CombineBins -> OptionValue[CombineBins]
			],
		{__},
			lhcchi2 = Total[Flatten[ChiSquareLHC[#,
				Coefficients -> OptionValue[Coefficients],
				EFTorder -> OptionValue[EFTorder],
				OperatorDimension -> OptionValue[OperatorDimension],
				EFTscale -> OptionValue[EFTscale],
				Luminosity -> OptionValue[Luminosity],
				RescaleError -> OptionValue[RescaleError],
				FF -> OptionValue[FF],
				CombineBins -> Default
			]& /@ dyobs]]
		];
		
		(* compute non-LHC likelihood and add *)
		If[!MatchQ[nondyobs,{}],
			chi2 = obsvector . invcovmatrix . obsvector;
			lhcchi2 = Total[lhcchi2];
		];
		(*chi2 = chi2 + lhcchi2;*)
	
	(* ####################### return/manipulate final expression ####################### *)
	If[MatchQ[OptionValue[DimensionlessCoefficients],True],
		Return[(*Expand[*)((chi2/.a_WC->a/OptionValue[EFTscale]^2)+lhcchi2)/.wilson/.GetParameters[](*]*)],
		Return[(*Expand[*)((lhcchi2/.a_WC->a*OptionValue[EFTscale]^2)+chi2)/.wilson/.GetParameters[](*]*)]
	]
	(*Return@1*)
];


(* ::Subsection:: *)
(*Individual observable contributions*)


Options[ChiSquarePiece] = {
	EFTscale :> GetEFTscale[],
	EFTorder :> GetEFTorder[]
};


ChiSquarePiece::smeftbelowEWscale = "SMEFT coefficients detected below the EW scale"


ChiSquarePiece[obs_Association, OptionsPattern[]] := Module[
	{
	npatmu, npSMEFT, np\[CapitalLambda],
	chi2p
	}
	,
	(*npatmu = obs["NP"];*)
	(*npatmu = Normal[Series[obs["NP"]/.a_WCL->a*eps/.b_WC->b*eps,{eps,0,2}]]/.eps->1;*)
	npatmu = EFTTruncate[obs["NP"],-OptionValue[EFTorder],ExpandComplex->True];
	(* Define npSMEFT as an expression in terms of SMEFT operators at either the EW scale (after LEFT running), or at whatever scale the purely SMEFT observable is defined *)
	If[MatchQ[DeleteDuplicates[Cases[npatmu,_WCL,All]],{}],
		(* only SMEFT coefficients present *)
		If[obs["Scale"] < \[Mu]EW, Message[ChiSquarePiece::smeftbelowEWscale]];
		npSMEFT = npatmu,
		(* at least one LEFT coefficient present, run to EW scale and match to SMEFT *)
		npSMEFT = MatchToSMEFT@LEFTRun[npatmu, obs["Scale"], \[Mu]EW]
	];
	(* Run in SMEFT up to the scale \[CapitalLambda] *)
	np\[CapitalLambda] = SMEFTRun[npSMEFT, Max[obs["Scale"],\[Mu]EW], OptionValue[EFTscale]]/.GetParameters[];
	chi2p = (obs["SM"]["Value"](1+np\[CapitalLambda])-obs["Exp"]["Value"]);
	Return@chi2p
];


(* ::Section:: *)
(*Truncate the (mixed) EFT*)


Options[EFTTruncate] = {ExpandComplex -> False};


EFTTruncate::wrongOption


EFTTruncate[expr_, lambdapower_Integer, OptionsPattern[]] := Module[
	{
	exprwithdimensions, eps,
	expanded
	}
	,
	Switch[OptionValue[ExpandComplex],
		True,
		exprwithdimensions = expr/.WCL[lab_,ind_]:>Power[eps,MassDimension[lab]-4]*(ReWCL[lab,ind]+I*ImWCL[lab,ind])/.WC[lab_,ind_]:>Power[eps,MassDimension[lab]-4]*(ReWC[lab,ind]+I*ImWC[lab,ind])//ComplexExpand,
		False,
		exprwithdimensions = expr/.WCL[lab_,ind_]:>Power[eps,MassDimension[lab]-4]*WCL[lab,ind]/.WC[lab_,ind_]:>Power[eps,MassDimension[lab]-4]*WC[lab,ind](*/.Conjugate[Times[eps,WCL[lab_,ind_]]]:>Times[eps,Conjugate[WCL[lab,ind]]]/.Conjugate[Times[eps,WC[lab_,ind_]]]:>Times[eps,Conjugate[WC[lab,ind]]]*),
		_,
		Print["Wrong value given for option ExpandComplex"];Abort[]
	];
	expanded = Normal[Series[exprwithdimensions,{eps,0,-lambdapower}]]/.eps->1;
	Return[expanded/.ReWCL[lab_,ind_]:>Re[WCL[lab,ind]]/.ReWC[lab_,ind_]:>Re[WC[lab,ind]]/.ImWCL[lab_,ind_]:>Im[WCL[lab,ind]]/.ImWC[lab_,ind_]:>Im[WC[lab,ind]]]
];
