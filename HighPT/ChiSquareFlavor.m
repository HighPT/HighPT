(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChiSquareFlavor`*)


(* ::Subtitle:: *)
(*Computing the likelihood from the low energy observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ChiSquareFlavor"]


PackageExport["Observables"]


PackageExport["RedefineInputs"]


(* ::Subsection:: *)
(*Internal*)


(*PackageScope["ExpCorrelation"]*)


(*PackageScope["THCorrelation"]*)


PackageScope["ExpCov"]
PackageScope["THCov"]


(* ::Chapter:: *)
(*Private:*)


ExpCov[i_,j_] := Module[
	{
		cov
	}
	,
	If[i==j,
		cov=ExpValue[i]["Uncertainty"]^2,
		If[NumberQ[ExpCorrelation[i,j]],
			cov=ExpValue[i]["Uncertainty"]*ExpValue[j]["Uncertainty"]*ExpCorrelation[i,j],
			cov=0
		];
	];
	Return@cov
]


THCov[i_,j_] := Module[
	{
		cov
	}
	,
	If[(NumberQ[SMPrediction[i]["Value"]] && NumberQ[SMPrediction[j]["Value"]]),
		If[i==j,
			cov=SMPrediction[i]["Uncertainty"]^2,
			If[NumberQ[THCorrelation[i,j]],
				cov=SMPrediction[i]["Uncertainty"]*SMPrediction[j]["Uncertainty"]*THCorrelation[i,j],
				cov=0
			];
		];,
		cov=0
	];
	Return@cov
]


Options[ChiSquareFlavor]={
	Observables -> All,
	EFTscale :> GetEFTscale[],
	Coefficients -> All,
	RedefineInputs -> False,
	Basis :> GetBasisAlignment[]
};


ChiSquareFlavor::invalidwc="The expression `1` is not a valid argument for the option WC (Wilson Coefficients).";


ChiSquareFlavor::invalidobs="The observables `1` are not implemented or don't exist"
ChiSquareFlavor::emptyobs="No observables selected"
ChiSquareFlavor::invalidinput="Invalid input for Obs"
ChiSquareFlavor::invalidpoleobs="PoleObs must be either True or False"
ChiSquareFlavor::nomodel="Model run mode is not implemented yet. Please switch to SMEFT"
ChiSquareFlavor::undefinedmode="Run mode undefined. Something went very wrong..."


ChiSquareFlavor[OptionsPattern[]] := Module[
	{
		chi2table,
		chi2,
		wilson,
		observables,
		covmatrix,
		covmatrixsymm,
		invcovmatrix,
		obsvector,
		obsvectorSMEFTEW,
		obsvectorSMEFT\[CapitalLambda]
	}
	,
	(* Set irrelevant WCs or Couplings to zero *)
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
			Message[ChiSquareFlavor::nomodel];Abort[],
		_,
			Message[ChiSquareFlavor::undefinedmode];Abort[]
	];
	(* Read observables and check everything is fine *)
	Switch[OptionValue[Observables],
		All,
			observables = FlavorObservables[]//Flatten,
		{},
			Message[ChiSquareFlavor::emptyobs];Abort[],
		__,
			If[ListQ[OptionValue[Observables]],
				If[SubsetQ[Flatten[FlavorObservables[]],OptionValue[Observables]],
					observables=OptionValue[Observables],
					Message[ChiSquareFlavor::invalidobs,Complement[OptionValue[Observables],Flatten[FlavorObservables[]]]];Abort[]
				],
				Message[ChiSquareFlavor::invalidinput];Abort[]
			]
	];
	obsvector=Table[
				If[NumberQ[SMPrediction[i]["Value"]/.null->0] && (SMPrediction[i]["Value"]/.null->0)!=0,
					LEFTRun[(ExpValue[i]["Value"]-SMPrediction[i]["Value"](1+NPContribution[i])),LowScale[i],DsixTools`EWSCALE],
					LEFTRun[(ExpValue[i]["Value"]-NPContribution[i]),LowScale[i],DsixTools`EWSCALE]
				],
				{i,observables}
				];
	obsvectorSMEFTEW=Table[If[LowScale[observables[[i]]]>DsixTools`EWSCALE,
							If[NumberQ[SMPrediction[i]["Value"]],
								MatchToSMEFT[obsvector[[i]]-Boole[OptionValue[RedefineInputs]]SMPrediction[i]["Value"]InputRedefinition[observables[i]],MatchingScale->LowScale[observables[[i]]],Basis->OptionValue[Basis]],
								MatchToSMEFT[obsvector[[i]],MatchingScale->LowScale[observables[[i]]],Basis->OptionValue[Basis]]
							  ],
							If[NumberQ[SMPrediction[i]["Value"]],
								MatchToSMEFT[obsvector[[i]]-Boole[OptionValue[RedefineInputs]]SMPrediction[i]["Value"]InputRedefinition[observables[i]],Basis->OptionValue[Basis]],
								MatchToSMEFT[obsvector[[i]],Basis->OptionValue[Basis]]
							  ]
							],
						{i,observables//Length}];
	obsvectorSMEFT\[CapitalLambda]=Table[If[LowScale[observables[[i]]]>DsixTools`EWSCALE,
							SMEFTRun[obsvectorSMEFTEW[[i]],LowScale[observables[[i]]],OptionValue[EFTscale]],
							SMEFTRun[obsvectorSMEFTEW[[i]],DsixTools`EWSCALE,OptionValue[EFTscale]]
							],
						{i,observables//Length}];
	(*covmatrix=Table[ExpCov[i,j]+THCov[i,j](*+NPCov[i,j]*),{i,observables},{j,observables}];*)
	covmatrix = (Table[Obs[i]["Exp"]["Uncertainty"]*ExpCorrelation[i,j]*Obs[j]["Exp"]["Uncertainty"],{i,observables},{j,observables}]+Table[Obs[i]["SM"]["Uncertainty"]*THCorrelation[i,j]*Obs[j]["SM"]["Uncertainty"],{i,observables},{j,observables}])/.null->0;
	(*covmatrixsymm=covmatrix+Transpose[covmatrix]-DiagonalMatrix[Diagonal[covmatrix]];*)
	covmatrixsymm = covmatrix;
	invcovmatrix=Inverse[covmatrixsymm];
	chi2= (obsvectorSMEFT\[CapitalLambda] . invcovmatrix . obsvectorSMEFT\[CapitalLambda])/.null->0/.wilson/.GetParameters[];
	Return[Expand[chi2/.a_WC->a/OptionValue[EFTscale]^2]]
]
