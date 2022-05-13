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


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


ExpCov[i_,j_] := Module[
	{
		cov
	}
	,
	If[i==j,
		cov=ExpValue[i][[2]]^2,
		If[NumberQ[ExpCorr[i,j]],
			cov=ExpValue[i][[2]]*ExpValue[j][[2]]*ExpCorr[i,j],
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
	If[(NumberQ[SMPrediction[i][[1]]] && NumberQ[SMPrediction[j][[1]]]),
		If[i==j,
			cov=SMPrediction[i][[2]]^2,
			If[NumberQ[THCorr[i,j]],
				cov=SMPrediction[i][[2]]*SMPrediction[j][[2]]*THCorr[i,j],
				cov=0
			];
		];,
		cov=0
	];
	Return@cov
]


Options[ChiSquareFlavor]={
	Observables -> All,
	Scale :> GetScale[],
	Coefficients -> All
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
		obsvector
	}
	,
	(* Set irrelevant WCs or Couplings to zero *)
	Switch[$RunMode,
		"SMEFT",
			Switch[OptionValue[Coefficients],
				All,
					wilson = Except[Alternatives@@(WC[#,_]&/@Join[$WCList2,$WCList4]),WC[___]]->0,
				{Rule[_,_]..},
					wilson = Append[OptionValue[WC],WC[___]->0],
				{WC[___]..},
					wilson = Except[Alternatives@@OptionValue[Coefficients],WC[___]]->0,
				{},
					wilson = {WC[___]->0},
				__,
					Message[Chi2Flavor::invalidwc,OptionValue[WC]];Abort[]	
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
				Message[Chi2Flavor::invalidinput];Abort[]
			]
	];
	
	(* Including correlations *)
	(* If there are observables selected, 
	construct the inverse covariance matrix and the vector (Oexp - Oth), 
	running this up to \[Mu]Z. 
	Then compute the chi2 and run to \[CapitalLambda] *)
	If[Length[observables]!=0
		,
		covmatrix=Table[
			ExpCov[i,j]+THCov[i,j](*+NPCov[i,j]*),
			{i,observables},{j,observables}];
		covmatrixsymm=covmatrix+Transpose[covmatrix]-DiagonalMatrix[Diagonal[covmatrix]];
		invcovmatrix=Inverse[covmatrixsymm];
		obsvector=Table[
					If[NumberQ[SMPrediction[i][[1]]],
						(ExpValue[i][[1]]-SMPrediction[i][[1]](1+NPContribution[i]))/.LEFTRun[LEFTSector[i],LowScale[i],"\[Mu]Z"],
						(ExpValue[i][[1]]-NPContribution[i])/.LEFTRun[LEFTSector[i],LowScale[i],"\[Mu]Z"]
					],
				  {i,observables}
				  ];
			chi2=(obsvector . invcovmatrix . obsvector)
				/.SMEFTLEFTMatching["full"]
				/.$NPScale->\[CapitalLambda]
				/.SMEFTRun[mEW,\[CapitalLambda]]
				/.wilson
				/.mEW->Mass[ZBoson]
				/.\[CapitalLambda]->OptionValue[Scale]
				/.ReplaceYukawas
				/.ReplaceGaugeCouplings
				/.GetParameters[]
		,
		chi2=0
	];
	
	Return[Expand[chi2]]
]
