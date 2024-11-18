(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChiSquareLEP2`*)


(* ::Subtitle:: *)
(*Compute the LEP2 likelihood from observables off the pole*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ChiSquareLEP2"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*ChiSquareLEP2*)


Options[ChiSquareLEP2]={
	Observables -> All,
	EFTscale :> GetEFTscale[],
	Coefficients -> All,
	DimensionlessCoefficients -> True
};


ChiSquareLEP2::invalidwc="The expression `1` is not a valid argument for the option WC (Wilson Coefficients).";
ChiSquareLEP2::nomodel="Model run mode is not implemented yet. Please switch to SMEFT"
ChiSquareLEP2::undefinedmode="Run mode undefined. Something went very wrong..."
ChiSquareLEP2::emptyobs="No observables selected"
ChiSquareLEP2::invalidobs="The observables `1` are not implemented or don't exist"
ChiSquareLEP2::invalidinput="Invalid input for Observables"


ChiSquareLEP2[OptionsPattern[]] := Module[
	{
	chi2,
	wilson,
	observables,
	obsvector,
	invcovmatrix
	}
	,
	OptionCheck[DimensionlessCoefficients,OptionValue[DimensionlessCoefficients]];
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
					Message[ChiSquareEW::invalidwc,OptionValue[Coefficients]];Abort[]	
			],
		"Model",
			Message[ChiSquareLEP2::nomodel];Abort[],
		_,
			Message[ChiSquareLEP2::undefinedmode];Abort[]
	];
	Switch[OptionValue[Observables],
		All,
			observables=LEP2Observables[],
		{},
			Message[ChiSquareLEP2::emptyobs];Abort[],
		__,
			If[ListQ[OptionValue[Observables]],
				If[SubsetQ[Flatten[LEP2Observables[]],OptionValue[Observables]],
					observables=OptionValue[Observables],
					Message[ChiSquareLEP2::invalidobs,Complement[OptionValue[Observables],Flatten[LEP2Observables[]]]];Abort[]
				],
				Message[ChiSquareLEP2::invalidinput];Abort[]
			]
	];
	
	chi2 = (Table[
		Table[
			((ExpValues[i][s]/.Around[a_,b_]->a)-(SMPredictions[i][s] + SMEFTRun[NPContributions[i][s],s,OptionValue[EFTscale]]))^2/(ExpValues[i][s]/.Around[a_,b_]->b)^2
			,
			{s,LowScales[i]}
			]
		,
		{i,observables}
	]/.GetParameters[]/.wilson)//Flatten//Total;
	
	(*obsvector=Table[
		Table[
	        ExpValues[i][s]["Value"]-(SMPredictions[i][s] + SMEFTRun[NPContributions[i][s],s,OptionValue[EFTscale]]),
	        {s,LowScales[i]}],
	    {i,observables}]//Flatten;
	invcovmatrix=DiagonalMatrix[Table[Table[1/ExpValues[i][s]["Uncertainty"]^2,{s,LowScales[i]}],{i,observables}]//Flatten];
	chi2 = obsvector.invcovmatrix.obsvector;*)

	If[MatchQ[OptionValue[DimensionlessCoefficients],True],
		Return[Expand[chi2/.a_WC->a/OptionValue[EFTscale]^2]],
		Return[Expand[chi2]]
	];
]
