(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChiSquareHiggs`*)


(* ::Subtitle:: *)
(*Compute the \[Chi]^2 for Higgs observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ChiSquareHiggs"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


Options[ChiSquareHiggs]={
	Observables -> All,
	EFTscale :> GetEFTscale[],
	Coefficients -> All
};


ChiSquareHiggs::invalidwc="The expression `1` is not a valid argument for the option WC (Wilson Coefficients).";
ChiSquareHiggs::nomodel="Model run mode is not implemented yet. Please switch to SMEFT"
ChiSquareHiggs::undefinedmode="Run mode undefined. Something went very wrong..."
ChiSquareHiggs::emptyobs="No observables selected"
ChiSquareHiggs::invalidobs="The observables `1` are not implemented or don't exist"
ChiSquareHiggs::invalidinput="Invalid input for Observables"


ChiSquareHiggs[OptionsPattern[]] := Module[
	{
	chi2,
	wilson,
	observables,
	covmatrix,
	covmatrixsymm,
	invcovmatrix,
	obsvector
	}
	,
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
					Message[ChiSquareHiggs::invalidwc,OptionValue[Coefficients]];Abort[]	
			],
		"Model",
			Message[ChiSquareHiggs::nomodel];Abort[],
		_,
			Message[ChiSquareHiggs::undefinedmode];Abort[]
	];
	Switch[OptionValue[Observables],
		All,
			observables=HiggsObservables[],
		{},
			Message[ChiSquareHiggs::emptyobs];Abort[],
		__,
			If[ListQ[OptionValue[Observables]],
				If[SubsetQ[Flatten[HiggsObservables[]],OptionValue[Observables]],
					observables=OptionValue[Observables],
					Message[ChiSquareHiggs::invalidobs,Complement[OptionValue[Observables],Flatten[HiggsObservables[]]]];Abort[]
				],
				Message[ChiSquareHiggs::invalidinput];Abort[]
			]
	];
	covmatrix=Table[
		ExpCov[i,j]+THCov[i,j],
		{i,observables},{j,observables}];
	covmatrixsymm=covmatrix+Transpose[covmatrix]-DiagonalMatrix[Diagonal[covmatrix]];
	invcovmatrix=Inverse[covmatrixsymm];
	obsvector=Table[
		ExpValue[i]["Value"]-(SMPrediction[i]["Value"](1 + NPContribution[i])),
		{i,observables}];
	chi2=SMEFTRun[(obsvector . invcovmatrix . obsvector)/.null->0,DsixTools`EWSCALE,OptionValue[EFTscale]]/.wilson/.GetParameters[];
	Return[Expand[chi2/.a_WC->a/OptionValue[EFTscale]^2]]
];
