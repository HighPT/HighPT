(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChiSquareEW`*)


(* ::Subtitle:: *)
(*Compute the EW likelihood from pole observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ChiSquareEW"]


PackageExport["\[Delta]g"]
PackageExport["Running"]
PackageExport["DimensionlessCoefficients"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Stuff from 2103.12074*)


PoleCoeff = {
	\[Delta]gWeL[1,1],
	\[Delta]gWeL[2,2],
	\[Delta]gWeL[3,3],
	\[Delta]gZeL[1,1],
	\[Delta]gZeL[2,2],
	\[Delta]gZeL[3,3],
	\[Delta]gZeR[1,1],
	\[Delta]gZeR[2,2],
	\[Delta]gZeR[3,3],
	\[Delta]gZuL[1,1],
	\[Delta]gZuR[1,1],
	\[Delta]gZdL[1,1],
	\[Delta]gZdR[1,1],
	\[Delta]gZdL[2,2],
	\[Delta]gZdR[2,2],
	\[Delta]gZuL[2,2],
	\[Delta]gZuR[2,2],
	\[Delta]gZdL[3,3],
	\[Delta]gZdR[3,3],
	\[Delta]mW
};


PoleBestFit = {
	{-1.2, 3.2},
	{-2.7, 2.6},
	{1.5, 4.0},
	{-0.20, 0.28},
	{0.1, 1.2},
	{-0.09, 0.59},
	{-0.43, 0.27},
	{0.0, 1.4},
	{0.62, 0.62},
	{-12, 23},
	{-4, 31},
	{-19, 36},
	{-30, 130},
	{11, 28},
	{32, 48},
	{-1.5, 3.6},
	{-3.3, 5.3},
	{3.1, 1.7},
	{21.9, 8.8},
	{0.29, 0.16}
}*10^-3;


PoleCorrMatrixInput = {
	{1,0.2,-0.59,-0.22,-0.09,0.01,0.16,-0.13,-0.08,-0.04,-0.06,-0.03,-0.06,-0.02,-0.04,-0.01,0.01,0.04,0.01,0.},
	{0,1,-0.39,-0.27,-0.11,0.01,0.2,-0.16,-0.1,-0.04,-0.06,-0.03,-0.07,-0.03,-0.04,-0.01,0.01,0.05,0.01,0.},
	{0,0,1,-0.18,-0.07,0.01,0.13,-0.11,-0.07,0.,0.,0.,0.,0.,0.,-0.01,0.,0.04,0.01,0.},
	{0,0,0,1,-0.09,-0.07,0.16,-0.04,0.04,0.04,0.06,0.03,0.06,0.03,0.04,0.07,0.08,-0.36,-0.35,0.},
	{0,0,0,0,1,0.06,-0.04,0.91,-0.04,0.,0.01,0.01,0.01,0.01,0.01,-0.02,-0.01,0.06,0.04,0.},
	{0,0,0,0,0,1,0.02,-0.03,0.41,-0.01,-0.01,-0.01,-0.02,0.,-0.01,-0.02,0.01,0.07,0.01,0.},
	{0,0,0,0,0,0,1,-0.07,-0.04,-0.02,-0.03,-0.02,-0.03,-0.01,-0.02,0.06,0.11,-0.34,-0.38,0.},
	{0,0,0,0,0,0,0,1,0.04,0.02,0.03,0.02,0.03,0.01,0.02,0.,-0.01,0.01,0.03,0.},
	{0,0,0,0,0,0,0,0,1,0.02,0.03,0.01,0.03,0.01,0.02,0.01,-0.01,-0.04,0.,0.},
	{0,0,0,0,0,0,0,0,0,1,0.5,0.68,0.69,0.07,-0.29,-0.05,0.09,-0.02,-0.01,0.},
	{0,0,0,0,0,0,0,0,0,0,1,0.55,0.94,-0.11,-0.39,0.07,0.07,-0.03,-0.02,0.},
	{0,0,0,0,0,0,0,0,0,0,0,1,0.54,-0.64,-0.08,-0.02,0.05,-0.01,0.,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,1,0.07,-0.46,0.05,0.09,-0.03,-0.02,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,1,-0.01,0.1,0.03,-0.02,-0.01,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.04,0.05,-0.02,-0.01,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.32,-0.11,-0.15,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-0.17,-0.14,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}
};
PoleCorrMatrix = PoleCorrMatrixInput + LowerTriangularize[Transpose[PoleCorrMatrixInput],-1];


PoleCovMatrix = Table[PoleCorrMatrix[[i,j]]*PoleBestFit[[i,2]]*PoleBestFit[[j,2]],{i,PoleCoeff//Length},{j,PoleCoeff//Length}];


(*PoleLikelihood = -2*Sum[PoleCoeff[[i]]*(Sum[Inverse[PoleCovMatrix][[i,j]]*PoleBestFit[[j,1]],{j,PoleCoeff//Length}]),{i,PoleCoeff//Length}] + PoleCoeff . Inverse[PoleCovMatrix] . PoleCoeff;*)
PoleLikelihood = (Transpose[PoleBestFit][[1]]-PoleCoeff) . Inverse[PoleCovMatrix] . (Transpose[PoleBestFit][[1]]-PoleCoeff);


(* ::Section:: *)
(*Matching 2103.12074 to our notation*)


\[CapitalDelta]GF = (*Param["vev"]^2*)(*/$NPScale^2*) WC["Hl3",{2,2}]+WC["Hl3",{1,1}]-1/2 WC["ll",{1,2,2,1}](*-1/2 WC["ll",{2,1,1,2}]*);


f[T3_,Q_]:=-Q (Param["g2"]Param["g1"])/(Param["g2"]^2-Param["g1"]^2) WC["HWB",{}]-(1/4 WC["HD",{}]+1/2 \[CapitalDelta]GF)(T3+Q Param["g1"]^2/(Param["g2"]^2-Param["g1"]^2))


SMEFTPoleMatching = {
\[Delta]gZ\[Nu]L[\[Alpha]_,\[Beta]_]:>\[Delta]gWeL[\[Alpha],\[Beta]]-\[Delta]gZeL[\[Alpha],\[Beta]],
\[Delta]gWeL[\[Alpha]_,\[Beta]_]:>(Param["vev"]^2(*/($NPScale^2)*))(WC["Hl3",{\[Alpha],\[Beta]}]+(f[1/2,0]-f[-1/2,-1])KroneckerDelta[\[Alpha],\[Beta]]),
\[Delta]gZeL[\[Alpha]_,\[Beta]_]:>(Param["vev"]^2(*/($NPScale^2)*))(-(1/2)WC["Hl3",{\[Alpha],\[Beta]}]-1/2 WC["Hl1",{\[Alpha],\[Beta]}]+f[-1/2,-1]KroneckerDelta[\[Alpha],\[Beta]]),
\[Delta]gZeR[\[Alpha]_,\[Beta]_]:>(Param["vev"]^2(*/($NPScale^2)*))(-(1/2)WC["He",{\[Alpha],\[Beta]}]+f[0,-1]KroneckerDelta[\[Alpha],\[Beta]]),
\[Delta]gWqL[i_,j_]:>Sum[\[Delta]gZuL[i,k]*Vckm[k,j],{k,3}]-Sum[Vckm[i,k]*\[Delta]gZdL[i,j],{k,3}],
\[Delta]gZuL[i_,j_]:>(Param["vev"]^2(*/($NPScale^2)*))(1/2 MassRotate[WC["Hq3",{i,j}],"uu"]-1/2 MassRotate[WC["Hq1",{i,j}],"uu"]+f[1/2,2/3]KroneckerDelta[i,j]),
\[Delta]gZdL[i_,j_]:>(Param["vev"]^2(*/($NPScale^2)*))(-(1/2)MassRotate[WC["Hq3",{i,j}],"dd"]-1/2 MassRotate[WC["Hq1",{i,j}],"dd"]+f[-1/2,-1/3]KroneckerDelta[i,j]),
\[Delta]gZuR[i_,j_]:>(Param["vev"]^2(*/($NPScale^2)*))(-(1/2)WC["Hu",{i,j}]+f[0,2/3]KroneckerDelta[i,j]),
\[Delta]gZdR[i_,j_]:>(Param["vev"]^2(*/($NPScale^2)*))(-(1/2)WC["Hd",{i,j}]+f[0,-1/3]KroneckerDelta[i,j]),
(*\[Delta]gWqR[i_,j_]:>(Param["vev"]^2(*/($NPScale^2)*))(-(1/2)WC["Hud",{i,j}]),*)
\[Delta]mW->1/2 \[Delta]gWeL[1,1]+1/2 \[Delta]gWeL[2,2]-Param["vev"]^2/4 WC["ll",{1,2,2,1}]
};


(* ::Section:: *)
(*ChiSquareEW*)


Options[ChiSquareEW]={
	Observables -> All,
	EFTscale :> GetEFTscale[],
	Coefficients -> All,
	\[Delta]g -> False,
	Running -> True,
	DimensionlessCoefficients -> True
};


ChiSquareEW::invalidwc="The expression `1` is not a valid argument for the option WC (Wilson Coefficients).";
ChiSquareEW::nomodel="Model run mode is not implemented yet. Please switch to SMEFT"
ChiSquareEW::undefinedmode="Run mode undefined. Something went very wrong..."
ChiSquareEW::emptyobs="No observables selected"
ChiSquareEW::noobs="No individual observables can be selected as of now. The only valid option for Observables is All."
ChiSquareEW::invalidobs="The observables `1` are not implemented or don't exist"
ChiSquareEW::invalidinput="Invalid input for Obs"


ChiSquareEW[OptionsPattern[]] := Module[
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
	OptionCheck[\[Delta]g,OptionValue[\[Delta]g]];
	OptionCheck[Running,OptionValue[Running]];
	OptionCheck[DimensionlessCoefficients,OptionValue[DimensionlessCoefficients]];
	Switch[$RunMode,
		"SMEFT",
			Switch[OptionValue[Coefficients],
				All,
					wilson = Except[Alternatives@@(WC[#,_]&/@GetAllWC),WC[___]]->0,
				{Rule[_,_]..},
					wilson = Append[OptionValue[WC],WC[___]->0],
				{WC[___]..},
					wilson = Except[Alternatives@@OptionValue[Coefficients],WC[___]]->0,
				{},
					wilson = {WC[___]->0},
				__,
					Message[Chi2EW::invalidwc,OptionValue[WC]];Abort[]	
			],
		"Model",
			Message[ChiSquareEW::nomodel];Abort[],
		_,
			Message[ChiSquareEW::undefinedmode];Abort[]
	];
	Switch[OptionValue[Observables],
		All,
			chi2=SMEFTRun[PoleLikelihood/.SMEFTPoleMatching/.SMEFTPoleMatching,91,OptionValue[EFTscale]]/.wilson(*/.mEW->Mass[ZBoson]/.\[CapitalLambda]->OptionValue[Scale]*)(*/.ReplaceYukawas/.ReplaceGaugeCouplings*)/.GetParameters[],
		{},
			Message[ChiSquareEW::emptyobs];Abort[],
		__,
			If[ListQ[OptionValue[Observables]],
				If[SubsetQ[Flatten[EWObservables[]],OptionValue[Observables]],
					observables=OptionValue[Observables],
					Message[ChiSquareEW::invalidobs,Complement[OptionValue[Observables],Flatten[EWObservables[]]]];Abort[]
				],
				Message[ChiSquareEW::invalidinput];Abort[]
			]
	];
	If[!MatchQ[OptionValue[Observables],All],
		(*Print["Computing EW observables individually!"];
		Print["Observables selected: ", observables];*)
		covmatrix=Table[
			ExpCov[i,j]+THCov[i,j],
			{i,observables},{j,observables}];
		covmatrixsymm=covmatrix+Transpose[covmatrix]-DiagonalMatrix[Diagonal[covmatrix]];
		(*Print[covmatrixsymm];*)
		invcovmatrix=Inverse[covmatrixsymm];
		obsvector=Table[
			ExpValue[i][[1]]-(SMPrediction[i][[1]] + NPContribution[i]),
			{i,observables}
		];
		If[MatchQ[OptionValue[\[Delta]g],False],
			If[MatchQ[OptionValue[Running],True],
				chi2=SMEFTRun[(obsvector . invcovmatrix . obsvector)/.Replace\[Delta]g,DsixTools`EWSCALE,OptionValue[EFTscale]]/.wilson/.GetParameters[];,
				chi2=(obsvector . invcovmatrix . obsvector)/.Replace\[Delta]g/.wilson/.GetParameters[];
			],
			chi2=(obsvector . invcovmatrix . obsvector)/.GetParameters[];
		];
	];
	If[MatchQ[OptionValue[DimensionlessCoefficients],True],
		Return[Expand[chi2/.a_WC->a/OptionValue[EFTscale]^2]],
		Return[Expand[chi2]]
	];
]
