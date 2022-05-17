(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChiSquareEW`*)


(* ::Subtitle:: *)
(*Compute the EW likelihood from pole observables*)
(*For now the likelihood is taken from 2103.12074 - change this at some point*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["ChiSquareEW"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Stuff from 2103.12074*)


PoleCoeff = {
	\[Delta]gWlL[1,1],
	\[Delta]gWlL[2,2],
	\[Delta]gWlL[3,3],
	\[Delta]gZlL[1,1],
	\[Delta]gZlL[2,2],
	\[Delta]gZlL[3,3],
	\[Delta]gZlR[1,1],
	\[Delta]gZlR[2,2],
	\[Delta]gZlR[3,3],
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


\[Delta]GFGF0 = ConstantInput["vev"]^2/$NPScale^2 (WC["Hl3",{2,2}]+WC["Hl3",{1,1}]-1/2 WC["ll",{1,2,2,1}]-1/2 WC["ll",{2,1,1,2}]);


SMEFTPoleMatching = {
\[Delta]gWlL[\[Alpha]_,\[Beta]_]->\[Delta]gZ\[Nu]L[\[Alpha],\[Beta]]-\[Delta]gZlL[\[Alpha],\[Beta]],
\[Delta]gZ\[Nu]L[\[Alpha]_,\[Beta]_]->Re[-(ConstantInput["vev"]^2/(2$NPScale^2))(WC["Hl1",{\[Alpha],\[Beta]}]-WC["Hl3",{\[Alpha],\[Beta]}])],
\[Delta]gZlL[\[Alpha]_,\[Beta]_]->Re[-(ConstantInput["vev"]^2/(2$NPScale^2))(WC["Hl1",{\[Alpha],\[Beta]}]+WC["Hl3",{\[Alpha],\[Beta]}])],
\[Delta]gZlR[\[Alpha]_,\[Beta]_]->Re[-(ConstantInput["vev"]^2/(2$NPScale^2))WC["He",{\[Alpha],\[Beta]}]],
\[Delta]gWqL[i_,j_]->Sum[\[Delta]gZuL[i,k]*V0[k,j],{k,3}]-Sum[V0[i,k]*\[Delta]gZdL[i,j],{k,3}],
\[Delta]gZuL[i_,j_]->Re[-(ConstantInput["vev"]^2/(2$NPScale^2))(WC["Hq1",{i,j}]-WC["Hq3",{i,j}])],
\[Delta]gZdL[i_,j_]->Re[-(ConstantInput["vev"]^2/(2$NPScale^2))(WC["Hq1",{i,j}]+WC["Hq3",{i,j}])],
\[Delta]gZuR[i_,j_]->Re[-(ConstantInput["vev"]^2/(2$NPScale^2))WC["Hu",{i,j}]],
\[Delta]gZdR[i_,j_]->Re[-(ConstantInput["vev"]^2/(2$NPScale^2))WC["Hd",{i,j}]],
\[Delta]gWqR[i_,j_]->-(ConstantInput["vev"]^2/(2$NPScale^2))WC["Hud",{i,j}],
\[Delta]mW->Re[(1/2)\[Delta]GFGF0]
};


(* ::Section:: *)
(*ChiSquareEW*)


Options[ChiSquareEW]={
	Observables -> All,
	Scale :> GetScale[],
	Coefficients -> All
};


ChiSquareEW::invalidwc="The expression `1` is not a valid argument for the option WC (Wilson Coefficients).";
ChiSquareEW::nomodel="Model run mode is not implemented yet. Please switch to SMEFT"
ChiSquareEW::undefinedmode="Run mode undefined. Something went very wrong..."
ChiSquareEW::emptyobs="No observables selected"
ChiSquareEW::noobs="No individual observables can be selected as of now. The only valid option for Observables is All."


ChiSquareEW[OptionsPattern[]] := Module[
	{
	chi2,
	wilson
	}
	,
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
					Message[Chi2EW::invalidwc,OptionValue[WC]];Abort[]	
			],
		"Model",
			Message[ChiSquareEW::nomodel];Abort[],
		_,
			Message[ChiSquareEW::undefinedmode];Abort[]
	];
	Switch[OptionValue[Observables],
		All,
			chi2=PoleLikelihood/.SMEFTPoleMatching/.SMEFTPoleMatching/.$NPScale->\[CapitalLambda]/.SMEFTRun[mEW,\[CapitalLambda]]/.wilson/.mEW->Mass[ZBoson]/.\[CapitalLambda]->OptionValue[Scale]/.ReplaceYukawas/.ReplaceGaugeCouplings/.GetParameters[],
		{},
			Message[ChiSquareEW::emptyobs];Abort[],
		__,
			Message[ChiSquareEW::noobs];Abort[]
	];
	Return[Expand[chi2]]
]
