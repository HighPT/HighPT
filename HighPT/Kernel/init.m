(* ::Package:: *)

$HighPTVersion="1.0.1";


(* ::Title:: *)
(*Initialization [HighPT`]*)


(* Generate warning for Mathematica versions earlier than 12.0.0 *)
If[$VersionNumber < 12.0, 
CellPrint[{
TextCell[
"HighPT` was developed for Mathematica 12.0.0 and later. 
Your current Mathematica version [" <> ToString@$Version <> "] might not be compatible. 
In case you are experiencing problems with HighPT`, please update your Mathematica version.",
"Text",Background->LightRed]
}]
]


(* Loading the package *)
If[MemberQ[$Packages,"HighPT`"],
	(* Avoid double loading the package *)
	Print[Style["The package HighPT` is already loaded. Please restart the kernel to reload it.",RGBColor[.8,.4706,0.2573]]],
	
	(* Set directory of XSection package *)
	Clear[$DirectoryHighPT, $LogoHighPT];
	$DirectoryHighPT= DirectoryName[$InputFileName, 2];
	If[$VersionNumber < 13.0,
		$LogoHighPT= First@ Import[FileNameJoin[{$DirectoryHighPT,"Kernel","logo.pdf"}]];
		,
		$LogoHighPT= First@ Import[FileNameJoin[{$DirectoryHighPT,"Kernel","logo.pdf"}],"PageGraphics"];
	];

	(* Loading *)
	Check[
		Get[FileNameJoin[{$DirectoryHighPT, "HighPT.m"}]],
		Print[Style["Loading failed!",RGBColor[.6,.0706,0.1373]]];
		Abort[]
	];
	
	CellPrint[ExpressionCell[Style[$LogoHighPT,Magnification->0.6],CellMargins->{{70,5},{5,5}}]];
	
	(* title *)
	Print@ Style["Authors: Lukas Allwicher, Darius A. Faroughy, Florentin Jaffredo, Olcyr Sumensari, and Felix Wilsch", Bold, 14];
	Print[
		Style["References: ", Bold, 14],
		Style[Hyperlink["arXiv:2207.10756","http://arxiv.org/abs/2207.10756", BaseStyle->RGBColor[0.04,0.22,0.52]], Bold, 14],
		Style[", ", Bold, 14],
		Style[Hyperlink["arXiv:2207.10714","http://arxiv.org/abs/2207.10714", BaseStyle->RGBColor[0.04,0.22,0.52]], Bold, 14]
	];
	Print[
		Style["Website: ", Bold, 14], 
		Style[Hyperlink["https://highpt.github.io","https://highpt.github.io", BaseStyle->RGBColor[0.04,0.22,0.52]], Bold, 14]
	];
	
	(* licensing *)
	Print@ Style["HighPT is free software released under the terms of the MIT License.", 12];
	(* Version *)
	Print@ Style["Version: "<>$HighPTVersion, 12];
	
	Print["____________________________________"];
	
	(* initialization *)
	(* when loading the package initialize with SMEFT at d=6 with NP^2 contributions and \[CapitalLambda]=1TeV *)
	HighPT`InitializeModel["SMEFT",
		HighPT`EFTorder          -> 4,
		HighPT`OperatorDimension -> 6,
		HighPT`EFTscale          -> 1000
	];
];
