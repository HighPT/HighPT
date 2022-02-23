(* ::Package:: *)

InstallHighPT::version="Warning: `1` has only been tested on Mathematica versions \[GreaterEqual] `2` and you are using Mathematica `3`";


InstallHighPT:=Module[{packageName,packageDir,MinVersion,HighPTLink,QuestionOverwrite,tmpFile,unzipDir,zipDir},

	(* Definitions *)
	packageName="HighPT";
	packageDir=FileNameJoin[{$UserBaseDirectory,"Applications","HighPT"}];
	MinVersion=11.0;
	HighPTLink="https://github.com/HighPT/HighPT/archive/refs/heads/master.zip";

	(* Messages *)
	QuestionOverwrite="HighPT is already installed. Do you want to replace the content of "<>packageDir<>" with a newly downloaded version?";

	(* Check Mathematica version *)
	If[$VersionNumber<MinVersion,
		Message[InstallHighPT::version,packageName,ToString[MinVersion],$VersionNumber];
	];

	(* Check if SuperTracer has already been installed *)
	If[
		DirectoryQ[packageDir],
		If[
			ChoiceDialog[QuestionOverwrite,{"Yes"->True,"No"->False},WindowFloating->True,WindowTitle->"HighPT installation detected"],
			Quiet@DeleteDirectory[packageDir,DeleteContents->True],
			Abort[]
		];
	];

	(* Download HighPT *)
	Print["Downloading HighPT from ",HighPTLink];

	tmpFile=Quiet@URLSave[HighPTLink];

	If[tmpFile===$Failed,
		Print["Failed to download HighPT.\nInstallation aborted!"];
		Abort[]
	];

	(* Unzip HighPT file *)
	Print["Extracting HighPT zip file"];

	unzipDir=tmpFile<>".dir";
	ExtractArchive[tmpFile,unzipDir];

	(* Move files to the Mathematica packages folder *)
	Print["Copying HighPT to "<>packageDir];

	zipDir=FileNames["HighPT.m",unzipDir,Infinity];
	CopyDirectory[DirectoryName[zipDir[[1]]],packageDir];

	(* Delete the extracted archive *)
	Quiet@DeleteDirectory[unzipDir,DeleteContents->True];
	Print["Installation complete!"];

];


InstallHighPT;
