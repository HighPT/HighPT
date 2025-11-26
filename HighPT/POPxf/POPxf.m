(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChiSquare`*)


(* ::Subtitle:: *)
(*Computation of the \[Chi]^2 likelihood*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["POPxf"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["GeneratePOPxf"]


PackageScope["ImportPOPxf"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Exporting POPxf files for EventYields*)


(*
	takes a single term as input and returns the string labeling its polynomial coefficient
*)
DetermineParamString[term_]:=Module[
	{
		pattern=Re[_String]|Im[_String]|_String|Power[Re[_String],_]|Power[Im[_String],_]|Power[_String,_],
		tmp,
		ReIm,
		str
	}
	,
	(* determine appearing couplings *)
	tmp=Cases[term,pattern,1];
	(* expand powers *)
	tmp=tmp/.Power[arg_,n_Integer]:>Sequence@@ConstantArray[arg,n];
	(* ensure sorting *)
	tmp=Sort@tmp;
	(* handle interference terms *)
	tmp=tmp/.{arg_}:>{"''",arg};
	(* handle real and imaginary parts *)
	ReIm="'"<>StringJoin[StringReplace[tmp/.{"''"->"R",_Re->"R",_Im->"I"},"'"~~__~~"'"->"R"]]<>"'";
	(* combine everything to obtain final string *)
	str="("<>StringRiffle[tmp/.{Re->Identity,Im->Identity},", "]<>", "<>ReIm<>")";
	
	str
]


(*
	takes a string (labeling a specific search) as input and generates the JSON file (with POPxf format) for the EventYield of that search 
*)
GeneratePOPxf[search_String]:=Module[
	{
		info=LHCSearch[search],
		obsName, paramNames,
		eventYield=EventYield[search,EFTscale->1 (* ensure WC are in units of (1 GeV)^-2 *)],
		POPxfAssociation,
		polyAssoc=<||>,
		nBin=0
	}
	,
	(* compute event yield and write in terms of real and imaginary parts *)
	eventYield=ComplexExpand[#,_WC,TargetFunctions->{Re, Im}]&/@eventYield;
	(* map to WCxf notation *)
	eventYield=eventYield/.HighPT`Python`PackagePrivate`MapToWCxf/.(0.->0);
	
	(* determine observable names for all bins, as "serach-name_lower-bin-edge" *)
	obsName=(search<>"_"<>ToString[#])&/@info["INFO","BINS","OBSERVABLE"];
	(* determine Wilson coefficient names *)
	paramNames=StringReplace[
		DeleteDuplicates@Cases[eventYield,HighPT`Python`PackagePrivate`WCxf[label_]:>label,All],
		"'"->""
	];
	
	(* simplify naming *)
	eventYield=eventYield/.HighPT`Python`PackagePrivate`WCxf->Identity;
	(* drop numerically small terms/fluctuations *)
	(*eventYield=Chop/@eventYield;*)
	(* drop small imaginary parts, from numericl fluctuations *)
	eventYield=eventYield/.Complex[r_,i_]->r;
	
	(* deterine all param combinations and create empty association *)
	Do[
		AssociateTo[polyAssoc,DetermineParamString[term]->{}]
		,
		{term,List@@Expand@Total[eventYield]}
	];
	(* loop over bins/observables *)
	Do[
		(* increment bincounter *)
		nBin+=1;
		(* loop over all terms of one bin and add their polynomial coefficient *)
		Do[
			AppendTo[polyAssoc[DetermineParamString[term]],FirstCase[term,_?NumericQ]]
			,
			{term,List@@Expand[bin]}
		];
		(* check that all polynomial coefficients have been added, if one is missing add zero *)
		Do[
			If[Length[polyAssoc[k]]!=nBin,
				AppendTo[polyAssoc[k],0.]
			]
			,
			{k,Keys[polyAssoc]}
		]
		,
		{bin,eventYield}
	];
	
	(* add SM/background prediction *)
	PrependTo[polyAssoc,"('', '', 'RR')"->info["BACKGROUND"]];
	
	(* association to be turned into POPxf JSON file *)
	POPxfAssociation=<|
		"$schema"->"https://json.schemastore.org/popxf-1.0.json",
		"metadata"-><|
			"observable_names"->obsName,
			"parameters"->Sort[paramNames],
			"basis"-><|
				"wcxf"-><|
					"eft"->"SMEFT",
					"basis"->"Warsaw"
				|>
			|>,
			"scale"->info["INFO","BINS","OBSERVABLE"],
			"reproducibility"->{<|
				"tool"-><|
					"name"->"HighPT",
					"version"->Global`$HighPTVersion
				|>
			|>},
			"misc"-><|"author"->{"F. Wilsch"}|>
		|>
		,
		"data"-><|
			"observable_central"->KeySortBy[polyAssoc,StringLength],
			"observable_uncertainties"-><|
				"total"->info["ERROR-BKG"]
			|>
		|>
	|>;
	
	(* export *)
	Export[FileNameJoin@{Global`$DirectoryHighPT,"POPxf","Drell-Yan",search<>"_popxf.json"}, POPxfAssociation]
]


GeneratePOPxf[]:=GeneratePOPxf/@{"di-tau-ATLAS","di-muon-CMS","di-electron-CMS","mono-tau-ATLAS","mono-muon-ATLAS","mono-electron-ATLAS","muon-tau-CMS","electron-tau-CMS","electron-muon-CMS"}


(* ::Section:: *)
(*Importing POPxf files for EventYields*)


(*
	takes a WCxf sting as input and returns the corresponding WC in HighPT notation
*)
WCxfToHighPT[str_]:=Module[
	{repl}
	,
	(* return 1 for SM part *)
	If[str==="''",Return[1]];
	(* replacement rules for Wilson coefficents *)
	repl={
		"'qe_"~~p_~~r_~~s_~~t_~~"'":>"WC[\"eq\",{"<>s<>","<>t<>","<>p<>","<>r<>"}]",
		"'"~~label__~~"_"~~p_~~r_~~s_~~t_~~"'":>"WC[\""<>label<>"\",{"<>p<>","<>r<>","<>s<>","<>t<>"}]",
		"'"~~label__~~"_"~~p_~~r_~~"'":>StringReplace["WC[\""<>label<>"\",{"<>p<>","<>r<>"}]","phi"->"H"]
	};
	ToExpression@StringReplace[str,repl]
]


(*
	takes a key string as input and returns the appropriate combination of WC and Re|Im for that term
*)
PolynomialTerm::RImismatch="Number of real and imaginary parts specified does not match number of coefficients.";

PolynomialTerm[key_String]:=Module[
	{
		vars=StringSplit[StringDelete[key,{"(",")"," "}],","],
		ReIm
	}
	,
	(* deterine real and imaginary parts *)
	ReIm=StringCases[Last[vars],"R"|"I"]/.{"R"->Re,"I"->Im};
	(* get Wilson coefficients *)
	vars=WCxfToHighPT/@vars[[;;-2]];
	(* apply Re and Im *)
	If[Length[ReIm]>0,
		If[Length[ReIm]!=Length[vars],Message[PolynomialTerm::RImismatch];Abort[]];
		vars=MapThread[(#1[#2])&,{ReIm,vars},1]
	];
	(* return coeff *)
	If[Length[vars]==1,
		Return[vars[[1]]],
		Return[Times@@vars]
	]
]


(*
	takes path to a POPxf JSON file as input and returns the corresponding opservabel in HighPT notation
*)
ImportPOPxf[search_]:=Module[
	{
		path= FileNameJoin@{Global`$DirectoryHighPT,"POPxf","Drell-Yan",search<>"_popxf.json"},
		file,
		obs,
		err
	}
	,
	(* import POPxf file *)
	file= Import[path,"RawJSON"];
	(* extract polynomial expression for observable *)
	obs= file["data","observable_central"];
	(* extract uncertainties for observable *)
	err= file["data","observable_uncertainties","total"];
	
	(* loop over polynomial coefficients to build observable *)
	obs= Sum[
		obs[key]*PolynomialTerm[key]
		,
		{key,Keys[obs]}
	];
	
	{obs,err}
]
