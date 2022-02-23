(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`Efficiencies`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["Efficiency"]


PackageScope["AuxEfficiency"]


PackageScope["IncludeEfficiencies"]


PackageScope["LoadEfficiencies"]


PackageExport["\[Epsilon]"] (* remove this? *)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*EventYield*)


(*
EventYield::usage="EventYield[{l1[\[Alpha]],l2[\[Beta]]}, \"mLLcuts\"->{800,13000}, \"pTcuts\"->{0,\[Infinity]}, \"Luminosity\"->139]
	Computes the expected number of events for the process p p -> \!\(\*SubscriptBox[OverscriptBox[\(l1\), \(_\)], \(\[Alpha]\)]\) \!\(\*SubscriptBox[\(l2\), \(\[Beta]\)]\), where l1,l2\[Element]{e,\[Nu]}.
	The optional arguments allow to modify the cuts for the invariant mass of the dilepton system (\"mLLcuts\"), the cuts for the transverse momentum of the leptons (\"pTcuts\"), and the luminosity of the colider (\"Luminosity\").";
*)


(* ::Section:: *)
(*Efficiencies*)


Efficiency::usage= "Efficiency[{structure,type}, chirality, flavors, bin]";


(* efficiencies are real numbers *)
Efficiency/:Conjugate[a_Efficiency]:= a
AuxEfficiency/:Conjugate[a_AuxEfficiency]:= a


(* Make two auxiliary efficiencies combine *)
AuxEfficiency/:Times[
	AuxEfficiency[I_, rest___],
	AuxEfficiency[J_, rest___]
]:= Efficiency[{I,J}, rest]


(* ::Subsection:: *)
(*Printing*)


MakeBoxes[
	Efficiency[type_String, {X_,Y_}, {s_,t_}, {\[Chi]l_,\[Chi]q_}, {\[Alpha]_,\[Beta]_,i_,j_}, bin_],
	TraditionalForm
]:= SubsuperscriptBox[
	RowBox[{
		"[",
		SubsuperscriptBox[
			SubscriptBox["\[Epsilon]",MakeBoxes[bin,TraditionalForm]],
			MakeBoxes[type,TraditionalForm],
			RowBox[{MakeBoxes[X,TraditionalForm],MakeBoxes[Y,TraditionalForm],", ", MakeBoxes[\[Chi]l,TraditionalForm], MakeBoxes[\[Chi]q,TraditionalForm]}]
		],
		"(",s,",",t,")",
		"]"}
	],
	RowBox[{ToString[i],ToString[j]}],
	RowBox[{ToString[\[Alpha]],ToString[\[Beta]]}]
]


(* ::Subsection:: *)
(*Simplify Efficiencies*)


(* all dipoles have same efficiency *)
Efficiency[arg___/;!FreeQ[{arg},DipoleQ|DipoleL]]:= ReleaseHold[Hold[Efficiency[arg]]/.DipoleQ|DipoleL -> "dipole"]


(* compactify first argument *)
Efficiency[{{type_,a1_}, {type_,a2_}}, rest___]:= (Efficiency[type, {a1,a2}, rest]/.{Scalar->"scalar", Vector->"vector", Tensor->"tensor"})

Efficiency[{{Scalar,a1_}, {Tensor,a2_}}, rest___]:= Efficiency["scalar-tensor", {a1,a2}, rest]
Efficiency[{{Tensor,a1_}, {Scalar,a2_}}, rest___]:= Efficiency["scalar-tensor", {a1,a2}, rest]


(* fix (s,t) powers for singular terms *)
Efficiency[str_String, {OrderlessPatternSequence[{type:Photon|ZBoson|WBoson,SM},x_]}, rest___]:= Efficiency[str, {{type,{0,0}},x}, rest]
Efficiency[str_String, {OrderlessPatternSequence[{type:Photon|ZBoson|WBoson,a1_?IntegerQ},x_]}, rest___]:= Efficiency[str, {{type,{0,a1}},x}, rest]


(* compactify second argument *)
Efficiency[str_String, {{type1_,a1_List}, {type2_,a2_List}}, rest___]:= Efficiency[str, Sort[{type1,type2}], Flatten[{a1+a2}], rest]


(* ::Section:: *)
(*Include Efficiencies*)


IncludeEfficiencies[expr_(*, bin_*)]:= Module[
	{rule, temp}
	,
	rule= FF[structure_, type_, chirality_List, flavors_List]:> AuxEfficiency[{structure,type}, chirality, flavors /. \[Nu][_]->\[Nu](*, bin*)] * FF[structure, type, chirality, flavors];
	temp= MyExpand[expr/.rule];
	Return[temp]
]


(* ::Section:: *)
(*Load Efficiencies*)


LoadEfficiencies::unabletoreadfiles= "Error while interpreting efficiency files.";


LoadEfficiencies::duobledef= "Efficiencies defined multiple times: `1`.";


LoadEfficiencies[proc_String(*{e[\[Alpha]_],e[\[Beta]_]}*)]:= Module[
	{(*directory,*) files, substitutions}
	,
	(*
	(* determine required directory *)
	Switch[{\[Alpha],\[Beta]},
		{3,3}, directory= "tata",
		{2,2}, directory= "mumu",
		{1,1}, directory= "ee"
	];
	*)
	
	(* load fiels in that directory *)
	files= Import[
		FileNameJoin[{Global`$DirectoryHighPT, "LHC_searches", $SearchDirectories[proc], "EfficiencyKernel", "*"}],
		"Table"
	];

	(* Find substitutions for all files *)
	substitutions= EfficiencyReplacements[files];
	
	(* check for double definitions *)
	If[!DuplicateFreeQ[substitutions[[;;,1]]],
		Message[
			LoadEfficiencies::duobledef,
			Cases[Tally[substitutions[[;;,1]]], {a_,n_/;n>1}:>a, All]
		]
	];
	
	Return[substitutions]
]


(* ::Subsection:: *)
(*Build up substitution rules for all efficiencies*)


EfficiencyReplacements[files_List]:= Module[
	{tab}
	,
	(* Build efficiency rules for each file *)
	tab= Table[
		BuildEfficiencies[file],
		{file,files}
	];
	(* flatten results *)
	tab= Flatten[tab, 1];
	(* return *)
	Return[tab]
]


(* ::Subsubsection:: *)
(*Build up substitution rules for one file*)


BuildEfficiencies[file_]:= Module[
	{info, effTable, eff, replace={}}
	,
	(* file header *)
	info= file[[2;;8,4;;]];
	(* efficiency table *)
	effTable= file[[13;;,4;;]];
	
	(* construct efficiency *)
	eff= EfficiencyFromHeader[info];
	
	(* construct replacement rules *)
	Do[
		AppendTo[
			replace,
			Append[eff,i] -> effTable[[i]]
		]
		,
		(* i: bin number *)
		{i, Length[effTable]}
	];
	
	Return[replace]
]


(* construct efficiency from the information in the header *)
EfficiencyFromHeader[info_]:= Module[
	{eff, lorentz, \[Chi], flavorL, flavorQ, type, coeff}
	,
	(* quark flavor *)
	Switch[info[[2,1]],
		"bb~", flavorQ= {d[3],d[3]},
		"ss~", flavorQ= {d[2],d[2]},
		"dd~", flavorQ= {d[1],d[1]},
		"bs~", flavorQ= {d[2],d[3]},
		"sb~", flavorQ= {d[3],d[2]},
		"bd~", flavorQ= {d[1],d[3]},
		"db~", flavorQ= {d[3],d[1]},
		"sd~", flavorQ= {d[1],d[2]},
		"ds~", flavorQ= {d[2],d[1]},
		
		"cc~", flavorQ= {u[2],u[2]},
		"uu~", flavorQ= {u[1],u[1]},
		"cu~", flavorQ= {u[1],u[2]},
		"uc~", flavorQ= {u[2],u[1]},
		
		"bc~", flavorQ= {u[2],d[3]},
		"bu~", flavorQ= {u[1],d[3]},
		"sc~", flavorQ= {u[2],d[2]},
		"su~", flavorQ= {u[1],d[2]},
		"dc~", flavorQ= {u[2],d[1]},
		"du~", flavorQ= {u[1],d[1]},
		
		"cb~", flavorQ= {d[3],u[2]},
		"cs~", flavorQ= {d[2],u[2]},
		"cd~", flavorQ= {d[1],u[2]},
		"ub~", flavorQ= {d[3],u[1]},
		"us~", flavorQ= {d[2],u[1]},
		"ud~", flavorQ= {d[1],u[1]},
		
		(* this should be all configurations *)
		_, Message[LoadEfficiencies::unabletoreadfiles]
	];
	
	(* lepton flavor *)
	Switch[info[[1,1]],
		(* di-lepton *)
		"tata", flavorL= {e[3],e[3]},
		"mumu", flavorL= {e[2],e[2]},
		"ee",   flavorL= {e[1],e[1]},
		
		(* mono-lepton *)
		"tavt", Switch[flavorQ,
					{_u,_d}, flavorL= {e[3],\[Nu]},
					{_d,_u}, flavorL= {\[Nu],e[3]},
					_, Message[LoadEfficiencies::unabletoreadfiles]
				],
		"muvm", Switch[flavorQ,
					{_u,_d}, flavorL= {e[2],\[Nu]},
					{_d,_u}, flavorL= {\[Nu],e[2]},
					_, Message[LoadEfficiencies::unabletoreadfiles]
				],
		"eve", Switch[flavorQ,
					{_u,_d}, flavorL= {e[1],\[Nu]},
					{_d,_u}, flavorL= {\[Nu],e[1]},
					_, Message[LoadEfficiencies::unabletoreadfiles]
				],
				
		(* LFV di-lepton *)
		"tamu", flavorL= {e[3],e[2]},
		"muta", flavorL= {e[2],e[3]},
		"tae",  flavorL= {e[3],e[1]},
		"eta",  flavorL= {e[1],e[3]},
		"emu",  flavorL= {e[1],e[2]},
		"mue",  flavorL= {e[2],e[1]},
		
		(* this should be all configurations *)
		_, Message[LoadEfficiencies::unabletoreadfiles]
	];
	
	(* lorentz structure *)
	lorentz= info[[3,1]];
	
	(* chiralities *)
	\[Chi]= Apply[
		Alternatives,
		info[[4]]/. {
			"LL" -> {Left,Left},
			"LR" -> {Left,Right},
			"RL" -> {Right,Left},
			"RR" -> {Right,Right}
		}
	];
	
	(* coefficient (s,t) powers *)
	coeff= info[[5]];
	
	(* type *)
	Switch[info[[6,1]],
		"Reg*Reg", type= {"regular","regular"},
		"A*Reg"|"Reg*A", type= {"regular",Photon},
		"Z*Reg"|"Reg*Z", type= {"regular",ZBoson},
		"W*Reg"|"Reg*W", type= {"regular",WBoson},
		"A*A", type= {Photon,Photon},
		"Z*Z", type= {ZBoson,ZBoson},
		"A*Z"|"Z*A", type= {Photon,ZBoson},
		"W*W", type= {WBoson,WBoson},
		_, Message[LoadEfficiencies::unabletoreadfiles]
	];
	
	(* build up the efficiency *)
	eff= Efficiency[lorentz, type, coeff, \[Chi], Join[flavorL,flavorQ]];
	
	Return[eff]
]
