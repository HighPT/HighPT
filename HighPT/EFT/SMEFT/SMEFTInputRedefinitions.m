(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SMEFTInputRedefinitions`*)


(* ::Subtitle:: *)
(*SMEFT Input Redefinitions*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["RedefineSMEFTCouplings"]


(* ::Chapter:: *)
(*Private:*)


(*list of parameters that do not get redefined in the SMEFT*)
SMEFTInputParameterList$default=Flatten[{
	(*Mass["WBoson"],*)
	Table[Mass[i],{i,{
	"e","\[Mu]","\[Tau]",
	"d","s","b",
	"u","c","t",
	"\[Pi]+","\[Pi]0",
	"K+","K0",
	"\[Eta]","\[Eta]'",
	"\[Rho]",
	"\[Phi]",
	"D+","D0","Ds",
	"B+","B0","Bs","Bc",
	"p","n",
	"ZBoson",
	"H"
}}],
	Param["\[Alpha]EM"],Param["g3"],Param["\[Alpha]S"],
	Table[Yukawa[lab,{i,j}],{lab,{"u","d","e"}},{i,1,3},{j,1,3}],
	Param["vev"]
}];

(*list of the parameters that get redefined*)
DimensionSixReplacements$default = Association[{
	Param["g1"]->-((Param["g1"]^2 Param["vev"]^2 (Param["g1"] WC["HD",{}]+4 Param["g2"] WC["HWB",{}]))/(4 (Param["g1"]^2-Param["g2"]^2))),
	Param["g2"]->(Param["vev"]^2 (Param["g2"]^3 WC["HD",{}]+4 Param["g1"] Param["g2"]^2 WC["HWB",{}]))/(4 (Param["g1"]^2-Param["g2"]^2))
}];

DimensionEightReplacements$default = Association[{
	Param["g1"]->-(1/(32 (Param["g1"]^2-Param["g2"]^2)^3)) Param["g1"]^2 Param["vev"]^4 (4 Param["g1"] (Param["g1"]^2-Param["g2"]^2)^2 WC["H61",{}]+4 Param["g1"] (Param["g1"]^2-Param["g2"]^2)^2 WC["H62",{}]-3 Param["g1"]^5 WC["HD",{}]^2+7 Param["g1"]^3 Param["g2"]^2 WC["HD",{}]^2+32 Param["g1"]^4 Param["g2"] WC["HB",{}] WC["HWB",{}]-64 Param["g1"]^2 Param["g2"]^3 WC["HB",{}] WC["HWB",{}]+32 Param["g2"]^5 WC["HB",{}] WC["HWB",{}]+24 Param["g1"]^2 Param["g2"]^3 WC["HD",{}] WC["HWB",{}]+8 Param["g2"]^5 WC["HD",{}] WC["HWB",{}]+32 Param["g1"]^4 Param["g2"] WC["HW",{}] WC["HWB",{}]-64 Param["g1"]^2 Param["g2"]^3 WC["HW",{}] WC["HWB",{}]+32 Param["g2"]^5 WC["HW",{}] WC["HWB",{}]+16 Param["g1"]^5 WC["HWB",{}]^2-16 Param["g1"]^3 Param["g2"]^2 WC["HWB",{}]^2+64 Param["g1"] Param["g2"]^4 WC["HWB",{}]^2+16 Param["g1"]^4 Param["g2"] WC["WBH41",{}]-32 Param["g1"]^2 Param["g2"]^3 WC["WBH41",{}]+16 Param["g2"]^5 WC["WBH41",{}]),
	Param["g2"]->-(1/(32 (-Param["g1"]^2+Param["g2"]^2)^3)) Param["g2"]^2 Param["vev"]^4 (4 Param["g2"] (Param["g1"]^2-Param["g2"]^2)^2 WC["H61",{}]+4 Param["g2"] (Param["g1"]^2-Param["g2"]^2)^2 WC["H62",{}]+7 Param["g1"]^2 Param["g2"]^3 WC["HD",{}]^2-3 Param["g2"]^5 WC["HD",{}]^2+32 Param["g1"]^5 WC["HB",{}] WC["HWB",{}]-64 Param["g1"]^3 Param["g2"]^2 WC["HB",{}] WC["HWB",{}]+32 Param["g1"] Param["g2"]^4 WC["HB",{}] WC["HWB",{}]+8 Param["g1"]^5 WC["HD",{}] WC["HWB",{}]+24 Param["g1"]^3 Param["g2"]^2 WC["HD",{}] WC["HWB",{}]+32 Param["g1"]^5 WC["HW",{}] WC["HWB",{}]-64 Param["g1"]^3 Param["g2"]^2 WC["HW",{}] WC["HWB",{}]+32 Param["g1"] Param["g2"]^4 WC["HW",{}] WC["HWB",{}]+64 Param["g1"]^4 Param["g2"] WC["HWB",{}]^2-16 Param["g1"]^2 Param["g2"]^3 WC["HWB",{}]^2+16 Param["g2"]^5 WC["HWB",{}]^2+16 Param["g1"]^5 WC["WBH41",{}]-32 Param["g1"]^3 Param["g2"]^2 WC["WBH41",{}]+16 Param["g1"] Param["g2"]^4 WC["WBH41",{}])
}];

AllParamsAsAFunctionOfSMEFTInputs$default = Association[Table[param-> param +DimensionSixReplacements$default[param]+DimensionEightReplacements$default[param] ,{param,Keys[DimensionSixReplacements$default]}]];


SMEFTInputParameterList$current = SMEFTInputParameterList$default;
AllParamsAsAFunctionOfSMEFTInputs$current = AllParamsAsAFunctionOfSMEFTInputs$default;
SMEFTInputScheme$current = {SMEFTInputParameterList$current,AllParamsAsAFunctionOfSMEFTInputs$current};


Options[RedefineSMEFTCouplings]={
SMEFTInputScheme :> (*{SMEFTInputParameterList$default,AllParamsAsAFunctionOfSMEFTInputs$default}*)SMEFTInputScheme$current, 
EFTorder :> GetEFTorder[],
OperatorDimension :> GetOperatorDimension[]
};

RedefineSMEFTCouplings::usage = "This function takes care of writing all SMEFT parameters in terms of input parameters; this excludes parameters like GF and the CKM, for which are written in terms of inputs already at the observable level";
RedefineSMEFTCouplings::hasGF = "The expression contains GF";
RedefineSMEFTCouplings::hasOtherParameters = "The expression contains the following parameters not covered by the input scheme: `1`";


RedefineSMEFTCouplings[expr_,OptionsPattern[]]:= Module[
{tmp,inputparams, paramstorewriteintermsoftheinputs, replacements,checklist, smeftvalues,variables,VariablesAfterSubstitutingSMEFTValues,exprafterreplacements},

(*store some useful information*)
inputparams =OptionValue[SMEFTInputScheme][[1]];
paramstorewriteintermsoftheinputs = Keys[OptionValue[SMEFTInputScheme][[2]]];
checklist= Join[inputparams,paramstorewriteintermsoftheinputs,Table[Vckm[i,j],{i,3},{j,3}]]//Flatten;
replacements = Values[OptionValue[SMEFTInputScheme][[2]]];
variables = DeleteCases[GetVariables[expr],_WC];
smeftvalues =SMEFTValue/@Complement[variables,inputparams];
(*Print[Complement[variables,inputparams]];*)

(*first substitute the smeftvalue of all parameters that are not input parameters*)
tmp = expr /. Thread[Complement[variables,inputparams]-> smeftvalues];
VariablesAfterSubstitutingSMEFTValues = DeleteCases[GetVariables[tmp],_WC];

(*check that once this is done the expression has the right form, specifically*)
(*1) that it does not contain Param["GF"]*)
If[!FreeQ[tmp,Param["GF"]],
Message[RedefineSMEFTCouplings::hasGF];
Return[$Failed];];

(*2) that it contains only parameters covered by the input scheme*)
If[Complement[VariablesAfterSubstitutingSMEFTValues,checklist]!={},Message[RedefineSMEFTCouplings::hasOtherParameters,Complement[variables,checklist]]; Return[$Failed];];

(*if the expression has the right form, replace all the parameters that are not input parameters*)
tmp = tmp/. Thread[paramstorewriteintermsoftheinputs-> replacements];

(*and truncate to the desired order*)
(*Return[EFTTruncate[tmp, EFTorder-> 2]]*)
Return[EFTTruncate[tmp, EFTorder-> OptionValue[EFTorder],OperatorDimension-> OptionValue[OperatorDimension]]]

]
