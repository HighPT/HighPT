(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`LEP2`*)


(* ::Subtitle:: *)
(*LEP2 observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["LEP2Observables"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["SMPredictions"]
PackageScope["ExpValues"]
PackageScope["LowScales"]
PackageScope["NPContributions"]


(* ::Chapter:: *)
(*Private:*)


LEP2Observables::usage = "LEP2Observables[] returns a list of all the LEP2 off-Z-pole observables implemented in HighPT"


LEP2Observables[] = {
	"\[Sigma](ee->\[Mu]\[Mu])",
	"\[Sigma](ee->\[Tau]\[Tau])",
	"AFB(ee->\[Mu]\[Mu])",
	"AFB(ee->\[Tau]\[Tau])",
	"d\[Sigma](ee->ee)/dcos\[Theta]"
	}


(* ::Section:: *)
(*Definitions*)


GeVtopb=0.389379*10^9;


ZCoupl[X_,j_]:=If[MatchQ[X,Left],
(ZCoupling["eL",{j,j}]/.a_WC->0)+\[Delta]gZ[e,Left,{j,j}]/.Replace\[Delta]g,
(ZCoupling["eR",{j,j}]/.a_WC->0)+\[Delta]gZ[e,Right,{j,j}]/.Replace\[Delta]g
]


\[Delta]\[Delta][X_,Y_]:=If[MatchQ[X,Y],1,0]


(* ::Section:: *)
(*ee -> ll*)


NN[X_,Y_,j_][s_]:=(4\[Pi] Param["\[Alpha]EM"])/s+Param["g2"]^2/Param["cW"]^2 ((Normal@Series[(ZCoupl[X,1]ZCoupl[Y,j])/(s-Mass["ZBoson"]^2+I Width["ZBoson"]Mass["ZBoson"])/.a_WC->eps*a,{eps,0,1}])/.eps->1)+(WC["ll",{1,1,j,j}]+WC["ll",{1,j,j,1}])\[Delta]\[Delta][X,Left]\[Delta]\[Delta][Y,Left]+WC["ee",{1,1,j,j}]\[Delta]\[Delta][X,Right]\[Delta]\[Delta][Y,Right]+WC["le",{1,1,j,j}]\[Delta]\[Delta][X,Left]\[Delta]\[Delta][Y,Right]+WC["le",{j,j,1,1}]\[Delta]\[Delta][X,Right]\[Delta]\[Delta][Y,Left]


\[Sigma]eell[j_,s_]:=GeVtopb s/(48\[Pi]) (3Abs[WC["le",{1,j,j,1}]]^2+Sum[Abs[NN[X,Y,j][s]]^2,{X,{Left,Right}},{Y,{Left,Right}}])


\[Sigma]FBeell[j_,s_]:=GeVtopb s/(64\[Pi]) (Sum[Abs[NN[X,Y,j][s]]^2 (2\[Delta]\[Delta][X,Y]-1),{X,{Left,Right}},{Y,{Left,Right}}])


AFBeell[j_,s_]:=1/\[Sigma]eell[j,s] \[Sigma]FBeell[j,s]


(* ::Subsection:: *)
(*\[Sigma] (ee -> \[Mu]\[Mu])*)


LowScales["\[Sigma](ee->\[Mu]\[Mu])"]={130,136,161,172,183,189,192,196,200,202,205,207};


SMPredictions["\[Sigma](ee->\[Mu]\[Mu])"]=<|
130->8.455,
136->7.292,
161->4.610,
172->3.950,
183->3.444,
189->3.207,
192->3.096,
196->2.961,
200->2.833,
202->2.766,
205->2.673,
207->2.628
|>;


ExpValues["\[Sigma](ee->\[Mu]\[Mu])"]=<|
130->Around[8.606,0.699]+Around[0,0.131],
136->Around[8.325,0.692]+Around[0,0.109],
161->Around[4.580,0.376]+Around[0,0.062],
172->Around[3.562,0.331]+Around[0,0.058],
183->Around[3.505,0.145]+Around[0,0.042],
189->Around[3.150,0.075]+Around[0,0.016],
192->Around[2.926,0.181]+Around[0,0.018],
196->Around[2.994,0.110]+Around[0,0.018],
200->Around[3.072,0.108]+Around[0,0.018],
202->Around[2.709,0.146]+Around[0,0.017],
205->Around[2.464,0.098]+Around[0,0.015],
207->Around[2.618,0.078]+Around[0,0.014]
|>;


NPContributions["\[Sigma](ee->\[Mu]\[Mu])"]=Association@Table[s->\[Sigma]eell[2,s^2]-(\[Sigma]eell[2,s^2]/._WC->0),{s,LowScales["\[Sigma](ee->\[Mu]\[Mu])"]}]


(* ::Subsection:: *)
(*\[Sigma] (ee -> \[Tau]\[Tau])*)


LowScales["\[Sigma](ee->\[Tau]\[Tau])"] = LowScales["\[Sigma](ee->\[Mu]\[Mu])"]


SMPredictions["\[Sigma](ee->\[Tau]\[Tau])"] = <|
130->8.452,
136->7.290,
161->4.610,
172->3.950,
183->3.444,
189->3.206,
192->3.096,
196->2.961,
200->2.832,
202->2.765,
205->2.672,
207->2.628
|>;


ExpValues["\[Sigma](ee->\[Tau]\[Tau])"] = <|
130->Around[9.020,0.944]+Around[0,0.175],
136->Around[7.167,0.851]+Around[0,0.143],
161->Around[5.715,0.553]+Around[0,0.139],
172->Around[4.053,0.469]+Around[0,0.092],
183->Around[3.367,0.174]+Around[0,0.049],
189->Around[3.204,0.107]+Around[0,0.032],
192->Around[2.860,0.246]+Around[0,0.032],
196->Around[2.961,0.152]+Around[0,0.029],
200->Around[2.952,0.148]+Around[0,0.029],
202->Around[2.838,0.208]+Around[0,0.022],
205->Around[2.783,0.149]+Around[0,0.028],
207->Around[2.502,0.109]+Around[0,0.028]
|>;


NPContributions["\[Sigma](ee->\[Tau]\[Tau])"]=Association@Table[s->\[Sigma]eell[3,s^2]-(\[Sigma]eell[3,s^2]/._WC->0),{s,LowScales["\[Sigma](ee->\[Tau]\[Tau])"]}]


(* ::Subsection:: *)
(*AFB (ee -> \[Mu]\[Mu])*)


LowScales["AFB(ee->\[Mu]\[Mu])"] = LowScales["\[Sigma](ee->\[Mu]\[Mu])"]


SMPredictions["AFB(ee->\[Mu]\[Mu])"] = <|
130->0.705,
136->0.684,
161->0.610,
172->0.591,
183->0.576,
189->0.569,
192->0.566,
196->0.562,
200->0.558,
202->0.556,
205->0.553,
207->0.552
|>;


ExpValues["AFB(ee->\[Mu]\[Mu])"] = <|
130->Around[0.694,0.059]+Around[0,0.012],
136->Around[0.707,0.061]+Around[0,0.011],
161->Around[0.542,0.069]+Around[0,0.012],
172->Around[0.673,0.077]+Around[0,0.012],
183->Around[0.564,0.034]+Around[0,0.008],
189->Around[0.571,0.020]+Around[0,0.005],
192->Around[0.551,0.051]+Around[0,0.007],
196->Around[0.592,0.030]+Around[0,0.005],
200->Around[0.519,0.031]+Around[0,0.005],
202->Around[0.547,0.045]+Around[0,0.005],
205->Around[0.556,0.034]+Around[0,0.004],
207->Around[0.535,0.028]+Around[0,0.004]
|>;


NPContributions["AFB(ee->\[Mu]\[Mu])"]=Association@Table[s->AFBeell[2,s^2]-(AFBeell[2,s^2]/._WC->0),{s,LowScales["AFB(ee->\[Mu]\[Mu])"]}]


(* ::Subsection:: *)
(*AFB (ee -> \[Tau]\[Tau])*)


LowScales["AFB(ee->\[Tau]\[Tau])"] = LowScales["\[Sigma](ee->\[Mu]\[Mu])"]


SMPredictions["AFB(ee->\[Tau]\[Tau])"] = <|
130->0.705,
136->0.684,
161->0.610,
172->0.591,
183->0.576,
189->0.569,
192->0.565,
196->0.561,
200->0.558,
202->0.556,
205->0.553,
207->0.552
|>;


ExpValues["AFB(ee->\[Tau]\[Tau])"] = <|
130->Around[0.682,0.079]+Around[0,0.016],
136->Around[0.761,0.089]+Around[0,0.013],
161->Around[0.764,0.061]+Around[0,0.013],
172->Around[0.357,0.098]+Around[0,0.013],
183->Around[0.604,0.044]+Around[0,0.011],
189->Around[0.590,0.026]+Around[0,0.007],
192->Around[0.590,0.067]+Around[0,0.008],
196->Around[0.464,0.044]+Around[0,0.008],
200->Around[0.539,0.041]+Around[0,0.007],
202->Around[0.535,0.058]+Around[0,0.009],
205->Around[0.618,0.040]+Around[0,0.008],
207->Around[0.590,0.034]+Around[0,0.010]
|>;


NPContributions["AFB(ee->\[Tau]\[Tau])"]=Association@Table[s->AFBeell[3,s^2]-(AFBeell[3,s^2]/._WC->0),{s,LowScales["AFB(ee->\[Tau]\[Tau])"]}]


(* ::Section:: *)
(*ee -> ee*)


(* ::Subsection:: *)
(*Functions*)


CC[Left,Left]=2*WC["ll",{1,1,1,1}];
CC[Right,Right]=2*WC["ee",{1,1,1,1}];
CC[Left,Right]=WC["le",{1,1,1,1}];
CC[Right,Left]=WC["le",{1,1,1,1}];


tt[X_,Y_,t_]:=Param["g2"]^2/Param["cW"]^2 ((Normal@Series[(ZCoupl[X,1]ZCoupl[Y,1])/(t-Mass["ZBoson"]^2+I Width["ZBoson"]Mass["ZBoson"])/.a_WC->eps*a,{eps,0,1}])/.eps->1)+(CC[X,Y]+CC[Y,X])/2


ss[X_,Y_,s_]:=Param["g2"]^2/Param["cW"]^2 ((Normal@Series[(ZCoupl[X,1]ZCoupl[Y,1])/(s-Mass["ZBoson"]^2+I Width["ZBoson"]Mass["ZBoson"])/.a_WC->eps*a,{eps,0,1}])/.eps->1)+(CC[X,Y]+CC[Y,X])/2


d\[Sigma]dcos\[Theta][s_,c_]:=GeVtopb 1/(32\[Pi] s) (2(4\[Pi] Param["\[Alpha]EM"])^2 ((u^2+s^2)/t^2+(u^2+t^2)/s^2+(2u^2)/(s t))+
Sum[(2(4\[Pi] Param["\[Alpha]EM"]))/t (Re[tt[X,Y,t]](u^2 \[Delta]\[Delta][X,Y]+s^2 (1-\[Delta]\[Delta][X,Y]))+Re[ss[X,Y,s]]u^2 \[Delta]\[Delta][X,Y])+
(2(4\[Pi] Param["\[Alpha]EM"]))/s (Re[ss[X,Y,s]](u^2 \[Delta]\[Delta][X,Y]+t^2 (1-\[Delta]\[Delta][X,Y]))+Re[tt[X,Y,t]]u^2 \[Delta]\[Delta][X,Y])+
Abs[tt[X,Y,t]]^2 (u^2 \[Delta]\[Delta][X,Y]+s^2 (1-\[Delta]\[Delta][X,Y]))+
Abs[ss[X,Y,s]]^2 (u^2 \[Delta]\[Delta][X,Y]+t^2 (1-\[Delta]\[Delta][X,Y]))+
2Re[tt[X,Y,t]\[Conjugate]ss[X,Y,s]]u^2 \[Delta]\[Delta][X,Y]
,{X,{Left,Right}},{Y,{Left,Right}}]
)/.t->(-s(1-c))/2/.u->(-s(1+c))/2/.GetParameters[]


wceeee={WC["ee",{1,1,1,1}],WC["HD",{}],WC["He",{1,1}],WC["Hl1",{1,1}],WC["Hl3",{1,1}],WC["Hl3",{2,2}],WC["HWB",{}],WC["le",{1,1,1,1}],WC["ll",{1,1,1,1}],WC["ll",{1,2,2,1}]}


MyReIm[WC[lab_,ind_]]:=ReWC[lab,ind]+I*ImWC[lab,ind]
replacecomplex=Table[i->MyReIm[i],{i,wceeee}]


realwceeee=Variables@(wceeee/.replacecomplex)


Options[integrated\[Sigma]]={SM->False};
integrated\[Sigma][s_,{min_,max_},OptionsPattern[]]:=Module[
{
ord0,
ord1,
ord2,
d\[Sigma],
ord0int,
ord1int,
ord2int,
res
}
,
d\[Sigma]=(d\[Sigma]dcos\[Theta][s^2,c]/.replacecomplex)//ComplexExpand;
ord0=d\[Sigma]/.Table[i->0,{i,realwceeee}];
ord1=(Coefficient[d\[Sigma],#]&/@realwceeee)/.Table[i->0,{i,realwceeee}];
ord2=1/2 (D[(D[d\[Sigma],#]&/@realwceeee),#]&/@realwceeee)/.Table[i->0,{i,realwceeee}];
{ord0int,ord1int,ord2int}=NIntegrate[#,{c,min,max},AccuracyGoal->5]&/@{ord0,ord1,ord2};
res=1/(max-min) (Boole[OptionValue[SM]]*ord0int+ord1int . realwceeee+realwceeee . ord2int . realwceeee);
Return@(res/.ImWC[lab_,ind_]:>Im[WC[lab,ind]]/.ReWC[lab_,ind_]:>Re[WC[lab,ind]])
]


dielectronbins={-0.9,-0.72,-0.54,-0.36,-0.18,0,0.09,0.18,0.27,0.36,0.45,0.54,0.63,0.72,0.81,0.9};


(* ::Subsection:: *)
(*Observable*)


LowScales["d\[Sigma](ee->ee)/dcos\[Theta]"] = {189,192,196,200,202,205,207}


SMPredictions["d\[Sigma](ee->ee)/dcos\[Theta]"] = <|
189->{1.590,1.816,2.162,2.681,3.906,5.372,6.892,9.610,13.345,18.445,30.476,51.012,95.563,212.390,689.989},
192->{1.539,1.754,2.091,2.604,3.778,5.205,6.692,9.242,12.800,18.776,29.471,49.338,92.079,206.087,669.173},
196->{1.483,1.695,2.000,2.498,3.610,4.999,6.406,8.832,12.326,18.039,28.300,47.362,88.473,198.250,642.688},
200->{1.420,1.623,1.885,2.409,3.435,4.770,6.157,8.471,11.773,17.262,27.117,45.607,85.143,190.786,617.718},
202->{1.401,1.579,1.836,2.361,3.356,4.669,6.017,8.320,11.554,16.891,26.583,44.786,83.473,186.904,605.070},
205->{1.355,1.539,1.786,2.280,3.253,4.479,5.820,8.077,11.200,16.322,25.722,43.217,80.939,180.878,586.205},
207->{1.339,1.517,1.745,2.240,4.194,4.380,5.729,7.972,11.019,16.053,25.254,42.456,79.639,178.042,576.688}
|>;


ExpValues["d\[Sigma](ee->ee)/dcos\[Theta]"] = <|
189->{
Around[1.401,0.161],
Around[2.030,0.160],
Around[2.162,0.170],
Around[2.298,0.186],
Around[4.321,0.230],
Around[4.898,0.348],
Around[6.090,0.404],
Around[8.838,0.476],
Around[12.781,0.576],
Around[19.586,0.707],
Around[30.598,0.895],
Around[50.488,1.135],
Around[95.178,1.520],
Around[211.427,2.900],
Around[679.146,5.773]
},
192->{
Around[1.300,0.364],
Around[2.099,0.419],
Around[1.871,0.385],
Around[1.808,0.422],
Around[3.800,0.519],
Around[5.015,0.891],
Around[5.695,0.976],
Around[9.239,1.175],
Around[12.941,1.414],
Around[20.761,1.807],
Around[26.466,2.074],
Around[49.382,2.671],
Around[89.676,3.615],
Around[204.579,6.760],
Around[655.724,12.588]
},
196->{
Around[1.470,0.261],
Around[1.527,0.221],
Around[2.058,0.250],
Around[2.788,0.284],
Around[3.646,0.318],
Around[5.887,0.521],
Around[6.233,0.591],
Around[9.016,0.694],
Around[13.444,0.856],
Around[18.856,0.977],
Around[27.056,1.223],
Around[49.391,1.619],
Around[88.163,2.154],
Around[197.369,4.121],
Around[637.846,8.003]
},
200->{
Around[1.483,0.245],
Around[1.638,0.214],
Around[2.068,0.227],
Around[2.362,0.250],
Around[4.251,0.313],
Around[5.244,0.506],
Around[5.888,0.571],
Around[8.244,0.667],
Around[9.506,0.736],
Around[16.376,0.920],
Around[27.000,1.214],
Around[44.614,1.537],
Around[86.454,2.060],
Around[190.962,3.941],
Around[604.986,7.608]
},
202->{
Around[1.568,0.368],
Around[1.344,0.276],
Around[2.107,0.345],
Around[3.240,0.406],
Around[2.911,0.394],
Around[4.603,0.628],
Around[6.463,0.861],
Around[7.457,0.957],
Around[11.032,1.113],
Around[16.428,1.338],
Around[27.153,1.643],
Around[46.490,2.214],
Around[87.253,2.887],
Around[189.026,5.516],
Around[599.860,10.339]
},
205->{
Around[1.102,0.205],
Around[1.470,0.195],
Around[2.050,0.231],
Around[2.564,0.255],
Around[3.410,0.300],
Around[5.308,0.472],
Around[5.836,0.571],
Around[7.996,0.635],
Around[10.607,0.764],
Around[14.729,0.874],
Around[26.189,1.157],
Around[43.124,1.497],
Around[79.255,1.976],
Around[179.842,3.838],
Around[587.999,7.527]
},
207->{
Around[1.440,0.196],
Around[1.426,0.163],
Around[1.889,0.177],
Around[2.156,0.198],
Around[3.215,0.233],
Around[4.434,0.357],
Around[6.393,0.463],
Around[6.951,0.481],
Around[11.221,0.615],
Around[15.933,0.739],
Around[25.676,0.923],
Around[42.075,1.188],
Around[77.611,1.569],
Around[173.825,3.002],
Around[573.637,6.024]
}
|>;


(*NPContributions["d\[Sigma](ee->ee)/dcos\[Theta]"] := Association@Table[s->Table[integrated\[Sigma][s,{dielectronbins[[j]],dielectronbins[[j+1]]}],{j,Length[dielectronbins]-1}],{s,LowScales["d\[Sigma](ee->ee)/dcos\[Theta]"]}]*)


(*NPContributions["d\[Sigma](ee->ee)/dcos\[Theta]"] << "bhabhaintegrals.m"*)


(*NPContributions["d\[Sigma](ee->ee)/dcos\[Theta]"] = Association[Table[s->Table[NPbin[i],{i,Length[dielectronbins]-1}],{s,LowScales["d\[Sigma](ee->ee)/dcos\[Theta]"]}]]*)


Get@FileNameJoin[{Global`$DirectoryHighPT,"bhabhaintegrals.dat"}];
