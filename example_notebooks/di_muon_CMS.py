import numpy as np

parameters = ['lq1_2211']

def di_muon_CMS(C):
	result = [
		-2065.786471208978*C['lq1_2211'] + 2484.852721011259*C['lq1_2211']**2, 
		-2106.9341223471815*C['lq1_2211'] + 2783.3198649304386*C['lq1_2211']**2, 
		-2124.2188241637155*C['lq1_2211'] + 3176.3896211758297*C['lq1_2211']**2, 
		-1981.3052120665263*C['lq1_2211'] + 3586.3439474495394*C['lq1_2211']**2, 
		-1962.8742098810862*C['lq1_2211'] + 3861.0982781784824*C['lq1_2211']**2, 
		-1852.6362359991283*C['lq1_2211'] + 4256.8790307706*C['lq1_2211']**2, 
		-1761.9877383848402*C['lq1_2211'] + 4630.962939925538*C['lq1_2211']**2, 
		-1709.6002582855085*C['lq1_2211'] + 5162.3592545297915*C['lq1_2211']**2, 
		-1542.3990195547744*C['lq1_2211'] + 5455.440552568175*C['lq1_2211']**2, 
		-1563.6106523989731*C['lq1_2211'] + 5929.448594032701*C['lq1_2211']**2, 
		-1486.6311977194289*C['lq1_2211'] + 6288.835797680256*C['lq1_2211']**2, 
		-1307.5628899647093*C['lq1_2211'] + 6663.0389027414785*C['lq1_2211']**2, 
		-1249.6443589708003*C['lq1_2211'] + 7158.469887116614*C['lq1_2211']**2, 
		-1148.1011369403147*C['lq1_2211'] + 7649.974668941471*C['lq1_2211']**2, 
		-1056.6408007377593*C['lq1_2211'] + 7844.830310332771*C['lq1_2211']**2, 
		-979.6354358614142*C['lq1_2211'] + 8175.863860484921*C['lq1_2211']**2, 
		-872.2314944081936*C['lq1_2211'] + 8427.826318307098*C['lq1_2211']**2, 
		-780.8692373231494*C['lq1_2211'] + 8645.997379349172*C['lq1_2211']**2, 
		-708.5718400965532*C['lq1_2211'] + 8880.160797202494*C['lq1_2211']**2, 
		-607.7345213442037*C['lq1_2211'] + 8871.324716778716*C['lq1_2211']**2, 
		-535.1460177374299*C['lq1_2211'] + 8913.062315300402*C['lq1_2211']**2, 
		-461.7180757115327*C['lq1_2211'] + 8750.003775770852*C['lq1_2211']**2, 
		-392.69013678134127*C['lq1_2211'] + 8575.123632494706*C['lq1_2211']**2, 
		-337.6620051916973*C['lq1_2211'] + 8345.602905776886*C['lq1_2211']**2, 
		-277.36199709431133*C['lq1_2211'] + 7946.191264488844*C['lq1_2211']**2, 
		-227.72320397043708*C['lq1_2211'] + 7537.655745060123*C['lq1_2211']**2, 
		-185.35600400169008*C['lq1_2211'] + 6917.349571174394*C['lq1_2211']**2, 
		-147.7730738489159*C['lq1_2211'] + 6246.966198296366*C['lq1_2211']**2, 
		-118.57854346291278*C['lq1_2211'] + 5605.832160663467*C['lq1_2211']**2, 
		-89.94820434649472*C['lq1_2211'] + 4776.556705990401*C['lq1_2211']**2, 
		-68.37389197692247*C['lq1_2211'] + 3954.5597884531294*C['lq1_2211']**2, 
		-48.74279479087391*C['lq1_2211'] + 3170.0797527066156*C['lq1_2211']**2, 
		-34.57114783882066*C['lq1_2211'] + 2469.176855723202*C['lq1_2211']**2, 
		-23.67132127400405*C['lq1_2211'] + 1823.4210709088097*C['lq1_2211']**2, 
		-15.432845778007446*C['lq1_2211'] + 1310.3740791264088*C['lq1_2211']**2, 
		-10.160962005334433*C['lq1_2211'] + 905.2584548583307*C['lq1_2211']**2, 
		-6.541213273023879*C['lq1_2211'] + 606.6900281996346*C['lq1_2211']**2, 
		-5.112867877298609*C['lq1_2211'] + 519.5015369217373*C['lq1_2211']**2, 
		-2.5085282421731465*C['lq1_2211'] + 287.47650749802494*C['lq1_2211']**2, 
		-0.8387184419655895*C['lq1_2211'] + 106.287127528088*C['lq1_2211']**2, 
		-0.2586602053325346*C['lq1_2211'] + 38.95638412490109*C['lq1_2211']**2, 
	]
	return result

