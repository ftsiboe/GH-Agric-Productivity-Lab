
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_land_tenure\data\tech_inefficiency_land_tenure_data",clear
decode CropID,gen(CropIDx)
*keep if CropIDx == "Pooled"
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
*loc crop "Pooled"
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_land_tenure\data\tech_inefficiency_land_tenure_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "`crop'"

sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)

mat Means=J(1,8,.)
qui foreach Var of var Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt AgeYr YerEdu HHSizeAE Depend CrpMix {
preserve
cap{
*loc Var Yield
reg `Var' c.Trend##i.OwnLnd, vce(cluster Clust) 
qui est store Model
testparm i.OwnLnd						//mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.OwnLnd			//trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins OwnLnd, eydx(Trend) grand coefl post
nlcom ("Trend_OwnLnd0":_b[Trend:0bn.OwnLnd]*100) ("Trend_OwnLnd1":_b[Trend:1.OwnLnd]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(OwnLnd) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_OwnLnd0 Trend_OwnLnd1 Trend_Pooled CATDif TrendDif Mean_OwnLnd0 Mean_OwnLnd1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(OwnLnd) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_OwnLnd0 `sx'_OwnLnd1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

qui foreach Var of var Female EqipMech Credit Extension EqipIrig{
preserve
cap{
*Overall and regional means 
qui logit `Var' c.Trend##i.OwnLnd, vce(cluster Clust) 
qui est store Model
testparm i.OwnLnd						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.OwnLnd			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins OwnLnd, eydx(Trend) grand coefl post
nlcom ("Trend_OwnLnd0":_b[Trend:0bn.OwnLnd]*100) ("Trend_OwnLnd1":_b[Trend:1.OwnLnd]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(OwnLnd) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_OwnLnd0 Trend_OwnLnd1 Trend_Pooled CATDif TrendDif Mean_OwnLnd0 Mean_OwnLnd1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(OwnLnd) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_OwnLnd0 `sx'_OwnLnd1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

tab OwnLnd,gen(OwnLnd)
ren (OwnLnd1 OwnLnd2) (OwnLnd0 OwnLnd1)

qui foreach Var of var OwnLnd0 OwnLnd1{
	cap{
	logit `Var' Trend, vce(cluster Clust) 
	margins, eydx(Trend) grand coefl post
	nlcom ("Trend_`Var'":_b[Trend]*100), post
	qui ereturn display
	mat A = r(table)'
	mat A = A[1...,1..8]

	tabstat `Var', stat(mean sem min max sd n) save
	foreach mt in StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = Trend_`Var' Mean_`Var'
	mat roweq A= Female
	mat li A
	mat Means = A\Means

	mat drop A

	qui levelsof Surveyx, local(SurveyList)
	foreach sx in `SurveyList'{
		mat A = J(1,8,.)
		tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) save
		foreach mt in StatTotal{
			mat B = r(`mt')'
			mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
			mat A =A\B
			mat drop B
		}
		mat rownames A = `sx'_miss `sx'_Pooled
		mat roweq A= Female
		mat Means = A\Means	
		mat drop A
	}
}
}

mat colnames Means = Beta SE Tv Pv Min Max SD N
/*
qui putexcel set "Results\Farmer_Age_Productivity_Ghana_Results.xlsx", sheet(Means) modify
qui putexcel A1=matrix(Means),names
mat li Means
*/
qui clear
qui svmat Means, names(col)
qui gen Coef=""
qui gen Equ=""
local Coef : rownames Means
local Equ  : roweq Means
			
qui forvalues i=1/`: word count `Coef'' {
replace Coef =`"`: word `i' of `Coef''"' in `i'
replace Equ  =`"`: word `i' of `Equ''"'  in `i'
}
qui gen CropIDx= "`crop'"
mat drop Means
if `ApID0' > 0 append using `Summaries'
save `Summaries', replace
loc ApID0=`ApID0'+1
}		
			
use `Summaries', clear

export excel CropIDx Equ Coef Beta SE Tv Pv Min Max SD N /*
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_land_tenure\results\tech_inefficiency_land_tenure_results.xlsx", /*
*/ sheet("Means") sheetmodify firstrow(variables) 

