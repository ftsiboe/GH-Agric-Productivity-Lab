
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_extension\data\tech_inefficiency_extension_data",clear
decode CropID,gen(CropIDx)
*keep if CropIDx == "Pooled"
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
*loc crop "Pooled"
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_extension\data\tech_inefficiency_extension_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "`crop'"
gen extCat = ext_available

sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)

mat Means=J(1,8,.)
qui foreach Var of var Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt AgeYr YerEdu HHSizeAE Depend CrpMix {
preserve
cap{
*loc Var Yield
reg `Var' c.Trend##i.extCat, vce(cluster Clust) 
qui est store Model
testparm i.extCat						//mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.extCat			//trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins extCat, eydx(Trend) grand coefl post
nlcom ("Trend_extCat0":_b[Trend:0bn.extCat]*100) ("Trend_extCat1":_b[Trend:1.extCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(extCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_extCat0 Trend_extCat1 Trend_Pooled CATDif TrendDif Mean_extCat0 Mean_extCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(extCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_extCat0 `sx'_extCat1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

qui foreach Var of var Female EqipMech Credit OwnLnd EqipIrig{
preserve
cap{
*Overall and regional means 
qui logit `Var' c.Trend##i.extCat, vce(cluster Clust) 
qui est store Model
testparm i.extCat						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.extCat			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins extCat, eydx(Trend) grand coefl post
nlcom ("Trend_extCat0":_b[Trend:0bn.extCat]*100) ("Trend_extCat1":_b[Trend:1.extCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(extCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_extCat0 Trend_extCat1 Trend_Pooled CATDif TrendDif Mean_extCat0 Mean_extCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(extCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_extCat0 `sx'_extCat1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

tab extCat,gen(extCat)
ren (extCat1 extCat2) (extCat0 extCat1)

qui foreach Var of var extCat0 extCat1{
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
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_extension\results\tech_inefficiency_extension_results.xlsx", /*
*/ sheet("Means") sheetmodify firstrow(variables) 

