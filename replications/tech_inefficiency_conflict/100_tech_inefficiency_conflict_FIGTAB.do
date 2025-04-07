
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_conflict\data\tech_inefficiency_conflict_data",clear
decode CropID,gen(CropIDx)
*keep if CropIDx == "Pooled"
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
*loc crop "Pooled"
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_conflict\data\tech_inefficiency_conflict_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "`crop'"

sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)

mat Means=J(1,8,.)
qui foreach Var of var Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt AgeYr YerEdu HHSizeAE Depend CrpMix /*
*/ index0 index1 index2 index3 index4 index5 index6{
preserve
cap{
reg `Var' c.Trend##i.index0CAT, vce(cluster Clust) 
qui est store Model
testparm i.index0CAT						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.index0CAT			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins index0CAT, eydx(Trend) grand coefl post
nlcom ("Trend_index0CAT0":_b[Trend:0bn.index0CAT]*100) ("Trend_index0CAT1":_b[Trend:1.index0CAT]*100) ("Trend_index0CAT2":_b[Trend:2.index0CAT]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(index0CAT) save
foreach mt in Stat1 Stat2 Stat3 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_index0CAT0 Trend_index0CAT1 Trend_index0CAT2 Trend_Pooled GenderDif TrendDif Mean_index0CAT0 Mean_index0CAT1 Mean_index0CAT2 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(index0CAT) save
	foreach mt in Stat1 Stat2 Stat3 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_index0CAT0 `sx'_index0CAT1 `sx'_index0CAT2 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

qui foreach Var of var Female OwnLnd EqipMech Credit Extension EqipIrig{
preserve
cap{
*Overall and regional means 
qui logit `Var' c.Trend##i.index0CAT, vce(cluster Clust) 
qui est store Model
testparm i.index0CAT						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.index0CAT			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins index0CAT, eydx(Trend) grand coefl post
nlcom ("Trend_index0CAT0":_b[Trend:0bn.index0CAT]*100) ("Trend_index0CAT1":_b[Trend:1.index0CAT]*100) ("Trend_index0CAT2":_b[Trend:2.index0CAT]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(index0CAT) save
foreach mt in Stat1 Stat2 Stat3 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_index0CAT0 Trend_index0CAT1 Trend_index0CAT2 Trend_Pooled GenderDif TrendDif Mean_index0CAT0 Mean_index0CAT1 Mean_index0CAT2 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(index0CAT) save
	foreach mt in Stat1 Stat2 Stat3 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_index0CAT0 `sx'_index0CAT1 `sx'_index0CAT2 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

tab index0CAT,gen(index0CAT)
ren (index0CAT1 index0CAT2 index0CAT3) (index0CAT0 index0CAT1 index0CAT2)

qui foreach Var of var index0CAT0 index0CAT1 index0CAT2{
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
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_conflict\results\tech_inefficiency_conflict_results.xlsx", /*
*/ sheet("Means") sheetmodify firstrow(variables) 


