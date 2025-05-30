use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_disability\data\tech_inefficiency_disability_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "Pooled"
qui levelsof CropIDx, local(levels)
tab disabled

qui foreach disab in disabled disabled_self disabled_spouse disabled_child disabled_close disabled_member{
	
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_disability\data\tech_inefficiency_disability_data",clear
decode CropID,gen(CropIDx)
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
  
*loc crop "Pooled"
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_disability\data\tech_inefficiency_disability_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "`crop'"
gen disagCat = `disab'
cap{

sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)

mat Means=J(1,8,.)
qui foreach Var of var Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt AgeYr YerEdu HHSizeAE Depend CrpMix {
preserve
cap{
*loc Var Yield
reg `Var' c.Trend##i.disagCat, vce(cluster Clust) 
qui est store Model
testparm i.disagCat						//mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.disagCat			//trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins disagCat, eydx(Trend) grand coefl post
nlcom ("Trend_disagCat0":_b[Trend:0bn.disagCat]*100) ("Trend_disagCat1":_b[Trend:1.disagCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(disagCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_disagCat0 Trend_disagCat1 Trend_Pooled CATDif TrendDif Mean_disagCat0 Mean_disagCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(disagCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_disagCat0 `sx'_disagCat1 `sx'_Pooled
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
qui logit `Var' c.Trend##i.disagCat, vce(cluster Clust) 
qui est store Model
testparm i.disagCat						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.disagCat			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins disagCat, eydx(Trend) grand coefl post
nlcom ("Trend_disagCat0":_b[Trend:0bn.disagCat]*100) ("Trend_disagCat1":_b[Trend:1.disagCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(disagCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_disagCat0 Trend_disagCat1 Trend_Pooled CATDif TrendDif Mean_disagCat0 Mean_disagCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(disagCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_disagCat0 `sx'_disagCat1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

tab disagCat,gen(disagCat)
ren (disagCat1 disagCat2) (disagCat0 disagCat1)

qui foreach Var of var disagCat0 disagCat1{
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
}		

use `Summaries', clear

export excel CropIDx Equ Coef Beta SE Tv Pv Min Max SD N /*
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_disability\results\tech_inefficiency_disability_results.xlsx", /*
*/ sheet("Means_`disab'") sheetmodify firstrow(variables) 

}

mat drop _all
sca drop _all
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_disability\data\tech_inefficiency_disability_data",clear
decode CropID,gen(CropIDx)
tabstat disabled disabCat* if CropIDx == "Cassava",by(Surveyx) save
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)
mat Means=J(1,8,.)

qui foreach Var in disabled disabCat1 disabCat2 disabCat3 disabCat4 disabCat5 disabCat6 disabCat7{
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var disabled
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			
			*Overall and regional means 
			qui logit `Var' i.Survey, vce(cluster Clust) 
			margins Survey, grand coefl post
			nlcom ("Trend":(_b[6bn.Survey]-_b[7.Survey])*100), post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1...,1..8]
			tabstat `Var' , stat(mean sem min max sd n) by(Surveyx) save
			foreach mt in Stat1 Stat2 StatTotal{
				mat B = r(`mt')'
				mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
				mat A =A\B
				mat drop B
			}

			mat rownames A = "`crop'_Trend" "`crop'_GLSS6" "`crop'_GLSS7" "`crop'_GLSS0"
			mat roweq A= `Var'
			mat Means = A\Means	
			mat drop A
		}
		restore
	}
}

mat li Means

mat colnames Means = Beta SE Tv Pv Min Max SD N

qui clear
qui svmat Means, names(col)
qui gen Coef=""
qui gen Variable=""
local Coef : rownames Means
local Variable  : roweq Means
			
qui forvalues i=1/`: word count `Coef'' {
replace Coef =`"`: word `i' of `Coef''"' in `i'
replace Variable  =`"`: word `i' of `Variable''"'  in `i'
}

split Coef, p("_") limit(2)
ren (Coef1 Coef2) (crop mesure)
keep Variable crop mesure Beta SE Tv Pv Min Max SD N
order Variable crop mesure Beta SE Tv Pv Min Max SD N

export excel Variable crop mesure Beta SE Tv Pv Min Max SD N /*
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_disability\results\tech_inefficiency_disability_results.xlsx", /*
*/ sheet("disability") sheetmodify firstrow(variables) 
