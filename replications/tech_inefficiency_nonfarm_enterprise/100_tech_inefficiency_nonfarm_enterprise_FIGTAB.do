use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_financial_inclusion\data\tech_inefficiency_financial_inclusion_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "Pooled"
qui levelsof CropIDx, local(levels)
tab credit_hh

qui foreach disab in credit_hh credit_self credit_spouse credit_child credit_close credit_member{
	
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_financial_inclusion\data\tech_inefficiency_financial_inclusion_data",clear
decode CropID,gen(CropIDx)
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
  
*loc crop "Pooled"
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_financial_inclusion\data\tech_inefficiency_financial_inclusion_data",clear
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

qui foreach Var of var Female EqipMech OwnLnd EqipIrig{
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
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_financial_inclusion\results\tech_inefficiency_financial_inclusion_results.xlsx", /*
*/ sheet("Means_`disab'") sheetmodify firstrow(variables) 

}



mat drop _all
sca drop _all
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_financial_inclusion\data\tech_inefficiency_financial_inclusion_data",clear
tab FinIdxCat,gen(FinIdxCatx)
decode CropID,gen(CropIDx)
unab Person: FinWorker YerEdu HHFinWorker // Variables related to the person
unab Insured: Insured_*   // Variables related to insurance
unab Banked: Banked Bank_Info_* NonBanked_Why_* // Variables related to banking
unab InstTyp: InstTyp_*   // Variables related to types of financial institutions
unab AccTyp: AccTyp_*     // Variables related to types of accounts
unab PrdTyp: PrdTyp_*   // Variables related to types of transaction products
unab Community: BankKm RoadKm TrnprtKm  // Variables related to the community

// Combine all the above variables into a single local macro 'Factors'
loc Factors credit_self `Insured' `Banked' `InstTyp' `AccTyp' `PrdTyp' `Community' FinIdxCatx* 

tabstat `Factors' if CropIDx == "Pooled",by(Surveyx) save
keep if CropIDx == "Pooled"
sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)
mat Means=J(1,8,.)

qui foreach Var in FinIdx FinIdxSi `Person'{
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var LoanAmt
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			*sum `Var' Trend
			*Overall and regional means 
			qui reg `Var' Trend, vce(cluster Clust) 
			margins, eydx(Trend) grand coefl post
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


qui foreach Var in `Factors' Applied {
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var InstTyp_Momo
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			
			mat A = J(1,8,.)
			tab Survey if `Var' == 1
			if(`r(r)' > 1){
				*Overall and regional means 
				qui logit `Var' i.Survey, vce(cluster Clust) 
				margins Survey, grand coefl post
				nlcom ("Trend":(_b[6bn.Survey]-_b[7.Survey])*100), post
				qui ereturn display
				mat A = r(table)'
				mat A = A[1...,1..8]  
			}
		
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

qui foreach Var in Refused Accept Proces{
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var InstTyp_Momo
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			keep if Applied == 1
			
			mat A = J(1,8,.)
			tab Survey if `Var' == 1
			if(`r(r)' > 1){
				*Overall and regional means 
				qui logit `Var' i.Survey, vce(cluster Clust) 
				margins Survey, grand coefl post
				nlcom ("Trend":(_b[6bn.Survey]-_b[7.Survey])*100), post
				qui ereturn display
				mat A = r(table)'
				mat A = A[1...,1..8]  
			}
		
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

unab loaners: Source_* Collateral_* Use_*

qui foreach Var in `loaners'  {
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var InstTyp_Momo
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			keep if Applied*Accept == 1
			
			mat A = J(1,8,.)
			tab Survey if `Var' == 1
			if(`r(r)' > 1){
				*Overall and regional means 
				qui logit `Var' i.Survey, vce(cluster Clust) 
				margins Survey, grand coefl post
				nlcom ("Trend":(_b[6bn.Survey]-_b[7.Survey])*100), post
				qui ereturn display
				mat A = r(table)'
				mat A = A[1...,1..8]  
			}
		
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

qui foreach Var in LoanAmt RePaid{
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var LoanAmt
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			keep if Applied*Accept == 1
			*sum `Var' Trend
			*Overall and regional means 
			qui reg `Var' Trend, vce(cluster Clust) 
			margins, eydx(Trend) grand coefl post
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

unab loaners: Refusal_*

qui foreach Var in `loaners' {
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var InstTyp_Momo
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			keep if Applied*Refused == 1
			
			mat A = J(1,8,.)
			tab Survey if `Var' == 1
			if(`r(r)' > 1){
				*Overall and regional means 
				qui logit `Var' i.Survey, vce(cluster Clust) 
				margins Survey, grand coefl post
				nlcom ("Trend":(_b[6bn.Survey]-_b[7.Survey])*100), post
				qui ereturn display
				mat A = r(table)'
				mat A = A[1...,1..8]  
			}
		
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

unab loaners: WhyNoLoan_*

qui foreach Var in `loaners' {
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var InstTyp_Momo
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			keep if Applied == 0
			
			mat A = J(1,8,.)
			tab Survey if `Var' == 1
			if(`r(r)' > 1){
				*Overall and regional means 
				qui logit `Var' i.Survey, vce(cluster Clust) 
				margins Survey, grand coefl post
				nlcom ("Trend":(_b[6bn.Survey]-_b[7.Survey])*100), post
				qui ereturn display
				mat A = r(table)'
				mat A = A[1...,1..8]  
			}
		
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
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_financial_inclusion\results\tech_inefficiency_financial_inclusion_results.xlsx", /*
*/ sheet("inclusion") sheetmodify firstrow(variables) 
