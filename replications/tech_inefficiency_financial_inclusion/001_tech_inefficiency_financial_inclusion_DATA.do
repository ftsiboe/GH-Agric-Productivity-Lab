/**************************************************************************
 * Filename: 001_tech_inefficiency_financial_inclusion_DATA.do
 * Author: Francis Tsiaboe (ftsiboe)
 * Date: 2025-04-05
 *
 * Purpose:
 * This script is designed to process and analyze data related to technology 
 * inefficiency and financial inclusion in Ghana. The objective is to ascertain 
 * whether observed production shortfalls in Ghana are solely due to farmer 
 * inefficiency, technology gaps, or some combination of the two.
 *
 * Directions for Citing:
 * When using this script or any part of this analysis in your work, please cite 
 * it as follows:
 * Tsiaboe, Francis. "Tech Inefficiency and Financial Inclusion Data Analysis." 
 * GitHub, 2025. https://github.com/ftsiboe/GH-Agric-Productivity-Lab
 **************************************************************************/

// Load the harmonized crop farmer data and clear the current dataset
use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data", clear

// Collapse the data to get the mean of specified variables by the grouping variables
collapse (mean) YerEdu EduLevel AgeYr Female WeightHH, by(Surveyx EaId HhId Mid Locality Head)

// Merge the current data with the harmonized financial inclusion data on specified keys
merge 1:1 Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_financial_inclusion_data"
keep if _merge==3 // Keep only the matched records
 
// Decode the 'Locality' variable to create a new variable 'Localityx'
decode Locality, gen(Localityx)

// Generate summary statistics for specified variables by the 'Surveyx' grouping variable
tabstat Credit LoanAmt Bank_Info Banked Insured InstTyp_* AccTyp_* PrdTyp_* YerEdu AgeYr Head, by(Surveyx)

// Principal component analysis (PCA)
loc Person FinWorker YerEdu HHFinWorker // Variables related to the person
loc Insured Insured_*   // Variables related to insurance
loc Banked Banked       // Variables related to banking
loc InstTyp InstTyp_*   // Variables related to types of financial institutions
loc AccTyp AccTyp_*     // Variables related to types of accounts
loc PrdTyp PrdTyp_*     // Variables related to types of transaction products
loc Loan Credit LoanAmt // Variables related to loans
loc Community BankKm RoadKm TrnprtKm  // Variables related to the community

// Combine all the above variables into a single local macro 'Factors'
loc Factors `Person' `Insured' `Banked' `InstTyp' `AccTyp' `PrdTyp' `Loan' `Community'

// Drop observations where any of the 'Factors' variables are missing
for var `Person' `Insured' `Banked' `InstTyp' `AccTyp' `PrdTyp' `Loan' `Community': drop if X==.
sum HhId `Person' `Insured' `Banked' `InstTyp' `AccTyp' `PrdTyp' `Loan' `Community'

// Perform PCA on the 'Factors' variables
pca `Factors', vce(nor) com(1)
mat A = r(table)' // Store the results in matrix A
mat B = J(5,4,.)  // Create an empty 5x4 matrix B

// Store various PCA statistics in matrix B
mat B[1,1] = e(N)        // Number of observations 
mat B[2,1] = e(rho)      // Fraction of explained variance
mat B[3,1] = e(v_rho)    // Variance of explained variance
mat B[4,1] = e(chi2_i)   // Chi-squared statistic for test of independence
mat B[4,4] = e(p_i)      // Significance of test of independence
mat B[5,1] = e(chi2_s)   // Chi-squared statistic for test of sphericity
mat B[5,4] = e(p_s)      // Significance of test of sphericity       
mat rownames B = N_0 rho_0 v_rho_0 chiI_0 chiS_0 // Set row names for matrix B

// Combine matrix A and matrix B into a new matrix FinIdx0
mat FinIdx0 = A[1....,1..4]\B

// Predict the principal component scores and store them in the variable COM
qui predict COM

// Loop through surveys and localities to perform PCA separately for each group
qui foreach sur in "GLSS6" "GLSS7" {
	qui foreach loc in "Rural" "Urban" {
		loc varlist 
		
		foreach x in `Factors' {
			// Summarize the variable 'x' for each survey and locality combination
			sum `x' if Surveyx == "`sur'" & Localityx == "`loc'"
			if `r(sd)' !=0 loc varlist `varlist' `x' // Append 'x' to 'varlist' if standard deviation is not zero
		} 
		
		// Perform PCA on the 'varlist' for each survey and locality combination
		pca `varlist' if Surveyx == "`sur'", vce(nor) com(1)
		mat A = r(table)' // Store the results in matrix A
		mat B = J(5,4,.)  // Create an empty 5x4 matrix B

		// Store various PCA statistics in matrix B
		mat B[1,1] = e(N)        // Number of observations 
		mat B[2,1] = e(rho)      // Fraction of explained variance
		mat B[3,1] = e(v_rho)    // Variance of explained variance
		mat B[4,1] = e(chi2_i)   // Chi-squared statistic for test of independence
		mat B[4,4] = e(p_i)      // Significance of test of independence
		mat B[5,1] = e(chi2_s)   // Chi-squared statistic for test of sphericity
		mat B[5,4] = e(p_s)      // Significance of test of sphericity       
		mat rownames B = N_`sur' rho_`sur' v_rho_`sur' chiI_`sur' chiS_`sur' // Set row names for matrix B
		
		// Combine matrix A and matrix B into a new matrix FinIdx0
		mat FinIdx0 = A[1....,1..4]\B
		
		// Predict the principal component scores and store them in the variable temp1
		qui predict temp1 if Surveyx == "`sur'" & Localityx == "`loc'"
		// Perform regression of COM on temp1 for each survey and locality combination
		reg COM temp1 if Surveyx == "`sur'" & Localityx == "`loc'"
		// Predict the financial index and store it in the variable FinIdx_`sur'_`loc'
		predict FinIdx_`sur'_`loc' if Surveyx == "`sur'" & Localityx == "`loc'"
		drop temp1 // Drop the temporary variable temp1
  }
}

// Generate a new variable FinIdx as the row mean of the financial indices
egen FinIdx = rowmean(FinIdx_*)

// Summarize the FinIdx variable
sum FinIdx

// Generate a new scaled financial index variable FinIdxSi
gen FinIdxSi = (FinIdx-r(min))/(r(max)-r(min))

// Generate a new categorical financial index variable FinIdxCat with 5 quantiles
xtile FinIdxCat = FinIdx [pw=WeightHH] , nq(5) 

// Keep only the specified variables
keep HhId EaId Mid Surveyx FinIdx FinIdxSi FinIdxCat

// Merge the current data with the harmonized financial inclusion data on specified keys
merge 1:1 Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_financial_inclusion_data"
keep if _merge==3 // Keep only the matched records
rename Credit CreditCat // Rename the 'Credit' variable to 'CreditCat'
drop _merge // Drop the '_merge' variable

// Merge the current data with the harmonized crop farmer data on specified keys
merge 1:m Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if _merge==3 // Keep only the matched records
drop _merge // Drop the '_merge' variable

replace Credit = LoanAmt > 0
// Compress the data to save space
compress

// Save the final dataset in Stata version 12 format
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_financial_inclusion\data\tech_inefficiency_financial_inclusion_data", replace ver(12)
