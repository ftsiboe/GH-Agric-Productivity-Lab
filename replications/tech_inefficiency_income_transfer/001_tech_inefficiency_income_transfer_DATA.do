/**************************************************************************
 * Filename: 001_tech_inefficiency_income_transfer_DATA.do
 * Author: Francis Tsiaboe (ftsiboe)
 * Date: 2025-04-05
 *
 * Purpose:
 * This script is designed to process and analyze data related to technology 
 * inefficiency and income transfer in Ghana. The objective is to ascertain 
 * whether observed production shortfalls in Ghana are solely due to farmer 
 * inefficiency, technology gaps, or some combination of the two.
 *
 * Directions for Citing:
 * When using this script or any part of this analysis in your work, please cite 
 * it as follows:
 * Tsiaboe, Francis. "Tech Inefficiency and income transfer Data Analysis." 
 * GitHub, 2025. https://github.com/ftsiboe/GH-Agric-Productivity-Lab
 **************************************************************************/

use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_income_transfer_data",clear
merg 1:m Surveyx EaId HhId using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
drop if _merge==1
for var remitt* : replace X=0 if X==. & inlist(remittance_amount_total,0,.)
keep if inlist(Surveyx,"GLSS5","GLSS6","GLSS7")
tabstat remitt*,by(Surveyx) stat(sd)
drop _merge

gen transfer = remittance_amount_total > 0
for var remittance_as_* remittance_for_*:gen _X = X > 0
ren _remittance_as_* transfer_*
ren _remittance_for_* transfer_*

for var transfer_*: replace X=. if X==0 & transfer == 1

tabstat transfer_*,by(Surveyx) stat(sd)
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_income_transfer\data\tech_inefficiency_income_transfer_data",replace ver(12)

