use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_nonfarm_enterprise_data",clear
merg 1:m Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if _merge==3
drop _merge Lnd* EduWhyNo RentHa
keep if inlist(Surveyx,"GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_nonfarm_enterprise\data\tech_inefficiency_nonfarm_enterprise_data",replace ver(12)