set maxvar 15000
use "E:\Data\South Africa\Deaths\Deaths2022\Deaths2022_MRCversionFINAL.dta" ,clear
capture drop covid
gen count=1
keep if inrange(epi_year,2014,2022)
keep if Nat==1&DeathType==1
drop if (UnderlyingCause =="U51") | (UnderlyingCause=="U52") | (UnderlyingCause=="W34")
drop if weekstart==.
drop if age==. | age<0
ren Sex sex
drop if sex >2
egen agegroup = cut(age), at(0, 5, 40, 60, 70, 80, 150)

gen LGH_MainGroup=Code_Main

recode LGH (5=21) (7/8=21) (11/13=21)  (15=21) (17=21)
gen covid = 0
gen LGH_CauseGroup = UnderlyingCause
replace covid =1 if LGH_CauseGroup =="B24" | LGH_CauseGroup =="B33"
replace covid =1 if LGH_CauseGroup =="E10" | LGH_CauseGroup =="E11" | LGH_CauseGroup =="E12" | LGH_CauseGroup =="E13" | LGH_CauseGroup =="E14"
replace LGH_CauseGroup="E10-E14" if LGH_CauseGroup =="E10" | LGH_CauseGroup =="E11" | LGH_CauseGroup =="E12" | LGH_CauseGroup =="E13" | LGH_CauseGroup =="E14"
replace covid =1 if LGH_CauseGroup =="I10" | LGH_CauseGroup =="I11" | LGH_CauseGroup =="I12" | LGH_CauseGroup =="I13" | LGH_CauseGroup =="I14"  | LGH_CauseGroup =="I15"
replace LGH_CauseGroup="I10-I15" if LGH_CauseGroup =="I10" | LGH_CauseGroup =="I11" | LGH_CauseGroup =="I12" | LGH_CauseGroup =="I13" | LGH_CauseGroup =="I14" | LGH_CauseGroup =="I15"
replace covid =1 if LGH_CauseGroup =="I20" | LGH_CauseGroup =="I21" | LGH_CauseGroup =="I22" | LGH_CauseGroup =="I23" | LGH_CauseGroup =="I24"  | LGH_CauseGroup =="I25"
replace LGH_CauseGroup="I20-I25" if LGH_CauseGroup =="I20" | LGH_CauseGroup =="I21" | LGH_CauseGroup =="I22" | LGH_CauseGroup =="I23" | LGH_CauseGroup =="I24" | LGH_CauseGroup =="I25"
replace covid =1 if LGH_CauseGroup =="I26" | LGH_CauseGroup =="I27" | LGH_CauseGroup =="I28"
replace LGH_CauseGroup="I26-I28" if LGH_CauseGroup =="I26" | LGH_CauseGroup =="I27" | LGH_CauseGroup =="I28"
replace covid =1 if LGH_CauseGroup =="I42"
replace covid =1 if LGH_CauseGroup =="I49" | LGH_CauseGroup =="I50" | LGH_CauseGroup =="I51"
replace LGH_CauseGroup="I49-I51" if LGH_CauseGroup =="I49" | LGH_CauseGroup =="I50" | LGH_CauseGroup =="I51"
replace covid =1 if LGH_CauseGroup =="I60" | LGH_CauseGroup =="I61" | LGH_CauseGroup =="I62" | LGH_CauseGroup =="I63" | LGH_CauseGroup =="I64"  | LGH_CauseGroup =="I65"  | LGH_CauseGroup =="I66" | LGH_CauseGroup =="I67" | LGH_CauseGroup =="I68" | LGH_CauseGroup =="I69"
replace LGH_CauseGroup="I60-I69" if LGH_CauseGroup =="I60" | LGH_CauseGroup =="I61" | LGH_CauseGroup =="I62" | LGH_CauseGroup =="I63" | LGH_CauseGroup =="I64"  | LGH_CauseGroup =="I65"  | LGH_CauseGroup =="I66" | LGH_CauseGroup =="I67" | LGH_CauseGroup =="I68" | LGH_CauseGroup =="I69"
replace covid =1 if LGH_CauseGroup =="J09" | LGH_CauseGroup =="J10" | LGH_CauseGroup =="J11" | LGH_CauseGroup =="J12" | LGH_CauseGroup =="J13"  | LGH_CauseGroup =="J14"  | LGH_CauseGroup =="J15" | LGH_CauseGroup =="J16" | LGH_CauseGroup =="J17" | LGH_CauseGroup =="J18"
replace LGH_CauseGroup="J09-J18" if LGH_CauseGroup =="J09" | LGH_CauseGroup =="J10" | LGH_CauseGroup =="J11" | LGH_CauseGroup =="J12" | LGH_CauseGroup =="J13"  | LGH_CauseGroup =="J14"  | LGH_CauseGroup =="J15" | LGH_CauseGroup =="J16" | LGH_CauseGroup =="J17" | LGH_CauseGroup =="J18"
replace covid =1 if LGH_CauseGroup =="J20" | LGH_CauseGroup =="J21" | LGH_CauseGroup =="J22"
replace LGH_CauseGroup = "J20-J22" if LGH_CauseGroup =="J20" | LGH_CauseGroup =="J21" | LGH_CauseGroup =="J22"
replace covid =1 if LGH_CauseGroup =="J45"
replace covid =1 if LGH_CauseGroup =="J80"
replace covid =1 if LGH_CauseGroup =="J96" | LGH_CauseGroup =="J97" | LGH_CauseGroup =="J98"
replace LGH_CauseGroup="J96-J98" if LGH_CauseGroup =="J96" | LGH_CauseGroup =="J97" | LGH_CauseGroup =="J98"
replace covid =1 if LGH_CauseGroup =="N17" | LGH_CauseGroup =="N18" | LGH_CauseGroup =="N19"
replace LGH_CauseGroup="N17-N19" if LGH_CauseGroup =="N17" | LGH_CauseGroup =="N18" | LGH_CauseGroup =="N19"

replace covid =1 if strpos(UnderlyingCause,"R")==1
replace covid =1 if LGH_CauseGroup =="I46"
replace LGH_CauseGroup="R00-R99+I46" if LGH_CauseGroup =="I46"
replace LGH_CauseGroup="R00-R99+I46" if LGH_Main==18
replace covid = 1 if LGH_CauseGroup =="U07"

replace LGH_CauseGroup =substr(UnderlyingCause,1,1) if covid==0
label define LGH_MainGroup 1"A00-B99" 2"C00-D48" 3"D50-D99" 4"E00-E90" 6"G00-G99" 9"I00-I99" 10"J00-J99" 14"N00-N99" 16"P00-P99" 18"R00-R99+I46" 20"Covid" 21"Others (F/H/K-M/O/Q)"
la val LGH_MainGroup LGH_MainGroup
replace LGH_Cause = "Z" if strlen(LGH_Cause)==1

replace LGH_Cause = "A00-B99" if LGH_Main ==1 & covid==0
replace LGH_Cause = "C00-D48" if LGH_Main ==2 & covid==0
replace LGH_Cause = "D50-D99" if LGH_Main ==3 & covid==0
replace LGH_Cause = "E00-E99*" if LGH_Main ==4 & covid==0
replace LGH_Cause = "G00-G99" if LGH_Main ==6 & covid==0
replace LGH_Cause = "I00-I99*" if LGH_Main ==9 & covid==0
replace LGH_Cause = "J00-J99*" if LGH_Main ==10 & covid==0
replace LGH_Cause = "N00-N99*" if LGH_Main ==14 & covid==0
replace LGH_Cause = "P00-P99" if LGH_Main ==16 & covid==0

replace LGH_Cause = "U07" if LGH_Main ==20 & covid==0
replace LGH_Cause = "ZZOthers (F/H/K-M/O/Q)" if LGH_Main ==21 & covid==0
encode LGH_Cause, gen(LGH_Cause_encoded)

collapse (sum) count, by(agegroup sex LGH_Cause_encoded covid weekstart epi_week epi_year)
la def agegroup 0"0-4" 5"5-39" 40"40-59" 60"60-69" 70"70-79" 80"80+"
la val agegroup agegroup
save "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",replace

** create baseline
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
keep if inrange(epi_year,2015,2019)
collapse (sum) count, by(epi_week epi_year sex age LGH)

	**new from here**
	nbreg count epi_year i.sex#i.agegroup#i.LGH i.epi_week#i.LGH if epi_week!=53
	merge 1:1 sex epi_week epi_year agegroup LGH_Cause_encoded using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_template_2022.dta"	
	drop if epi_year==2014
	predict baseline, xb
	replace baseline = exp(baseline)
	replace baseline = 0 if LGH==26								//COVID
	egen wy = concat(epi_year epi_week), p(w)
	epiweek2 wy, s(from) e(to)
	drop wy to
	rename from weekstart
	sort epi_year epi_week
	drop _merge 
	order epi_week epi_year weekstart count baseline
	* end new 

replace baseline = 0 if LGH==26
sort epi_year epi_week age sex LGH
replace baseline = (baseline[_n-324]+baseline[_n+324])/2 if epi_week==53

drop count
compress
save "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta", replace


***START ANALYSIS HERE***


** ALL NATURAL COD
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge


collapse (sum) count (sum) baseline, by(weekstart)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2017,2022)
sort week
la var count "Reported deaths"
la var baseline "Baseline deaths (2015-2019)"
twoway (line count weekstart,lcolor(red))||(line baseline weekstart,lcolor(blue)), legend(position(6) row(1)) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2017 1Jan2018 1Jan2019 1Jan2020 1Jan2021 1Jan2022 1Jan2023  ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\AllNatural_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), legend(position(6) row(1)) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2017 1Jan2018 1Jan2019 1Jan2020 1Jan2021 1Jan2022 1Jan2023 ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\AllNatural_excess.png", as(png) replace

** COD
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge

collapse (sum) count (sum) baseline, by(weekstart LGH)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort LGH weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(LGH,yrescale legend(off) note("Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023,alternate  ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\COD_count.png", as(png) replace


gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(LGH,yrescale legend(off) note("Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023,alternate ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\COD_excess.png", as(png) replace



** SEX
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge

collapse (sum) count (sum) baseline, by(weekstart sex)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort sex weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(sex, legend(off) note("")) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate ) xtitle("") yscale(range(0 12000)) ylabel(0(2000)12000)
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Sex_count.png", as(png) replace


gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(sex,yrescale legend(off)) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate ) xtitle("")yscale(range(0 8000)) ylabel(0(1000)8000) note("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Sex_excess.png", as(png) replace


******************************************************************
************************ AGE *************************************
******************************************************************

use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge

collapse (sum) count (sum) baseline, by(weekstart age)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort age weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(age, yrescale legend(size(small) position(6)) note("")) legend(row(1) size(small) symxsize(5)) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023,alternate labsize(medsmall)) xtitle("") 
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\AgeGroups_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(age, yrescale legend(off) note("")) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\AgeGroups_excess.png", as(png) replace

***************
** AGES 0-4 **
***************
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
keep if age==0
*drop if covid==0
bysort LGH: egen maxvalLGH = max(count)
drop if maxvalLGH <10

collapse (sum) count (sum) baseline, by(weekstart LGH)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort LGH weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(LGH, yrescale legend(size(small) position(6))  note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) legend(row(1) size(small) symxsize(5)) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age0_CoD_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(LGH,yrescale legend(off) note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age0_CoD_excess.png", as(png) replace


***************
** AGES 5-39 **
***************
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
keep if age==5
*drop if covid==0
bysort LGH: egen maxvalLGH = max(count)
drop if maxvalLGH <10

collapse (sum) count (sum) baseline, by(weekstart LGH)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort LGH weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(LGH, yrescale legend(size(small) position(6))  note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) legend(row(1) size(small) symxsize(5)) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age5_CoD_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(LGH,yrescale legend(off) note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023 , alternate ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age5_CoD_excess.png", as(png) replace

***************
** AGES 40-59 **
***************
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
keep if age==40
*drop if covid==0
bysort LGH: egen maxvalLGH = max(count)
drop if maxvalLGH <10

collapse (sum) count (sum) baseline, by(weekstart LGH)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort LGH weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(LGH, yrescale legend(size(small) position(6))  note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) legend(row(1) size(small) symxsize(5)) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age40_CoD_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(LGH,yrescale legend(off) note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023,alternate  ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age40_CoD_excess.png", as(png) replace

****************
** AGES 60-69 **
****************
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
keep if age==60
*drop if covid==0
bysort LGH: egen maxvalLGH = max(count)
drop if maxvalLGH <10

collapse (sum) count (sum) baseline, by(weekstart LGH)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort LGH weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(LGH, yrescale legend(size(small) position(6))  note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) legend(row(1) size(small) symxsize(5)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age60_CoD_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(LGH,yrescale legend(off) note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate  ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age60_CoD_excess.png", as(png) replace



****************
** AGES 70-79 **
****************
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
keep if age==70
*drop if covid==0
bysort LGH: egen maxvalLGH = max(count)
drop if maxvalLGH <10

collapse (sum) count (sum) baseline, by(weekstart LGH)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort LGH weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(LGH, yrescale legend(size(small) position(6))  note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) legend(row(1) size(small) symxsize(5)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age70_CoD_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(LGH,yrescale legend(off) note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate  ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age70_CoD_excess.png", as(png) replace


****************
**  AGES 80+  **
****************
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
keep if age==80
*drop if covid==0
bysort LGH: egen maxvalLGH = max(count)
drop if maxvalLGH <10

collapse (sum) count (sum) baseline, by(weekstart LGH)
epiweek weekstart, epiw(epiweek) epiy(epiyear)
drop if weekstart==.
keep if inrange(epiyear,2020,2022)
sort LGH weeks
la var baseline "Baseline deaths (2015-2019)"
la var count "Reported deaths"
twoway line count baseline weekstart ,by(LGH, yrescale legend(size(small) position(6))  note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) legend(row(1) size(small) symxsize(5)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age80_CoD_count.png", as(png) replace

gen excess = count-baseline
la var excess "Excess natural deaths"
twoway (line excess  weekstart,lcolor(blue)), by(LGH,yrescale legend(off) note("(Grouped) causes where maximum observed weekly deaths is less than 10 have been omitted" "Grouped causes of death marked with * EXCLUDE separately analysed causes of death in that group",size(vsmall))) tscale(range(1Nov2019 1Feb2023)) tlabel(1Jan2020 1Jan2021 1Jan2022 1Jan2023, alternate  ) xtitle("")
graph export "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\Age80_CoD_excess.png", as(png) replace



*****TABLES****
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"

drop if _merge==2
drop _merge
keep if inrange(epi_year,2020,2022)
merge m:1 weekstart using "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_waves_defn_2022.dta"
drop _merge



*drop if covid==0

gen excess = count-baseline
table epi_year [iw=count], nformat(%9.0f)
table epi_year [iw=excess], nformat(%9.0f)
table agegr epi_year [iw=count], nformat(%9.0f)
table agegr epi_year [iw=excess], nformat(%9.0f)
table sex epi_year [iw=count], nformat(%9.0f)
table sex epi_year [iw=excess], nformat(%9.0f)
table LGH epi_year [iw=count], nformat(%9.0f)
table LGH epi_year [iw=excess], nformat(%9.0f)
table wave LGH (sex agegroup) [iw=count], nformat(%9.0f) nototals
collapse (sum) count (sum) excess (sum) baseline, by(wave LGH sex agegroup)


***heatmap tables
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"

drop if _merge==2
drop _merge
keep if inrange(epi_year,2020,2022)
merge m:1 weekstart using "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_waves_defn_2022.dta"
drop _merge
gen excess = count-baseline
collapse (sum) count (sum) excess, by(epi_year covid age LGH)





** create SAMRC comparison
use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
gen excess = count- baseline
drop if !inrange(epi_year,2020,2022)
collapse (sum) excess, by(weekstart covid)
table week covid [iw= excess], nformat(%11.3f)

use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"
drop if _merge==2
drop _merge
gen excess = count- baseline
drop if !inrange(epi_year,2020,2022)
gen covid_signal = 1 if covid==1 & LGH==26
replace covid_signal = 0 if covid==1 & LGH!=26
table epi_year covid_signal [iw=excess], nformat(%12.3f)
table weekstart covid_signal [iw=excess], nformat(%12.3f)


*WAVES COMPARISON TABLE

use "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_collapsed2022.dta",clear
merge m:1 sex epi_week epi_year age LGH using  "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_MasterFile_baseline_2022.dta"

drop if _merge==2
drop _merge
keep if inrange(epi_year,2020,2022)
merge m:1 weekstart using "C:\Users\01404747\OneDrive - University of Cape Town\Academic\Applications and Projects\Covid\MRC_deaths\LancetGlobal_2025\LGH_waves_defn_2022.dta"
drop _merge

gen excess = count-baseline
gen covid_signal = 1 if covid==1 & LGH==26
replace covid_signal = 0 if covid==1 & LGH!=26
replace covid_signal = 2 if covid_signal==.
collapse (sum) count (sum) excess, by( wave covid_signal)
table covid_signal wave [iw=excess], nformat(%12.3f)
