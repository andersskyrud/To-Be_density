/** ANALYSES FOR THE DENSITY ARTICLE IN THE TO-BE TRIAL **/

/* ---------- Setup ---------- */

clear all
cd "M:\Mammografi\Prosjekter\02 Tomosyntese\C Hordaland\Prosjekter\TOBE-1 - artikkel 4\"
use Analysefil_artikkel4.dta, clear

// Some labels 

label define ScreeningType 1 "DM" 2 "DBT"
label value ScreeningType ScreeningType

label define VBD_gr 0 "First quintile" 1 "Second quintile" 2 "Third quintile" 3 "Fourth quintile" 4 "Fifth quintile"
label value VBD_gr VBD_gr

label define VDG_5ed 1 "VDG 1" 2 "VDG 2" 3 "VDG 3" 4 "VDG 4"
label value VDG_5ed VDG_5ed

// Global graph scheme for prettier graphs 

set scheme s1mono

/* ---------- Table 1 ---------- */

// Descriptive statistics with median and IQR for continuous variables and percentages for categorical variables

table ScreeningType, c(p50 alder p25 alder p75 alder) // age
tab ScreeningType pre_sub, row // screening history
table ScreeningType, c(p50 BMI p25 BMI p75 BMI) // BMI
table ScreeningType, c(p50 BV_cm3 p25 BV_cm3 p75 BV_cm3) // Breast volume
table ScreeningType, c(p50 VBD p25 VBD p75 VBD) // Volumetric breast density
table ScreeningType, c(p50 FGV_cm3 p25 FGV_cm3 p75 FGV_cm3) // Fibroglandular volume 
table ScreeningType, c(p50 H_mm p25 H_mm p75 H_mm) // Compressed breast thickness
tab ScreeningType VDG_5ed, row // Volpara Density Grades
table VDG_5ed ScreeningType, c(min VBD max VBD)
tab ScreeningType VBD_gr, row // Volumetric breast density quintiles
table VBD_gr ScreeningType, c(min VBD max VBD)

/* ---------- Figure 2 ---------- */ 

// Bar graphs with 95 % confidence interval around the mean. Texts are overlaid through a combination of relabeling and scatter plotting strings directly onto the graph.

preserve 
replace ScreeningType = 0 if ScreeningType == 2
label define VDG_5ed2 1 `""VDG 1" "n=3929 n=3212" "p=0.001""' 2 `""VDG 2" "n=6216 n=6280" "p=0.002""' 3 `""VDG 3" "n=3152 n=3655" "p=0.88""' 4 `""VDG 4" " n=962 n=1136" "p=0.26""'
label value VDG_5ed VDG_5ed2
collapse (mean) meanrecall = recall (sd) sdrecall = recall (count) n = recall, by(ScreeningType VDG_5ed)
gen prosentrecall = string(100 * meanrecall, "%8.1f") + "%"
gen hirecall = meanrecall + invttail(n-1,0.025)*(sdrecall / sqrt(n))
gen lorecall = meanrecall - invttail(n-1,0.025)*(sdrecall / sqrt(n))
graph twoway (bar meanrecall ScreeningType, fcolor(red%40)) (rcap hirecall lorecall ScreeningType, lcolor(blue%40)) (scatter meanrecall ScreeningType, ms(none) mlabel(prosentrecall) mlabpos(6)), ///   
	by(VDG_5ed, title("A: Recall", ///
	span size(5)) subtitle("Volpara Density Grades (VDG)") rows(1) legend(off) note("")) subtitle(,fcolor(white) bcolor(white) margin(medium)) ///
	yscale(range(0 0.05)) xlabel(0 "DBT" 1 "DM", noticks) xtitle("Screening technique") ytitle("Recall (%)") ///
	ylabel(0(0.01)0.05 0 "0" 0.01 "1" 0.02 "2" 0.03 "3" 0.04 "4" 0.05 "5", grid angle(h))  plotregion(color(white)) graphregion(color(white))
graph save figurrecall.gph, replace
restore 

preserve 
replace ScreeningType = 0 if ScreeningType == 2
label define VDG_5ed2 1 `""VDG 1" "n=3929 n=3212" "p<0.001""' 2 `""VDG 2" "n=6216 n=6280" "p<0.001""' 3 `""VDG 3" "n=3152 n=3655" "p=0.95""' 4 `""VDG 4" " n=962 n=1136" "p=0.21""'
label value VDG_5ed VDG_5ed2
collapse (mean) meanFP = FP (sd) sdFP = FP (count) n = FP, by(ScreeningType VDG_5ed)
gen prosentFP = string(100 * meanFP, "%8.1f") + "%"
gen hiFP = meanFP + invttail(n-1,0.025)*(sdFP / sqrt(n))
gen loFP = meanFP - invttail(n-1,0.025)*(sdFP / sqrt(n))
graph twoway (bar meanFP ScreeningType, fcolor(red%40)) (rcap hiFP loFP ScreeningType, lcolor(blue%40)) (scatter meanFP ScreeningType, ms(none) mlabel(prosentFP) mlabpos(6)), ///
	by(VDG_5ed, title("B: False positives" ///
	, span size(5)) subtitle("Volpara Density Grades (VDG)") rows(1) legend(off) note("")) subtitle(,fcolor(white) bcolor(white)) ///
	yscale(range(0 0.05)) xlabel(0 "DBT" 1 "DM", noticks) xtitle("Screening technique") ytitle("False positives (%)") ///
	ylabel(0(0.01)0.05 0 "0" 0.01 "1" 0.02 "2" 0.03 "3" 0.04 "4" 0.05 "5", grid angle(h))  plotregion(color(white)) graphregion(color(white))
graph save figurFP.gph, replace
restore 

preserve 
replace ScreeningType = 0 if ScreeningType == 2
label define VDG_5ed2 1 `""VDG 1" "n=3929 n=3212" "p=0.44""' 2 `""VDG 2" "n=6216 n=6280" "p=0.10""' 3 `""VDG 3" "n=3152 n=3655" "p=0.10""' 4 `""VDG 4" " n=962 n=1136" "p=0.72""'
label value VDG_5ed VDG_5ed2
collapse (mean) meanbiopsi = biopsi (sd) sdbiopsi = biopsi (count) n = biopsi, by(ScreeningType VDG_5ed)
gen prosentbiopsi = string(100 * meanbiopsi, "%8.1f") + "%"
gen hibiopsi = meanbiopsi + invttail(n-1,0.025)*(sdbiopsi / sqrt(n))
gen lobiopsi = meanbiopsi - invttail(n-1,0.025)*(sdbiopsi / sqrt(n))
graph twoway (bar meanbiopsi ScreeningType, fcolor(red%40)) (rcap hibiopsi lobiopsi ScreeningType, lcolor(blue%40)) (scatter meanbiopsi ScreeningType, ms(none) mlabel(prosentbiopsi) mlabpos(6)), ///   
	by(VDG_5ed, title("C: Biopsy" ///
	, span size(5)) subtitle("Volpara Density Grades (VDG)") rows(1) legend(off) note("")) subtitle(,fcolor(white) bcolor(white)) ///
	yscale(range(0 0.03)) xlabel(0 "DBT" 1 "DM", noticks) xtitle("Screening technique") ytitle("Biopsy (%)") ///
	ylabel(0(0.005)0.03 0 "0" 0.005 "0.5" 0.01 "1" 0.015 "1.5" 0.02 "2" 0.025 "2.5" 0.03 "3", grid angle(h))  plotregion(color(white)) graphregion(color(white))
graph save figurbiopsi.gph, replace
restore 

preserve 
replace ScreeningType = 0 if ScreeningType == 2
label define VDG_5ed2 1 `""VDG 1" "n=3929 n=3212" "p=0.96""' 2 `""VDG 2" "n=6216 n=6280" "p=0.31""' 3 `""VDG 3" "n=3152 n=3655" "p=0.82""' 4 `""VDG 4" " n=962 n=1136" "p=0.98""'
label value VDG_5ed VDG_5ed2
collapse (mean) meanSDC = SDC (sd) sdSDC = SDC (count) n = SDC, by(ScreeningType VDG_5ed)
gen prosentSDC = string(100 * meanSDC, "%8.2f") + "%"
gen hiSDC = meanSDC + invttail(n-1,0.025)*(sdSDC / sqrt(n))
gen loSDC = meanSDC - invttail(n-1,0.025)*(sdSDC / sqrt(n))
graph twoway (bar meanSDC ScreeningType, fcolor(red%40)) (rcap hiSDC loSDC ScreeningType, lcolor(blue%40)) (scatter meanSDC ScreeningType, ms(none) mlabel(prosentSDC) mlabpos(6)), ///   
	by(VDG_5ed, title("D: Screen-detected cancers" ///
	, span size(5)) subtitle("Volpara Density Grades (VDG)") rows(1) legend(off) note("")) subtitle(,fcolor(white) bcolor(white)) ///
	yscale(range(0 0.015)) xlabel(0 "DBT" 1 "DM", noticks) xtitle("Screening technique") ytitle("Screen-detected cancers (%)") ///
	ylabel(0(0.002)0.014 0 "0" 0.002 "0.2" 0.004 "0.4" 0.006 "0.6" 0.008 "0.8" 0.01 "1" 0.012 "1.2" 0.014 "1.4", grid angle(h))  plotregion(color(white)) graphregion(color(white))
graph save figurSDC.gph, replace
restore 

preserve 
replace ScreeningType = 0 if ScreeningType == 2
label define VDG_5ed2 1 `""VDG 1" "n=3929 n=3212" "p=0.15""' 2 `""VDG 2" "n=6216 n=6280" "p=0.01""' 3 `""VDG 3" "n=3152 n=3655" "p=0.86""' 4 `""VDG 4" " n=962 n=1136" "p=0.59""'
label value VDG_5ed VDG_5ed2
collapse (mean) meanrecall = recall (mean) meanSDC = SDC (count) n = recall if recall == 1, by(ScreeningType VDG_5ed)
gen meanPPV1 = meanSDC/meanrecall
gen prosentPPV1 = string(100 * meanPPV1, "%8.1f") + "%"
gen hiPPV1 = meanPPV1 + 1.96*(sqrt((meanPPV1*(1-meanPPV1))/n))
gen loPPV1 = meanPPV1 - 1.96*(sqrt((meanPPV1*(1-meanPPV1))/n))
graph twoway (bar meanPPV1 ScreeningType, fcolor(red%40)) (rcap hiPPV1 loPPV1 ScreeningType, lcolor(blue%40)) (scatter meanPPV1 ScreeningType, ms(none) mlabel(prosentPPV1) mlabpos(6)), ///   
	by(VDG_5ed, title("E: Positive Predictive Value-1", ///
	span size(5)) subtitle("Volpara Density Grades (VDG)") rows(1) legend(off) note("")) subtitle(,fcolor(white) bcolor(white)) ///
	yscale(range(0 0.5)) xlabel(0 "DBT" 1 "DM", noticks) xtitle("Screening technique") ytitle("Positive Predictive Value-1 (%)") ///
	ylabel(0(0.1)0.5 0 "0" 0.1 "10" 0.2 "20" 0.3 "30" 0.4 "40" 0.5 "50", grid angle(h))  plotregion(color(white)) graphregion(color(white))
graph save figurPPV1.gph, replace
restore 

preserve
replace ScreeningType = 0 if ScreeningType == 2
label define VDG_5ed2 1 `""VDG 1" "n=3929 n=3212" "p=0.62""' 2 `""VDG 2" "n=6216 n=6280" "p=0.01""' 3 `""VDG 3" "n=3152 n=3655" "p=0.38""' 4 `""VDG 4" " n=962 n=1136" "p=0.80""'
label value VDG_5ed VDG_5ed2
collapse (mean) meanbiopsi = recall (mean) meanSDC = SDC (count) n = biopsi if biopsi == 1, by(ScreeningType VDG_5ed)
gen meanPPV2 = meanSDC/meanbiopsi
gen prosentPPV2 = string(100 * meanPPV2, "%8.1f") + "%"
gen hiPPV2 = meanPPV2 + 1.96*(sqrt((meanPPV2*(1-meanPPV2))/n))
gen loPPV2 = meanPPV2 - 1.96*(sqrt((meanPPV2*(1-meanPPV2))/n))
graph twoway (bar meanPPV2 ScreeningType, fcolor(red%40)) (rcap hiPPV2 loPPV2 ScreeningType, lcolor(blue%40)) (scatter meanPPV2 ScreeningType, ms(none) mlabel(prosentPPV2) mlabpos(6)), ///   
	by(VDG_5ed, title("F: Positive Predictive Value-3" ///
	, span size(5)) subtitle("Volpara Density Grades (VDG)") rows(1) legend(off) note("")) subtitle(,fcolor(white) bcolor(white)) ///
	yscale(range(0 0.75)) xlabel(0 "DBT" 1 "DM", noticks) xtitle("Screening technique") ytitle("Positive Predictive Value-3 (%)") ///
	ylabel(0(0.1)0.75 0 "0" 0.1 "10" 0.2 "20" 0.3 "30" 0.4 "40" 0.5 "50" 0.6 "60" 0.7 "70", grid angle(h)) /// 
	plotregion(color(white)) graphregion(color(white)) 
graph save figurPPV2.gph, replace
restore 

gr combine figurrecall.gph figurFP.gph figurbiopsi.gph figurSDC.gph figurPPV1.gph figurPPV2.gph, iscale(0.33) col(2) caption("DBT: Digital Breast Tomosynthesis, DM: Digital Mammography", size(half_tiny))
graph export figur2.png, replace height(4000) width(3200)
graph export figur2_tif.tif, replace height(4000) width(3200)
erase figurFP.gph 
erase figurrecall.gph 
erase figurbiopsi.gph 
erase figurSDC.gph 
erase figurPPV1.gph 
erase figurPPV2.gph

/* ---------- Table 2 and 3 ---------- */ 

// Histopathological tumour characteristics

// Tumour size variable replaced with a more precise one from a radiological review

replace SLdm_solitærettf = "" if SLdm_solitærettf == "kalk"
destring SLdm_solitærettf, replace
replace Tumorstr = SLdm_solitærettf if SLdm_solitærettf != .

// Crude proportions 

tab Gruppe if Gruppe==3 & ScreeningType == 1 & VDG_5ed != .
tab Gruppe if Gruppe==3 & ScreeningType == 2 & VDG_5ed != .
table VDG_5ed if Gruppe==3 & ScreeningType == 1 & Tumorstr<999, c(median Tumorstr iqr Tumorstr p25 Tumorstr p75 Tumorstr)
table VDG_5ed if Gruppe==3 & ScreeningType == 2 & Tumorstr<999, c(median Tumorstr iqr Tumorstr p25 Tumorstr p75 Tumorstr)
tab Grad VDG_5ed if Gruppe==3 & ScreeningType == 1, col 
tab Grad VDG_5ed if Gruppe==3 & ScreeningType == 2, col 
tab pN VDG_5ed if Gruppe==3 & ScreeningType == 1, col 
tab pN VDG_5ed if Gruppe==3 & ScreeningType == 2, col
tab subtype VDG_5ed if Gruppe==3 & ScreeningType == 1, col mi 
tab subtype VDG_5ed if Gruppe==3 & ScreeningType == 2, col mi 

// Confidence intervals 

by VDG_5ed : ci mean Tumorstr if ScreeningType == 1 & Tumorstr<999 & Gruppe==3, total 
by VDG_5ed : ci mean Tumorstr if ScreeningType == 2 & Tumorstr<999 & Gruppe==3, total
proportion Grad if ScreeningType == 1 & Grad != 9 & VDG_5ed != ., citype(exact) percent
proportion Grad if ScreeningType == 2 & Grad != 9 & VDG_5ed != ., citype(exact) percent 
proportion Grad if ScreeningType == 1 & Grad != 9 & VDG_5ed != ., citype(exact) over(VDG_5ed) percent
proportion Grad if ScreeningType == 2 & Grad != 9 & VDG_5ed != ., citype(exact) over(VDG_5ed) percent 
proportion pN if ScreeningType == 1 & pN != 999 & VDG_5ed != .  & Gruppe==3, citype(exact) percent
proportion pN if ScreeningType == 2 & pN != 999 & VDG_5ed != .  & Gruppe==3, citype(exact) percent 
proportion pN if ScreeningType == 1 & pN != 999 & VDG_5ed != .  & Gruppe==3, citype(exact) over(VDG_5ed) percent
proportion pN if ScreeningType == 2 & pN != 999 & VDG_5ed != .  & Gruppe==3, citype(exact) over(VDG_5ed) percent 
proportion subtype if ScreeningType == 1 & subtype != . & VDG_5ed != . & Gruppe==3, citype(exact) percent
proportion subtype if ScreeningType == 2 & subtype != . & VDG_5ed != . & Gruppe==3, citype(exact) percent 
proportion subtype if ScreeningType == 1 & subtype != . & VDG_5ed != . & Gruppe==3, citype(exact) over(VDG_5ed) percent
proportion subtype if ScreeningType == 2 & subtype != . & VDG_5ed != . & Gruppe==3, citype(exact) over(VDG_5ed) percent 


/* ---------- Table 4a-c ---------- */ 

// relative risks produced through log-binomial regression models with exponentiated coefficients for recall, false positiv and screen-detected cancers by density, adjusting for a continuous measure of breast volume. 
// All models run unadjusted before final adjusted models.

glm recall i.VDG_5ed if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall i.alder_gr if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall i.pre_sub if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall i.VDG_5ed if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall i.alder_gr if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall i.pre_sub if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm recall i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform

glm FP i.VDG_5ed if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP i.alder_gr if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP i.pre_sub if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP i.VDG_5ed if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP i.alder_gr if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP i.pre_sub if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm FP i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform

glm SDC i.VDG_5ed if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC i.alder_gr if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC i.pre_sub if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC i.VDG_5ed if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC i.alder_gr if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC i.pre_sub if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform
glm SDC i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(log) eform

/* ---------- Supplemental material ---------- */ 

// Supplemental table S1-S3 - Sensitivity analysis

glm SDC i.VBD_gr if ScreeningType == 1, family(binomial) link(log) eform
glm SDC i.VBD_gr i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm SDC i.VBD_gr if ScreeningType == 2, family(binomial) link(log) eform
glm SDC i.VBD_gr i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm recall i.VBD_gr if ScreeningType == 1, family(binomial) link(log) eform
glm recall i.VBD_gr i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm recall i.VBD_gr if ScreeningType == 2, family(binomial) link(log) eform
glm recall i.VBD_gr i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm FP i.VBD_gr if ScreeningType == 1, family(binomial) link(log) eform
glm FP i.VBD_gr i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm FP i.VBD_gr if ScreeningType == 2, family(binomial) link(log) eform
glm FP i.VBD_gr i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm SDC i.VBD_gr i.alder_gr i.pre_sub BMI if ScreeningType == 1, family(binomial) link(log) eform
glm SDC i.VBD_gr i.alder_gr i.pre_sub BMI if ScreeningType == 2, family(binomial) link(log) eform

glm recall i.VBD_gr i.alder_gr i.pre_sub BMI if ScreeningType == 1, family(binomial) link(log) eform
glm recall i.VBD_gr i.alder_gr i.pre_sub BMI if ScreeningType == 2, family(binomial) link(log) eform

glm FP i.VBD_gr i.alder_gr i.pre_sub BMI if ScreeningType == 1, family(binomial) link(log) eform
glm FP i.VBD_gr i.alder_gr i.pre_sub BMI if ScreeningType == 2, family(binomial) link(log) eform

glm SDC i.VBD_tokat if ScreeningType == 1, family(binomial) link(log) eform
glm SDC i.VBD_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm SDC i.VBD_tokat if ScreeningType == 2, family(binomial) link(log) eform
glm SDC i.VBD_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm recall i.VBD_tokat if ScreeningType == 1, family(binomial) link(log) eform
glm recall i.VBD_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm recall i.VBD_tokat if ScreeningType == 2, family(binomial) link(log) eform
glm recall i.VBD_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm FP i.VBD_tokat if ScreeningType == 1, family(binomial) link(log) eform
glm FP i.VBD_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm FP i.VBD_tokat if ScreeningType == 2, family(binomial) link(log) eform
glm FP i.VBD_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm SDC i.VDG_tokat if ScreeningType == 1, family(binomial) link(log) eform
glm SDC i.VDG_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm SDC i.VDG_tokat if ScreeningType == 2, family(binomial) link(log) eform
glm SDC i.VDG_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm recall i.VDG_tokat if ScreeningType == 1, family(binomial) link(log) eform
glm recall i.VDG_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm recall i.VDG_tokat if ScreeningType == 2, family(binomial) link(log) eform
glm recall i.VDG_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

glm FP i.VDG_tokat if ScreeningType == 1, family(binomial) link(log) eform
glm FP i.VDG_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1, family(binomial) link(log) eform
glm FP i.VDG_tokat if ScreeningType == 2, family(binomial) link(log) eform
glm FP i.VDG_tokat i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2, family(binomial) link(log) eform

/* ---------- Supplemental material, Table S4 ---------- */

// The numbers behind the bar graphs 

// Code for Z tests for proportions looped over categories and performance measures provide CIs 

foreach var of varlist konsensus FP recall biopsi SDC {
	prtest `var' if VDG_5ed == 1, by(ScreeningType)
	}
foreach var of varlist konsensus FP recall biopsi SDC {
	prtest `var' if VDG_5ed == 2, by(ScreeningType)
	}
foreach var of varlist konsensus FP recall biopsi SDC {
	prtest `var' if VDG_5ed == 3, by(ScreeningType)
	}
foreach var of varlist konsensus FP recall biopsi SDC {
	prtest `var' if VDG_5ed == 4, by(ScreeningType)
	}

// Crude threeway tables

table recall VDG_5ed ScreeningType
table biopsi VDG_5ed ScreeningType
table SDC VDG_5ed ScreeningType

// Immediate code for Z tests for PPV 

prtesti 81 18 106 15, c // VDG 1 - PPV-1
prtesti 48 18 46 15, c // VDG 1 - PPV-3
prtesti 200 48 267 39, c // VDG 2 - PPV-1
prtesti 103 48 129 39, c // VDG 2 - PPV-3
prtesti 129 23 147 25, c // VDG 3 - PPV-1
prtesti 78 23 69 25, c // VDG 3 - PPV-3
prtesti 30 6 46 7, c // VDG 4 - PPV-1
prtesti 19 6 25 7, c // VDG 4 - PPV-3

/* ---------- Supplemental material, Table S5 ---------- */

// Correlation matrix
pwcorr BMI BV_cm3 VBD FGV_cm3, sig

/* ---------- Supplemental material, Table S6 ---------- */

// Log-binomial regression models for DM and DBT combined using interaction terms
glm recall i.VDG_5ed, family(binomial) link(log) eform allbase
glm recall i.ScreeningType, family(binomial) link(log) eform allbase
glm recall i.alder_gr, family(binomial) link(log) eform allbase
glm recall i.pre_sub, family(binomial) link(log) eform allbase
glm recall BV_cm3, family(binomial) link(log) eform allbase
glm recall i.VDG_5ed##i.ScreeningType i.alder_gr i.pre_sub BV_cm3, family(binomial) link(log) eform allbase

glm FP i.VDG_5ed, family(binomial) link(log) eform allbase
glm FP i.ScreeningType, family(binomial) link(log) eform allbase
glm FP i.alder_gr, family(binomial) link(log) eform allbase
glm FP i.pre_sub, family(binomial) link(log) eform allbase
glm FP BV_cm3, family(binomial) link(log) eform allbase
glm FP i.VDG_5ed##i.ScreeningType i.alder_gr i.pre_sub BV_cm3, family(binomial) link(log) eform allbase

glm SDC i.VDG_5ed, family(binomial) link(log) eform allbase
glm SDC i.ScreeningType, family(binomial) link(log) eform allbase
glm SDC i.alder_gr, family(binomial) link(log) eform allbase
glm SDC i.pre_sub, family(binomial) link(log) eform allbase
glm SDC BV_cm3, family(binomial) link(log) eform allbase
glm SDC i.VDG_5ed##i.ScreeningType i.alder_gr i.pre_sub BV_cm3, family(binomial) link(log) eform allbase

/* ---------- Supplemental material, Figure S1 ---------- */

// Histogram
twoway (histogram VBD if ScreeningType == 1, lcolor(blue%0) color(blue%30) start(1.5) width(1) freq) (histogram VBD if ScreeningType == 2, freq lcolor(red%0) color(red%30) width(1) start(1.5)), legend(order(1 "DM" 2 "DBT"))
graph export Figur4.png, width(800) height(600) replace
graph export Figur4_tif.tif, width(800) height(600) replace


/* _____________________________________________________________________________________________________________ */

/* ---------- UNUSED ADDITIONAL ANALYSES ---------- */

// Alternative logistic regression models

glm recall i.VDG_5ed if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall i.alder_gr if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall i.pre_sub if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall i.VDG_5ed if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall i.alder_gr if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall i.pre_sub if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm recall i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform

glm FP i.VDG_5ed if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP i.alder_gr if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP i.pre_sub if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP i.VDG_5ed if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP i.alder_gr if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP i.pre_sub if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm FP i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform

glm SDC i.VDG_5ed if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC i.alder_gr if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC i.pre_sub if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 1 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC i.VDG_5ed if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC i.alder_gr if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC i.pre_sub if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform
glm SDC i.VDG_5ed i.alder_gr i.pre_sub BV_cm3 if ScreeningType == 2 & VDG_5ed != . & alder_gr != . & pre_sub != . & BV_cm3 != ., family(binomial) link(logit) eform

// Graph using splines for SDC over a continuous VBD variable 

mkspline VBDcubDM = VBD if ScreeningType == 1, cubic nknots(4) 

glm SDC VBDcubDM* i.alder_gr i.pre_sub BV_cm3, family(binomial) link(logit) eform
estat ic

predictnl xb1 = _b[_cons] + _b[VBDcubDM1]*VBDcubDM1 + _b[VBDcubDM2]*VBDcubDM2 + _b[VBDcubDM3]*VBDcubDM3, se(se1)

gen lb_xb1 = xb1 - invnormal(0.975)*se1
gen ub_xb1 = xb1 + invnormal(0.975)*se1 

gen lb1 = exp(lb_xb1)/(1+exp(lb_xb1))
gen ub1 = exp(ub_xb1)/(1+exp(ub_xb1))

gen or1 = exp(xb1)
gen p1 = or1 / (1 + or1)

mkspline VBDcubDBT = VBD if ScreeningType == 2, cubic nknots(4)

glm SDC VBDcubDBT* i.alder_gr i.pre_sub BV_cm3, family(binomial) link(logit) eform 
estat ic

predictnl xb2 = _b[_cons] + _b[VBDcubDBT1]*VBDcubDBT1 + _b[VBDcubDBT2]*VBDcubDBT2 + _b[VBDcubDBT3]*VBDcubDBT3, se(se2)

gen lb_xb2 = xb2 - invnormal(0.975)*se2
gen ub_xb2 = xb2 + invnormal(0.975)*se2

gen lb2 = exp(lb_xb2)/(1+exp(lb_xb2))
gen ub2 = exp(ub_xb2)/(1+exp(ub_xb2))

gen or2 = exp(xb2)
gen p2 = or2 / (1 + or2)

bysort VBD: gen merk = 1 if _n == 1
tw (rarea lb1 ub1 VBD if VBD <= 20, color(blue%30) sort) (rarea lb2 ub2 VBD if VBD <= 20, color(red%30) sort) (line p1 VBD if VBD <= 20, sort) (line p2 VBD if VBD <= 20, sort), xtitle("Volumetric Breast Density") ytitle("SDC") legend(order(1 "DM" 2 "DBT")) xlabel(2(2)20) ylabel(.0 "0 %" .004 "0.4 %" .008 "0.8 %" .012 "1.2 %" .016 "1.6 %", angle(h))
graph export SplinesSDC.png, width(800) height(600) replace

drop VBDcubDM1-p2

// Graph using splines for recall over a continuous VBD variable 

mkspline VBDcubDM = VBD if ScreeningType == 1, cubic nknots(4) 

glm recall VBDcubDM* i.alder_gr i.pre_sub BV_cm3, family(binomial) link(logit) eform 
estat ic

predictnl xb1 = _b[_cons] + _b[VBDcubDM1]*VBDcubDM1 + _b[VBDcubDM2]*VBDcubDM2 + _b[VBDcubDM3]*VBDcubDM3, se(se1) 

gen lb_xb1 = xb1 - invnormal(0.975)*se1
gen ub_xb1 = xb1 + invnormal(0.975)*se1

gen lb1 = exp(lb_xb1)/(1+exp(lb_xb1))
gen ub1 = exp(ub_xb1)/(1+exp(ub_xb1))

gen or1 = exp(xb1)
gen p1 = or1 / (1 + or1)

mkspline VBDcubDBT = VBD if ScreeningType == 2, cubic nknots(4)

glm recall VBDcubDBT* i.alder_gr i.pre_sub BV_cm3, family(binomial) link(logit) eform 
estat ic

predictnl xb2 = _b[_cons] + _b[VBDcubDBT1]*VBDcubDBT1 + _b[VBDcubDBT2]*VBDcubDBT2 + _b[VBDcubDBT3]*VBDcubDBT3, se(se2)

gen lb_xb2 = xb2 - invnormal(0.975)*se2
gen ub_xb2 = xb2 + invnormal(0.975)*se2

gen lb2 = exp(lb_xb2)/(1+exp(lb_xb2))
gen ub2 = exp(ub_xb2)/(1+exp(ub_xb2))

gen or2 = exp(xb2)
gen p2 = or2 / (1 + or2)

tw (rarea lb1 ub1 VBD if VBD <= 20, color(blue%30) sort) (rarea lb2 ub2 VBD if VBD <= 20, color(red%30) sort) (line p1 VBD if VBD <= 20, sort) (line p2 VBD if VBD <= 20, sort), xtitle("Volumetric Breast Density") ytitle("Recall") legend(order(1 "DM" 2 "DBT")) xlabel(2(2)20) ylabel(.0 "0 %" .02 "2 %" .04 "4 %" .06 "6 %" .08 "8 %" .1 "10 %", angle(h))
graph export SplinesRecall.png, width(800) height(600) replace

drop VBDcubDM1-p2

// Crude spline graph, SDC over VBD

mkspline VBDcubDM = VBD if ScreeningType == 1, cubic nknots(4)

glm SDC VBDcubDM*, family(binomial) link(logit) eform 
estat ic 

predictnl xb1 = _b[_cons] + _b[VBDcubDM1]*VBDcubDM1 + _b[VBDcubDM2]*VBDcubDM2 + _b[VBDcubDM3]*VBDcubDM3, se(se1) 

gen lb_xb1 = xb1 - invnormal(0.975)*se1
gen ub_xb1 = xb1 + invnormal(0.975)*se1 

gen lb1 = exp(lb_xb1)/(1+exp(lb_xb1))
gen ub1 = exp(ub_xb1)/(1+exp(ub_xb1)) 

gen or1 = exp(xb1)
gen p1 = or1 / (1 + or1) 

mkspline VBDcubDBT = VBD if ScreeningType == 2, cubic nknots(4) 

glm SDC VBDcubDBT*, family(binomial) link(logit) eform 
estat ic

predictnl xb2 = _b[_cons] + _b[VBDcubDBT1]*VBDcubDBT1 + _b[VBDcubDBT2]*VBDcubDBT2 + _b[VBDcubDBT3]*VBDcubDBT3, se(se2)

gen lb_xb2 = xb2 - invnormal(0.975)*se2
gen ub_xb2 = xb2 + invnormal(0.975)*se2

gen lb2 = exp(lb_xb2)/(1+exp(lb_xb2))
gen ub2 = exp(ub_xb2)/(1+exp(ub_xb2))

gen or2 = exp(xb2)
gen p2 = or2 / (1 + or2)

bysort VBD: gen merk = 1 if _n == 1
tw (rarea lb1 ub1 VBD if VBD <= 20, color(blue%30) sort) (rarea lb2 ub2 VBD if VBD <= 20, color(red%30) sort) (line p1 VBD if VBD <= 20, sort) (line p2 VBD if VBD <= 20, sort), xtitle("Volumetric Breast Density") ytitle("SDC") legend(order(1 "DM" 2 "DBT")) xlabel(2(2)20) ylabel(.0 "0 %" .004 "0.4 %" .008 "0.8 %" .012 "1.2 %" .016 "1.6 %", angle(h))
graph export SplinesSDC_crude.png, width(800) height(600) replace

drop VBDcubDM1-p2 

// Crude spline graph, recall over VBD

mkspline VBDcubDM = VBD if ScreeningType == 1, cubic nknots(4) 

glm recall VBDcubDM*, family(binomial) link(logit) eform 
estat ic

predictnl xb1 = _b[_cons] + _b[VBDcubDM1]*VBDcubDM1 + _b[VBDcubDM2]*VBDcubDM2 + _b[VBDcubDM3]*VBDcubDM3, se(se1) 

gen lb_xb1 = xb1 - invnormal(0.975)*se1
gen ub_xb1 = xb1 + invnormal(0.975)*se1

gen lb1 = exp(lb_xb1)/(1+exp(lb_xb1))
gen ub1 = exp(ub_xb1)/(1+exp(ub_xb1))

gen or1 = exp(xb1)
gen p1 = or1 / (1 + or1)

mkspline VBDcubDBT = VBD if ScreeningType == 2, cubic nknots(4)

glm recall VBDcubDBT*, family(binomial) link(logit) eform 
estat ic

predictnl xb2 = _b[_cons] + _b[VBDcubDBT1]*VBDcubDBT1 + _b[VBDcubDBT2]*VBDcubDBT2 + _b[VBDcubDBT3]*VBDcubDBT3, se(se2)

gen lb_xb2 = xb2 - invnormal(0.975)*se2
gen ub_xb2 = xb2 + invnormal(0.975)*se2

gen lb2 = exp(lb_xb2)/(1+exp(lb_xb2))
gen ub2 = exp(ub_xb2)/(1+exp(ub_xb2))

gen or2 = exp(xb2)
gen p2 = or2 / (1 + or2)

tw (rarea lb1 ub1 VBD if VBD <= 20, color(blue%30) sort) (rarea lb2 ub2 VBD if VBD <= 20, color(red%30) sort) (line p1 VBD if VBD <= 20, sort) (line p2 VBD if VBD <= 20, sort), xtitle("Volumetric Breast Density") ytitle("Recall") legend(order(1 "DM" 2 "DBT")) xlabel(2(2)20) ylabel(.0 "0 %" .02 "2 %" .04 "4 %" .06 "6 %" .08 "8 %" .1 "10 %", angle(h))
graph export SplinesRecall_crude.png, width(800) height(600) replace

drop VBDcubDM1-p2
