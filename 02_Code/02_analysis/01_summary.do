

/*******************************************************************************
Title: Generate Descriptive Statistics and PSM Balance Diagnostics

Purpose: This script generates descriptive statistics and compares individual,
         parental, and household characteristics between caregivers and
         non-caregivers, before and after propensity score matching (PSM).

         The script:
         - Calculates means and standard deviations for the overall sample,
           caregivers, and non-caregivers.
         - Conducts t-tests to assess differences between the two groups.
         - Calculates absolute mean differences for each covariate.
         - Exports descriptive statistics and comparison results to Excel.
         - Compares covariate balance before and after PSM.
         - Generates a graphical representation of absolute mean differences
           before and after matching.

Intake data:
- Before-PSM sample:
  "${final_data}/before_psm_sample_${MQ_start_wave}-${MQ_end_wave}.dta"
- After-PSM sample:
  "${final_data}/after_psm_sample_${MQ_start_wave}-${MQ_end_wave}.dta"

Output data:
- Descriptive statistics exported to "${tables}main.xlsx".
- Absolute mean difference datasets:
  - "${final_data}abs_mean_diff_before.dta"
  - "${final_data}abs_mean_diff_after.dta"
- Covariate balance comparison dataset (combined before and after matching).
- Balance comparison graph:
  "${graphs}abs_mean_diff.png"

Created: [MM.DD.2024] - Huong Nguyen

Updates:
- [MM.DD.YYYY] - [Author]: [Description of updates]

*******************************************************************************/

cap log close
log using "${logs}`c(current_date)'_01_summary_stat.log", replace
display "Log timestamp: `c(current_date)' `c(current_time)'"

****************************************************
* Set globals with mode
****************************************************
 
global sample divorce_sample_before_matching marital_status_sample
global sample_num: word count $sample
global file Table1

global x_vars ///
    rcesd ///
    ragey_b female white black other_race foreign_born ///
    Educ_less_HS HS_degree at_least_some_college ///
    full_time_work ///
    rafeduc rameduc ///
    hnkid low_income ///
    divorced

local x_labels `" "Depressive symptoms score (CES-D)" "Age (years)" "Female" "White" "Black" "Other races" "Foreign-born" "Less than high school" "High school degree" "At least some college" "Full-time employment" "Father's education (years)" "Mother's education (years)" "Number of children in household" "Log net worth" "Divorce" "'

forval i = 1/$sample_num {
	local s: word `i' of $sample
	if `i' == 1 {
		local sheet before 
	}
	if `i' == 2 {
		local sheet after 
	}

use "${final_data}`s'.dta", clear 

*----------------------------------------
* Loop through variables and calculate statistics
*----------------------------------------
putexcel set "${tables}Table.xlsx", sheet("${file}_`sheet'") modify
putexcel  B1 = ("All sample") B2 = ("Mean") C2 = ("(SD)") ///
         D1 = ("Caregivers") D2 = ("Mean") E2 = ("(SD)") ///
         F1 = ("Non-caregivers") F2 = ("Mean") G2 = ("(SD)") ///
         H2 = ("p-val")  ///
	 I2 = ("Absolute Mean Difference")
	 
distinct hhidpn 
return list 
putexcel B23 = ("`r(N)'") B24 = ("`r(ndistinct)'")

distinct hhidpn if ever_care == 1
return list 
putexcel D23 = ("`r(N)'") D24 = ("`r(ndistinct)'")

distinct hhidpn if ever_care == 0
return list 
putexcel F23 = ("`r(N)'") F24 = ("`r(ndistinct)'")

putexcel A23 = ("Person by wave obs") A24 = ("Individuals")

local row = 3
local i = 1
local keep_vars

*============================================================
* INDIVIDUAL CHARACTERISTICS
*============================================================

putexcel A`row' = ("Individual characteristics")
local row = `row' + 1

local individual_vars ///
    rcesd ///
    ragey_b female white black other_race foreign_born ///
    Educ_less_HS HS_degree at_least_some_college ///
    full_time_work

foreach var of local individual_vars {

    * Overall summary
    quietly summarize `var'
    local all_mean = r(mean)
    local all_sd   = r(sd)

    * Caregivers
    quietly summarize `var' if ever_care == 1
    local care_mean = r(mean)
    local care_sd   = r(sd)

    * Non-caregivers
    quietly summarize `var' if ever_care == 0
    local nocare_mean = r(mean)
    local nocare_sd   = r(sd)

    * Absolute mean difference
    local abs_mean_diff = abs(`care_mean' - `nocare_mean')

    * T-test
    quietly ttest `var', by(ever_care)
    local pval = r(p)

    * Variable label
    local var_lab : word `i' of `x_labels'

    * Format
    local all_mean_str     = string(`all_mean', "%9.3f")
    local all_sd_str       = string(`all_sd', "%9.3f")
    local care_mean_str    = string(`care_mean', "%9.3f")
    local care_sd_str      = string(`care_sd', "%9.3f")
    local nocare_mean_str  = string(`nocare_mean', "%9.3f")
    local nocare_sd_str    = string(`nocare_sd', "%9.3f")
    local pval_str         = string(`pval', "%9.3f")
    local diff_str         = string(`abs_mean_diff', "%9.3f")

    * Write to Excel
    putexcel A`row' = ("`var_lab'") ///
        B`row' = ("`all_mean_str'") ///
        C`row' = ("(`all_sd_str')") ///
        D`row' = ("`care_mean_str'") ///
        E`row' = ("(`care_sd_str')") ///
        F`row' = ("`nocare_mean_str'") ///
        G`row' = ("(`nocare_sd_str')") ///
        H`row' = ("`pval_str'") ///
        I`row' = ("`diff_str'")

    gen ad_`var' = `abs_mean_diff'
local keep_vars `keep_vars' ad_`var'

    local row = `row' + 1
    local i = `i' + 1
}


*============================================================
* PARENTAL CHARACTERISTICS
*============================================================

putexcel A`row' = ("Parental characteristics")
local row = `row' + 1

local parental_vars rafeduc rameduc

foreach var of local parental_vars {

    quietly summarize `var'
    local all_mean = r(mean)
    local all_sd   = r(sd)

    quietly summarize `var' if ever_care == 1
    local care_mean = r(mean)
    local care_sd   = r(sd)

    quietly summarize `var' if ever_care == 0
    local nocare_mean = r(mean)
    local nocare_sd   = r(sd)

    local abs_mean_diff = abs(`care_mean' - `nocare_mean')

    quietly ttest `var', by(ever_care)
    local pval = r(p)

    local var_lab : word `i' of `x_labels'

    local all_mean_str     = string(`all_mean', "%9.3f")
    local all_sd_str       = string(`all_sd', "%9.3f")
    local care_mean_str    = string(`care_mean', "%9.3f")
    local care_sd_str      = string(`care_sd', "%9.3f")
    local nocare_mean_str  = string(`nocare_mean', "%9.3f")
    local nocare_sd_str    = string(`nocare_sd', "%9.3f")
    local pval_str         = string(`pval', "%9.3f")
    local diff_str         = string(`abs_mean_diff', "%9.3f")

    putexcel A`row' = ("`var_lab'") ///
        B`row' = ("`all_mean_str'") ///
        C`row' = ("(`all_sd_str')") ///
        D`row' = ("`care_mean_str'") ///
        E`row' = ("(`care_sd_str')") ///
        F`row' = ("`nocare_mean_str'") ///
        G`row' = ("(`nocare_sd_str')") ///
        H`row' = ("`pval_str'") ///
        I`row' = ("`diff_str'")

    gen ad_`var' = `abs_mean_diff'
local keep_vars `keep_vars' ad_`var'

    local row = `row' + 1
    local i = `i' + 1
}


*============================================================
* HOUSEHOLD CHARACTERISTICS
*============================================================

putexcel A`row' = ("Household characteristics")
local row = `row' + 1

local household_vars hnkid rlog_networth

foreach var of local household_vars {

    quietly summarize `var'
    local all_mean = r(mean)
    local all_sd   = r(sd)

    quietly summarize `var' if ever_care == 1
    local care_mean = r(mean)
    local care_sd   = r(sd)

    quietly summarize `var' if ever_care == 0
    local nocare_mean = r(mean)
    local nocare_sd   = r(sd)

    local abs_mean_diff = abs(`care_mean' - `nocare_mean')

    quietly ttest `var', by(ever_care)
    local pval = r(p)

    local var_lab : word `i' of `x_labels'

    local all_mean_str     = string(`all_mean', "%9.3f")
    local all_sd_str       = string(`all_sd', "%9.3f")
    local care_mean_str    = string(`care_mean', "%9.3f")
    local care_sd_str      = string(`care_sd', "%9.3f")
    local nocare_mean_str  = string(`nocare_mean', "%9.3f")
    local nocare_sd_str    = string(`nocare_sd', "%9.3f")
    local pval_str         = string(`pval', "%9.3f")
    local diff_str         = string(`abs_mean_diff', "%9.3f")

    putexcel A`row' = ("`var_lab'") ///
        B`row' = ("`all_mean_str'") ///
        C`row' = ("(`all_sd_str')") ///
        D`row' = ("`care_mean_str'") ///
        E`row' = ("(`care_sd_str')") ///
        F`row' = ("`nocare_mean_str'") ///
        G`row' = ("(`nocare_sd_str')") ///
        H`row' = ("`pval_str'") ///
        I`row' = ("`diff_str'")

   gen ad_`var' = `abs_mean_diff'
local keep_vars `keep_vars' ad_`var'

    local row = `row' + 1
    local i = `i' + 1
}


*============================================================
* OUTCOME
*============================================================

putexcel A`row' = ("Outcome")
local row = `row' + 1

local var  divorced

quietly summarize `var'
local all_mean = r(mean)
local all_sd   = r(sd)

quietly summarize `var' if ever_care == 1
local care_mean = r(mean)
local care_sd   = r(sd)

quietly summarize `var' if ever_care == 0
local nocare_mean = r(mean)
local nocare_sd   = r(sd)

local abs_mean_diff = abs(`care_mean' - `nocare_mean')

quietly ttest `var', by(ever_care)
local pval = r(p)

local var_lab : word `i' of `x_labels'

local all_mean_str     = string(`all_mean', "%9.3f")
local all_sd_str       = string(`all_sd', "%9.3f")
local care_mean_str    = string(`care_mean', "%9.3f")
local care_sd_str      = string(`care_sd', "%9.3f")
local nocare_mean_str  = string(`nocare_mean', "%9.3f")
local nocare_sd_str    = string(`nocare_sd', "%9.3f")
local pval_str         = string(`pval', "%9.3f")
local diff_str         = string(`abs_mean_diff', "%9.3f")

putexcel A`row' = ("`var_lab'") ///
    B`row' = ("`all_mean_str'") ///
    C`row' = ("(`all_sd_str')") ///
    D`row' = ("`care_mean_str'") ///
    E`row' = ("(`care_sd_str')") ///
    F`row' = ("`nocare_mean_str'") ///
    G`row' = ("(`nocare_sd_str')") ///
    H`row' = ("`pval_str'") ///
    I`row' = ("`diff_str'")


preserve 
	keep `keep_vars'
	gen sheet = 1 if "`sheet'" == "before"
	replace sheet = 2 if "`sheet'" == "after"
	save "${final_data}abs_mean_diff_`sheet'.dta", replace
restore
} 

clear
set obs 0

gen sheet = . 

	append using "${final_data}abs_mean_diff_before.dta"
	append using "${final_data}abs_mean_diff_after.dta"
	drop sheet
	duplicates drop
	xpose, clear varname
	rename v1 before 
	rename v2 after
	rename _varname vars
	drop if before == 1
	gen var_labs = ""

replace var_labs = "CES-D score" if vars == "ad_rcesd"

replace var_labs = "Age (years)" if vars == "ad_ragey_b"
replace var_labs = "Female" if vars == "ad_female"
replace var_labs = "White" if vars == "ad_white"
replace var_labs = "Black" if vars == "ad_black"
replace var_labs = "Other races" if vars == "ad_other_race"
replace var_labs = "Foreign-born" if vars == "ad_foreign_born"

replace var_labs = "Less than high school" if vars == "ad_Educ_less_HS"
replace var_labs = "High school degree" if vars == "ad_HS_degree"
replace var_labs = "At least some college" if vars == "ad_at_least_some_college"

replace var_labs = "Full-time employment" if vars == "ad_full_time_work"

replace var_labs = "Father's education" if vars == "ad_rafeduc"
replace var_labs = "Mother's education" if vars == "ad_rameduc"

replace var_labs = "Number of children in household" if vars == "ad_hnkid"
replace var_labs = "Log net worth" if vars == "ad_rlog_networth"

replace var_labs = "Divorce" if vars == "ad_divorced"
gen disp_num = .

* Individual characteristics
replace disp_num = 1  if vars == "ad_rcesd"
replace disp_num = 2  if vars == "ad_ragey_b"
replace disp_num = 3  if vars == "ad_female"
replace disp_num = 4  if vars == "ad_white"
replace disp_num = 5  if vars == "ad_black"
replace disp_num = 6  if vars == "ad_other_race"
replace disp_num = 7  if vars == "ad_foreign_born"
replace disp_num = 8  if vars == "ad_Educ_less_HS"
replace disp_num = 9  if vars == "ad_HS_degree"
replace disp_num = 10 if vars == "ad_at_least_some_college"
replace disp_num = 11 if vars == "ad_full_time_work"

* Parental characteristics
replace disp_num = 12 if vars == "ad_rafeduc"
replace disp_num = 13 if vars == "ad_rameduc"

* Household characteristics
replace disp_num = 14 if vars == "ad_hnkid"
replace disp_num = 15 if vars == "ad_rlog_networth"

* Outcome
//replace disp_num = 16 if vars == "ad_divorced"

sort disp_num

	graph bar before after, over(var_labs , sort(disp_num) label(angle(45) labsize(small))) ///
	legend(order(1 "Before Matching" 2 "After Matching") position(1) ring(0) cols(1)) ///
    ytitle("Absolute Mean Difference") ///
    graphregion(color(white)) plotregion(color(white))
graph export "${graphs}FigureA1-abs_mean_diff.png", replace 

  display "${graphs}FigureA1-abs_mean_diff.png"
