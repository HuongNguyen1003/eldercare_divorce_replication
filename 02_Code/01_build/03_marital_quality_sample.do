
cap log close
display "Log timestamp: `c(current_date)' `c(current_time)'"
log using "${logs}`c(current_date)'_02_marital_quality_sample.log", replace

/*******************************************************************************
Title: Construct Caregiving and Marital Quality Analysis Sample

Purpose: This script constructs the analytical sample for examining the
         relationship between caregiving for parents (mother and father),
         marital quality, and family characteristics using the Health and
         Retirement Study (HRS).

         The script merges RAND HRS Longitudinal File, RAND HRS Family Data,
         and standardized marital quality measures; reshapes the data into
         person-wave format; constructs caregiving and marital quality
         measures; identifies ever-caregivers and non-caregivers; and applies
         propensity score matching to construct comparable treatment and
         control groups.

Intake data:
- RAND HRS Longitudinal File (1992-2020).
- RAND HRS Family Data (1992-2018).
- HRS marital quality measures (Waves 7-15, 2004-2020).

Output data:
- "${final_data}/non_caregivers_sample.dta"
- "${final_data}before_psm_sample_${MQ_start_wave}-${MQ_end_wave}.dta"
- "${final_data}after_psm_sample_${MQ_start_wave}-${MQ_end_wave}.dta"
- "${graphs}psm_balance_marital_quality.png"

Created: 09.12.2024 - Huong Nguyen

Updates:
- [MM.DD.YYYY] - [Author]: [Description of updates]

*******************************************************************************/

use "${final_data}sample_before_any_restrictions.dta", clear 

********************************************************************************
* PART 9. CONSTRUCT  ANALYSIS SAMPLE
********************************************************************************

**# 9.1 Restrict to waves with marital quality information

keep if wave > 6

**# 9.2 Condition 1: First observed wave must be non-caregiving

* Identify the first observed wave for each respondent

cap drop N
by hhidpn: gen N = _n

* Retain respondents who are non-caregivers in their first observed wave

by hhidpn: egen keep_group = ///
    total(rcaregivers == 0 & N == 1)

drop if keep_group == 0

drop keep_group


**# 9.3 Condition 2: Must be married in the first observed wave

drop N

cap by hhidpn: gen N = _n

by hhidpn: egen keep_group = ///
    total(inlist(rmstat, 1, 2, 3) & N == 1)

keep if keep_group == 1

drop keep_group


**# 9.4 Condition 3: Family with only mother alive

* (a) Father must no longer be alive

preserve

    by hhidpn: egen keep_group = ///
        total(rdadliv == 0 & N == 1)

    keep if keep_group == 1

restore


* (b) Mother must be alive in the first observed wave

sort hhidpn wave

drop N

by hhidpn: gen N = _n

by hhidpn: egen keep_group = ///
    total(rmomliv == 1 & N == 1)

keep if keep_group == 1

drop keep_group


**# 9.5 Save clean marital quality sample
save "${final_data}before_psm_sample_${MQ_start_wave}-${MQ_end_wave}.dta", replace

*Condition 4: Match two sample that has similar characteristics:
	keep if N == 1
	global x_vars rcesd  rameduc rafeduc  Educ_less_HS HS_degree at_least_some_college ragey_b  hnkid  white_caucasian rahispan female foreign_born full_time_work    rlog_networth
	cap ssc install psmatch2
	psmatch2 ever_care $x_vars, neighbor(1) noreplacement 
	
	 * Construct pair id
        gen long pair = .
        replace pair = _id if _treated == 0
        replace pair = _n1 if _treated == 1
	* Keep only valid pairs
        bysort pair: gen paircount = _N
        keep if paircount == 2
	* Order treated == 1 , treated == 0 
	gsort pair -_treated   // treated==1 lên trước, control==0 sau
	* keep matched pair only 
	keep if pair != . 
	bys hhidpn: keep if _n == 1 
	isid hhidpn
********************************************************************
* Step 4: Balance diagnostics (no birth_year_month here)
********************************************************************
psgraph, legend(label(1 "Non-Caregivers") label(2 "Caregivers"))
//graph export "${graphs}psm_balance_marital_quality.png", replace 
pstest 	$x_vars
return list
//esttab using "${tables}cov_balance_marital_quality.xlsx", replace
save "${final_data}match_psm_marital_quality.dta", replace

use "${final_data}before_psm_sample_${MQ_start_wave}-${MQ_end_wave}.dta", clear 
merge m:1 hhidpn using "${final_data}match_psm_marital_quality.dta"
keep if _m == 3 
drop _m 	
 
save "${final_data}after_psm_sample_${MQ_start_wave}-${MQ_end_wave}.dta", replace  

