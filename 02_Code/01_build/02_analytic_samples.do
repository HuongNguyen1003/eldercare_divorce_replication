

cap log close
display "Log timestamp: `c(current_date)' `c(current_time)'"
log using "${logs}`c(current_date)'_02_analytic_sample.log", replace

/*******************************************************************************
Title: Construct Caregiving and Marital Status Analysis Samples

Purpose: This script constructs longitudinal analytical samples to examine
         the relationship between parental caregiving, particularly continued
         caregiving for mothers, and marital outcomes.

         The script:
         - Loads the merged RAND HRS longitudinal, family, and marital quality
           dataset.
         - Constructs caregiving indicators based on personal care and
           errand assistance hours.
         - Identifies continued caregiving and first caregiving waves.
         - Constructs demographic, socioeconomic, health, and marital
           characteristics.
         - Identifies ever-caregivers and non-caregivers.
         - Applies sample restrictions based on respondents' initial
           caregiving and marital status.
         - Identifies caregivers with at least three pre-caregiving waves
           and three post-caregiving waves.
         - Classifies caregivers by caregiving duration.
         - Constructs matched samples using four alternative matching
           approaches:
             1. Propensity Score Matching (PSM)
             2. Exact 1:1 Matching Without Replacement
             3. Coarsened Exact Matching (CEM)
             4. Mahalanobis Distance Matching

Intake data:
- Merged RAND HRS longitudinal and family data with marital quality measures:
  "${final_data}merge_all.dta"

Output data:
- Create divorce sample before matching sample:
  "${final_data}divorce_sample_before_matching.dta"
- PSM matched sample:
  "${final_data}match_psm.dta"
- PSM-based marital status sample:
  "${final_data}marital_status_sample.dta"
- Exact matching:
  "${final_data}exact_match_one_to_one.dta"
  "${final_data}exact_match.dta"
- CEM matching:
  "${final_data}cem_match_one_to_one.dta"
  "${final_data}cem_match.dta"
- Mahalanobis matching:
  "${final_data}mahalanobis_match_one_to_one.dta"
  "${final_data}mb_match.dta"

Additional outputs:
- PSM balance graph:
  "${graphs}G4_main_psm_balance.png"
- PSM covariate balance table:
  "${tables}T1_main_cov_balance.xlsx"
- Potential exact matching pairs:
  "${final_data}all_exact_pairs.dta"
- Separate matched treated and control longitudinal samples.

Created: 11.04.2024 - Huong Nguyen
Updates:
- [MM.DD.YYYY] - [Author]: [Description of updates]

*******************************************************************************/


clear all 
set maxvar 20000
*Merge RAND HRS Longitudinal File and RAND HRS Family Data files
{
use "${raw_data}randhrs1992_2020v1.dta"
merge 1:1 hhidpn using "${raw_data}randhrsfamr1992_2018v2.dta"
drop _merge 
merge 1:1 rahhidpn using "${final_data}marital_quality_${MQ_start_wave}-${MQ_end_wave}.dta"
}


*Keep variables of interest
{
keep hhidpn r*iwstat r*cesd  r*hearts  r*hibpe  r*shlt  r*racem  r*gender  r*mstat  r*agey_b  raeduc h*child h*gkid r*lbrf  h*atotb r*mpchelp   r*mmemdis r*momage   r*meduc r*feduc h*nson h*ndau r*ppcr r*prpcrh s*prpcrh r*perd r*prerdh s*prerdh r*mlivwho  r*momliv  r*dadliv raedyrs rabplace r*mpchelp r*mlvlone rahispan h*nkid r*liws r*understand r*relyprobl r*openworries r*demand r*criticize r*letdown r*nerves r*closesp r*wtresp

drop r*mmstat  r*fmstat 

duplicates report hhidpn 
duplicates list hhidpn
duplicates drop
drop if missing(hhidpn)

*transform wide to long format 
reshape long r@iwstat r@cesd  r@hearts  r@hibpe  r@shlt  r@racem  r@gender  r@mstat r@agey_b  h@child h@gkid r@lbrf  h@atotb r@mpchelp   r@mmemdis r@momage   r@meduc r@feduc h@nson h@ndau r@ppcr r@prpcrh s@prpcrh r@perd r@prerdh s@prerdh  r@mlivwho  r@momliv  r@dadliv r@inlbrf    r@mlvlone h@nkid r@liws r@understand r@relyprobl r@openworries r@demand r@criticize r@letdown r@nerves r@closesp r@wtresp, i(hhidpn) j(wave)

keep if riwstat == 1
}

//use "${final_data}merge_all.dta", clear 

*create caregiver variable 
{
sort hhidpn wave 
by hhidpn: gen rcaregivers = 1 if (rprerdh > 0 & !missing(rprerdh)) | (rprpcrh > 0 & !missing(rprpcrh) ) 

by hhidpn: replace rcaregivers = 1 if rprerdh > 0 &  inlist(rprpcrh, ., .s ) 
by hhidpn: replace rcaregivers = 1 if rprpcrh > 0 & inlist(rprerdh, . , .s)

by hhidpn: replace rcaregivers = 0 if (inlist(rprerdh, ., .s ) | rprerdh < 1) & (inlist(rprpcrh, . , .s) | rprpcrh < 1)

*define someone care for mom but mom died is non-caregiver 
*by hhidpn: replace rcaregivers = 0  if (rcaregivers == 1 & rmomliv == 0) 

}

*Generate continued_care variables
{
xtset hhidpn wave
by hhidpn: gen continued_care = 1 if rcaregivers == 1 & L.rcaregivers == 1 
by hhidpn: replace continued_care = 0 if rcaregivers == 0 & L.rcaregivers == 1
}
*Generate binary variables
{
*Generate mother_died variable 
gen mother_alive = 1 if rmomliv == 1
replace mother_alive = 0 if rmomliv == 0

gen divorced = 0 if  inlist(rmstat,1,2,3)
replace divorced = 1 if inlist(rmstat,4,5,6)
replace divorced = . if missing(rmstat)
*gender 
gen female = (ragender == 2)

*Job status
gen full_time_work = (rlbrf == 1) 
gen part_time_work = (rlbrf ==2) 
gen not_working = (rlbrf > 2) 

*networth
replace hatotb = 0 if hatotb < 0 
gen rlog_networth = log(hatotb) 
replace rlog_networth = log(hatotb + 1) if hatotb == 0

*Age, age square
gen rage2 = ragey_b^2 

*foreign born or not
gen foreign_born = 0 if rabplace != 11 & rabplace != .
replace foreign_born = 1 if rabplace == 11

*Education variables 

gen Educ_less_HS = (raeduc == 1) if !missing(raeduc)

gen HS_degree = (raeduc == 2)
*replace HS_degree = 0 if !inrange(raeduc,2,3) & !missing(raeduc)

gen at_least_some_college = 1 if inrange(raeduc,4,5)
replace at_least_some_college = 0 if !inrange(raeduc,4,5) & !missing(raeduc)

*Race variable
gen white_caucasian = (raracem == 1) if !missing(raracem)
gen black = (raracem == 2) if !missing(raracem)
gen other_race = (raracem == 3) if !missing(raracem)

gen good_excellent_health = (rshlt == 1| rshlt == 2)
replace good_excellent_health = 0 if inlist(rshlt,3,4,5)

*Indicator method for missing values
gen rmpchelp_miss = inlist(rmpchelp,.a, .d , .m , .r , .s)
gen rmlvlone_miss = inlist(rmlvlone,.a, .d , .m , .r , .s )
replace rmpchelp = 0 if inlist(rmpchelp,.a, .d , .m , .r , .s)
replace rmlvlone = 0 if inlist(rmlvlone,.a, .d , .m , .r , .s )

*Low income 
gen low_income = hatotb <= 0 //define financially vulnerable households as those with non-positive net worth.
label define inc 0 "Not low income" 1 "Low income"
label values low_income inc

}


*Marital quality 
*Positive marital quality: replace missing with mean 
{
local positive_list runderstand rrelyprobl ropenworries rclosesp
egen mean_all = rowmean(`positive_list')
foreach i of local positive_list {
	replace `i' = mean_all if missing(`i')
}
drop mean_all
local positive_list runderstand rrelyprobl ropenworries rclosesp
egen positive_quality_avg = rowmean(`positive_list')
egen positive_quality_sum = rowtotal(`positive_list')
order runderstand rrelyprobl ropenworries rclosesp, after(positive_quality_avg)

local negative_list rdemand rcriticize rletdown rnerves
egen mean_all = rowmean(`negative_list')

foreach i of local negative_list {
	replace `i' = mean_all if missing(`i')
}
drop mean_all

local negative_list rdemand rcriticize rletdown rnerves
egen negative_quality_avg = rowmean(`negative_list')
egen negative_quality_sum = rowtotal(`negative_list')
order rdemand rcriticize rletdown rnerves, after(negative_quality_avg)
}
*create Quantiles of the Dependent Variable
{
xtile quantile_negative_quality_sum = negative_quality_sum,nq(4)
xtile quantile_negative_quality_avg = negative_quality_avg, nq(4)
xtile quantile_positive_quality_sum = positive_quality_sum,nq(4)
xtile quantile_positive_quality_avg = positive_quality_avg, nq(4)
}
*Indexing sum 
{
gen good_perception = 1 if positive_quality_sum <= 4 
replace good_perception = 0 if positive_quality_sum > 4

gen bad_perception = 1 if negative_quality_sum <= 4
replace bad_perception = 0 if negative_quality_sum > 4
}


*Label variables: 
{
local Health_measures rcesd 
label variable rcesd "Depressive symptoms score (CESD-8)"

local Demographic_characteristics ragender female is_eldest_female married ragey_b raedyrs hchild
label variable female "Female"
label variable ragey_b "Age"
label variable raedyrs "Education (years)"
label var Educ_less_HS "Less than high school"
label var HS_degree "High school degree"
label var at_least_some_college "At least some college"
label var white_caucasian "White/Causasian"
label var black "Black"
label var other_race "Other races"
label var rahispan "Hispanic"
label var foreign_born "Foreign born"
label var rlog_networth "Log(networth)"
label var rafeduc "Father's education(years)"
label variable hnkid "Number of children reported"

local Work_wealth_measures full_time at_least_part_time hatotb
label variable hatotb "Net worth"
label var full_time_work "Work full time"
label var part_time_work "Work at least part time"

local mother_characteristics rmpchelp rmmemdis rmomage rameduc 
label variable rmomage "Mother's age"
label variable rameduc "Mother's education (years)"


label variable rperd "Spend 100+ caring for parents since last two years"
label variable rprpcrh "Personal Care: Hours Respondent Helped Parents"
label variable rprerdh "Errand Help: Hours Respondent Helped Parents "
label variable rcaregivers "Caregivers or not"


label variable rmpchelp " Mother needs help with personal care "
label variable rmlvlone " Mother can be left alone for 1h+ " 


label variable rcaregivers "Care for mom"
label variable mother_alive "Mom survives"

}


*Two group: 1.ever caregiver, 2.never caregiver
bys hhidpn : egen temp_max_care  = max(rcaregiver) 
bys hhidpn : gen ever_care = temp_max_care > 0 

save "${final_data}sample_before_any_restrictions.dta", replace 

*Condition 1: first time enter the sample must be non-caregivers 
cap drop N 
by hhidpn: gen N = _n
by hhidpn: egen keep_group = total(rcaregivers == 0 & N == 1)
drop if keep_group == 0
drop keep_group

*Condition 2: The first time people entering survey must be married 
	drop N
	cap by hhidpn: gen N = _n
	by hhidpn: egen keep_group = total(inlist(rmstat, 1,2,3) & N == 1 )
	keep if keep_group == 1
	drop keep_group 
	
/*	
*Condition 3: Family with only mother alive
	*(a) dad is no longer alive 
	preserve 
	by hhidpn: egen keep_group = total(rdadliv == 0  & N == 1 )
	keep if keep_group == 1 
	restore 
	
	*(b) mom is alive in the first wave surveryed 
	sort hhidpn wave 
	drop N
	by hhidpn: gen N = _n 
	by hhidpn: egen keep_group = total(rmomliv == 1 & N == 1)
	keep if keep_group == 1
	drop keep_group 
	*/

**# gen first_care_wave 
*------------------------------------------------------------*
* 1. Sort data
sort hhidpn wave

*------------------------------------------------------------*
* 2. Identify first wave of caregiving
by hhidpn: egen first_care_wave = min(cond(rcaregivers == 1, wave, .))

* Flag first caregiving wave
gen first_care = (wave == first_care_wave) if !missing(first_care_wave)

* 3. Check whether respondent is continuously observed
*    for at least 3 years BEFORE caregiving
* We need observations at t-1, t-2, t-3
forvalues k = 1/3 {
    by hhidpn: gen pre`k' = (wave == first_care_wave - `k')
}

* Count number of required pre-caregiving waves
by hhidpn: egen n_pre = total(pre1 | pre2 | pre3)

*------------------------------------------------------------*
* 4. Check 3 waves AFTER caregiving
forvalues k = 1/3 {
    by hhidpn: gen post`k' = (wave == first_care_wave + `k')
}

by hhidpn: egen n_post = total(post1 | post2 | post3)

*------------------------------------------------------------*
* 5. Final caregiver eligibility flag
gen caregiver_eligible = ///
    (first_care_wave < . & n_pre == 3 & n_post == 3)

label var caregiver_eligible ///
    "Caregiver with 3 continuous pre-years and 3 post-waves"

**# caregivers type 
bys hhidpn (wave): egen total_care_wave = total(rcaregivers) 

tab total_care_wave

gen short_caregivers = total_care_wave == 1 
gen long_term_caregivers = total_care_wave >= 2

save "${final_data}divorce_sample_before_matching.dta", replace 

use "${final_data}divorce_sample_before_matching.dta", clear 
*Condition 4: Match two sample that has similar characteristics:
	keep if N == 1
	global x_vars rcesd  rameduc rafeduc  Educ_less_HS HS_degree at_least_some_college ragey_b  hnkid  white_caucasian  female foreign_born full_time_work    rlog_networth
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
cap log close 
log using "${tables}TableA2_main_cov_balance.log"
psgraph, legend(label(1 "Non-Caregivers") label(2 "Caregivers"))
graph export "${graphs}FigureA2_main_psm_balance.png", replace 
pstest 	$x_vars
return list
esttab using "${tables}TableA2_main_cov_balance.xlsx", replace
log close 
save "${final_data}match_psm.dta", replace

use "${final_data}divorce_sample_before_matching.dta", clear 
merge m:1 hhidpn using "${final_data}match_psm.dta"
keep if _m == 3 
drop _m 

save "${final_data}marital_status_sample.dta", replace  

**# Exact match 
{
*=========================================================
* EXACT 1:1 MATCHING WITHOUT REPLACEMENT
*=========================================================

*---------------------------------------------------------
* 0. Variables used for exact matching
*---------------------------------------------------------

global individual_vars ///
    rcesd ///
    ragey_b female white black other_race foreign_born ///
    Educ_less_HS HS_degree at_least_some_college ///
    full_time_work

global household_vars ///
    hnkid low_income

global parental_vars ///
    rafeduc rameduc

local matched_vars $individual_vars $household_vars $parental_vars


*---------------------------------------------------------
* 1. Prepare one observation per individual
*---------------------------------------------------------

use "${final_data}divorce_sample_before_matching.dta", clear

sort hhidpn wave

by hhidpn: keep if _n == 1

* Keep only observations with nonmissing matching variables
egen nmiss_match = rowmiss(`matched_vars')
drop if nmiss_match > 0
drop nmiss_match


*---------------------------------------------------------
* 2. Create treated pool
*---------------------------------------------------------

preserve

keep if ever_care == 1

gen id_treated = _n

keep id_treated hhidpn `matched_vars'

rename hhidpn hhidpn_treated

duplicates drop

tempfile treated
save `treated', replace

restore


*---------------------------------------------------------
* 3. Create control pool
*---------------------------------------------------------

keep if ever_care == 0

gen id_control = _n

keep id_control hhidpn `matched_vars'

rename hhidpn hhidpn_control

duplicates drop

tempfile control
save `control', replace


*---------------------------------------------------------
* 4. Generate all possible exact matches
*---------------------------------------------------------

use `treated', clear

joinby `matched_vars' using `control'

* Each row is one possible treated-control match

count
display "Number of possible exact matches = " r(N)

save "${final_data}all_exact_pairs.dta", replace


*---------------------------------------------------------
* 5. Randomize potential matches
*---------------------------------------------------------

use "${final_data}all_exact_pairs.dta", clear

set seed 12345

gen random_order = runiform()

* Randomize the order of treated individuals
bysort id_treated: egen treated_random = mean(random_order)

* Randomize controls within each treated person
sort treated_random random_order

tempfile allpairs
save `allpairs', replace


*---------------------------------------------------------
* 6. Sequential 1:1 matching WITHOUT replacement
*---------------------------------------------------------

* Empty file containing matched pairs
clear
set obs 0

gen id_treated = .
gen id_control = .
gen hhidpn_treated = .
gen hhidpn_control = .

tempfile matched
save `matched', replace


* Empty file containing controls already used
clear
set obs 0

gen id_control = .

tempfile used
save `used', replace


* Obtain list of treated individuals
use `allpairs', clear

levelsof id_treated, local(treated_list)


foreach t of local treated_list {

    *---------------------------------------------
    * Get potential controls for this treated
    *---------------------------------------------

    use `allpairs', clear

    keep if id_treated == `t'

    * Randomize potential controls
    sort random_order

    *---------------------------------------------
    * Remove controls already used
    *---------------------------------------------

    merge m:1 id_control using `used'

    * _merge == 3 means control has already been used
    keep if _merge == 1

    drop _merge

    *---------------------------------------------
    * If no unused control remains, skip
    *---------------------------------------------

    count

    if r(N) == 0 {
        continue
    }

    *---------------------------------------------
    * Select ONE control
    *---------------------------------------------

    keep in 1

    *---------------------------------------------
    * Save selected pair
    *---------------------------------------------

    save `matched', replace


    *---------------------------------------------
    * Add selected control to used list
    *---------------------------------------------

    preserve

        keep id_control

        append using `used'

        duplicates drop id_control, force

        save `used', replace

    restore
}


*---------------------------------------------------------
* 7. Load final matched pairs
*---------------------------------------------------------

use `matched', clear

sort id_treated

count

display "Number of treated individuals matched = " r(N)


*---------------------------------------------------------
* 8. Quality checks
*---------------------------------------------------------

* Each treated person should appear exactly once
bysort id_treated: gen n_treated = _N

tab n_treated

assert n_treated == 1

drop n_treated


* Each control should appear exactly once
bysort id_control: gen n_control = _N

tab n_control

assert n_control == 1

drop n_control


* Verify 1:1 matching
count

display "Final number of matched treated-control pairs = " r(N)


*---------------------------------------------------------
* 9. Save matched pair file
*---------------------------------------------------------

save "${final_data}exact_match_one_to_one.dta", replace

*=========================================================
* 10. Get longitudinal observations for MATCHED TREATED
*=========================================================

use "${final_data}exact_match_one_to_one.dta", clear

keep hhidpn_treated

duplicates drop

rename hhidpn_treated hhidpn

merge 1:m hhidpn using "${final_data}divorce_sample_before_matching.dta"

keep if _merge == 3
drop _merge

gen matched_treated = 1

save "${final_data}exact_match_treated.dta", replace

*=========================================================
* 11. Get longitudinal observations for MATCHED CONTROLS
*=========================================================

use "${final_data}exact_match_one_to_one.dta", clear

keep hhidpn_control

duplicates drop

rename hhidpn_control hhidpn

merge 1:m hhidpn using "${final_data}divorce_sample_before_matching.dta"

keep if _merge == 3
drop _merge

gen matched_treated = 0

save "${final_data}exact_match_control.dta", replace

*=========================================================
* 12. Final matched longitudinal dataset
*=========================================================

use "${final_data}exact_match_treated.dta", clear

append using "${final_data}exact_match_control.dta"
}
save "${final_data}exact_match.dta", replace

**# CEM 

************************************************************
* CEM 1:1 MATCHING
* Matching variables aligned with previous exact matching
************************************************************

use "${final_data}divorce_sample_before_matching.dta", clear
{
sort hhidpn wave

************************************************************
* 1. Keep one observation per individual
************************************************************

by hhidpn: keep if _n == 1


************************************************************
* 2. Coarsen continuous variables
************************************************************
*-----------------------------------------------------------
* Age: 5-year groups
*-----------------------------------------------------------

recode ragey_b ///
    (min/39 = 1) ///
    (40/44 = 2) ///
    (45/49 = 3) ///
    (50/54 = 4) ///
    (55/59 = 5) ///
    (60/64 = 6) ///
    (65/69 = 7) ///
    (70/74 = 8) ///
    (75/79 = 9) ///
    (80/max = 10), ///
    gen(ragey_b_cem)

label define age_cem ///
    1 "<40" ///
    2 "40-44" ///
    3 "45-49" ///
    4 "50-54" ///
    5 "55-59" ///
    6 "60-64" ///
    7 "65-69" ///
    8 "70-74" ///
    9 "75-79" ///
    10 "80+"

label values ragey_b_cem age_cem

*-----------------------------------------------------------
* Parental education
*-----------------------------------------------------------
*
* If rafeduc and rameduc are years of education, coarsen them.
*
*-----------------------------------------------------------
* Father's education
*-----------------------------------------------------------

recode rafeduc ///
    (min/8   = 1) ///
    (9/11    = 2) ///
    (12      = 3) ///
    (13/15   = 4) ///
    (16/max  = 5), ///
    gen(rafeduc_cem)

label define fedu_cem ///
    1 "<=8 years" ///
    2 "9-11 years" ///
    3 "12 years" ///
    4 "13-15 years" ///
    5 "16+ years"

label values rafeduc_cem fedu_cem

*-----------------------------------------------------------
* Mother's education
*-----------------------------------------------------------

recode rameduc ///
    (min/8   = 1) ///
    (9/11    = 2) ///
    (12      = 3) ///
    (13/15   = 4) ///
    (16/max  = 5), ///
    gen(rameduc_cem)


label values rafeduc_cem fedu_cem


************************************************************
* 3. Create CEM strata
************************************************************

egen cem_strata = group( ///
    ragey_b_cem ///
    rcesd ///
    female ///
    white black other_race ///
    foreign_born ///
    Educ_less_HS HS_degree at_least_some_college ///
    full_time_work ///
    hnkid ///
    low_income ///
    rafeduc_cem ///
    rameduc_cem )


************************************************************
* 4. Check distribution of strata
************************************************************

tab cem_strata

bysort cem_strata: egen n_treat = total(ever_care == 1)
bysort cem_strata: egen n_ctrl  = total(ever_care == 0)

tab n_treat
tab n_ctrl


************************************************************
* 5. Keep only strata containing BOTH treated and controls
************************************************************

keep if n_treat > 0 & n_ctrl > 0

drop n_treat n_ctrl


************************************************************
* 6. Randomize observations within each CEM stratum
************************************************************

set seed 12345

gen u = runiform()

sort cem_strata ever_care u


************************************************************
* 7. Create treated file
************************************************************

preserve

keep if ever_care == 1

by cem_strata: gen pairid = _n

rename hhidpn hhidpn_treated

keep ///
    cem_strata ///
    pairid ///
    hhidpn_treated

tempfile treated
save `treated', replace

restore


************************************************************
* 8. Create control file
************************************************************

keep if ever_care == 0

by cem_strata: gen pairid = _n

rename hhidpn hhidpn_control

keep ///
    cem_strata ///
    pairid ///
    hhidpn_control

tempfile control
save `control', replace


************************************************************
* 9. 1:1 matching WITHOUT replacement
************************************************************

use `treated', clear

merge 1:1 cem_strata pairid using `control'

keep if _merge == 3
drop _merge


************************************************************
* 10. Quality checks
************************************************************

* Each treated individual appears once
isid hhidpn_treated

* Each control individual appears once
isid hhidpn_control

* Number of matched pairs
count

display "Number of matched pairs = " r(N)


************************************************************
* 11. Save matched pairs
************************************************************

save "${final_data}cem_match_one_to_one.dta", replace


************************************************************
* 12. Expand matched CONTROLS to longitudinal data
************************************************************

use "${final_data}cem_match_one_to_one.dta", clear

keep hhidpn_control

duplicates drop

rename hhidpn_control hhidpn

merge 1:m hhidpn using ///
    "${final_data}divorce_sample_before_matching.dta"

keep if _merge == 3
drop _merge

gen matched_treated = 0

save "${final_data}cem_match_control.dta", replace


************************************************************
* 13. Expand matched TREATED to longitudinal data
************************************************************

use "${final_data}cem_match_one_to_one.dta", clear

keep hhidpn_treated

duplicates drop

rename hhidpn_treated hhidpn

merge 1:m hhidpn using ///
    "${final_data}divorce_sample_before_matching.dta"

keep if _merge == 3
drop _merge

gen matched_treated = 1

save "${final_data}cem_match_treated.dta", replace


************************************************************
* 14. Combine treated and controls
************************************************************

use "${final_data}cem_match_treated.dta", clear

append using "${final_data}cem_match_control.dta"


************************************************************
* 15. Save final longitudinal CEM sample
************************************************************
}
save "${final_data}cem_match.dta", replace


**# MB

********************************************************************
* Mahalanobis Matching (1:1, without replacement) using kmatch
*
* IMPORTANT
* ----------
* This do-file is a TEMPLATE for Stata 18 + kmatch.
* Because kmatch stores matched IDs differently from psmatch2,
* you MUST verify the variable names created by idgenerate().
*
* Install once:
*     ssc install moremata, replace
*     ssc install kmatch, replace
********************************************************************

use "${final_data}divorce_sample_before_matching.dta", clear
{
sort hhidpn wave


********************************************************************
* 1. Keep one observation per individual
********************************************************************

by hhidpn: keep if _n == 1


********************************************************************
* 2. Matching variables
********************************************************************

local individual_vars ///
    rcesd ///
    ragey_b ///
    female ///
    white ///
    black ///
    other_race ///
    foreign_born ///
    Educ_less_HS ///
    HS_degree ///
    at_least_some_college ///
    full_time_work

local household_vars ///
    hnkid ///
    low_income

local parental_vars ///
    rafeduc ///
    rameduc

local matched_vars ///
    `individual_vars' ///
    `household_vars' ///
    `parental_vars'


********************************************************************
* 3. Keep variables needed for matching
********************************************************************

keep ///
    hhidpn ///
    ever_care ///
    first_care_wave ///
    `matched_vars'


********************************************************************
* 4. Remove observations with missing matching variables
********************************************************************

egen nmiss_match = rowmiss(`matched_vars')

tab nmiss_match

drop if nmiss_match > 0

drop nmiss_match


********************************************************************
* 5. Create permanent matching ID
********************************************************************
*
* IMPORTANT:
* Do NOT recreate this ID later from clean_data.dta.
* This ID is the ID that kmatch will use to identify controls.
********************************************************************

gen match_id = _n

isid match_id


********************************************************************
* 6. Save the exact matching sample BEFORE kmatch
********************************************************************

tempfile matching_base
save `matching_base', replace


********************************************************************
* 7. Check treatment and control groups
********************************************************************

tab ever_care

count if ever_care == 1
local N_treated = r(N)

count if ever_care == 0
local N_control = r(N)

display "Treated observations = `N_treated'"
display "Control observations = `N_control'"


********************************************************************
* 8. Mahalanobis 1:1 matching WITHOUT replacement
********************************************************************
*
* nn(1)       = one nearest neighbor
* wor         = without replacement
* idgenerate  = generate ID of matched control
* idvar()     = use match_id as the observation ID
********************************************************************

kmatch md ever_care ///
    `matched_vars', ///
    nn(1) ///
    wor ///
    idgenerate(match) ///
    idvar(match_id)


********************************************************************
* 9. Inspect matching variables generated by kmatch
********************************************************************

describe match*

list ///
    match_id ///
    ever_care ///
    match1 ///
    in 1/20


********************************************************************
* 10. Keep treated observations with a matched control
********************************************************************

keep if ever_care == 1
keep if !missing(match1)


********************************************************************
* 11. Rename matched control ID
********************************************************************

rename match1 match_id_control


********************************************************************
* 12. Keep matched treated-control IDs
********************************************************************

keep ///
    match_id ///
    match_id_control

rename match_id match_id_treated

tempfile matched_ids
save `matched_ids', replace


********************************************************************
* 13. Recover treated respondent information
********************************************************************

use `matching_base', clear

keep ///
    match_id ///
    hhidpn ///
    ever_care ///
    first_care_wave

rename match_id match_id_treated
rename hhidpn match_hhidpn_treated
rename first_care_wave treated_care_wave

tempfile treated_info
save `treated_info', replace


********************************************************************
* 14. Recover control respondent information
********************************************************************

use `matching_base', clear

keep ///
    match_id ///
    hhidpn

rename match_id match_id_control
rename hhidpn match_hhidpn_control

tempfile control_info
save `control_info', replace


********************************************************************
* 15. Construct final matched-pair dataset
********************************************************************

use `matched_ids', clear


* Attach treated information
merge 1:1 match_id_treated using `treated_info'

keep if _merge == 3
drop _merge


* Attach control information
merge 1:1 match_id_control using `control_info'

keep if _merge == 3
drop _merge


********************************************************************
* 16. Quality checks
********************************************************************

* One observation per treated
isid match_id_treated

* One observation per control
isid match_id_control

* Treated and control must be different respondents
assert match_hhidpn_treated != match_hhidpn_control

* Number of matched pairs
count

display "Final number of matched pairs = " r(N)


* Check respondent IDs
duplicates report match_hhidpn_treated
duplicates report match_hhidpn_control

list ///
    match_hhidpn_treated ///
    match_hhidpn_control ///
    treated_care_wave ///
    in 1/20


********************************************************************
* 17. Save final matched-pair file
********************************************************************

save "${final_data}mahalanobis_match_one_to_one.dta", replace

********************************************************************
* Mahalanobis Distance Matching
* 1:1 matching WITHOUT replacement
*
* Matching variables are the same as the Exact/CEM specification.
*
* Required packages:
*     ssc install moremata, replace
*     ssc install kmatch, replace
********************************************************************

use "${final_data}divorce_sample_before_matching.dta", clear

sort hhidpn wave


********************************************************************
* 1. Keep one observation per individual
********************************************************************

by hhidpn: keep if _n == 1


********************************************************************
* 2. Matching variables
********************************************************************

local individual_vars ///
    rcesd ///
    ragey_b ///
    female ///
    white ///
    black ///
    other_race ///
    foreign_born ///
    Educ_less_HS ///
    HS_degree ///
    at_least_some_college ///
    full_time_work

local household_vars ///
    hnkid ///
    low_income

local parental_vars ///
    rafeduc ///
    rameduc

local matched_vars ///
    `individual_vars' ///
    `household_vars' ///
    `parental_vars'


********************************************************************
* 3. Keep variables needed for matching
********************************************************************

keep ///
    hhidpn ///
    ever_care ///
    first_care_wave ///
    `matched_vars'


********************************************************************
* 4. Remove observations with missing matching variables
********************************************************************

egen nmiss_match = rowmiss(`matched_vars')

tab nmiss_match

drop if nmiss_match > 0

drop nmiss_match


********************************************************************
* 5. Create permanent matching ID
********************************************************************
*
* IMPORTANT:
* Do NOT recreate this ID later from clean_data.dta.
* This ID is the ID that kmatch will use to identify controls.
********************************************************************

gen match_id = _n

isid match_id


********************************************************************
* 6. Save the exact matching sample BEFORE kmatch
********************************************************************

tempfile matching_base
save `matching_base', replace


********************************************************************
* 7. Check treatment and control groups
********************************************************************

tab ever_care

count if ever_care == 1
local N_treated = r(N)

count if ever_care == 0
local N_control = r(N)

display "Treated observations = `N_treated'"
display "Control observations = `N_control'"


********************************************************************
* 8. Mahalanobis 1:1 matching WITHOUT replacement
********************************************************************
*
* nn(1)       = one nearest neighbor
* wor         = without replacement
* idgenerate  = generate ID of matched control
* idvar()     = use match_id as the observation ID
********************************************************************

kmatch md ever_care ///
    `matched_vars', ///
    nn(1) ///
    wor ///
    idgenerate(match) ///
    idvar(match_id)


********************************************************************
* 9. Inspect matching variables generated by kmatch
********************************************************************

describe match*

list ///
    match_id ///
    ever_care ///
    match1 ///
    in 1/20


********************************************************************
* 10. Keep treated observations with a matched control
********************************************************************

keep if ever_care == 1
keep if !missing(match1)


********************************************************************
* 11. Rename matched control ID
********************************************************************

rename match1 match_id_control


********************************************************************
* 12. Keep matched treated-control IDs
********************************************************************

keep ///
    match_id ///
    match_id_control

rename match_id match_id_treated

tempfile matched_ids
save `matched_ids', replace


********************************************************************
* 13. Recover treated respondent information
********************************************************************

use `matching_base', clear

keep ///
    match_id ///
    hhidpn ///
    ever_care ///
    first_care_wave

rename match_id match_id_treated
rename hhidpn match_hhidpn_treated
rename first_care_wave treated_care_wave

tempfile treated_info
save `treated_info', replace


********************************************************************
* 14. Recover control respondent information
********************************************************************

use `matching_base', clear

keep ///
    match_id ///
    hhidpn

rename match_id match_id_control
rename hhidpn match_hhidpn_control

tempfile control_info
save `control_info', replace


********************************************************************
* 15. Construct final matched-pair dataset
********************************************************************

use `matched_ids', clear


* Attach treated information
merge 1:1 match_id_treated using `treated_info'

keep if _merge == 3
drop _merge


* Attach control information
merge 1:1 match_id_control using `control_info'

keep if _merge == 3
drop _merge


********************************************************************
* 16. Quality checks
********************************************************************

* One observation per treated
isid match_id_treated

* One observation per control
isid match_id_control

* Treated and control must be different respondents
assert match_hhidpn_treated != match_hhidpn_control

* Number of matched pairs
count

display "Final number of matched pairs = " r(N)


* Check respondent IDs
duplicates report match_hhidpn_treated
duplicates report match_hhidpn_control

list ///
    match_hhidpn_treated ///
    match_hhidpn_control ///
    treated_care_wave ///
    in 1/20


********************************************************************
* 17. Save final matched-pair file
********************************************************************

save "${final_data}mahalanobis_match_one_to_one.dta", replace


********************************************************************
* 18. Expand MATCHED TREATED to longitudinal data
********************************************************************

use "${final_data}mahalanobis_match_one_to_one.dta", clear

keep ///
    match_hhidpn_treated ///
    match_hhidpn_control ///
    treated_care_wave

rename match_hhidpn_treated hhidpn

merge 1:m hhidpn using ///
    "${final_data}divorce_sample_before_matching.dta"

keep if _merge == 3
drop _merge


* Treated indicator
gen matched_treated = 1


* Treated respondent's actual caregiving onset
gen placebo_shock_time = treated_care_wave

tempfile treated_long
save `treated_long', replace


********************************************************************
* 19. Expand MATCHED CONTROLS to longitudinal data
********************************************************************

use "${final_data}mahalanobis_match_one_to_one.dta", clear

keep ///
    match_hhidpn_treated ///
    match_hhidpn_control ///
    treated_care_wave

rename match_hhidpn_control hhidpn

merge 1:m hhidpn using ///
    "${final_data}divorce_sample_before_matching.dta"
keep if _merge == 3
drop _merge


* Control indicator
gen matched_treated = 0


* Use treated person's caregiving wave as placebo treatment time
gen placebo_shock_time = treated_care_wave

tempfile controls_long
save `controls_long', replace


********************************************************************
* 20. Append treated and controls
********************************************************************

use `treated_long', clear

append using `controls_long'


********************************************************************
* 21. Check final sample
********************************************************************

tab matched_treated

bysort hhidpn: assert matched_treated == matched_treated[1]

count

display "Final longitudinal observations = " r(N)


********************************************************************
* 26. Save final Mahalanobis matched dataset
********************************************************************
}
save "${final_data}mb_match.dta", replace

