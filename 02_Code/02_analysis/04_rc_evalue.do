
cap log close
log using "${logs}`c(current_date)'_evalue.log", replace
display "Log timestamp: `c(current_date)' `c(current_time)'"


/*
Title: E-value Sensitivity Analysis for the Association Between Parental Caregiving and Divorce

Purpose:
    Assess the robustness of the estimated association between parental
    caregiving and divorce to potential unobserved confounding.

    1. Prepare the survival analysis dataset:
       - Define start and stop times for person-wave observations.
       - Set up the Cox survival model using divorce as the failure event.

    2. Estimate Cox proportional hazards models:
       - Run four specifications with progressively richer sets of controls.
       - Calculate coefficients, standard errors, hazard ratios, and
         95% confidence intervals.
       - Calculate E-values for the estimated association and its
         confidence interval.

    3. Benchmark E-values against observed covariates:
       - Examine the associations of selected covariates with caregiving.
       - Estimate the associations of the same covariates with divorce.
       - Include socioeconomic characteristics, depressive symptoms,
         age, and employment status.
       - Standardize continuous covariates using a one-standard-deviation
         contrast and evaluate binary covariates using a 0-versus-1 contrast.
       - Compare the observed covariate associations with the E-value
         benchmark from the preferred Cox model.

Output:
    Input data:
        - ${final_data}psm_divorce_sample.dta

    Excel results:
        - ${tables}main.xlsx
            - Sheet "e-value": Cox regression results and E-values
        - ${tables}evalue_benchmark_table.xlsx
            - E-value benchmarking against observed covariates

    Log files:
        - ${logs}`c(current_date)'_evalue_${change}.log
        - evalue_benchmark.log

Modified by: Huong Nguyen
Date: 2026/01/31
*/

****************************************************
*  log: main hoặc rc5b
****************************************************

****************************************************
* Set globals with mode
****************************************************
 global dataset marital_status_sample
 global file psm

****************************************************
* Run analyses according to mode 
****************************************************
use "${final_data}$dataset.dta", clear

**# set up cox 

sort hhidpn wave

* Start of interval = current wave
by hhidpn (wave): gen start_time = wave

* End of interval = next observed wave
by hhidpn (wave): gen stop_time = wave[_n+1]

* Drop last observation for each person
drop if missing(stop_time)

* Check
list hhidpn wave start_time stop_time in 1/100, sepby(hhidpn) 

stset stop_time, ///
    enter(time start_time) ///
    failure(divorced == 1) ///
    id(hhidpn)
*Check if observations after divorced is not included 
bysort hhidpn (wave): egen divorce_wave = ///
    min(cond(divorced == 1, wave, .))
gen after_divorce = wave > divorce_wave ///
    if !missing(divorce_wave)

tab after_divorce
count if after_divorce == 1 & _st == 1
* 1. Child characteristics
global individual_vars ///
    rcesd ///
    ragey_b female white black other_race foreign_born ///
    Educ_less_HS HS_degree at_least_some_college ///
    full_time_work
* 2. Household characteristics
global household_vars hnkid low_income

* 3. Parental characteristics
global parental_vars rafeduc rameduc

local full_controls rcesd ragey_b female white black other_race foreign_born Educ_less_HS HS_degree at_least_some_college full_time_work hnkid low_income
local spec1 
local spec2 $individual_vars
local spec3 $individual_vars $household_vars 
local spec4 $individual_vars $household_vars $parental_vars

**# COEFICIENT

********************************************************************
* Cox regressions + E-values
********************************************************************
********************************************************************
* Prepare Excel output
********************************************************************
putexcel set "${tables}Table.xlsx", sheet("Table4-Evalue") modify

putexcel A1=("Model") ///
         B1=("Coef") ///
         C1=("SE") ///
         D1=("HR") ///
         E1=("Lower95CI") ///
         F1=("Upper95CI") ///
         G1=("E-value") ///
         H1=("E-value (CI)")
	 
forvalues i = 1/4 {

stcox rcaregivers `spec`i'' , nohr vce(cluster hhidpn)
est store m`i'


    ************************************************
    * Coefficient
    ************************************************

    local b  = _b[rcaregivers]
    local se = _se[rcaregivers]

    local z = invnormal(.975)

    local lb_coef = `b' - `z'*`se'
    local ub_coef = `b' + `z'*`se'

    ************************************************
    * Hazard ratio
    ************************************************

    local hr = exp(`b')
    local lb = exp(`lb_coef')
    local ub = exp(`ub_coef')

    ************************************************
    * Significance stars
    ************************************************

    local p = 2*normal(-abs(`b'/`se'))

    local star ""
    if (`p'<0.01) local star "***"
    else if (`p'<0.05) local star "**"
    else if (`p'<0.10) local star "*"

    local coefstar_sp`i' = string(`b',"%9.3f")+"`star'"
    local std_sp`i'      = string(`se',"%9.3f")
    local exp_sp`i'      = string(`hr',"%9.3f")

    ************************************************
    * E-value
    ************************************************

    if (`hr' >= 1) {

        local evalue = `hr' + sqrt(`hr'*(`hr'-1))

        if (`lb' > 1) {
            local evalue_ci = `lb' + sqrt(`lb'*(`lb'-1))
        }
        else {
            local evalue_ci = 1
        }

    }
    else {

        local hr_inv = 1/`hr'

        local evalue = `hr_inv' + sqrt(`hr_inv'*(`hr_inv'-1))

        if (`ub' < 1) {

            local ub_inv = 1/`ub'

            local evalue_ci = `ub_inv' + sqrt(`ub_inv'*(`ub_inv'-1))
        }
        else {
            local evalue_ci = 1
        }

    }

    ************************************************
    * Other statistics
    ************************************************

    local N_`i' = e(N)

    quietly summarize divorced if e(sample)

    local depvar_mean_`i' = r(mean)
    local str_mean_`i' = string(`depvar_mean_`i'',"%9.6f")

    ************************************************
    * Export
    ************************************************

    local row = `i'+1

    putexcel A`row'=("Model `i'") ///
             B`row'=("`coefstar_sp`i''") ///
             C`row'=("(`std_sp`i' ')") ///
             D`row'=("`exp_sp`i' '") ///
             E`row'=(`lb') ///
             F`row'=(`ub') ///
             G`row'=(`evalue') ///
             H`row'=(`evalue_ci')

}

********************************************************************
* E-VALUE BENCHMARKING: VanderWeele & Ding (2017) Bounding Factor
* Comparing observed confounders against E-value = 2.396
********************************************************************

* Assumes:
*   - Dataset is already the matched estimation sample used in Table 2
*   - Panel structure set with: xtset id wave  (or stset for survival)
*   - Variables: rcaregivers (0/1), divorced (0/1 event), 
*                low_income, rcesd, Educ_less_HS, HS_degree, 
*                at_least_some_college, full_time_work,
*                plus other controls: ragey_b female white black other_race 
*                foreign_born hnkid log_networth rafeduc rameduc

capture log close
log using "evalue_benchmark.log", replace

********************************************************************
* STEP 0: Set up survival data (adjust to your actual setup)
********************************************************************
//stset wave, id(id) failure(divorced==1)

********************************************************************
* STEP 1: Store the main E-value target (from your preferred model)
********************************************************************
scalar E_target = 2.396

********************************************************************
* STEP 2: Define covariate list to benchmark
********************************************************************
local benchmark_vars low_income rcesd Educ_less_HS HS_degree at_least_some_college full_time_work

* Full control set used in your main Cox specification (Table 2, col 4)
* excluding the benchmark variable itself each time


********************************************************************
* STEP 3: Create results matrix to store output
********************************************************************
matrix results = J(6, 5, .)
matrix rownames results = low_income rcesd Educ_less_HS HS_degree at_least_some_college full_time_work
matrix colnames results = RR_to_rcaregivers HR_to_Divorce BoundingFactor E_target Ratio_BF_to_E

local row = 1

********************************************************************
* STEP 4: Loop over each benchmark covariate
********************************************************************
foreach z of local benchmark_vars {

    display as text "=========================================="
    display as text "Benchmarking covariate: `z'"
    display as text "=========================================="

    * Controls = full control list minus the current Z
    local controls_minus_z : list full_controls - z

    *
    * (a) Z --> rcaregivers  (logistic regression, report as risk ratio)
    
    * Using logistic + margins to approximate risk ratio (safer than OR
    * when outcome prevalence is not rare; rcaregivers is fairly common here)
    
    logit rcaregivers `z' `controls_minus_z', vce(cluster hhidpn)
    
    * Approximate risk ratio via predicted margins at Z=1 vs Z=0
    * (only valid for binary Z; for continuous Z like rcesd, 
    *  interpret as RR per 1-SD increase — see note below)
    
    capture confirm numeric variable `z'
    quietly sum `z'
    local z_is_binary = (r(min)==0 & r(max)==1)

    if `z_is_binary' == 1 {
        margins, at(`z'=(0 1)) post
        local rr_num = el(r(table), 1, 2)
        local rr_den = el(r(table), 1, 1)
        scalar RR_EZ = `rr_num' / `rr_den'
    }
    else {
        * For continuous Z (e.g., rcesd), compute RR per 1-SD change
        quietly sum `z'
        local sd_z = r(sd)
        local mean_z = r(mean)
        logit rcaregivers `z' `controls_minus_z', vce(cluster hhidpn)
        margins, at(`z'=(`mean_z' `=`mean_z'+`sd_z'')) post
        local rr_num = el(r(table), 1, 2)
        local rr_den = el(r(table), 1, 1)
        scalar RR_EZ = `rr_num' / `rr_den'
    }

    display as result "RR(Z -> rcaregivers) for `z' = " RR_EZ

   
    * (b) Z --> divorced (Cox PH, controlling for rcaregivers)
    
    stcox `z' rcaregivers `controls_minus_z', vce(cluster hhidpn)
    
    * Hazard ratio for Z directly from stcox (already exponentiated)
    scalar HR_ZD = exp(_b[`z'])
    
    display as result "HR(Z -> divorced) for `z' = " HR_ZD

    *
    * (c) Compute Bounding Factor (VanderWeele & Ding 2017)
    * BF = (RR_EZ * RR_ZD) / (RR_EZ + RR_ZD - 1)
    
    scalar RR1 = RR_EZ
    scalar RR2 = HR_ZD
    
    * Ensure both RRs are >=1 (flip if <1, since bounding factor 
    * formula assumes RR >= 1; use reciprocal if needed)
    if RR1 < 1 {
        scalar RR1 = 1/RR1
    }
    if RR2 < 1 {
        scalar RR2 = 1/RR2
    }
    
    scalar BF = (RR1 * RR2) / (RR1 + RR2 - 1)
    
    display as result "Bounding Factor for `z' = " BF
    display as result "Compare to E-value target = " E_target

    
    * Store in results matrix
   
    matrix results[`row', 1] = RR_EZ
    matrix results[`row', 2] = HR_ZD
    matrix results[`row', 3] = BF
    matrix results[`row', 4] = E_target
    matrix results[`row', 5] = BF / E_target

    local row = `row' + 1
}

********************************************************************
* STEP 5: Display summary table
********************************************************************
matrix list results, format(%9.3f)

* Export to a clean table (optional)
esttab matrix(results, fmt(%9.3f)) using "${tables}Table4_Evalue.xlsx", replace 
********************************************************************
* STEP 6 (Complementary): Oster (2019) Coefficient-Stability Test
* Requires: ssc install psacalc
********************************************************************
ssc install psacalc, replace

* Uncontrolled model (Table 2, column 1 equivalent)
* NOTE: psacalc is designed for OLS/linear models; for Cox models,
* the standard approach is to approximate using a linear probability 
* model (LPM) on the divorced indicator as a robustness proxy, 
* since psacalc does not natively support stcox.

* (a) Restricted model: no controls
*reg divorced ********************************************************************
* E-VALUE BENCHMARKING: VanderWeele & Ding (2017) Bounding Factor
* Comparing observed confounders against E-value = 2.396
********************************************************************

* Assumes:
*   - Dataset is already the matched estimation sample used in Table 2
*   - Panel structure set with: xtset id wave  (or stset for survival)
*   - Variables: rcaregivers (0/1), divorced (0/1 event), 
*                low_income, rcesd, Educ_less_HS, HS_degree, 
*                at_least_some_college, full_time_work,
*                plus other controls: ragey_b female white black other_race 
*                foreign_born hnkid log_networth rafeduc rameduc


********************************************************************
* STEP 0: Set up survival data (adjust to your actual setup)
********************************************************************

********************************************************************
* STEP 1: Store the main E-value target (from your preferred model)
********************************************************************
scalar E_target = 2.396

********************************************************************
* STEP 2: Define covariate list to benchmark
********************************************************************
local benchmark_vars low_income rcesd Educ_less_HS HS_degree at_least_some_college full_time_work

* Full control set used in your main Cox specification (Table 2, col 4)
* excluding the benchmark variable itself each time


********************************************************************
* STEP 3: Create results matrix to store output
********************************************************************
matrix results = J(6, 5, .)
matrix rownames results = low_income rcesd Educ_less_HS HS_degree at_least_some_college full_time_work
matrix colnames results = RR_to_rcaregivers HR_to_Divorce BoundingFactor E_target Ratio_BF_to_E

local row = 1

********************************************************************
* STEP 4: E-VALUE BENCHMARKING AGAINST OBSERVED COVARIATES
* Compare observed covariate associations with E-value = 2.396
********************************************************************

* E-value from preferred Cox model
scalar E_target = 2.396

* Covariates to benchmark
* Include age and depressive symptoms because these are substantively
* plausible confounders, in addition to socioeconomic characteristics.
local benchmark_vars ///
    low_income ///
    rcesd ///
    ragey_b ///
    Educ_less_HS ///
    HS_degree ///
    at_least_some_college ///
    full_time_work

* Full control set from preferred Cox specification
local full_controls ///
    rcesd ragey_b female white black other_race foreign_born ///
    Educ_less_HS HS_degree at_least_some_college ///
    full_time_work hnkid low_income rafeduc rameduc

********************************************************************
* Create results matrix
********************************************************************

matrix results = J(7, 4, .)

matrix rownames results = ///
    low_income ///
    rcesd ///
    ragey_b ///
    Educ_less_HS ///
    HS_degree ///
    at_least_some_college ///
    full_time_work

matrix colnames results = ///
    RR_Caregiving ///
    HR_Divorce ///
    E_Value ///
    SD_or_Contrast

local row = 1

********************************************************************
* Loop over benchmark covariates
********************************************************************

foreach z of local benchmark_vars {

    display as text "=========================================="
    display as text "Benchmarking covariate: `z'"
    display as text "=========================================="

    *---------------------------------------------------------------
    * Remove the benchmark covariate from the control set
    *---------------------------------------------------------------

    local controls_minus_z : list full_controls - z

    *---------------------------------------------------------------
    * Determine whether Z is binary or continuous
    *---------------------------------------------------------------

    quietly summarize `z'
    local z_min = r(min)
    local z_max = r(max)

    local z_is_binary = (`z_min' == 0 & `z_max' == 1)

    *---------------------------------------------------------------
    * Create standardized version for continuous covariates
    *---------------------------------------------------------------

    capture drop __z_std

    if `z_is_binary' == 0 {

        quietly summarize `z'
        local sd_z = r(sd)

        generate double __z_std = `z' / `sd_z'

        local z_model __z_std
        local contrast "1 SD"

    }
    else {

        local z_model `z'
        local contrast "0 vs 1"
    }

    display as text "Scale: `contrast'"

    ******************************************************************
    * (A) Association between Z and caregiving
    ******************************************************************

    quietly logit rcaregivers ///
        `z_model' `controls_minus_z', ///
        vce(cluster hhidpn)

    * Adjusted predicted probabilities
    if `z_is_binary' == 1 {

        quietly margins, at(`z'=(0 1)) post

    }
    else {

        quietly margins, at(__z_std=(0 1)) post
    }

    * Extract adjusted probabilities
    matrix M = r(table)

    local p0 = M[1,1]
    local p1 = M[1,2]

    * Adjusted risk ratio
    scalar RR_C = `p1' / `p0'

    display as result ///
        "RR (Z -> caregiving) = " %9.3f RR_C

    ******************************************************************
    * (B) Association between Z and divorce
    ******************************************************************

    quietly stcox ///
        `z_model' rcaregivers `controls_minus_z', ///
        vce(cluster hhidpn)

    * Hazard ratio for Z
    scalar HR_D = exp(_b[`z_model'])

    display as result ///
        "HR (Z -> divorce) = " %9.3f HR_D

    ******************************************************************
    * (C) Store results
    ******************************************************************

    matrix results[`row',1] = RR_C
    matrix results[`row',2] = HR_D
    matrix results[`row',3] = E_target

    if `z_is_binary' == 1 {
        matrix results[`row',4] = 1
    }
    else {
        matrix results[`row',4] = `sd_z'
    }

    local row = `row' + 1

    * Clean up standardized variable
    capture drop __z_std
}

********************************************************************
* STEP 5: Display results
********************************************************************

matrix list results, format(%9.3f)

********************************************************************
* STEP 6: Export to Excel
********************************************************************

putexcel set "${tables}TableA5_evalue_benchmark_table.xlsx", replace

* Title
putexcel A1 = ("E-value Benchmarking Against Observed Covariates")

* Header
putexcel A3 = ("Covariate") ///
         B3 = ("Association with Caregiving") ///
         C3 = ("Association with Divorce") ///
         D3 = ("E-value") ///
         E3 = ("Exposure Contrast")

* Covariate names
putexcel A4 = ("Having negative networth")
putexcel A5 = ("Depressive symptoms")
putexcel A6 = ("Age")
putexcel A7 = ("Less than high school")
putexcel A8 = ("High school degree")
putexcel A9 = ("At least some college")
putexcel A10 = ("Full-time work")

* Results
putexcel B4 = results[1,1]
putexcel C4 = results[1,2]
putexcel D4 = results[1,3]
putexcel E4 = ("0 vs 1")

putexcel B5 = results[2,1]
putexcel C5 = results[2,2]
putexcel D5 = results[2,3]
putexcel E5 = ("1 SD")

putexcel B6 = results[3,1]
putexcel C6 = results[3,2]
putexcel D6 = results[3,3]
putexcel E6 = ("1 SD")

putexcel B7 = results[4,1]
putexcel C7 = results[4,2]
putexcel D7 = results[4,3]
putexcel E7 = ("0 vs 1")

putexcel B8 = results[5,1]
putexcel C8 = results[5,2]
putexcel D8 = results[5,3]
putexcel E8 = ("0 vs 1")

putexcel B9 = results[6,1]
putexcel C9 = results[6,2]
putexcel D9 = results[6,3]
putexcel E9 = ("0 vs 1")

putexcel B10 = results[7,1]
putexcel C10 = results[7,2]
putexcel D10 = results[7,3]
putexcel E10 = ("0 vs 1")


display as result ///
    "E-value benchmarking table exported to: ${tables}evalue_benchmark_table.xlsx"

log close
