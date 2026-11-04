cap log close 
log using "${logs}survial_analysis.log", replace 

/*
Title: Cox Proportional Hazards and Weibull AFT Analysis of Caregiving and Divorce

Purpose:
    Examine the association between parental caregiving and the risk and
    timing of divorce using Cox proportional hazards and Weibull
    accelerated failure time (AFT) models.

    1. Prepare the survival analysis dataset:
       - Define start and stop times for each person-wave observation.
       - Set up the survival data using divorce as the failure event.
       - Check whether observations after divorce are excluded.

    2. Estimate Cox proportional hazards models:
       - Run four specifications with progressively richer sets of controls.
       - Cluster standard errors at the individual level.
       - Calculate and report coefficients, standard errors, hazard ratios,
         and sample sizes.

    3. Assess the proportional hazards (PH) assumption:
       - Graphical diagnostics using log-log survival plots.
       - Adjusted log-log survival plots.
       - Observed versus predicted survival plots.
       - Schoenfeld residuals and formal PH tests.

    4. Estimate Weibull accelerated failure time (AFT) models:
       - Run four specifications using the same sets of control variables.
       - Calculate and report coefficients, standard errors, and
         exponentiated coefficients (time ratios).

Output:
    Input data:
        - ${final_data}psm_divorce_sample.dta

    Excel results:
        - ${tables}main.xlsx
            - Sheet "cox-divorce": Cox regression results
            - Sheet "AFT": Weibull AFT regression results

    Diagnostic graphs:
        - ${graphs}06_AC1_`file'_stplot_nolntime.png
        - ${graphs}06_AC2_`file'_stplot_nonegative.png
        - ${graphs}06_AC3_`file'_stplot_nonegative_adjusted.png
        - ${graphs}06_AC4_`file'_stcoxkm.png
        - ${graphs}06_AC5_`file'_stphtest_rank_detail.png
        - ${graphs}06_AC5_`file'_stphtest_rank_plot.png

Modified by: Huong Nguyen
Date: 2026/01/31
*/


**# Check cox PH assumption 
use "${final_data}marital_status_sample.dta", clear 

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
*Check if observations after divorce is not included 
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


local spec1 
local spec2 $individual_vars
local spec3 $individual_vars $household_vars 
local spec4 $individual_vars $household_vars $parental_vars

forvalues i = 1/4 {

stcox rcaregivers `spec`i'' , nohr vce(cluster hhidpn)
est store m`i'

matrix T = r(table)

            scalar b_sp`i' = T[1,1]
	    scalar se = T[2,1]
            scalar p = T[4,1]

        local star ""
            if (p < 0.01) local star "***"
            else if (p < 0.05) local star "**"
            else if (p < 0.10) local star "*"
	    
	local coefstar_sp`i' = string(b_sp`i',"%9.3f") + "`star'"
	local std_sp`i' = string(se, "%9.3f")
	local coef_sp`i'_num = exp(b_sp`i')
	local exp_sp`i' = string(`coef_sp`i'_num',"%9.3f")
    
	
    * Add N
   local N_`i' = e(N)
    
    *dep var mean 
    summarize divorced if e(sample)
    local depvar_mean_`i' = r(mean)
    local str_mean_`i' = string(`depvar_mean_`i'', "%9.6f")
}


/********************************************************************
    Step 4: Export ANY health shock results to Excel
********************************************************************/
**# output 1
putexcel set "${tables}Table.xlsx", sheet("Table2-cox") modify

putexcel B1=("(1)") C1=("(2)") D1=("(3)") E1=("(4)") F1=("(5)") 
putexcel B2=("Divorce") C2=("Divorce") D2=("Divorce") E2=("Divorce") F2=("Divorce") 
putexcel A3=("Caregiving for parents") ///
	A4 = ("(Se)") ///
	A5 = ("HR") ///
        A6=("Individual characteristics") ///
        A7=("Household characteristics") ///
        A8=("Parental characteristics") ///
        A9=("Person x wave observations") 
local col_coefs B C D E F 

local row_spec1 ""
local row_spec2 6
local row_spec3  6  7
local row_spec4   6 7 8
local row_spec5  6 7 8 9


forval i = 1/4 {
    local col_coef : word `i' of `col_coefs'
    local row_spec : word `i' of `row_specs`i''
di "`row_spec'"

    putexcel `col_coef'3 = ("`coefstar_sp`i''")
   putexcel `col_coef'4 = ("(`std_sp`i'')")
    putexcel `col_coef'5 = ("`exp_sp`i''")
    
    if "`col_coef'" != "B" {
        * Duyệt từng row
        foreach r of local row_spec`i' {
		di "`r'"
            putexcel `col_coef'`r' = ("x")
        }
    }
    
    putexcel `col_coef'9 = ("`N_`i''")

    }


**# Check PH assumption 
cap log close 
log using "${tables}ph_test.log"
{
* 1 - Graphical way: 
	/*
	* 1-1 log-log survival time 
	stphplot, by(rcaregivers) nonegative nolntime 
	graph export "${graphs}06_AC1_`file'_stplot_nolntime.png", replace 
	stphplot, by(rcaregivers) nonegative 
	graph export "${graphs}06_AC2_`file'_stplot_nonegative.png", replace 

	
	*1-2 log-log survial time, adjusted to other covariates 
	stphplot, strata(rcaregivers) adjust(`spec4') nonegative nolntime 
	graph export "${graphs}06_AC3_`file'_stplot_nonegative_adjusted.png", replace 
	*1-3 Observed vs Predicted Plot 
	stcoxkm, by(rcaregivers)
	graph export "${graphs}06_AC4_`file'_stcoxkm.png", replace 
	*/
* 2 - Goodness of fit : Schoenfeld 
	stcox  rcaregivers  `spec4',  vce(cluster hhidpn) schoenfeld(schoen*) scaledsch(scaled*)

	stphtest, rank detail
	//graph export "${graphs}06_AC5_`file'_stphtest_rank_detail.png", replace 
	stphtest, rank plot(rcaregivers)
	graph export "${graphs}Figure1_stphtest_rank_plot.png", replace 
}
log close 

**# AFT 
	
forvalues i = 1/4 {
        streg rcaregivers `spec`i'' , ///
            distribution(weibull) ///
            time ///
            vce(cluster hhidpn)
	    
        est store m`i'

    matrix T = r(table)

            scalar b_sp`i' = T[1,1]
	    scalar se = T[2,1]
            scalar p = T[4,1]

        local star ""
            if (p < 0.01) local star "***"
            else if (p < 0.05) local star "**"
            else if (p < 0.10) local star "*"
	    
	local coefstar_sp`i' = string(b_sp`i',"%9.3f") + "`star'"
	local std_sp`i' = string(se, "%9.3f")
	local coef_sp`i'_num = exp(b_sp`i')
	local exp_sp`i' = string(`coef_sp`i'_num',"%9.3f")
    
    
    * Add N
   local N_`i' = e(N)
    
    *dep var mean 
    summarize divorced if e(sample)
    local depvar_mean_`i' = r(mean)
    local str_mean_`i' = string(`depvar_mean_`i'', "%9.6f")
}


/********************************************************************
    Step 4: Export ANY health shock results to Excel
********************************************************************/
**# output 1
putexcel set "${tables}Table.xlsx", sheet("Table5-AFT") modify

putexcel B1=("(1)") C1=("(2)") D1=("(3)") E1=("(4)") F1=("(5)") 
putexcel B2=("Time to Divorce") C2=("Time to Divorce") D2=("Time to Divorce") E2=("Time to Divorce") F2=("Time to Divorce") 
putexcel A3=("Caregiving for parents") ///
	A4 = ("(Se)") ///
	A5 = ("HR") ///
        A6=("Individual characteristics") ///
        A7=("Household characteristics") ///
        A8=("Parental characteristics") ///
        A9=("Person x wave observations") 
local col_coefs B C D E F 

local row_spec1 ""
local row_spec2 6
local row_spec3  6  7
local row_spec4   6 7 8
local row_spec5  6 7 8 9


forval i = 1/4 {
    local col_coef : word `i' of `col_coefs'
    local row_spec : word `i' of `row_specs`i''
di "`row_spec'"

    putexcel `col_coef'3 = ("`coefstar_sp`i''")
   putexcel `col_coef'4 = ("(`std_sp`i'')")
    putexcel `col_coef'5 = ("`exp_sp`i''")
    
    if "`col_coef'" != "B" {
        * Duyệt từng row
        foreach r of local row_spec`i' {
		di "`r'"
            putexcel `col_coef'`r' = ("x")
        }
    }
    
    putexcel `col_coef'9 = ("`N_`i''")

    }
    
cap log close 
