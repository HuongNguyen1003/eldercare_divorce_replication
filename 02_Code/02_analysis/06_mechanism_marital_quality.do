

cap log close
log using "${logs}`c(current_date)'_mechanism_perception.log", replace
display "Log timestamp: `c(current_date)' `c(current_time)'"

/*
Title: Analyze the Association Between Maternal Caregiving and Marital Quality Perceptions

Purpose:
    Examine the association between caring for parents and individuals'
    perceptions of marital quality using ordered logit regressions.

    1. Estimate the association between parental caregiving and positive
       marital quality perceptions:
       - Understanding
       - Openness in discussing worries
       - Closeness to spouse
       - Feeling let down by spouse

    2. Use the following individual, parental, and household controls:
       - Individual characteristics: gender, depressive symptoms, age,
         education, foreign-born status, race, and household income.
       - Parental characteristics: mother's and father's education.
       - Household characteristics: number of children.

    3. Calculate the average marginal effect of caregiving on the
       probability of reporting the lowest response category for each
       marital quality outcome.

    4. Assess statistical power:
       - Calculate observed power at a two-sided 5% significance level.
       - Calculate the required sample size to achieve 80% power.

    5. Export marginal effects, standard errors, dependent variable means,
       sample sizes, observed power, and required sample sizes to Excel.

Output:
    Input data:
        - ${final_data}psm_divorce_sample.dta

    Excel results:
        - ${tables}main.xlsx
            - Sheet "perception": Marginal effects and statistical power
              calculations for marital quality outcomes.

Modified by: Huong Nguyen
Date: 2026/01/31
*/

****************************************************
* Set globals with mode
****************************************************

    global dataset marital_status_sample
    global file perception 

****************************************************
* Run analyses according to mode 
****************************************************
use "${final_data}$dataset.dta", clear
*control
local individual female rcesd ragey_b Educ_less_HS foreign_born  rahispan white_caucasian low_income
local parent rameduc rafeduc
local household hnkid 
local time i.wave

* Full set of baseline controls
local base_controls `individual' `parent' `household' 
local spec1 `individual'
local spec2 `individual' `parent' 
local spec3 `individual' `parent' `household'
local spec4 `individual' `parent' `household' `time'


*y variables
local y_vars1 runderstand  ropenworries rclosesp   rletdown

local y_labs1 `"" A lot Understanding"  " A lot Open"  " A lot Close" "A lot Letdown" "'
local y_num1: word count `y_vars1'

local y_vars2   rdemand rcriticize rletdown rnerves 

local y_labs2 `" "A lot Demanding"  "A lot Critical" "A lot Letdown" "A lot Annoying" "'
local y_num2: word count `y_vars2'

**# COEFICIENT

********************************************************************
* Regressions
********************************************************************

forvalues i = 1/`y_num1' {

    local y : word `i' of `y_vars1'

    ****************************************************************
    * 1. Ordered logit
    ****************************************************************

     ologit `y' rcaregivers `base_controls', ///
        vce(cluster hhidpn)

    * Save estimation sample
    tempvar esample
    gen byte `esample' = e(sample)

   
****************************************************************
    * 2. Save estimation sample and N
    ****************************************************************

    scalar N_current_`i' = e(N)

    local N_`i' = e(N)



    ****************************************************************
    * 5. Dependent variable mean
    ****************************************************************

    quietly summarize `y' if e(sample)

    local depvar_mean_`i' = r(mean)

    local str_mean_`i' = ///
        string(`depvar_mean_`i'', "%9.5f")


    ****************************************************************
    * 2. Marginal effect of caregiving
    *
    * rcaregivers is binary:
    * change from 0 -> 1
    ****************************************************************

     margins, dydx(rcaregivers) ///
        predict(outcome(1))

****************************************************************
* 3. Extract marginal effect and SE
****************************************************************

    matrix T = r(table)

    scalar b_`i'  = T[1,1]
    scalar se_`i' = T[2,1]
    scalar p_`i'  = T[4,1]



    ****************************************************************
    * 6. Significance stars
    ****************************************************************

    local star ""

    if (p_`i' < 0.01) {
        local star "***"
    }
    else if (p_`i' < 0.05) {
        local star "**"
    }
    else if (p_`i' < 0.10) {
        local star "*"
    }



    ****************************************************************
    * 4. POWER CALCULATION
    ****************************************************************

    * Observed z-statistic

    scalar z_obs_`i' = ///
        abs(b_`i' / se_`i')


    *--------------------------------------------------------------
    * A. Observed power
    *--------------------------------------------------------------

    * Two-sided alpha = 0.05

    scalar z_alpha = invnormal(0.975)

    scalar power_`i' = ///
        normal(-z_alpha - z_obs_`i') + ///
        1 - normal(z_alpha - z_obs_`i')


    *--------------------------------------------------------------
    * B. Required N for 80% power
    *--------------------------------------------------------------

    scalar z_power = invnormal(0.80)

    scalar beta_abs_`i' = abs(b_`i')


    if (beta_abs_`i' > 0 & se_`i' > 0) {

        * SE required for 80% power

        scalar se_target_`i' = ///
            beta_abs_`i' / ///
            (z_alpha + z_power)

        * Required sample size

        scalar N_required_`i' = ///
            N_current_`i' * ///
            (se_`i' / se_target_`i')^2

        * Round up

        scalar N_required_`i' = ///
            ceil(N_required_`i')

    }
    else {

        scalar N_required_`i' = .

    }

    ****************************************************************
    * 7. Save results for Excel
    ****************************************************************

    local coefstar_`i' = ///
        string(b_`i', "%9.4f") + "`star'"

    local std_`i' = ///
        string(se_`i', "%9.4f")

    local power_`i' = ///
        string(power_`i', "%9.3f")

    local N_required_`i' = ///
        string(N_required_`i', "%12.0f")


    ****************************************************************
    * 8. Display
    ****************************************************************

    di "------------------------------------------------------------"
    di "Outcome: `y'"
    di "Coefficient       = " %9.4f b_`i'
    di "SE                = " %9.4f se_`i'
    di "p-value           = " %9.4f p_`i'
    di "N                 = " N_current_`i'
    di "Depvar mean       = " %9.5f `depvar_mean_`i''
    di "Observed power    = " %9.4f power_`i'
    di "Required N (80%)  = " %12.0f N_required_`i'
    di "------------------------------------------------------------"

}


/********************************************************************
    Step 4: Export ANY health shock results to Excel
********************************************************************/

putexcel set "${tables}Table.xlsx", sheet("Table6-perception") modify
//putexcel A2=("Panel A: Positive Marital Quality ")
putexcel A2=("Caring for Mom") ///
	A3 = ("(Se)") ///
	A4 = ("Depvar mean") ///
	A5=("Observations") ///
	A6 = ("Observed power") ///
	A7 = ("Required N (Power=0.8, p < 0.1)")

local col_coefs B C D E F G H I J K L M N O P Q


forval i = 1/`y_num1' {
	local y_lab : word `i' of `y_labs1'
    local col_coef : word `i' of `col_coefs'
    local row_spec : word `i' of `row_specs`i''
di "`row_spec'"
	putexcel `col_coef'1 = ("`y_lab'")
    putexcel `col_coef'2 = ("`coefstar_`i''")
   putexcel `col_coef'3 = ("(`std_`i'')")
    
    putexcel `col_coef'4 = ("`str_mean_`i''")
	putexcel `col_coef'5 = ("`N_`i''")
	putexcel `col_coef'6 = ("`power_`i''")
	putexcel `col_coef'7 = ("`N_required_`i''")
	

    }
 
