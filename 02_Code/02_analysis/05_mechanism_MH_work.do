
cap log close
log using "${logs}`c(current_date)'_mechanism_health_employment.log", replace
display "Log timestamp: `c(current_date)' `c(current_time)'"

/*
Title: Analyze the Effects of Parental Caregiving on Employment and Mental Health

Purpose:
    Examine the association between caring for parents and individuals'
    mental health and employment outcomes using OLS regressions.

    1. Estimate the association between parental caregiving and three
       outcomes:
       - Mental health (CES-D score)
       - Full-time employment
       - Not working

    2. Control for individual and household characteristics, including
       gender, age, education, race, foreign-born status, parental
       education, and number of household children.

    3. Calculate statistical power:
       - Estimate observed statistical power for each outcome.
       - Calculate the required sample size to achieve 80% power
         at a two-sided 5% significance level.

    4. Report regression coefficients, standard errors, statistical
       significance, sample sizes, dependent variable means, observed
       power, and required sample sizes.

Output:
    Input data:
        - ${final_data}psm_divorce_sample.dta

    Excel results:
        - ${tables}main.xlsx
            - Sheet "mechanism": Regression results and statistical
              power calculations for the three outcomes.

Modified by: Huong Nguyen
Date: 2026/01/31
*/



****************************************************
* Set globals with mode
****************************************************

global dataset marital_status_sample
global file mechanism

****************************************************
* Load data
****************************************************

use "${final_data}$dataset.dta", clear


****************************************************
* Controls
****************************************************

local individual female ragey_b Educ_less_HS foreign_born white black
local parent rameduc rafeduc
local household hnkid
local time i.wave

local base_controls `individual' `household' `parent'

local spec1 `individual'
local spec2 `individual' `parent'
local spec3 `individual' `parent' `household'
local spec4 `individual' `parent' `household'


****************************************************
* Dependent variables
****************************************************

local y_vars rcesd full_time_work not_working

local y_labs `" "Mental Health (score)" "Working full time" "Not working" "'

local y_num : word count `y_vars'


********************************************************************
* Regressions
********************************************************************

forvalues i = 1/`y_num' {

    local y : word `i' of `y_vars'


    ****************************************************************
    * 1. OLS regression
    ****************************************************************

    quietly reg `y' rcaregivers `base_controls', robust


    ****************************************************************
    * 2. Save estimation sample and N
    ****************************************************************

    scalar N_current_`i' = e(N)

    local N_`i' = e(N)


    ****************************************************************
    * 3. Extract coefficient, SE, p-value
    ****************************************************************

    matrix T = r(table)

    scalar b_`i'  = T[1,1]
    scalar se_`i' = T[2,1]
    scalar p_`i'  = T[4,1]


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
    * 5. Dependent variable mean
    ****************************************************************

    quietly summarize `y' if e(sample)

    local depvar_mean_`i' = r(mean)

    local str_mean_`i' = ///
        string(`depvar_mean_`i'', "%9.5f")


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


********************************************************************
* Export mechanism results to Excel
********************************************************************

putexcel set "${tables}Table.xlsx", ///
    sheet("Table7-mechanism") modify


********************************************************************
* Row labels
********************************************************************

putexcel A3 = ("Caring for parents") ///
         A4 = ("(SE)") ///
         A5 = ("Person by wave obs") ///
         A6 = ("Depvar mean") ///
         A7 = ("Observed power") ///
         A8 = ("Required N (Power=0.8, p < 0.1)")


********************************************************************
* Column names
********************************************************************

local col_coefs B C D E F


********************************************************************
* Export results
********************************************************************

forvalues i = 1/`y_num' {

    * Outcome label
    local y_lab : word `i' of `y_labs'

    * Excel column
    local col_coef : word `i' of `col_coefs'


    * Outcome name
    putexcel `col_coef'2 = ("`y_lab'")


    * Coefficient
    putexcel `col_coef'3 = ///
        ("`coefstar_`i''")


    * Standard error
    putexcel `col_coef'4 = ///
        ("(`std_`i'')")


    * Number of observations
    putexcel `col_coef'5 = ///
        ("`N_`i''")


    * Dependent variable mean
    putexcel `col_coef'6 = ///
        ("`str_mean_`i''")


    * Observed power
    putexcel `col_coef'7 = ///
        ("`power_`i''")


    * Required N
    putexcel `col_coef'8 = ///
        ("`N_required_`i''")

}
