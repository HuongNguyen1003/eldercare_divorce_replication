cap log close 
log using "${log}rc_other_matching", replace 

local modes psm cem mahalanobis 
local j = 1
foreach mode in `modes' {

****************************************************
* Set globals with mode
****************************************************
if "`mode'" == "mahalanobis" {
    global dataset mb_match
    global file mb_match 
}
else if "`mode'" == "cem" {
global dataset cem_match
global file exact_match

}

else if "`mode'" == "exact_match" {
global dataset exact_match
global file exact_match

}
 
else if "`mode'" == "psm" {
global dataset marital_status_sample
global file psm

}
else {
    di as error "Mode not recognized: `mode'"
    exit 198
}

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

**# covariates 
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
}


********************************************************************
* Regression: Spec 4 across matching methods
* One Excel sheet
*
* Columns:
*     B = PSM
*     C = Exact Match
*     D = CEM
*     E = Mahalanobis Match
********************************************************************

local modes psm  cem mahalanobis


********************************************************************
* 1. Set up Excel output
********************************************************************

putexcel set "${tables}Table.xlsx", ///
    sheet("Table3-rc_matching") modify


********************************************************************
* 2. Table labels
********************************************************************

putexcel A1 = ("Matching method") ///
         B1 = ("PSM") ///
         C1 = ("CEM") ///
         D1 = ("Mahalanobis Match")

putexcel A2 = ("Dependent variable") ///
         B2 = ("Divorce") ///
         C2 = ("Divorce") ///
         D2 = ("Divorce") 

putexcel A3  = ("Caregiving for parents") ///
         A4  = ("(SE)") ///
         A5  = ("HR") ///
         A6  = ("Individual characteristics") ///
         A7  = ("Household characteristics") ///
         A8  = ("Parental characteristics") ///
         A9 = ("Person × wave observations") ///
         A10 = ("Mean of dependent variable")


********************************************************************
* 3. Column assignment
********************************************************************

local col = 2


********************************************************************
* 4. Loop over matching methods
********************************************************************
local i = 1
foreach mode in `modes' {

    ********************************************************
    * Set dataset according to matching method
    ********************************************************

    if "`mode'" == "mahalanobis" {

        global dataset mb_match
        global file mb_match

    }
    else if "`mode'" == "cem" {

        global dataset cem_match
        global file cem_match

    }
    else if "`mode'" == "exact_match" {

        global dataset exact_match
        global file exact_match

    }
    else if "`mode'" == "psm" {

        global dataset marital_status_sample
        global file psm

    }
    else {

        di as error "Mode not recognized: `mode'"
        exit 198

    }


    ********************************************************
    * Load data
    ********************************************************

    use "${final_data}$dataset.dta", clear

    sort hhidpn wave


    ********************************************************
    * Set up Cox model
    ********************************************************

    by hhidpn (wave): gen start_time = wave

    by hhidpn (wave): gen stop_time = wave[_n+1]

    drop if missing(stop_time)


    stset stop_time, ///
        enter(time start_time) ///
        failure(divorced == 1) ///
        id(hhidpn)


    ********************************************************
    * Covariates
    ********************************************************

    global individual_vars ///
        rcesd ///
        ragey_b female white black other_race foreign_born ///
        Educ_less_HS HS_degree at_least_some_college ///
        full_time_work

    global household_vars ///
        hnkid low_income

    global parental_vars ///
        rafeduc rameduc


    ********************************************************
    * Spec 4 ONLY
    ********************************************************

    local spec4 ///
        $individual_vars ///
        $household_vars ///
        $parental_vars


    ********************************************************
    * Run Cox model
    ********************************************************

    stcox rcaregivers `spec4', ///
        nohr ///
        vce(cluster hhidpn)


    ********************************************************
    * Extract caregiving coefficient
    ********************************************************

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
    * 7. Save results for Excel
    ****************************************************************

    local coefstar_`i' = ///
        string(b_`i', "%9.4f") + "`star'"

    local std_`i' = ///
        string(se_`i', "%9.4f")

    ********************************************************
    * Hazard ratio
    ********************************************************

    scalar HR_`i' = exp(b_`i')

    local hr_`i' = string(HR_`i', "%9.3f")


    ********************************************************
    * Number of observations
    ********************************************************

    local N = e(N)


    ********************************************************
    * Mean of dependent variable
    ********************************************************

    quietly summarize divorced if e(sample)

    local depvar_mean = r(mean)

    local mean = string(`depvar_mean', "%9.4f")


    ********************************************************
    * Convert column number to Excel letter
    ********************************************************

    local col_letter ""

    if `col' == 2 {
        local col_letter "B"
    }
    else if `col' == 3 {
        local col_letter "C"
    }
    else if `col' == 4 {
        local col_letter "D"
    }
    else if `col' == 5 {
        local col_letter "E"
    }


    ********************************************************
    * Export results
    ********************************************************
di "`coefstar'"
    * Coefficient
    putexcel `col_letter'3 = ("`coefstar_`i''")

    * Standard error
    putexcel `col_letter'4 = ("(`std_`i'')")

    * Hazard ratio
    putexcel `col_letter'5 = ("`hr_`i''")


    ********************************************************
    * Specification indicators
    ********************************************************

    putexcel `col_letter'6 = ("x")   // Individual characteristics
    putexcel `col_letter'7 = ("x")   // Household characteristics
    putexcel `col_letter'8 = ("x")   // Parental characteristics

   
    ********************************************************
    * N
    ********************************************************

    putexcel `col_letter'9 = ("`N'")


    ********************************************************
    * Dependent variable mean
    ********************************************************

    putexcel `col_letter'10 = ("`mean'")


    ********************************************************
    * Move to next column
    ********************************************************

    local col = `col' + 1
local i = `i' + 1
}


********************************************************************
* 5. Formatting
********************************************************************

putexcel A1:E1, bold
putexcel A2:E2, bold
putexcel A3:A11, bold

putexcel A1:E11, hcenter

putexcel A1:A11, left


********************************************************************
* 6. Save Excel file
********************************************************************

putexcel save


log close 

**
