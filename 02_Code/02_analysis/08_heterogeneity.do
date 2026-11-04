/*
Title: Heterogeneity Analysis of the Association Between Parental Caregiving and Divorce

Purpose:
    Examine whether the association between parental caregiving and
    divorce varies across different individual, socioeconomic, and
    household subgroups using Cox proportional hazards models.

    1. Estimate subgroup-specific Cox proportional hazards models for:
       - Household economic status (positive vs. non-positive net worth)
       - Gender (male vs. female)
       - Education (less than high school vs. high school or higher)
       - Race (White, Black, and other racial groups)
       - Number of children (one child vs. more than one child)
       - Employment status (working full time vs. not working full time)

    2. Adjust for relevant individual, parental, and household
       characteristics within each subgroup, excluding the variable
       used to define that subgroup from the control set.

    3. Extract and report subgroup-specific estimates:
       - Coefficients and standard errors
       - Hazard ratios
       - Number of person-wave observations
       - Number of unique individuals

    4. Visualize heterogeneity in caregiving effects:
       - Combine subgroup-specific estimates.
       - Calculate and display 95% confidence intervals.
       - Generate a coefficient plot comparing estimates across groups.

Output:
    Input data:
        - ${final_data}psm_divorce_sample.dta

    Excel results:
        - ${tables}main.xlsx
            - Sheet "group": Subgroup-specific Cox regression results.

    Intermediate datasets:
        - ${final_data}boy.dta
        - Subgroup-specific coefficient datasets.
        - ${final_data}coef_list.dta

    Graphs:
        - ${graphs}group.png
            Subgroup-specific caregiving estimates with 95% confidence
            intervals.

    Log file:
        - ${logs}`c(current_date)'_heterogeneity_divore.log

Modified by: Huong Nguyen
Date: 2026/01/31
*/

cap log close
log using "${logs}`c(current_date)'_heterogeneity_divore.log", replace
display "Log timestamp: `c(current_date)' `c(current_time)'"

global dataset marital_status_sample
global file heterogeneity 
global graph G
use "${final_data}${dataset}.dta", clear 

*==============================
* 1. Base controls (full set)
*==============================
local individual female rcesd ragey_b Educ_less_HS full_time_work part_time_work foreign_born white_caucasian rahispan
local parent rameduc rafeduc
local household hnkid low_income
local time i.wave

* Full set of baseline controls
local base_controls `individual' `parent' `household' 
*==============================
* 2. Group definitions
*   format:
*   groupname  "if condition"   "controls_to_drop"
*==============================
local groups ///
low_income "low_income == 1" "low_income" ///
not_low_income "low_income == 0" "low_income" ///
boy "female==0" "female" ///
female "female==1" "female" ///
HS_gtHS "Educ_less_HS==0" "Educ_less_HS" ///
ltHS "Educ_less_HS==1" "Educ_less_HS" ///
other_race "other_race == 1" "other_race" ///
black "black==1" "black" ///
white "white_caucasian == 1" "white_caucasian" ///
multikid "hnkid >1" "hnkid" ///
onekid "hnkid==1" "hnkid" ///
ft_work "full_time_work == 1" "full_time_work" ///
not_ft_work "full_time_work == 0" "full_time_work"

local var_labs `""Without positive net worth" "With positive net worth" "Male" "Female"  "Education: HS or higher" "Education: less than HS" "Other races" "Black"  "White"  "Having more than one kid" "Having one kid" "Working full time" "Not Working full time""'

* 2. Group definitions (correct)
local nwords : word count `groups'
putexcel set "${tables}Table.xlsx", sheet("TableA4-group") modify
*(a) set up header 
putexcel A1 = ("Group") B1 = ("Coef") C1=("(se)") D1=("HR") E1 = ("People by Wave Obs") F1=("Unique individual") 
local row = 2
local var_num = 1 

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
forvalues i = 1(3)`nwords' {

    local gname : word `i' of `groups'
    local gcond : word `=`i'+1' of `groups'
    local dropc : word `=`i'+2' of `groups'
    local var_lab: word `var_num' of `var_labs'

    di "=============================="
    di "Group: `gname'"
    di "Condition: `gcond'"
    di "Drop control: `dropc'"

    * Build controls for this group
    local controls_g


    foreach v of local base_controls {
        local todrop = 0
        foreach d of local dropc {
            if "`v'" == "`d'" local todrop = 1
        }
        if `todrop' == 0 local controls_g `controls_g' `v'
    }

    di "Controls used: `controls_g'"



        preserve
            keep if `gcond'

           eststo g_`gname':  stcox rcaregivers  `controls_g'  , /// 
                nohr vce(cluster hhidpn)
            matrix T = r(table)

    ****************************************************************
    * 3. Extract coefficient, SE, p-value
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
    * 7. Save results for Excel
    ****************************************************************

    local coefstar_`i' = ///
        string(b_`i', "%9.3f") + "`star'"

    local std_`i' = ///
        string(se_`i', "%9.3f")
	
            summarize divorced if e(sample)
            local depmean = r(mean)
	    local depmean_str = string(`depmean', "%9.3f")
            local N = e(N)

            egen tag = tag(hhidpn)
            count if tag==1 
            local Nkid = r(N)
	    

       * Write to Excel
	
	local hr = exp(b_`i')
	local hr_str = string(`hr', "%9.3f")
	putexcel A`row' = ("`var_lab'")
	putexcel B`row' = ("`coefstar_`i''")
	putexcel C`row' = ("(`std_`i'')")
	putexcel D`row' = ("`hr_str'")
	putexcel E`row' = `N'
	putexcel F`row' = `Nkid'
	
restore
local row = `row' + 1
local var_num = `var_num' + 1
    }

**# Graph the heterogeneity 

*(1) create mini datasets of each group 
forvalues i = 1(3)`nwords' {

    local gname : word `i' of `groups'
    local gcond : word `=`i'+1' of `groups'
    local dropc : word `=`i'+2' of `groups'

    di "=============================="
    di "Group: `gname'"
    di "Condition: `gcond'"
    di "Drop control: `dropc'"
    
    	estimates restore g_`gname'
	preserve 
	parmest, norestore list(estimate stderr p ) escal(N r2)
	rename es_2 r2
	
	keep if parm == "rcaregivers"
	replace parm = "`gname'" if parm == "rcaregivers"
	save "${final_data}`gname'.dta", replace
	restore 
    }


local nwords : word count `groups'
di "`nwords'"
local num_control = `nwords'/3 
	di "`num_control'"
	
*(2) append all minidata set 
use "${final_data}boy.dta", clear 
forvalues i = 1(3)`nwords' {

    local gname : word `i' of `groups'
    local gcond : word `=`i'+1' of `groups'
    local dropc : word `=`i'+2' of `groups'
	
    di "=============================="
    di "Group: `gname'"
    di "Condition: `gcond'"
    di "Drop control: `dropc'"
    
    	append using "${final_data}`gname'.dta"
    }
    
duplicates drop

* Gen stars for markers
gen stars = ""
replace stars = "*"   if p <= 0.10
replace stars = "**"  if p <= 0.05
replace stars = "***" if p <= 0.01

* Gen labels for markers
gen est_lbl = string(round(estimate, 0.001), "%9.3f") + stars

cap gen disp_num = 0 

replace disp_num = 19 if parm == "low_income"
replace disp_num = 18 if parm == "not_low_income"

replace disp_num = 16 if parm == "boy"
replace disp_num = 15 if parm == "female"

replace disp_num = 12 if parm == "ltHS"
replace disp_num = 13 if parm == "HS_gtHS"

replace disp_num = 8  if parm == "white"
replace disp_num = 9  if parm == "black"
replace disp_num = 10 if parm == "other_race"

replace disp_num = 6 if parm == "onekid"
replace disp_num = 5 if parm == "multikid"

replace disp_num = 3 if parm == "ft_work"
replace disp_num = 2 if parm == "not_ft_work"
sort disp_num

save "${final_data}coef_list.dta", replace 
use "${final_data}coef_list.dta", clear 
	
twoway ///
    (scatter disp_num estimate, mcolor("black") mlabel(est_lbl) mlabpos(1) mlabgap(0.2) mlabcolor(black)) ///
    (rcap  min95 max95 disp_num, lcolor(grey) horizontal) ///
    , ///
    xline(0, lcolor(grey) lpattern(dash)) /// zero line trên trục Y (estimate)yline(4 7 10 13 , lcolor(grey) lpattern(dash)) /// nhóm phân cách trên trục X (disp_num)
    ytitle("") ///
    xtitle("") ///
    ylabel( ///
    2  "Not Working full time" ///
    3  "Working full time" ///
    5  "Having more than one kid" ///
    6  "Having one kid" ///
    8  "White" ///
    9 "Black" ///
    10  "Other races" ///
    12 "Education: less than HS" ///
    13 "Education: HS or higher" ///
    15 "Female" ///
    16 "Male" ///
	18 "With positive net worth" ///
	19 "Without positive net worth" , ///
angle(0)) ///
    legend(pos(6) label(1 "Estimation") label(2 "95% CI") rows(1)) ///
	graphregion(color(white))
graph export "${graphs}Figure2-group.png", replace 

****************************************************
* End of do-file
****************************************************


