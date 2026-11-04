
cap log close
log using "${logs}`c(current_date)'_km_surv_curve.log", replace
display "Log timestamp: `c(current_date)' `c(current_time)'"


/*
Title: Kaplan-Meier Survival and Cumulative Hazard Analysis of Caregiving and Divorce

Purpose:
    Examine differences in divorce-free survival and cumulative divorce
    hazards between caregiving and non-caregiving individuals.

    1. Prepare the survival analysis dataset:
       - Define caregiving status labels.
       - Construct start and stop times based on survey waves.
       - Set up survival data using divorce as the failure event.
       - Check whether caregiving status varies over time.
       - Verify that observations after divorce are excluded from
         the survival analysis.

    2. Compare survival experiences between caregiving groups:
       - Conduct a log-rank test to compare survival distributions.
       - Generate Kaplan-Meier survival probability curves by caregiving
         status, including risk tables.
       - Generate cumulative hazard curves to compare the accumulation
         of divorce risk over time.

Output:
    Input data:
        - ${final_data}psm_divorce_sample.dta

    Graphs:
        - ${graphs}km_survival.png
            Kaplan-Meier survival probability by caregiving status.
        - ${graphs}cummulative_hazard.png
            Cumulative hazard of divorce by caregiving status.

    Statistical output:
        - Log-rank test comparing caregiving and non-caregiving groups.
        - Time-varying caregiving status diagnostic.
        - Post-divorce observation check.

Modified by: Huong Nguyen
Date: 2026/01/31
*/

****************************************************
use "${final_data}marital_status_sample.dta", clear 

label define caregiver ///
    0 "Non-caregiving" ///
    1 "Caregiving"
label values rcaregivers caregiver

**# set up cox — wave time
sort hhidpn wave
by hhidpn (wave): gen rel_start = wave - wave[1]
by hhidpn (wave): gen rel_stop  = rel_start + 1

stset rel_stop, ///
    enter(time rel_start) ///
    failure(divorced == 1) ///
    id(hhidpn)

bysort hhidpn (wave): egen first_wave = min(wave)

//list hhidpn wave start_time stop_time rcaregivers in 1/100, sepby(hhidpn)

* Check if rcaregivers is time-varying
stvary rcaregivers

* Check if after divorce is dropped
bysort hhidpn (wave): egen divorce_wave = ///
    min(cond(divorced == 1, wave, .))
gen after_divorce = wave > divorce_wave if !missing(divorce_wave)
tab after_divorce
count if after_divorce == 1 & _st == 1   // phải = 0 nếu setup đúng


********************************************************************
* Kaplan-Meier and Cumulative Hazard Graphs (export PNG + PDF)
********************************************************************

*--------------------------
* 1. Log-rank test (Mantel-Byar, time-varying covariate)
*--------------------------
sts test rcaregivers, logrank

*--------------------------
* 2. Kaplan-Meier Survival Probability (Simon-Makuch style)
*--------------------------
sts graph, by(rcaregivers) ///
    risktable(, size(small)) ///
    yscale(range(0.75 1.00)) ///
    ylabel(0.75(0.05)1.00, format(%4.2f) angle(0) labsize(small)) ///
    xlabel(1(1)15, labsize(small)) ///
    ytitle("Survival probability", size(medsmall)) ///
    xtitle("Analysis time (wave)", size(medsmall)) ///
    legend(order(2 "Caregiving" 1 "Non-caregiving") ///
           pos(7) ring(0) col(1) ///
           size(vsmall) region(lstyle(none))) ///
    title("") subtitle("") ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(km_surv_60, replace)

graph export "${graphs}Figure3_km_survival.png", replace width(2400)
/*
*--------------------------
* 3. Cumulative Hazard
*--------------------------
sts graph, by(rcaregivers) cumhaz ///
    risktable(, size(small)) ///
    ytitle("Cumulative hazard of divorce", size(medsmall)) ///
    xtitle("Analysis time (waves)", size(medsmall)) ///
    legend(order(2 "Caregiving" 1 "Non-caregiving") ///
           pos(11) ring(0) col(1) size(vsmall) region(lstyle(none))) ///
    title("") subtitle("") ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(cumhaz_60, replace)

graph export "${graphs}cummulative_hazard.png", replace width(2400)
*/
