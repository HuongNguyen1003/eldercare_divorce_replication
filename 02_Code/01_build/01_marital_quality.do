
cap log close
display "Log timestamp: `c(current_date)' `c(current_time)'"
log using "${logs}`c(current_date)'_01_marital_quality.log", replace

/*******************************************************************************
Title: Construct Marital Quality Measures

Purpose: This script standardizes and merges marital quality measures from the
         Health and Retirement Study (HRS) across survey waves.

Intake data:
- HRS Core data, Leave-Behind (LB) questionnaire files:
  - 2004 (Wave 7)
  - 2006 (Wave 8)
  - 2008 (Wave 9)
  - 2010 (Wave 10)
  - 2012 (Wave 11)
  - 2014 (Wave 12)
  - 2016 (Wave 13)
  - 2018 (Wave 14)
  - 2020 (Wave 15)

Output data:
- Wave-specific standardized marital quality datasets saved in
  "${processed_data}".
- Merged longitudinal marital quality dataset saved in:
  "${final_data}/marital_quality_${MQ_start_wave}-${MQ_end_wave}.dta"

Created: [MM.DD.YYYY] - Huong Nguyen

Updates:
- [MM.DD.YYYY] - [Author]: Standardized marital quality variables and merged
  HRS Leave-Behind questionnaire data across Waves 7-15.

*******************************************************************************/

/*******************************************************************************
MERGE MARITAL QUALITY DATA ACROSS WAVES
*******************************************************************************/

**# Merge Waves 7-15

clear all
cd "${raw_data}"

use r7marital_quality.dta, clear

merge 1:1 hhidpn using r8marital_quality.dta
drop _merge

merge 1:1 hhidpn using r9marital_quality.dta
drop _merge

merge 1:1 hhidpn using r10marital_quality.dta
drop _merge

merge 1:1 hhidpn using r11marital_quality.dta
drop _merge

merge 1:1 hhidpn using r12marital_quality.dta
drop _merge

merge 1:1 hhidpn using r13marital_quality.dta
drop _merge

merge 1:1 hhidpn using r14marital_quality.dta
drop _merge

merge 1:1 hhidpn using r15marital_quality.dta
drop _merge

rename hhidpn rahhidpn

save "${final_data}marital_quality_${MQ_start_wave}-${MQ_end_wave}.dta", replace


cap log close
