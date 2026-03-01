/*******************************************************************************
Project: NEKN33 - Exercise 4
Author: Alexander Eriksson Byström, Erik Alm Ramberg, Jacob Lindberg, Nathalie Hånell
Date: March 2026
*******************************************************************************/

clear all
set more off

// Load data
use "exercise_4_DiD.dta", clear

/*******************************************************************************
Exercise 1: Difference-in-Differences
*******************************************************************************/

// Question 1: Describe and summarize the data
describe
summarize

// Question 2: Sample means by group
// Create dummy for anykids (already exists as children, but here we group)
gen nkids = 0
replace nkids = 1 if children == 1 // self explanatory
replace nkids = 2 if children >= 2 // *

// Create conditional earnings variable
gen earn_cond = earn if work == 1

// Calculate means by group
tabstat urate children nonwhite finc earn earn_cond age ed work unearn, by(nkids) stat(mean)

// Question 4: Construct treatment and post-expansion variables
gen anykids = (children > 0)
gen post93 = (year >= 1994)

// Verify the variables
tab anykids
tab year post93

// Question 5: Plot mean annual employment rates by year
preserve
collapse (mean) work, by(year anykids)

// Create the plot
twoway (line work year if anykids==1, lpattern(solid) lcolor(blue) mlabel(work)) ///
       (line work year if anykids==0, lpattern(dash) lcolor(red) mlabel(work)), ///
       xline(1993.5, lcolor(black) lpattern(dot)) ///
       xlabel(1991(1)1996) ///
       title("Mean Annual Employment Rates") ///
       subtitle("Treatment vs. Control Group") ///
       legend(label(1 "Treatment (Any Kids)") label(2 "Control (No Kids)")) ///
       ytitle("Employment Rate") xtitle("Year")

graph export "employment_trends.png", replace
restore

// Question 6: Unconditional Difference-in-Differences
// Calculate means and standard errors by group and time period
mean work, over(anykids post93)

// Question 7: Conditional Difference-in-Differences (Regression-based)
// Basic DiD model without additional covariates
regress work i.anykids##i.post93

// Question 8: DiD with demographic characteristics
gen age2 = age^2
gen ed2 = ed^2
regress work i.anykids##i.post93 unearn children nonwhite age age2 ed ed2

// Question 9: State unemployment rate and group interaction
gen urate_anykids = urate * anykids
regress work i.anykids##i.post93 unearn children nonwhite age age2 ed ed2 urate urate_anykids

// Question 10: Treatment effect by 1 vs 2+ children
regress work i.nkids##i.post93 unearn nonwhite age age2 ed ed2 urate

// Question 11: Placebo treatment model (Pre-reform only)
preserve
keep if year <= 1993
gen placebo_post = (year >= 1992)
regress work i.anykids##i.placebo_post unearn nonwhite age age2 ed ed2 urate
restore

/*******************************************************************************
Exercise 2: Twin Fixed Effects
*******************************************************************************/

// Load twin data
use "exercise_4_twins.dta", clear

// Question 1: Standard OLS regression
regress lnwage educ1 female age agesq nonwhite if type == 3

// Question 3: Within-pair differences
preserve
keep if type == 3
bysort pairid (id): gen d_lnwage = lnwage[1] - lnwage[2]
bysort pairid (id): gen d_educ = educ1[1] - educ1[2]
keep if d_lnwage != .
twoway (scatter d_lnwage d_educ)
graph export "twins_scatter.png", replace
histogram d_educ, discrete percent
graph export "twins_hist.png", replace
restore

// Question 4: Twin Fixed Effects
// (i) Standard FE
xtset pairid
xtreg lnwage educ1 if type == 3, fe

// (ii) First-Difference model
preserve
keep if type == 3
bysort pairid (id): gen d_lnwage = lnwage[1] - lnwage[2]
bysort pairid (id): gen d_educ = educ1[1] - educ1[2]
keep if d_lnwage != .
regress d_lnwage d_educ, noconstant

// (iii) FD model restricted to pairs with different education levels
regress d_lnwage d_educ if d_educ != 0, noconstant

// (iv) Compare characteristics based on education difference
gen diff_educ_status = (d_educ != 0)
tabstat momed daded nonwhite female age, by(diff_educ_status) stat(mean)
restore

// Question 5: IV Estimation (Addressing Measurement Error)
preserve
keep if type == 3
bysort pairid (id): gen d_lnwage = lnwage[1] - lnwage[2]
bysort pairid (id): gen d_educ1 = educ1[1] - educ1[2]
bysort pairid (id): gen d_educ2 = educ2[1] - educ2[2]
keep if d_lnwage != .

// IV Regression: Instrumenting self-reported diff with sibling-reported diff
ivregress 2sls d_lnwage (d_educ1 = d_educ2), noconstant
restore

