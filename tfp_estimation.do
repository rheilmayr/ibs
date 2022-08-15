cd "$opal_wd"
log using "build/code/logs/tfp_estimates_$S_DATE_2.log", text
ieboilstart,	versionnumber(13.1) ///
				custom(	ssc install prodest)

`r(version)'

disp "DateTime: $S_DATE $S_TIME"

global input_data "build/temp/IBS"
global input_file "$input_data/IBS_panel_pre_tfp"

//// Recording dropped observations in a tex table 
/* See https://github.com/paulnov/stata-tex for usage */
do "dependencies/stata-tex/stata-tex.do"
/* Table is generated at the end of cleaning file by program table_from_tpl */

/* Keys that store information appear in in base_sample_template and base_sample_csv */
global tfp_template_s 	"paper/templates/tables/tfp_estimates_template_small.tpl"
global tfp_csv_s 			"build/temp/IBS/tfp/tfp_estimates.csv"
global tfp_output_s		"build/output/tables/tfp_estimates_small.tex"

global tfp_template 	"paper/templates/tables/tfp_estimates_template.tpl"
global tfp_csv 			"build/temp/IBS/tfp/tfp_estimates.csv"
global tfp_output		"build/output/tables/tfp_estimates.tex"

cap erase $base_sample_csv
cap erase $base_sample_output


local agg_types "co"
local estimator "lp wrdg"

global cleaning_types 5 6 12
global tfp_list

forvalues s = 2/2 {
	foreach c of numlist $cleaning_types {
		use $input_file, clear
		xtset firm_id year
					***************** Setup *****************
		* Prepare randomization for bootstrap
		set seed 75843
		*set seed 12345
		capture drop random
		gen random=uniform()
		sort random

		egen sec2dig`s' = group(kbli`s')

		local avars 1 2

		foreach a of numlist `avars' {
			foreach k in `agg_types' {				
				local CAP fc_est_tot_imp`c'`k'_ln 				
				foreach met in `estimator' {
					local specs "l_m l_e"
					if "`met'" == "lp" {
						local correction """ acf"
						local attrition """"
						local pred_type "residuals"
					}
					else if "`met'" == "wrdg" {
						local correction """"
						local attrition """"
						local pred_type "residuals"
					}
					else if "`met'" == "op" {
						local specs "l_i l_f"
						local correction """ acf"
						local attrition """"
						local pred_type "residuals"
					}
					foreach p in `pred_type' {
						if "`p'" == "residuals" {
							local fsres
							local pr "r"
						}
						else if "`p'" == "omega" {
							local fsres "fsres(fsres)"
							local pr "o"
						}

						foreach acf in `correction' {
							if "`acf'" == "acf" {
								local acf_excl "lm_e lsm_e"
								local specs : list specs - acf_excl
								local va "va"
								local DV value_added_self_imp`a'_ln
							}
							else {
								local va "va"
								local DV value_added_self_imp`a'_ln
							}
							foreach att in `attrition' {
								foreach sp in `specs' {
									gen b_lab_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen b_cap_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen obs_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.

									gen b_lab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen se_lab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen b_slab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen se_slab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen b_uslab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen se_uslab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen b_mat_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen se_mat_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen b_cap_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen se_cap_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									gen obs_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.

									* Loop below includes a check  for the minimum number of observations.
									gen obstest2`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =0

									* Generate TFP variable
									gen tfp`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' =.
									local tfp tfp`c'`k'`a'`s'`sp'`met'`acf'`att'`pr'
									global tfp_list $tfp_list `tfp'							


									if "`sp'" == "l_m" {
										local FREE  workers_total_imp`a'_ln
										local PROXY materials_tot_imp`a'_ln
									}
									else if "`sp'" == "l_e" {
										local FREE workers_total_imp`a'_ln
										local PROXY elec_qty_imp`a'_ln
									}							
									else if "`sp'" == "ls_m" {
										local FREE workers_prod_imp`a'_ln workers_other_imp`a'_ln 
										local PROXY materials_tot_imp`a'_ln
									}
									else if "`sp'" == "lm_e" {
										local FREE workers_total_imp`a'_ln materials_tot_imp`a'_ln
										local PROXY elec_qty_imp`a'_ln
									}
									else if "`sp'" == "l_me" {
										local FREE workers_total_imp`a'_ln
										local PROXY elec_qty_imp`a'_ln materials_tot_imp`a'_ln
									}
									else if "`sp'" == "ls_me" {
										local FREE workers_prod_imp`a'_ln workers_other_imp`a'_ln
										local PROXY elec_qty_imp`a'_ln materials_tot_imp`a'_ln
									}
									else if "`sp'" == "ls_e" {
										local FREE workers_prod_imp`a'_ln workers_other_imp`a'_ln
										local PROXY elec_qty_imp`a'_ln
									}
									else if "`sp'" == "lsm_e" {
										local FREE workers_prod_imp`a'_ln workers_other_imp`a'_ln materials_tot_imp`a'_ln
										local PROXY elec_qty_imp`a'_ln
									}
									else if "`sp'" == "l_i" {
										local FREE workers_total_imp`a'_ln
										local PROXY fc_add_imp_ln
									}
									else if "`sp'" == "l_f" {
										local FREE workers_total_imp`a'_ln
										local PROXY inv_tot_imp_ln
									}
										***************** Loop for sector-based TFP estimation *****************
									qui sum sec2dig`s', detail
									di r(max)
									local rmax = r(max)

									forvalues i = 1(1)`rmax' {
										di "Sector " "`i'" " OLS"

										* Simple OLS regression
										cap reg `DV' `FREE' `CAP' ///
											if sec2dig`s'==`i'

										* Saving the coefficients for later
										cap replace b_lab_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
											_b[workers_total_imp`a'_ln] if sec2dig`s'==`i'																	
										cap replace b_slab_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
											_b[workers_other_imp`a'_ln] if sec2dig`s'==`i'										
										cap replace b_uslab_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
											_b[workers_prod_imp`a'_ln] if sec2dig`s'==`i'										
										cap replace b_mat_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
											_b[materials_tot_imp`a'_ln] if sec2dig`s'==`i'										
										cap replace b_cap_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
											_b[`CAP'] if sec2dig`s'==`i'

										* Saving # of firms, # of observations
										replace obs_OLS`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = e(N) if sec2dig`s'==`i'

										*  Now LP method
										di "Sector " "`i'"
										di "`sp'"
										cap n prodest `DV' if sec2dig`s'==`i',	///
												free(`FREE') state(`CAP') proxy(`PROXY') ///					///
													`va' met(`met') `acf' `att' opt(nm) reps(20)	///
													id(firm_id) t(year) `fsres'
									    if c(rc) == 0 {
									        predict tfp_`i' if sec2dig`s'==`i', `p'
											replace tfp`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = tfp_`i' if sec2dig`s'==`i'
											drop tfp_`i' 
											cap drop fsres
											


											* Saving the coefficients for later
											cap replace b_lab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_b[workers_total_imp`a'_ln] if sec2dig`s'==`i'							
											cap replace se_lab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_se[workers_total_imp`a'_ln] if sec2dig`s'==`i'	
											cap store_est_tpl using $tfp_csv,	/// 	
												coef(workers_total_imp`a'_ln) ///
												name(`i'lab`c'`k'`a'`s'`sp'`met'`acf'`att'`pr') all


											cap replace b_slab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_b[workers_other_imp`a'_ln] if sec2dig`s'==`i'
											cap replace se_slab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_se[workers_other_imp`a'_ln] if sec2dig`s'==`i'
											cap store_est_tpl using $tfp_csv,	/// 	
												coef(workers_other_imp`a'_ln) ///
												name(`i'slab`c'`k'`a'`s'`sp'`met'`acf'`att'`pr') all
											
											cap replace b_uslab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_b[workers_prod_imp`a'_ln] if sec2dig`s'==`i'
											cap replace se_uslab_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_se[workers_prod_imp`a'_ln] if sec2dig`s'==`i'
											cap store_est_tpl using $tfp_csv,	/// 	
												coef(workers_prod_imp`a'_ln) ///
												name(`i'uslab`c'`k'`a'`s'`sp'`met'`acf'`att'`pr') all
											
											cap replace b_mat_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_b[materials_tot_imp`a'_ln] if sec2dig`s'==`i'
											cap replace se_mat_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_se[materials_tot_imp`a'_ln] if sec2dig`s'==`i'											
											cap store_est_tpl using $tfp_csv,	/// 	
												coef(materials_tot_imp`a'_ln) ///
												name(`i'mat`c'`k'`a'`s'`sp'`met'`acf'`att'`pr') all

											
											cap replace b_cap_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_b[`CAP'] if sec2dig`s'==`i'
											cap replace se_cap_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = ///
												_se[`CAP'] if sec2dig`s'==`i'											
											cap store_est_tpl using $tfp_csv,	/// 	
												coef(`CAP') ///
												name(`i'cap`c'`k'`a'`s'`sp'`met'`acf'`att'`pr') all

											
											* Saving # of firms, # of observations
											replace obs_PR`c'`k'`a'`s'`sp'`met'`acf'`att'`pr' = e(N) if sec2dig`s'==`i'				
											
											    }
									    else if c(rc) == 2001 {
									        display "Insufficient results for Sector`i': moving on."
											replace obstest2`c'`k'`a'`s'`sp'`met'`acf'`att'`pr'=1 if sec2dig`s'==`i'
									    }
									    else {
									        display "Unanticipated error in regression with i = `i'"
									        /* exit `c(rc)' */
									    }
								    }
							}
							}
						}
					}
				}
			}
		}
	save build/temp/IBS/tfp/IBS_panel_wtfp`s'_c`c'.dta, replace
	ds sec2dig`s' kbli`s', not
	collapse `r(varlist)', by(sec2dig`s' kbli`s')
	cap drop _merge
	merge m:1 kbli`s' using "build/input/IBS/meta/industry_concordance/two_digit_boilerplate_2000_`s'.dta"
	drop _merge
	save build/output/tables/tfp/IBS_panel_tfp`s'_c`c'.dta, replace
	}
}


use $input_file, clear
xtset firm_id year


forvalues s = 2/2 {
	foreach c of numlist $cleaning_types {
		cap drop _merge
		di "s`s'_c`c'"
		merge 1:1 firm_id year using build/temp/IBS/tfp/IBS_panel_wtfp`s'_c`c'.dta, nogen
	}
}

saveold build/output/data/IBS_panel_wtfp.dta, version(12) replace



use build/output/tables/tfp/IBS_panel_tfp2_c5, clear
keep label_en

forvalues s = 2/2 {
	foreach c of numlist $cleaning_types {
		cap drop _merge
		merge 1:1 label_en using build/output/tables/tfp/IBS_panel_tfp`s'_c`c'.dta, nogen
	}
}

sort kbli1
save build/output/tables/tfp/IBS_panel_tfp.dta, replace

table_from_tpl, 	t($tfp_template) r($tfp_csv) 			///
					o($tfp_output)

table_from_tpl, 	t($tfp_template_s) r($tfp_csv_s) 			///
					o($tfp_output_s)

log close
