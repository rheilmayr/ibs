use D:\\cloud\\Dropbox\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\data\\ibs\\ucsb_ibs.dta, clear

xtset firm_id year

prodest ln_value_added, free(ln_labor_cost ln_ffb_val) state(ln_fc_est_tot) proxy(ln_electricity)
predict tfp

markupest mkup, method(dlw) output(ln_value_added) inputvar(ln_ffb_val) free(ln_labor_cost ln_ffb_val) state(ln_fc_est_tot) proxy(ln_electricity) prodestopt("") corrected verbose
// predict mkup2, markups

save D:\\cloud\\Dropbox\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\data\\ibs\\ucsb_ibs_tfp.dta, replace
