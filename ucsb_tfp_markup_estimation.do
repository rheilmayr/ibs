use D:\\cloud\\Dropbox\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\data\\ibs\\ucsb_ibs.dta, clear

prodest ln_value_added, free(ln_workers) state(ln_fc_est_tot) proxy(ln_materials) id(firm_id) t(year)
predict tfp

markupest mkup, method(dlw) output(ln_value_added) inputvar(ln_workers) free(ln_workers) state(ln_fc_est_tot) proxy(ln_materials) id(firm_id) t(year) prodestopt("") corrected verbose

save D:\\cloud\\Dropbox\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\data\\ibs\\ucsb_ibs_tfp.dta, replace
