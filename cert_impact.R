#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr (rheilmayr@ucsb.edu)
# Project name: RSPO_productivity
#
# Input files:
#   - ibs_matched_rspo_ci_year_feb2020.csv: Merged UCSB / MCC dataset of mills with manufacturing census data
#
#
#
# TODO:
# - Develop metric of profitability
# - Look for heterogeneity based on local competition
# - Add controls for conditional parallel trends. Possible variables: Group, Island/Province, Capacity. Currently failing unconditional parallel trends tests.
#
# Current interpretation:
# - Certification increases input and output prices
# - Certification reduces production and number of workers
# - Net effect is a decrease in revenues. Likely even bigger decrease in profitability
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Imports ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(jsonlite)
library(plm)
library(bacondecomp)
library(did)
library(ggpubr)
library(sf)

ihsTransform <- function(y) {log(y + (y ^ 2 + 1) ^ 0.5)}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set workspace ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\collaborations\\ucsb-kraus\\data\\")
setwd(wdir)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load and clean data ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
df <- read.csv(paste0(wdir, "ucsb\\ibs_matched_rspo_ci_year_feb2020.csv"))
df <- df %>% 
  as_tibble() %>%
  select(-X) %>%
  filter(!is.na(uml_id))

df <- df %>%
  mutate(cert = year>=ci_year,
         cert = replace_na(cert, 0),
         cert_start = replace_na(ci_year, 0),
         ln_ffb_price = log(ffb_price_imp1),
         ln_ffb_val = log(in_val_ffb),
         ln_cpo_price = log(cpo_price_imp1),
         ln_pko_price = log(pko_price_imp1),
         ln_rev = log(revenue_total),
         ln_workers = log(workers_total_imp3),
         ln_cpo_vol = log(out_ton_cpo_imp1),
         ln_pko_vol = log(out_ton_pko_imp1),
         ln_ffb_vol = log(in_ton_ffb_imp1),
         ln_oer = log(in_ton_ffb_imp1 / out_ton_cpo_imp1))

df <- st_as_sf(x = df,                         
               coords = c("lon", "lat"),
               crs = 4326)



# test = df %>%
#   filter(cert==1,
#          year==2014) %>%
#   select(uml_id, year, ln_ffb_price) %>% 
#   drop_na()


# balanced_df <- clean_df %>% 
#   filter(!(year %in% c(2011, 2012))) %>% 
#   pdata.frame(index = c("firm_id", "year")) %>% 
#   make.pbalanced(balance.type = "shared.individuals")

# # %%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Pre-treatment outcomes ----- 
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%
# pt_df <- df %>% 
#   filter(year<2008,
#          year>2004) %>%
#   group_by(firm_id) %>% 
#   summarise(pt_ffb_price = mean(ln_ffb_price, na.rm = TRUE),
#             pt_workers = mean(ln_workers, na.rm = TRUE),
#             pt_revenue = mean(ln_rev, na.rm = TRUE),
#             pt_cpo_price = mean(ln_cpo_price, na.rm = TRUE)) %>% 
#   drop_na()
# 
# 
# df <- df %>% 
#   inner_join(pt_df, by = 'firm_id')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Regressions ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Revenue impacts

tally_obs <- function(did_df){
  obs_tally <- did_df %>%
    as_tibble() %>% 
    group_by(cert_start, year) %>% 
    tally()
  
  tally_heatmap <- obs_tally %>%
    filter(cert_start != 0) %>%
    ggplot(aes(x = year, y = cert_start, fill = n)) +
    geom_tile()
  
  tally_table <- obs_tally <- obs_tally %>% 
    pivot_wider(id_cols = cert_start,
                names_from = year,
                values_from = n)
  tally_summary <- list(heatmap = tally_heatmap, 
                        table = tally_table)
  return(tally_summary)
}


plot_did <- function(out_var, did_results){
  did_plot <- ggdid(did_results)
  did_plot <- did_plot +
    labs(y = out_var, 
         x = "Years from certification",
         colour = "After\ncertification") +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)
  return(did_plot)
}


run_did <- function(out_var, did_data, control = "notyettreated"){
  did_df <- did_data %>% 
    filter(!is.na(!!rlang::sym(out_var)))
  
  tally_summary <- tally_obs(did_df)
  
  did_mod <- att_gt(yname=out_var,
                    gname="cert_start",
                    idname="firm_id",
                    tname="year",
                    xformla=~1,
                    data=did_df,
                    est_method="dr",
                    print_details=TRUE,
                    control_group = "notyettreated",
                    panel = FALSE
  )
  Wpval <- did_mod$Wpval
  agg_did <- aggte(did_mod, type = "dynamic", na.rm = TRUE)
  did_plot <- plot_did(out_var, agg_did)
  output <- list(tally_summary = tally_summary, 
                 wpval = Wpval, 
                 agg_did = agg_did, 
                 did_plot = did_plot)
  return(output)
  
}



out_var <- "ln_rev"
results <- run_did(out_var, df)

did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         # year>2000,
         # year<2016
         )
summary(did_df %>% select(out_var))


# did_df <- did_df %>% 
#   filter(!is.na(out_var),
#          (cert_start>2009 & cert_start<2016) | cert_start==0)


qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="dr",
                  print_details=TRUE,
                  # alp = 0.1
                  control_group = "notyettreated",
                  panel = FALSE
)
summary(qual_cs)
ggdid(qual_cs)

qual_att <- aggte(qual_cs, type = "simple")
summary(qual_att)

qual_es <- aggte(qual_cs, type = "dynamic")
qual_es <- aggte(qual_cs, type = "dynamic")
summary(qual_es)
ln_rev_plot <- ggdid(qual_es)
ln_rev_plot +
  labs(y = "Total revenue (log)", 
       x = "Years from certification",
       colour = "After\ncertification") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)


# # FFB value
# out_var <- "ln_ffb_val"
# did_df <- df %>% 
#   filter(!is.na(!!rlang::sym(out_var)),
#          year>2000 & year<2016)
# summary(did_df %>% select(out_var))
# 
# obs_tally <- did_df %>%
#   as_tibble() %>% 
#   group_by(cert_start, year) %>% 
#   tally()
# 
# # did_df <- did_df %>% 
# #   filter(!is.na(out_var),
# #          (cert_start>2009 & cert_start<2016) | cert_start==0)
# 
# 
# qual_cs <- att_gt(yname=out_var,
#                   gname="cert_start",
#                   idname="firm_id",
#                   tname="year",
#                   xformla=~ln_ffb_vol,
#                   data=did_df,
#                   est_method="dr",
#                   print_details=TRUE,
#                   # alp = 0.1
#                   control_group = "notyettreated"
# )
# summary(qual_cs)
# ggdid(qual_cs)
# 
# qual_att <- aggte(qual_cs, type = "simple")
# summary(qual_att)
# 
# qual_es <- aggte(qual_cs, type = "dynamic", balance_e = 4)
# summary(qual_es)
# ln_rev_plot <- ggdid(qual_es)
# ln_rev_plot +
#   labs(y = "Total revenue (log)", 
#        x = "Years from certification",
#        colour = "After\ncertification") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)




# Output price impacts
out_var <- "ln_cpo_price"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)))

obs_tally <- did_df %>%
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(!is.na(out_var),
#          (cert_start>2009 & cert_start<2016) | cert_start==0)


qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  control_group = "notyettreated",
                  panel = FALSE
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "dynamic",  balance_e = 3)
qual_es <- aggte(qual_cs, type = "dynamic", na.rm = TRUE)

summary(qual_es)
cpo_price_plot <- ggdid(qual_es)
cpo_price_plot +
  labs(y = "CPO output price (log)", 
       x = "Years from certification",
       colour = "After\ncertification") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)



# Output volume impacts
out_var <- "ln_cpo_vol"
did_df <- df %>%
  filter(!is.na(!!rlang::sym(out_var)),
         year>2000 & year<2016
         )

obs_tally <- did_df %>%
  group_by(cert_start, year) %>%
  tally()

did_df <- did_df %>%
  filter(!is.na(out_var),
         (cert_start>2009 & cert_start<2016) | cert_start==0)

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  control_group = "notyettreated"
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "dynamic", na.rm = TRUE)
summary(qual_es)
cpo_vol_plot <- ggdid(qual_es)
cpo_vol_plot +
  labs(y = "CPO output volume (log)",
       x = "Years from certification",
       colour = "After\ncertification") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)


# Input FFB price
out_var <- "ln_ffb_price"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         year>2000 & year<2020
         )

obs_tally <- did_df %>%
  as_tibble() %>% 
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(!is.na(out_var),
#          (cert_start>2009 & cert_start<2016) | cert_start==0)

# did_df <- did_df %>%
#   filter(cert_start %in% list(2010, 2011, 0))

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="dr",
                  print_details=TRUE,
                  panel = FALSE,
                  control_group = "notyettreated"
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "simple", na.rm = TRUE)
summary(qual_es)

qual_es <- aggte(qual_cs, type = "dynamic", na.rm = TRUE)
summary(qual_es)
ffb_price_plot <- ggdid(qual_es)
ffb_price_plot +
  labs(y = "FFB input price (log)", 
       x = "Years from certification",
       colour = "After\ncertification") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)




out_var <- "ln_ffb_vol"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         year>2000 & year<2016
         )

obs_tally <- did_df %>%
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(cert_start %in% list(2010, 2012, 2015, 0))

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  panel = FALSE,
                  control_group = "notyettreated"
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "simple", na.rm = TRUE)
summary(qual_es)

qual_es <- aggte(qual_cs, type = "dynamic", na.rm = TRUE)
summary(qual_es)
ggdid(qual_es)




out_var <- "ln_pko_price"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         year>2006 & year<2016)

obs_tally <- did_df %>%
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(cert_start %in% list(2010, 2011, 2012, 2013, 2014, 2015, 0))

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  control_group = "notyettreated",
                  panel = FALSE
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "simple")
summary(qual_es)

qual_es <- aggte(qual_cs, type = "dynamic")
summary(qual_es)
ggdid(qual_es)



out_var <- "ln_pko_vol"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         year>2006 & year<2016)

obs_tally <- did_df %>%
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(cert_start %in% list(2010, 2011, 2012, 2013, 2014, 2015, 0))

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  control_group = "notyettreated",
                  panel = FALSE
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "simple")
summary(qual_es)

qual_es <- aggte(qual_cs, type = "dynamic")
summary(qual_es)
ggdid(qual_es)






# Number of workers
out_var <- "ln_workers"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         year>2000 & year<2016)

obs_tally <- did_df %>%
  as_tibble() %>% 
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(cert_start %in% list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 0))

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  panel = FALSE,
                  control_group = "notyettreated"
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "simple", balance_e = 4)
summary(qual_es)

qual_es <- aggte(qual_cs, type = "dynamic")
summary(qual_es)
ffb_price_plot <- ggdid(qual_es)
ffb_price_plot +
  labs(y = "Number of workers (log)", 
       x = "Years from certification",
       colour = "After\ncertification") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)



# Export share (pko slight uptick, cpo no impact)
out_var <- "prex_pko_imp1"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         year>2000 & year<2016)

obs_tally <- did_df %>%
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(cert_start %in% list(2009, 2010, 2011, 2012, 2013, 2014, 0))

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  panel = FALSE,
                  control_group = "notyettreated"
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "simple", balance_e = 3)
summary(qual_es)

qual_es <- aggte(qual_cs, type = "dynamic")
summary(qual_es)
ffb_price_plot <- ggdid(qual_es)
ffb_price_plot +
  labs(y = "Export share", 
       x = "Years from certification",
       colour = "After\ncertification") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)





# OER
out_var <- "ln_oer"
did_df <- df %>% 
  filter(!is.na(!!rlang::sym(out_var)),
         year>2000 & year<2016)

obs_tally <- did_df %>%
  group_by(cert_start, year) %>% 
  tally()

# did_df <- did_df %>% 
#   filter(cert_start %in% list(2010, 2015, 0))

qual_cs <- att_gt(yname=out_var,
                  gname="cert_start",
                  idname="firm_id",
                  tname="year",
                  xformla=~1,
                  data=did_df,
                  est_method="reg",
                  print_details=TRUE,
                  panel = FALSE,
                  control_group = "notyettreated"
)
summary(qual_cs)
ggdid(qual_cs)

qual_es <- aggte(qual_cs, type = "simple", balance.e = 3)
summary(qual_es)

qual_es <- aggte(qual_cs, type = "dynamic")
summary(qual_es)
ffb_price_plot <- ggdid(qual_es)
ffb_price_plot +
  labs(y = "Number of workers (log)", 
       x = "Years from certification",
       colour = "After\ncertification") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plots ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
