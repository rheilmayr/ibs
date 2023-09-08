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
# - Dig into markup calculations
# - Further explore heterogeneity based on local competition
# - Make better use of pre-treatment trends tests
# - Add controls for conditional parallel trends. Possible variables: Group, Island/Province, Capacity. Currently failing unconditional parallel trends tests for some variables
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
library(fixest)
library(tidyverse)
library(jsonlite)
library(plm)
library(bacondecomp)
library(did)
library(ggpubr)
library(sf)
library(viridis)
library(patchwork)
library(prodest)
library(readxl)
library(sf)
library(units)
library(haven)
library(tidylog)
library(naniar)
library(fixest)

ihsTransform <- function(y) {log(y + (y ^ 2 + 1) ^ 0.5)}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set workspace ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\data\\")
setwd(wdir)
fig_dir <- paste0(dropbox_dir,"\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\output\\figs\\")

## Full trase mill list
trase_dir <- paste0(dropbox_dir, "\\collaborations\\trase\\Trase\\Indonesia\\palm\\mill_lists\\tracker\\")
mill_est <- read_xlsx(paste0(trase_dir, "mill_yr_tracker.xlsx"))
mill_est <- mill_est %>% 
  select(trase_code, earliest_yr_exist, latitude, longitude)
mill_cap <- read_xlsx(paste0(trase_dir, "mill_caps_tracker.xlsx"))
mill_cap <- mill_cap %>% 
  rowwise() %>%
  mutate(max_cap=max(c_across(starts_with("cap_2")), na.rm = TRUE)) %>% 
  filter(is.finite(max_cap)) %>% 
  select(trase_code, max_cap)
all_mills <- mill_est %>% 
  left_join(mill_cap, by = "trase_code") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load and clean data ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Process Kim's supply chain model data
sc_df <- read.csv(paste0(wdir, "carlson/ffbDataset.csv"))
sc_df <- sc_df %>% 
  select(millCode, ffbYear, supplyChain2) %>% 
  distinct() %>% 
  mutate(supplyChain2 = ifelse(supplyChain2 == "unknown", NA, supplyChain2),
         supplyChain2 = ifelse(supplyChain2 == "MB and IP/SG", "IP/SG", supplyChain2))

first_ip <- sc_df %>% 
  filter(supplyChain2 == "IP/SG") %>% 
  group_by(millCode) %>% 
  summarise(first_ip_yr = min(ffbYear))

mills_w_sc <- sc_df %>% 
  select(millCode) %>% 
  distinct() %>% 
  left_join(first_ip, by = "millCode") %>% 
  mutate(first_ip_yr = replace_na(first_ip_yr, 9999),
         trase_code = str_replace(millCode, "M", "M-0")) %>% 
  select(-millCode)

# Load main dataset
df <- read.csv(paste0(wdir, "ibs\\ibs_matched_rspo_ci_year.csv"))
df <- df %>% 
  group_by(trase_code) %>% 
  mutate(ever_cert = max(cert))

df <- df %>% 
  left_join(mills_w_sc, by = "trase_code")
df <- df %>% 
  mutate(ever_ipsg = first_ip_yr < 9999,
         only_mb = first_ip_yr == 9999)

## Data issue - shouldn't be getting cert==0 and either MB or IPSG
df %>% group_by(ever_cert, ever_ipsg) %>% tally()
df %>% group_by(ever_cert, only_mb) %>% tally()


stata_df <- read_dta(paste0(wdir, "ibs\\ucsb_ibs_tfp.dta")) %>% 
  select(firm_id, year, tfp, mkup)
df <- df %>% 
  left_join(stata_df, by = c("firm_id", "year"))


cpo_belawan_prices <- df %>% 
  group_by(year) %>%
  summarise(cpo_fob_price = mean(fob_blwn_cpo_y, na.rm = TRUE),
            cpo_dom_price = mean(dom_blwn_cpo_y, na.rm = TRUE))


df <- df %>% 
  as_tibble()  %>%
  select(-X) %>% 
  filter(!is.na(uml_id))

df <- st_as_sf(x = df,                         
               coords = c("lon", "lat"),
               crs = 4326)


df <- df %>%
  mutate(ln_tfp = log(tfp),
         ln_mkup = log(mkup),
         ln_va_share = log(exp(ln_value_added) / exp(ln_rev)),
         price_ratio = exp(ln_ffb_price) / exp(ln_cpo_price),
         ln_price_ratio = log(price_ratio),
         ln_va = log(value_added_self_imp2)) %>% 
  left_join(cpo_belawan_prices, by = "year") %>% 
  mutate(cpo_premium = cpo_price_imp2 / cpo_dom_price,
         ln_cpo_premium = log(cpo_premium))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Start to differentiate market competitiveness ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
base_year = 2010
dist_threshold <- 40
current_mills <- all_mills %>% 
  filter(earliest_yr_exist <= base_year) %>% 
  group_by(trase_code) %>% 
  summarise(capacity = first(max_cap))
  
mill_dist <- st_distance(current_mills)
rownames(mill_dist) <- current_mills$trase_code
colnames(mill_dist) <- current_mills$trase_code
mill_dist <- as_tibble(mill_dist) %>% 
  drop_units()
mill_dist <- mill_dist / 1000

mill_dist <- mill_dist %>%
  mutate(trase_code = names(mill_dist)) %>% 
  mutate(across(.cols = !trase_code, ~(.x < dist_threshold)))

neighbor_list <- c()
for (mill in current_mills %>% pull(trase_code)){
  neighbors <- mill_dist %>% 
    filter(get(mill) == TRUE) %>% 
    pull(trase_code)
  neighbor_list[mill] <- list(neighbors)
}

summarize_neighbors <- function(trase_code) {
  neighbors <- neighbor_list[trase_code]
  n_neighbors <- length(neighbors[[1]]) - 1
  cap_neighbors <- current_mills %>% 
    filter(trase_code %in% neighbors[[1]]) %>% 
    pull(capacity) %>% 
    sum()
  sum_neighbors <- data.frame("n_neighbors" = n_neighbors, "cap_neighbors" = cap_neighbors)
  return(sum_neighbors)
}

current_mills <- current_mills %>% 
  mutate(sum_neighbors = map(trase_code, summarize_neighbors)) %>% 
  unnest(sum_neighbors) %>% 
  mutate(mkt_power = capacity / cap_neighbors,
         cap_neighbors = cap_neighbors - capacity)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define main functions ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
tally_obs <- function(did_df){
  tally_df <- did_df %>% 
    filter(cert_start>0) %>% 
    mutate(years_to_cert = year - cert_start)
  
  obs_tally <- tally_df %>%
    as_tibble() %>% 
    group_by(cert_start, years_to_cert) %>% 
    tally()
  
  tally_heatmap <- obs_tally %>%
    filter(cert_start != 0) %>%
    ggplot(aes(x = years_to_cert, y = cert_start, fill = n)) +
    geom_tile()
  
  tally_heatmap <- tally_heatmap +
    labs(x = "Years from certification") +
    theme_bw() +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey", size = 1) +
    scale_fill_viridis() 
  
  tally_table <- obs_tally <- obs_tally %>% 
    pivot_wider(id_cols = cert_start,
                names_from = years_to_cert,
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
    scale_color_manual(values=c("#29AF7FFF", "#440154FF")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey", size = 1)
  return(did_plot)
}

plot_trends <- function(out_var, did_data){
  trend_data <- did_data %>% 
    mutate(ever_certified = cert_start > 0) %>% 
    group_by(ever_certified, year) %>% 
    summarize(!!out_var := mean(.data[[out_var]], na.rm = TRUE)) 
  
  trend_plot <- trend_data %>%
    ggplot(aes_string(x = "year", y = out_var, group = "ever_certified", color = "ever_certified")) +
    geom_line() +
    theme_bw() +
    scale_color_manual(values=c("#29AF7FFF", "#440154FF")) +
    labs(title = "Raw trend in outcome",
         y = out_var,
         x = "Year",
         colour = 'Ever\ncertified')
  return(trend_plot)
}


align_plots <- function(trend_plot, did_plot, tally_heatmap, agg_did){
  xlims <- layer_scales(did_plot)$x$range$range
  xlims[1] = xlims[1] - 1
  xlims[2] = xlims[2] + 1
  combined_plot <- trend_plot / (did_plot + xlim(xlims)) / (tally_heatmap + xlim(xlims))
  combined_plot[[2]] = combined_plot[[2]] + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.x = element_blank() )
  
  combined_plot <- combined_plot +
    plot_layout(heights = c(5, 5, 2))
  
  return(combined_plot)
}


pretest_did <- function(out_var, did_df, control = "notyettreated"){
  pretest <- conditional_did_pretest(yname=out_var,
                                     gname="cert_start",
                                     idname="firm_id",
                                     tname="year",
                                     xformla= ~1,
                                     data=did_df,
                                     est_method="dr",
                                     print_details=FALSE,
                                     control_group = control,
                                     allow_unbalanced_panel = TRUE,
                                     panel = TRUE)
  return(pretest)
}


run_did <- function(out_var, did_data, control = "notyettreated", save_plot = FALSE){
  did_df <- did_data %>% 
    filter(!is.na(!!rlang::sym(out_var)))
  
  tally_summary <- tally_obs(did_df)
  
  did_mod <- att_gt(yname=out_var,
                    gname="cert_start",
                    idname="firm_id",
                    tname="year",
                    # xformla= ~1 + ln_rev,
                    xformla= ~1,
                    data=did_df,
                    est_method="dr",
                    print_details=FALSE,
                    control_group = control,
                    allow_unbalanced_panel = TRUE,
                    panel = TRUE,
                    # base_period = "universal",
                    # anticipation = 3
  )
  Wpval <- did_mod$Wpval
  agg_did <- aggte(did_mod, type = "dynamic", na.rm = TRUE)
  att <- as.character(round(agg_did$overall.att, digits = 3))
  att.se <- as.character(round(agg_did$overall.se, digits = 3))
  
  did_plot <- plot_did(out_var, agg_did)
  ymax <- layer_scales(did_plot)$y$range$range[2]
  # did_plot <- did_plot + 
  #   annotate("text", x =0.1, y = (ymax - 0.1), label = paste0("Overall ATT: ", att, ";\nSE: ", att.se), hjust = 0)
  
  trend_plot <- plot_trends(out_var, did_data)
  
  combined_plot <- align_plots(trend_plot, did_plot, tally_summary$heatmap, agg_did)
  
  if (save_plot==TRUE) {
    ggsave(filename = paste0(fig_dir, out_var, ".svg"),
           plot = combined_plot)
  }
  
  
  output <- list(agg_did = agg_did, 
                 summary = combined_plot)

  return(output)
  
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Explore missing data ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
df %>% 
  select(firm_id, year, cert_start, ln_rev, ln_ffb_price, ln_cpo_price, ln_workers, 
         ln_wage, ln_price_ratio, ln_tfp, ln_mkup, sh_share, out_ton_cpo_imp1, ln_va, ffb_price_imp1) %>% 
  miss_var_summary()



# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run DID analyses ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
df <- df %>%
  st_drop_geometry() %>% 
  left_join(current_mills %>% as_tibble(),
            by = "trase_code") %>% 
  as_tibble()
            
mod_df <- df %>% 
  filter(year>=2000)

var_list <- list("ln_va",
                 "ln_rev",
                 "ln_ffb_price",
                 "ln_cpo_premium",
                 "ln_wage",
                 "ln_tfp",
                 "ln_mkup")

for (out_var in var_list) {
  run_did(out_var, mod_df, save_plot = TRUE)
}

ip_df <- mod_df %>% filter(cert==0 | ever_ipsg==1)
mb_df <- mod_df %>% filter(cert==0 | ever_ipsg==0)
run_did("ln_cpo_premium", ip_df, save_plot = FALSE)
run_did("ln_cpo_premium", mb_df, save_plot = FALSE)



# Contrasting two possible designs
run_did("ln_wage", mod_df, control = "notyettreated", save_plot = FALSE)
run_did("ln_wage", mod_df %>% filter(cert_start!=0), control = "notyettreated", save_plot = FALSE)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Heterogeneity by local competition among mills ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
mkt_power_stable <- df %>% 
  group_by(trase_code) %>% 
  summarise(cap = mean(capacity),
            ncap = mean(cap_neighbors)) %>% 
  mutate(mkt_power_stable = cap / (cap + ncap)) %>% 
  select(trase_code, mkt_power_stable)

comp_df <- df %>%
  left_join(mkt_power_stable, by = "trase_code") %>% 
  mutate(compet_ntile = ntile(mkt_power_stable, 2)) 

mod_df <- comp_df %>% 
  filter(year>=2000,
         compet_ntile == 1)


run_did("ln_ffb_price", mod_df, save_plot = FALSE)


# Markets with lots of competing mills
mod_df <- comp_df %>% 
  filter(year>=2000,
         compet_ntile == 2)

run_did("ln_ffb_price", mod_df, save_plot = FALSE)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Heterogeneity by smallholder share ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mean_sm_shr <- mod_df$sh_share %>% mean(na.rm = TRUE)
run_did("ln_ffb_price", mod_df %>% filter(sh_share < mean_sm_shr), control = "nevertreated", save_plot = FALSE)
run_did("ln_ffb_price", mod_df %>% filter(sh_share > mean_sm_shr), control = "nevertreated", save_plot = FALSE)



# # %%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Productivity estimation ----- 
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# y = prod_df$ln_value_added
# fX = prod_df$ln_workers
# sX = prod_df$ln_fc
# pX = prod_df$ln_materials
# idvar = prod_df$firm_id
# timevar = prod_df$year
# prod_mod <- prodestWRDG(Y = y, fX = fX, sX = sX, pX = pX, idvar = idvar, timevar = timevar )
# summary(prod_mod)
