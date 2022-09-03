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
# - Develop metric of profitability / TFP
# - Worker salaries?
# - Look for heterogeneity based on local competition
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
df <- read.csv(paste0(wdir, "ibs\\ibs_matched_rspo_ci_year.csv"))
stata_df <- read_dta(paste0(wdir, "ibs\\ucsb_ibs_tfp.dta")) %>% 
  select(firm_id, year, tfp, mkup)
df <- df %>% 
  left_join(stata_df, by = c("firm_id", "year"))


df <- df %>% 
  as_tibble()  %>%
  select(-X) %>% 
  filter(!is.na(uml_id))

df <- st_as_sf(x = df,                         
               coords = c("lon", "lat"),
               crs = 4326)


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
  mutate(cap_neighbors = cap_neighbors - capacity)

current_mills <- current_mills %>% 
  mutate(compet_ntile = ntile(n_neighbors, 2))


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


align_plots <- function(did_plot, tally_heatmap, agg_did){
  xlims <- layer_scales(did_plot)$x$range$range
  xlims[1] = xlims[1] - 1
  xlims[2] = xlims[2] + 1
  combined_plot <- (did_plot + xlim(xlims)) / (tally_heatmap + xlim(xlims))
  combined_plot[[1]] = combined_plot[[1]] + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.x = element_blank() )
  
  combined_plot <- combined_plot +
    plot_layout(heights = c(5, 2))
  
  return(combined_plot)
}


run_did <- function(out_var, did_data, control = "notyettreated", save_plot = FALSE){
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
                    print_details=FALSE,
                    control_group = "notyettreated",
                    panel = FALSE
  )
  Wpval <- did_mod$Wpval
  agg_did <- aggte(did_mod, type = "dynamic", na.rm = TRUE)
  att <- as.character(round(agg_did$overall.att, digits = 3))
  att.se <- as.character(round(agg_did$overall.se, digits = 3))
  
  did_plot <- plot_did(out_var, agg_did)
  ymax <- layer_scales(did_plot)$y$range$range[2]
  did_plot <- did_plot + 
    annotate("text", x =0.1, y = (ymax - 0.1), label = paste0("Overall ATT: ", att, ";\nSE: ", att.se), hjust = 0)
  
  combined_plot <- align_plots(did_plot, tally_summary$heatmap, agg_did)
  
  if (save_plot==TRUE) {
    ggsave(filename = paste0(fig_dir, out_var, ".svg"),
           plot = combined_plot)
  }
  
  
  output <- list(agg_did = agg_did, 
                 summary = combined_plot)

  return(output)
  
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run DID analyses ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
df <- df %>% 
  left_join(current_mills %>% as_tibble(),
            by = "trase_code")
            
mod_df <- df %>% 
  filter(year>=2000)

var_list <- list("ln_rev",
                 "ln_ffb_price",
                 "ln_cpo_price",
                 "ln_pko_price",
                 "ln_workers",
                 "ln_cpo_export_shr",
                 "ln_wage")

for (out_var in var_list) {
  run_did(out_var, mod_df, save_plot = TRUE)
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Heterogeneity by local competition among mills ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Markets with few competing mills
mod_df <- df %>% 
  filter(year>=2000,
         compet_ntile == 1)

run_did("ln_ffb_price", mod_df, save_plot = FALSE)


# Markets with lots of competing mills
mod_df <- df %>% 
  filter(year>=2000,
         compet_ntile == 2)

run_did("ln_ffb_price", mod_df, save_plot = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Output to stata ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
run_did("tfp", df, save_plot = TRUE)
run_did("mkup", df, save_plot = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Productivity estimation ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%

y = prod_df$ln_value_added
fX = prod_df$ln_workers
sX = prod_df$ln_fc
pX = prod_df$ln_materials
idvar = prod_df$firm_id
timevar = prod_df$year
prod_mod <- prodestWRDG(Y = y, fX = fX, sX = sX, pX = pX, idvar = idvar, timevar = timevar )
summary(prod_mod)
