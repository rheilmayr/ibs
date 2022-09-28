#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr (rheilmayr@ucsb.edu)
# Project name: RSPO_productivity
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Imports ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
library(fixest)
library(aws.s3)
library(tidyverse)
library(lubridate)
library(janitor)
library(forcats)
library(anomalize) # Need to add anomaly detection
library(tibbletime)
library(stringr)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# AWS credentials ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
bol <- read_delim(get_object(object = "indonesia/trade/bol/out/BOL_INDONESIA_COMPILED_2013_2020.csv", bucket), delim = ";")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean data ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
bol <- bol %>% 
  clean_names()

bol <- bol %>%
  mutate(
    date = ymd(date),
    month_yr = format_ISO8601(date, precision = "ym"),
    year = format_ISO8601(date, precision = "y")
  )


bol <- bol %>% 
  mutate(unit_price = fob / kg_net * 1000) # Convert to price per tonne

bol <- bol %>% 
  mutate(any_rspo = certification %in% c("RSPO - MB", "RSPO - OTHER", "RSPO - SG"),
         certification = fct_relevel(certification, 
                                     "NOT CERTIFIED", "ISCC", "RSPO - MB", "RSPO - OTHER", "RSPO - SG"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Remove anomalies -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
## NOTE: In 2013-2016, data is all reported on the first day of each month. In 2017-2021 temporal resolution improves to daily.
## NOTE: 2017-2018 have lots of unit_price outliers randomly distributed above true values; 2019 has lots of outliers randomly distributed below true values

id_anomalies = function(bol_df){
  variation <- bol_df %>% 
    group_by(month_yr) %>% 
    summarize(mean = mean(unit_price),
              sd = sd(unit_price)) %>% 
    mutate(year = as.integer(str_sub(month_yr, 1, 4)))
  
  full_sd <- variation %>% 
    filter(! year %in% c(2017, 2018, 2019)) %>% 
    pull(sd) %>% 
    mean()
  
  variation <- variation %>% 
    select(month_yr, mean) %>% 
    mutate(upper = mean + (2 * full_sd),
           lower = mean - (2 * full_sd))
  
  bol_df <- bol_df %>% 
    left_join(variation, by = "month_yr") 
  
  
  bol_df <- bol_df %>% 
    mutate(anomaly = case_when(
      (year %in% c(2013, 2014, 2015, 2016, 2020)) & ((unit_price > mean + 3 * full_sd) | (unit_price < mean - 3 * full_sd)) ~ "anomaly",
      (year %in% c(2017, 2018)) & ((unit_price > mean + 1 * full_sd) | (unit_price < mean - 5 * full_sd)) ~ "anomaly",
      (year %in% c(2019)) & ((unit_price > mean + 5 * full_sd) | (unit_price < mean - 1 * full_sd)) ~ "anomaly",
      TRUE ~ "ready to use"))
  
  # bol_df <- bol_df %>% 
  #   filter(anomaly == "ready to use")
  
  return(bol_df)
}


## Filter anomalies in cPO data
cpo_bol <- bol %>% 
  filter(type == "CPO",
         unit_price < 2000)

cpo_bol <- cpo_bol %>% 
  id_anomalies()

cpo_bol %>% 
  ggplot(aes(x = date, y = unit_price, group = anomaly, color = anomaly)) +
  geom_point()

cpo_bol <- cpo_bol %>% 
  filter(anomaly == "ready to use")


## Filter anomalies in RPO data
rpo_bol <- bol %>% 
  filter(type == "RPO",
         unit_price < 2000)

rpo_bol <- rpo_bol %>% 
  id_anomalies()

rpo_bol %>% 
  ggplot(aes(x = date, y = unit_price, group = anomaly, color = anomaly)) +
  geom_point()


rpo_bol <- rpo_bol %>% 
  filter(anomaly == "ready to use")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Tables and plots of price trends ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
cpo_price_table <- cpo_bol %>% 
  filter(!certification %in% c("RSPO - OTHER", "ISCC")) %>% 
  group_by(month_yr, certification) %>% 
  summarize(volume = sum(kg_net),
            fob = sum(fob)) %>% 
  mutate(unit_price = fob / volume) %>% 
  select(month_yr, certification, unit_price)

cpo_price_table %>% 
  ggplot(aes(x = month_yr, y = unit_price, group = certification, color = certification)) +
  geom_point()

cpo_price_table_wide <- cpo_price_table %>% 
  pivot_wider(names_from = any_rspo, values_from = unit_price)



rpo_price_table <- rpo_bol %>% 
  filter(!certification %in% c("RSPO - OTHER", "ISCC")) %>% 
  group_by(month_yr, certification) %>% 
  summarize(volume = sum(kg_net),
            fob = sum(fob)) %>% 
  mutate(unit_price = fob / volume) %>% 
  select(month_yr, certification, unit_price)

rpo_price_table %>% 
  ggplot(aes(x = month_yr, y = unit_price, group = certification, color = certification)) +
  geom_line()

rpo_price_table_wide <- rpo_price_table %>% 
  pivot_wider(names_from = certification, values_from = unit_price)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%
# Premium regression ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%
test_premium <- function(mod_df) {
  mod_df <- mod_df %>% 
    mutate(port_by_date = paste0(port_loading, month_yr))
  mod <- feols(unit_price ~ factor(certification) | port_by_date + exporter_id, weights = mod_df$kg_net, data = mod_df)
  # mod <- feols(unit_price ~ factor(certification) | month_yr + port_loading + exporter_id, weights = mod_df$kg_net, data = mod_df)
  print(summary(mod))
  
  proportions <- mod_df %>% 
    group_by(certification) %>% 
    summarise(vol = sum(kg_net)) %>% 
    mutate(share = prop.table(vol)) %>% 
    print()
  
  ref_price <- mod_df %>% 
    filter(any_rspo == FALSE) %>% 
    pull(unit_price) %>% 
    mean(na.rm = TRUE)
  
  ref_price = round(ref_price)
  
  print(paste0("Average price for reference: ", as.character(ref_price)))
  
  return(mod)
}


cpo_mod <- cpo_bol %>% 
  filter(!certification %in% c("RSPO - OTHER", "ISCC")) %>% 
  test_premium()


rpo_mod <- rpo_bol %>% 
  filter(!certification %in% c("RSPO - OTHER", "ISCC")) %>% 
  test_premium()
