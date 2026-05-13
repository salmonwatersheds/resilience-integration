###############################################################################
# Code to update and compile data on biological status, habitat status, and 
# climate change exposure from PSF's database.
###############################################################################


library(dplyr)

# Functions to connect to PSF database
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

###############################################################################
# Read in and transform existing data
###############################################################################

#------------------------------------------------------------------------------
# Biological status
#------------------------------------------------------------------------------

# We have probability of being in different biostatus categories
# 1 = good, 2 = fair, 3 = poor
# Calculate single score as 1 * prob(good) + 2 * prob(fair) + 3* prob(poor)

biostatus0 <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset101_output")

# Benchmarks
benchmarks0 <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset102_output")

# Filter to only include Fraser region
biostatus <- biostatus0 %>%
  filter(region == "Fraser")

benchmarks <- benchmarks0 %>%
  filter(region == "Fraser")

# Fill in Fraser CO that are "not-assessed" - not yet in database
ind_FrCONA <- which(biostatus$species_name == "Coho" & biostatus$psf_status == "not-assessed")

biostatus[ind_FrCONA, c("sr_red_prob", "sr_yellow_prob", "sr_green_prob")] <- rep(c(0, 0, 100), each = 5)
biostatus$psf_status_type[ind_FrCONA] <- "sr"
biostatus$psf_status[ind_FrCONA] <- "good"
biostatus$sr_status[ind_FrCONA] <- "good"

# Check Fraser CK that are "not-assessed"
ind_FrCKNA <- which(biostatus$species_name == "Chinook" & biostatus$psf_status == "not-assessed")
biostatus[ind_FrCKNA, c("cu_name_pse", "percentile_red_prob", "percentile_yellow_prob", "percentile_green_prob", "percentile_status")] # Not totally inconsistent with DFO; use for now

biostatus$psf_status_type[ind_FrCKNA] <- "percentile"
biostatus$psf_status[ind_FrCKNA] <- biostatus$percentile_status[ind_FrCKNA]

# Change probs to numeric
for(j in grep('prob', names(biostatus))){
  biostatus[, j] <- as.numeric(biostatus[, j])
}

# Look at psf_status_type == "absolute": do we want to use other benchmarks?
biostatus %>% filter(psf_status_type == "absolute") %>% 
  select(species_name, cuid, cu_name_pse, sr_status, percentile_status, psf_status)
# Can add in Boundary Bay (Fall 4-1) CK (cuid 302; CU_IN = CK-02); no longer DD
# South Thompson - Adams River Upper (CK-82, cuis 333); data deficient
# Two CUs (Taseko-ES and Widgeon are fair under percentiles...)
# These CUs have always been small; use percentile status

# Create new variable to hold changed status types for absolute
biostatus$resilience_status_type <- biostatus$psf_status_type
abs_cond <- which(biostatus$psf_status_type == "absolute")
biostatus$resilience_status_type[abs_cond] <- case_when(
  !is.na(biostatus$sr_status[abs_cond]) & biostatus$psf_status[abs_cond] != "data-deficient" ~ "sr",
  is.na(biostatus$sr_status[abs_cond]) & !is.na(biostatus$percentile_status[abs_cond]) & biostatus$psf_status[abs_cond] != "data-deficient" ~ "percentile",
  biostatus$psf_status[abs_cond] == "data-deficient" ~ NA)

biostatus %>% filter(psf_status_type == "absolute") %>% 
  select(species_name, cuid, cu_name_pse, sr_status, percentile_status, psf_status, resilience_status_type)

# Create resilience status outcomes that may differ
biostatus$resilience_status <- case_when(
  biostatus$psf_status != "data-deficient" & biostatus$resilience_status_type == "sr" ~ biostatus$sr_status,
  biostatus$psf_status != "data-deficient" & biostatus$resilience_status_type == "percentile" ~ biostatus$percentile_status,
  biostatus$psf_status == "data-deficient" ~ "data-deficient"
)

# Create common variable for probabilities regardless of psf_status_type
biostatus <- biostatus %>%
  left_join(benchmarks %>% select(cuid, curr_spw, curr_spw_end_year)) %>%
  mutate(
    red_prob = case_when(
      resilience_status_type == "sr" ~ sr_red_prob/100,
      resilience_status_type == "percentile" ~ percentile_red_prob/100),#,
      # psf_status_type == "absolute" ~ 1 - 0.5 * curr_spw/1500),
    yellow_prob = case_when(
      resilience_status_type == "sr" ~ sr_yellow_prob/100,
      resilience_status_type == "percentile" ~ percentile_yellow_prob/100),#,
      # psf_status_type == "absolute" ~ 0.5 * curr_spw/1500),
    green_prob = case_when(
      resilience_status_type == "sr" ~ sr_green_prob/100,
      resilience_status_type == "percentile" ~ percentile_green_prob/100)#,
      # psf_status_type == "absolute" ~ 0)
  ) %>%
  # Create score to put biostatus outcomes on a continuum based on probabilities
  mutate(pop_score = 1 * green_prob + 2 * yellow_prob + 3 * red_prob)
  
# Which CUs don't have a score? -> Extinct or data deficient. OK.
biostatus %>% filter(is.na(pop_score)) %>% select(species_name, cu_name_pse, psf_status)

sum(!is.na(biostatus$pop_score))

# What year's are status current to?
biostatus %>% filter(!is.na(pop_score)) %>% summarise(unique(curr_spw_end_year))

#------------------------------------------------------------------------------
# Habitat status
#------------------------------------------------------------------------------

# Download Habitat Assessment Threat Summaries for Salmon and Steelhead Conservation Units
# https://data.salmonwatersheds.ca/result?datasetid=556

habstatus <- read.csv("data/dataset556_cu_habitat_threats.csv") %>%
  filter(region == "Fraser") %>%
  select(cuid, total_spawning_weight, cumulative_lopct, cumulative_modpct, cumulative_hipct, cumulative_defpct) %>%
  # Create score to put single number to habstatus
  mutate(hab_score = 1 * cumulative_lopct/100 + 2 * cumulative_modpct/100 + 3 * cumulative_hipct/100)

dim(habstatus) #67 CUs

#------------------------------------------------------------------------------
# Climate change exposure
#------------------------------------------------------------------------------

exposure <- read.csv("data/exposure_overall.csv") %>%
  filter(rcp == "rcp45", period == "mid") %>%
  group_by(cuid) %>%
  summarise(clim_score = median(exp_simple),
            clim_score_min = min(exp_simple),
            clim_score_max = max(exp_simple))

# Adams & Momich ES sockeye have been split into Adams ES and Momich ES
# Adams is 'extinct' and Momich is 'data deficient'...so moot point.

###############################################################################
# Bring data together and save
###############################################################################

data <- biostatus %>% 
  filter(!is.na(pop_score)) %>% 
  select(species_name, cuid, cu_name_pse, resilience_status, resilience_status_type, pop_score) %>%
  dplyr::rename("biostatus" = "resilience_status",
                "biostatus_type" = "resilience_status_type") %>%
  left_join(habstatus %>% select(cuid, hab_score)) %>%
  left_join(exposure %>% select(cuid, clim_score))

# write to csv
write.csv(data, file = "output/CU_pophabclim_scores.csv", row.names = FALSE)

