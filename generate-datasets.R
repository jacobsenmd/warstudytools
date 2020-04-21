library(tidyverse)
library(countrycode)

# Parameters -----------------------------------------------------------------------

# This specifies the number of peaceful years that need to pass in a conflict
# before we consider it a break between separate episodes
peace_years_for_new_episode<-3

# Initialization ----------------------------------------------------------

# Create/recreate directory structure
unlink("data", recursive=T)
unlink("out", recursive=T)
dir.create("data", showWarnings = FALSE)
dir.create("out", showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)

# Download source datasets

# UCDP PRIO Armed Conflict dataset
download.file("http://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-191.Rdata.zip", "tmp/ucdp-prio-acd-191.Rdata.zip")

# Correlates of War Direct Contiguity v3.2 dataset
download.file("https://correlatesofwar.org/data-sets/direct-contiguity/direct-contiguity-v3-2/at_download/file", "tmp/DirectContiguity320.zip")

# Extract the PRIO .RDS file from the .ZIP file and save its contacts to a tibble
acd<-unz("tmp/ucdp-prio-acd-191.Rdata.zip", filename = "UcdpPrioConflict_v19_1.rds") %>% 
  gzcon() %>% 
  readRDS() %>%
  as_tibble()


# Extract and load the COW Direct Contiguity dataset
# We only care about years contained within the ACD
contdird<-read_csv(unz("tmp/DirectContiguity320.zip", filename="DirectContiguity320/contdird.csv")) %>%
  filter(year >= min(acd$year) & year <= max(acd$year))

# Create a contiguity table in a format that can be joined to the country.years dataset
contig.states<-contdird %>%
  rename(cowc=state1ab) %>%
  group_by(year, cowc) %>% 
  summarise(contig_states = paste0(unique(state2ab), collapse=","),
            contig_land_count = sum(conttype == 1),
            contig_sea_count = sum(conttype != 1),
            contig_total_count = length(unique(state2ab)))


# Return the number of armed groups to appear in a new year that weren't present the previous year
# where old_year and new_year are comma-delimited strings as used by ACD
# i.e. if old_year="299, 300, 292" and new_year="299, 300, 302" then it returns 1 (302 is new)
count_new_groups<-function(old_year, new_year) {
  old_year_vec<-strsplit(old_year,", ")[[1]]
  new_year_vec<-strsplit(new_year, ", ")[[1]]
  s<-setdiff(new_year_vec, old_year_vec)
  return(length(s))
}

# Find the set of all items in a vector of comma-delimited item
# i.e. observation1 = "100, 200, 300" and observation = "100, 301, 302"
# then it returns "100, 200, 300, 301, 302"
unique_values_in_comma_sep_vectors<-function(arg) {
  combined_vector<-unlist(strsplit(arg, split=", "))
  unique_vector<-unique(combined_vector)
  return (paste(unique_vector,collapse=", "))
}

# Generate the episode.years dataset --------------------------------------

episode.years<-acd %>%
  
  mutate(side_a_id = as.character(side_a_id)) %>%
  mutate(side_b_id = as.character(side_b_id)) %>%
  mutate(side_a = as.character(side_a)) %>%
  mutate(side_b = as.character(side_b)) %>%
  mutate(side_a_2nd = as.character(side_a_2nd)) %>%
  mutate(side_b_2nd = as.character(side_b_2nd)) %>%
  mutate(location = as.character(location)) %>%
  mutate(gwno_loc = as.character(gwno_loc)) %>%
  
  # Calculate the previous years of peace before each conflict-year, then use this information to identify new
  # episodes and assign incremental episode numbers and unique episode ids within a conflict.
  group_by(conflict_id) %>%
  mutate(peace_years = year - lag(year) - 1) %>%
  mutate(new_episode = replace_na(as.numeric(peace_years >= peace_years_for_new_episode), 0)) %>%
  mutate(episode_num = cumsum(new_episode[!is.na(new_episode)]) + 1) %>%
  mutate(episode_id = paste(conflict_id, "-", episode_num, sep="")) %>%
  mutate(conflict_year = year - min(year) + 1) %>%
  mutate(conflict_start_year = min(year)) %>%
  mutate(conflict_stop_year = max(year))%>%

# Add columns for actor counts by using the number of commas in each side id list
# Ex. "" -> 0
# Ex. "Government of Mali" -> 1
# Ex. "Government of Australia, Government of United Kingdom" -> 2
  mutate(side_a_count = ifelse(side_a_id=="", 0, str_count(side_a_id, ",") + 1)) %>%
  mutate(side_b_count = ifelse(side_b_id=="", 0, str_count(side_b_id, ",") + 1)) %>%
  mutate(side_a_2nd_count = ifelse(side_a_2nd=="", 0, str_count(side_a_2nd, ",") + 1)) %>%
  mutate(side_b_2nd_count = ifelse(side_b_2nd=="", 0, str_count(side_b_2nd, ",") + 1)) %>%
  
  # Count new groups that appear this year that weren't in the same conflict last year
  # We use mapply to call the count_new_groups function for each year in a given conflict
  mutate(side_a_new = mapply(count_new_groups, lag(side_a_id), side_a_id)) %>%
  mutate(side_b_new = mapply(count_new_groups, lag(side_b_id), side_b_id)) %>%
  ungroup() %>%
  
  # Add dummy variables based on region
  mutate(europe = str_detect(region, "1")) %>%
  mutate(mideast = str_detect(region, "2")) %>%
  mutate(asia = str_detect(region, "3")) %>%
  mutate(africa = str_detect(region, "4")) %>%
  mutate(americas = str_detect(region, "5")) %>%
  
  # Add a centerseeking indicator for wars fought for central governance
  mutate(centerseeking = incompatibility >= 2) %>%
  
  # Indicate wars with external support on both sides
  mutate(counter.intervention = (side_a_2nd_count > 0 & side_b_2nd_count > 0))

# Group by episode to compute some episode-level variables
episode.years<-episode.years %>%
  group_by(episode_id) %>%
  mutate(episode__year = year - min(year) + 1) %>%
  mutate(episode_start_year = min(year)) %>%
  mutate(episode_stop_year = max(year)) %>%
  ungroup()

# For actor lists, replace commas inside parentheses with semicolors. Allowing commas inside parentheses makes it
# impossible to split actors by comma.
# This regex uses character matching and backreferences
# We use ^\\) to catch situations like this: (A, B), (C, D) where the two sets of parentheses capture the
# middle comma
episode.years$side_a<-sub("(\\([^\\)]*),(.*\\))","\\1;\\2", episode.years$side_a, fixed=FALSE)
episode.years$side_b<-sub("(\\([^\\)]*),(.*\\))","\\1;\\2", episode.years$side_b, fixed=FALSE)
episode.years$side_a_2nd<-sub("(\\([^\\)]*),(.*\\))","\\1;\\2", episode.years$side_a_2nd, fixed=FALSE)
episode.years$side_b_2nd<-sub("(\\([^\\)]*),(.*\\))","\\1;\\2", episode.years$side_b_2nd, fixed=FALSE)

# Uncomment to see the lines where we made substitutions in the code above
# episode.years %>% filter(grepl(";", side_b)) %>% select(side_b) %>% View()


# Generate the episodes dataset -------------------------------------------

episodes<-episode.years %>%
  group_by(episode_id) %>%
  summarise( conflict_id = conflict_id[1],
             episode_num = episode_num[1],
             startyear = min(year), 
             stopyear = max(year),
             africa = africa[1],
             americas = americas[1],
             asia = asia[1],
             europe = europe[1],
             mideast = mideast[1],
             incompatibility = incompatibility[1],
             type_of_conflict = type_of_conflict[1],
             territory_name = territory_name[1],
             max_intensity_level = max(intensity_level),
             conflict_duration = (conflict_stop_year[1] - conflict_start_year[1] + 1),
             episode_duration = (episode_stop_year[1] - episode_start_year[1] + 1),
             side_a = unique_values_in_comma_sep_vectors(side_a),
             side_b = unique_values_in_comma_sep_vectors(side_b),
             side_a_id = unique_values_in_comma_sep_vectors(side_a_id),
             side_b_id = unique_values_in_comma_sep_vectors(side_b_id),
             side_a_2nd = unique_values_in_comma_sep_vectors(side_a_2nd),
             side_b_2nd = unique_values_in_comma_sep_vectors(side_b_2nd),
             location = unique_values_in_comma_sep_vectors(location),
             side_a_count = ifelse(side_a_id=="", 0, str_count(side_a_id, ",") + 1),
             side_b_count = ifelse(side_b_id=="", 0, str_count(side_b_id, ",") + 1),
             side_a_2nd_count = ifelse(side_a_2nd=="", 0, str_count(side_a_2nd, ",") + 1),
             side_b_2nd_count = ifelse(side_b_2nd=="", 0, str_count(side_b_2nd, ",") + 1)
  )

# Generate the conflicts dataset ------------------------------------------

conflicts<-episode.years %>%
  group_by(conflict_id) %>%
  summarise( startyear=min(year), 
             stopyear=max(year),
             africa=africa[1],
             americas=americas[1],
             asia=asia[1],
             europe=europe[1],
             mideast=mideast[1],
             incompatibility=incompatibility[1],
             type_of_conflict=type_of_conflict[1],
             territory_name=territory_name[1],
             max_intensity_level=max(intensity_level),
             conflict_duration = (conflict_stop_year[1] - conflict_start_year[1] + 1),
             num_episodes = max(episode_num),
             side_a = unique_values_in_comma_sep_vectors(side_a),
             side_b = unique_values_in_comma_sep_vectors(side_b),
             side_a_id = unique_values_in_comma_sep_vectors(side_a_id),
             side_b_id = unique_values_in_comma_sep_vectors(side_b_id),
             side_a_2nd = unique_values_in_comma_sep_vectors(side_a_2nd),
             side_b_2nd = unique_values_in_comma_sep_vectors(side_b_2nd),
             location = unique_values_in_comma_sep_vectors(location),
             side_a_count = ifelse(side_a_id=="", 0, str_count(side_a_id, ",") + 1),
             side_b_count = ifelse(side_b_id=="", 0, str_count(side_b_id, ",") + 1),
             side_a_2nd_count = ifelse(side_a_2nd=="", 0, str_count(side_a_2nd, ",") + 1),
             side_b_2nd_count = ifelse(side_b_2nd=="", 0, str_count(side_b_2nd, ",") + 1)
  )

# 

# Generate country.years dataset ------------------------------------------

# Generate a unique list of Gleditsch country codes with NA values removed
unique.countries<-sort(unique(codelist$gwn)[!is.na(unique(codelist$gwn))])

# Create a tibble with an observation for every gwn country for every year covered by the ACD
country.years<-expand_grid(gwn=unique.countries, year=min(episode.years$year):max(episode.years$year)) %>%
  
  # Add a Correlates of War country code
  mutate(cowc = countrycode(gwn, "gwn", "cowc")) %>%
  left_join(contig.states, by = c("cowc", "year")
)


# TODO handle cowc NAs


# TODO sum interstate participation 
# TODO sum intrastate participation
# TODO sum secondary participation
# TODO record number of contiguous states
# TODO record code of contiguous states
# TODO record centroid lat-lon



# Output datasets ---------------------------------------------------------
rm(acd, contdir)
unlink("tmp", recursive=T)
saveRDS(episode.years, "data/episode_years.rds")
saveRDS(episodes, "data/episodes.rds")
saveRDS(conflicts, "data/conflicts.rds")