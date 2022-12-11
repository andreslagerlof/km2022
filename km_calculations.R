
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(openxlsx)

# Load data and functions -------------------------------------------------

# load tibbles
load(here("assets", "data", "sm_uttagning.Rdata"))
load(here("assets", "data", "res_km_4.RData"))


# function for ordinary competitions
calculate_points_sv <- function(df, plats) {
  tot <- df %>% 
    mutate(points = case_when(
      plats == 1 ~ 30,
      plats == 2 ~ 26,
      plats == 3 ~ 22,
      plats <= 8 ~ 18,
      plats <= 17 ~ 14,
      plats <= 32 ~ 10,
      plats <= 64 ~ 6,
      plats <= 96 ~ 2,
      plats <= 128 ~ 1,
      TRUE ~ 0
    )) 
}


# Prep data ---------------------------------------------------------------

# Modify res
res_km_mod <- res |> 
  rename(no = rank, 
         kon = gender) |> 
  mutate(
    comp_name = "km",
    comp_type = "sv"
  ) |> 
  select(no:name, comp_name, comp_type) 

# Export to xlsx
write.xlsx(res_km_mod, "res_km.xlsx")

# I had to split name into f_name, l_name in xlsx

# Import the data
res_km <- read.xlsx("res_km.xlsx")

res_km <- res_km |> 
  rename(plats = no)

tot <- calculate_points_sv(res_km)

tot_mod <- tot |> 
  mutate(l_name = toupper(l_name)) |> 
  unite("namn", l_name:f_name, sep = " ") |> 
  select(-name, -plats) |> 
  mutate(kon = toupper(kon))

# Modify fff_points_tot
fff_points <- fff_points_tot |> 
  select(-no, -plats, -poule) |> 
  relocate(kon, namn, comp_name, comp_type, points)

# Check variables
names(fff_points)    
names(tot_mod)


new_tot <- bind_rows(fff_points, tot_mod)



# Finalize calcultions ----------------------------------------------------

# calculate grand total points for all competitions 
grand_tot <- new_tot %>% 
  group_by(namn, kon) %>%
  summarise(sum_points = sum(points, na.rm = TRUE)) %>%
  arrange(desc(sum_points))

## split datasets to male and female datasets

# female
grand_tot_f <- grand_tot %>% 
  filter(kon == "F")

# male
grand_tot_m <- grand_tot %>% 
  filter(kon == "M")

## Create female dataset
piv_comp_f <- fff_points_tot %>%
  filter(kon == "F") %>%
  select(namn, comp_name, points) %>% 
  pivot_wider(names_from = comp_name, values_from = points) 

# Create new piv df with totals column
res_f <- piv_comp_f %>%
  full_join(grand_tot_f, by = "namn") %>% 
  arrange(desc(sum_points))

# Add ranking
res_f <- res_f %>% 
  mutate(rank = min_rank(desc(sum_points))) %>%
  relocate(rank, everything()) %>% 
  select(-kon)

# Change column order
res_f <- res_f %>% 
  relocate(bernadotte, .after = abo)

# Capitalize first letter in column names
res_f <- res_f %>% 
  rename_with(str_to_title)

# Rename columns for final table Note: change for each competition!!
standings_f <- res_f %>% 
  rename("#" = "Rank", "Namn" = "Namn", "Åbo" = "Abo",
         "Summa Poäng" = "Sum_points")

## Create male dataset
piv_comp_m <- fff_points_tot %>%
  filter(kon == "M") %>%
  select(namn, comp_name, points) %>% 
  pivot_wider(names_from = comp_name, values_from = points) 

# Create new piv df with totals column
res_m <- piv_comp_m %>%
  full_join(grand_tot_m, by = "namn") %>% 
  arrange(desc(sum_points))

# Add ranking
res_m <- res_m %>% 
  mutate(rank = min_rank(desc(sum_points))) %>%
  relocate(rank, everything()) %>% 
  select(-kon)

# Change column order
res_m <- res_m %>% 
  relocate(bernadotte, .after = abo)

# Capitalize first letter in column names
res_m <- res_m %>% 
  rename_with(str_to_title)

# Rename columns for final table Note: change for each competition!!
standings_m <- res_m %>% 
  rename("#" = "Rank", "Namn" = "Namn", "Åbo" = "Abo",
         "Summa Poäng" = "Sum_points")

