# This function takes a dataframe with a variable called "place" and calculates points

calculate_points <- function(df, place) {
  tot <- df %>% 
    mutate(points = case_when(
      place == 1 ~ 25,
      place == 2 ~ 22,
      place == 3 ~ 19,
      place == 4 ~ 17,
      place == 5 ~ 15,
      place == 6 ~ 13,
      place == 7 ~ 12,
      place == 8 ~ 11,
      place == 9 ~ 10,
      place == 10 ~ 9,
      place == 11 ~ 8,
      place == 12 ~ 7,
      place == 13 ~ 6,
      place == 14 ~ 5,
      place == 15 ~ 4,
      place == 16 ~ 3,
      place == 17 ~ 2,
      between(place, 18, 40) ~ 1
    ))
}
