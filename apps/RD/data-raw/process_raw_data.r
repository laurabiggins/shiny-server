library("tidyverse")
#tt_data <- read_csv("data/tt.csv")
tt_data <- read_csv("data-raw/top trumps - Sheet1.csv")

n_names <- nrow(tt_data)

tt_processed <- tt_data %>%
  gather(-Name, key = "characteristic", value = "value") %>%
  group_by(characteristic) %>%
  mutate(pos = rank(value, ties.method = "max")) %>%
  mutate(percentage_rank = (pos/n_names)*100) %>%
  ungroup

#save(tt_processed, file = "data/tt_processed.rds")
saveRDS(tt_processed, file = "data/tt_processed.rds")
#usethis::use_data(tt_data)
#usethis::use_data(tt_processed) # this only works for packages


