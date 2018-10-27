pacman::p_load(XML, dplyr, ggplot2)

years_w_data <- c(2010:2017)

data <- lapply(years_w_data, FUN=get_data)

data_all <- do.call("rbind", data)

# Fix organization names
data_all$Organization <-
  gsub("258. DAMVAD", "445. DAMVAD Analytics", data_all$Organization)
data_all$Organization <-
  gsub("62. Rockwool Fondens Forskningsenhed", "62. Rockwool Fonden", data_all$Organization)

# Who's had the most new projects i 2015-2017?
distinct_projects <- data_all %>% 
  group_by(Project_number) %>% 
  filter(Year==min(Year))

most_projects <- distinct_projects %>% 
  filter(Year>=2015) %>% 
  group_by(Organization) %>% 
  summarise(projects = n()) %>% 
  arrange(desc(projects))

# New projects in DAMVAD Analytics over the years
DAMVAD_Analytics <- distinct_projects %>% 
  filter(Organization == "445. DAMVAD Analytics") %>% 
  group_by(Year) %>% 
  summarise(projects = n())
