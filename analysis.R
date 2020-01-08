pacman::p_load(XML, dplyr, ggplot2, readr)

years_w_data <- c(2010:2018)

source("get_data.R")
data <- lapply(years_w_data, FUN=get_data)

data_all <- do.call("rbind", data)

data_all$Organization <-  recode(data_all$Organization,
         "258. DAMVAD" = "445. DAMVAD Analytics",
         "62. Rockwool Fondens Forskningsenhed" = "62. Rockwool Fonden",
         "475. IMS Health, Real World Evidence Solutions Denmark" = "475. IMS Health Technology Solutions Denmark A/S",
         "387. Moos-Bjerre Analyse" = "387. Moos-Bjerre & Lange ApS")

# Who's had the most new projects i 2015-2017?
distinct_projects <- data_all %>% 
  group_by(Project_number) %>% 
  filter(Year==min(Year))

source("private_companies.R")

most_projects <- distinct_projects %>% 
  filter(Year>=2016) %>%
  filter(Organization %in% private_companies) %>% 
  group_by(Organization) %>% 
  summarise(Projects = n()) %>% 
  arrange(desc(Projects))

# write_csv(most_projects, "projects_private_companies.csv")

ggplot(data = most_projects,
       aes(x = reorder(Organization, Projects), y = Projects, fill="#97d8f2")) +
  geom_col() +
  scale_fill_manual(values ="#97d8f2") +
  labs(x="", title = "DAMVAD Analytics started more projects than any other private company in the past three years") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.9)) +
  # scale_y_discrete(expand=expand_scale(mult=c(0,0.1))) +
  theme(legend.position = "none")

# New projects in DAMVAD Analytics over the years
DAMVAD_Analytics <- distinct_projects %>% 
  filter(Organization == "445. DAMVAD Analytics") %>% 
  group_by(Year) %>% 
  summarise(Projects = n())

ggplot(data = DAMVAD_Analytics,
       aes(x = Year, y = Projects, fill="#97d8f2")) +
  geom_col() +
  scale_fill_manual(values ="#97d8f2") +
  labs(x="", title = "DAMVAD Analytics typically has 10-15 projects every year") +
  theme_minimal() +
  theme(legend.position = "none")
