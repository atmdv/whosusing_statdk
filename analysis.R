pacman::p_load(XML, dplyr, ggplot2)

years_w_data <- c(2010:2017)

source("get_data.R")
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

private_companies <- c("445. DAMVAD Analytics",
                       "53. COWI A/S, Division 1",
                       "48. Epinion A/S",
                       "44. PLS RAMBØLL Management A/S",
                       "329. Incentive",
                       "162. Teknologisk Institut – Center for Analyse og Erhvervsfremme",
                       "318. Mploy A/S",
                       "224. Deloitte Consulting, Deloitte, Statsautoriseret Revisionspartnerselskab",
                       "480. Højbjerre Brauer Schultz",
                       "72. Copenhagen Economics",
                       "387. Moos-Bjerre & Lange ApS",
                       "243. Oxford Research A/S",
                       "293. McKinsey & Company",
                       "411. BDO Consulting",
                       "481. IRIS Group",
                       "488. Analyse & Tal",
                       "461. Marselisborg")

most_projects <- distinct_projects %>% 
  filter(Year>=2015) %>%
  filter(Organization %in% private_companies) %>% 
  group_by(Organization) %>% 
  summarise(Projects = n()) %>% 
  arrange(desc(Projects))

ggplot(data = most_projects,
       aes(x = reorder(Organization, Projects), y = Projects, fill="#97d8f2")) +
  geom_col() +
  scale_fill_manual(values ="#97d8f2") +
  labs(x="", title = "DAMVAD Analytics started more projects than any other private company in the past three years") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9)) +
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
  labs(x="", title = "DAMVAD Analytics typically starts 10-15 projects every year") +
  theme_minimal() +
  theme(legend.position = "none")
