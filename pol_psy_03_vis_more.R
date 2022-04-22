
library(plotly)
library(tidyverse)
library(htmlwidgets)

df <- readxl::read_excel("data/commWnoatt.xlsx") %>% 
  select(degree, community:sex) %>% drop_na() %>%
  mutate(community = paste0("Community_", community))

cc_df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") %>%
  filter(COUNTRY %in% c(df$location)) %>% 
  select(COUNTRY, CODE) %>% 
  rename(location = COUNTRY)

df <- df %>% full_join(cc_df, by = "location")

summ_df <- function(comm_number) {
  summ_comm_df <- df %>%
    filter(community == comm_number) %>%
    group_by(CODE, location) %>%
    summarise(degree = sum(degree)) %>%
    rename(number_of_coauthors = degree) %>%
    ungroup()
  return(summ_comm_df)
}

plotly_output <- function(dat_fra) {
  comm_fig <- plot_ly(dat_fra, type = "choropleth",
                      locations = dat_fra$CODE,
                      z = dat_fra$number_of_coauthors,
                      text = dat_fra$location,
                      colorscale = "OrRd")
  return(comm_fig)
}

bp_summ <- function(comm_num) {
  df %>% filter(community == comm_num) %>%
    ggplot(aes(x = field)) +
    geom_bar(stat = "count", position = position_dodge()) +
    facet_grid(title ~ sex) + theme_bw() + theme(axis.text.x = element_text(angle = 45))
}

# community 1 -------------------------------------------------------------

comm_01 <- summ_df("Community_1")
comm_01_fig <- plotly_output(comm_01) %>% 
  layout(title = "Community 1 -- Transdisciplinary 1 | Color indicates the aggregated degree centrality in the country")
comm_01_fig
saveWidget(comm_01_fig, file = "comm_01.html", selfcontained = TRUE)
bp_summ("Community_1") + labs(title = "Community 1",
                              subtitle = "Top 25 co-authors summary",
                              x = "Discipline")

# community 4 -------------------------------------------------------------

comm_04 <-  summ_df("Community_4")
comm_04_fig <- plotly_output(comm_04) %>% 
  layout(title = "Community 4 -- Psychology Dominated | Color indicates the aggregated degree centrality in the country")
comm_04_fig
saveWidget(comm_04_fig, file = "html_maps/comm_04.html", selfcontained = TRUE)
bp_summ("Community_4") + labs(title = "Community 4",
                              subtitle = "Top 25 co-authors summary",
                              x = "Discipline")

# community 5 -------------------------------------------------------------

comm_05 <- summ_df("Community_5")
comm_05_fig <- plotly_output(comm_05) %>% 
  layout(title = "Community 5 -- Psychology Dominated | Color indicates the aggregated degree centrality in the country")
comm_05_fig
saveWidget(comm_05_fig, file = "html_maps/comm_05.html", selfcontained = TRUE)
bp_summ("Community_5") + labs(title = "Community 5",
                              subtitle = "Top 25 co-authors summary",
                              x = "Discipline")

# community 6 -------------------------------------------------------------

comm_06 <- summ_df("Community_6")
comm_06_fig <- plotly_output(comm_06) %>% 
  layout(title = "Community 6 -- Psychology Dominated | Color indicates the aggregated degree centrality in the country")
comm_06_fig
saveWidget(comm_06_fig, file = "html_maps/comm_06.html", selfcontained = TRUE)
bp_summ("Community_6") + labs(title = "Community 6",
                              subtitle = "Top 25 co-authors summary",
                              x = "Discipline")

# community 11 ------------------------------------------------------------

comm_11 <- summ_df("Community_11")
comm_11_fig <- plotly_output(comm_11) %>% 
  layout(title = "Community 11 -- Political Science Dominated | Color indicates the aggregated degree centrality in the country")
comm_11_fig
saveWidget(comm_11_fig, file = "html_maps/comm_11.html", selfcontained = TRUE)
bp_summ("Community_11") + labs(title = "Community 11",
                               subtitle = "Top 25 co-authors summary",
                               x = "Discipline")

# community 12 ------------------------------------------------------------

comm_12 <- summ_df("Community_12")
comm_12_fig <- plotly_output(comm_12) %>% 
  layout(title = "Community 12 -- Transdisciplinary 2 | Color indicates the aggregated degree centrality in the country")
comm_12_fig
saveWidget(comm_12_fig, file = "html_maps/comm_12.html", selfcontained = TRUE)
bp_summ("Community_12") + labs(title = "Community 12",
                               subtitle = "Top 25 co-authors summary",
                               x = "Discipline")

# community 15 ------------------------------------------------------------

comm_15 <- summ_df("Community_15")
comm_15_fig <- plotly_output(comm_15) %>% 
  layout(title = "Community 15 -- Psychology Dominated | Color indicates the aggregated degree centrality in the country")
comm_15_fig
saveWidget(comm_15_fig, file = "html_maps/comm_15.html", selfcontained = TRUE)
bp_summ("Community_15") + labs(title = "Community 15",
                               subtitle = "Top 25 co-authors summary",
                               x = "Discipline")

