
load("data/nw_df.RData")
library(tidyverse)
library(tidygraph)
library(ggraph)

set.seed(666)
nw_w_cm_comm <- nw_df %>% as_tbl_graph(directed = FALSE) %>%
  mutate(closeness = suppressWarnings(tidygraph::centrality_closeness()),
         betweenness = suppressWarnings(tidygraph::centrality_betweenness()),
         degree = suppressWarnings(tidygraph::centrality_degree()),
         eigen = suppressWarnings(tidygraph::centrality_eigen()),
         community = as.factor(group_louvain())) %>% 
  filter(name != "")

cm <- as_tibble(nw_w_cm_comm) # 6259
cm %>% arrange(desc(eigen))
cm %>% arrange(eigen)
cm %>% arrange(desc(degree))
cm %>% arrange(degree)
cm %>% arrange(desc(betweenness))
cm %>% arrange(betweenness)

cm4eda <- cm %>% select(closeness:community)
cm4eda %>% ggplot(aes(x = fct_rev(fct_infreq(community)))) +
  geom_bar() + labs(x = "Detected communities labeled as numbers", y = "Size of detected communities",
                    title = "Co-authorship network of political psychology scholars",
                    subtitle = "N = 6259 authors/nodes in the network") + 
  coord_flip() + theme_bw()

bp_btw <- cm4eda %>% ggplot(aes(y = betweenness, x = community)) + 
  geom_boxplot() + coord_flip() + theme_bw()
bp_cls <- cm4eda %>% ggplot(aes(y = closeness, x = community)) + 
  geom_boxplot() + labs(x = "") + 
  coord_flip() + theme_bw()
bp_deg <- cm4eda %>% ggplot(aes(y = degree, x = community)) + 
  geom_boxplot() + labs(x = "") + 
  coord_flip() + theme_bw()
bp_evc <- cm4eda %>% ggplot(aes(y = eigen, x = community)) + 
  geom_boxplot() + labs(x = "") + 
  coord_flip() + theme_bw()

library(patchwork)
pw1 <- bp_btw | bp_cls | bp_deg | bp_evc 
pw1 + plot_annotation(title = "Co-authorship network of political psychology scholars",
                      subtitle = "Boxplots of network centrality measures by detected community",
                      caption = "Chosen communities for further investigation are 1, 4, 5, 6, 11, 12, 15")

nw_comm_ds <- cm %>% group_by(community) %>%
  summarise(mean_bw = mean(betweenness),
            med_bw = median(betweenness),
            min_bw = min(betweenness),
            max_bw = max(betweenness),
            mean_de = mean(degree),
            med_de = median(degree),
            min_de = min(degree),
            max_de = max(degree),
            mean_ec = mean(eigen),
            med_ec = median(eigen),
            min_ec = min(eigen),
            max_ec = max(eigen)) %>%
  mutate_at(2:13, round, 2)

round(mean(nw_comm_ds$mean_de), 3) # 2.933
round(median(nw_comm_ds$mean_de), 3) # 2.935
round(mean(cm$degree), 3) # 3.165
round(median(cm$degree), 3) # 1
n_distinct(cm$degree) # 42

cm %>% janitor::tabyl(degree)
cm %>% ggplot(aes(degree)) + geom_histogram(bins = 200) + theme_bw()
cm %>% ggplot(aes(degree)) + geom_density() + theme_bw()
cm %>% ggplot(aes(betweenness)) + geom_histogram(bins = 200) + theme_bw()
cm %>% ggplot(aes(betweenness)) + geom_density() + theme_bw()

psych::describe(cm[,2:5])
skimr::skim(cm[,2:5])

library(EnvStats)
options(max.print = 5000)
rosnerTest(cm$degree, k = 600)$all.stats # 9 degrees are clearly outliers
options(max.print = 10000)
rosnerTest(cm$degree, k = 750)$all.stats # 5 degrees are outliers
rosnerTest(cm$degree, k = 1000)$all.stats # even 3 degrees are outliers
rosnerTest(cm$degree, k = 1500)$all.stats # 2 degrees are no outliers

cm %>% filter(community %in% c(1, 4, 5, 6, 11, 12, 15)) %>%
  select(community, degree, betweenness) %>%
  ggplot(aes(x = community, y = degree)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    justification = -.2, 
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .15, 
    outlier.color = NA
  ) +
  ggdist::stat_dots(
    side = "left", 
    justification = 1.1, 
    binwidth = .25
  ) + 
  coord_cartesian(xlim = c(1.2, NA)) + theme_bw()

set.seed(666)
nw_w_cm_comm %>% 
  ggraph(layout = "fr") + 
  geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) + 
  geom_node_point(aes(colour = community), size = .5) + 
  theme_graph() +
  labs(title = "Co-authorship network of political psychology scholars",
       subtitle = "N = 6259 authors/nodes in the network",
       caption = "Community detection with Louvain algorithm")

plot_nw_comm <- function(comm_num) {
  set.seed(666)
  cmm1 <- nw_w_cm_comm %>% filter(community == comm_num) %>%
    ggraph(layout = 'fr') + 
    geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) + 
    geom_node_point(aes(colour = community), size = .5) + 
    theme_graph() + theme(legend.position = "none") 
}

cmm1 <- plot_nw_comm(1) + labs(subtitle = "Community 1")
cmm4 <- plot_nw_comm(4) + labs(subtitle = "Community 4")
cmm5 <- plot_nw_comm(5) + labs(subtitle = "Community 5")
cmm6 <- plot_nw_comm(6) + labs(subtitle = "Community 6")
cmm11 <- plot_nw_comm(11) + labs(subtitle = "Community 11")
cmm12 <- plot_nw_comm(12) + labs(subtitle = "Community 12")
cmm15 <- plot_nw_comm(15) + labs(subtitle = "Community 15")

pw2 <- (cmm1 | cmm4 | cmm5 | cmm6) / (cmm11 | cmm12 | cmm15)
pw2 + plot_annotation(title = "Co-authorship network of political psychology scholars",
                      subtitle = "broken down by 7 selected communities")

