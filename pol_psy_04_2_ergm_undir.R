
load("data/nw_df.RData")
library(tidyverse)
library(tidygraph)
library(ggraph)
library(network)
library(ergm)
library(ergm.userterms)

nw_obj <- nw_df %>% as_tbl_graph(directed = FALSE)
nw_edges <- nw_df
nw_names <- as_tibble(nw_obj) %>% select(name)
nw_nodes <- nw_names

## undirected
nw_ud <- as.network(nw_df,
                    vertices = nw_nodes,
                    directed = FALSE, multiple = TRUE)


# edges + triangles -------------------------------------------------------

summary(nw_ud ~ edges + triangles)
# edges triangle 
# 8943     2594 
set.seed(666)
mod_edges_triangles <- ergm(nw_ud ~ edges
                            + triangles,
                            control = control.ergm(
                              parallel = 6,
                              parallel.type = "PSOCK",
                              MCMLE.maxit = 15,
                              MCMLE.effectiveSize = NULL)
                            )
mcmc.diagnostics(mod_edges_triangles)
summary(mod_edges_triangles)


# edges + gwesp -----------------------------------------------------------

summary(nw_ud ~ edges +  gwesp(0.3, fixed = TRUE))
#    edges gwesp.fixed.0.3 
# 8943.000        3710.822 
set.seed(666)
mod_edges_gwesp <- ergm(nw_ud ~ edges
                        + gwesp(0.3, fixed = TRUE),
                        control = control.ergm(
                          parallel = 6,
                          parallel.type = "PSOCK",
                          MCMLE.maxit = 15,
                          MCMLE.effectiveSize = NULL)
                        )
mcmc.diagnostics(mod_edges_gwesp)
summary(mod_edges_gwesp)


# edges + 2 degrees -------------------------------------------------------

summary(nw_ud ~ edges +  degree(2))
# edges degree2 
# 8943     655

set.seed(666)
mod_edges_2degrees <- ergm(nw_ud ~ edges
                           + degree(2),
                           control = control.ergm(
                             parallel = 6,
                             parallel.type = "PSOCK",
                             MCMLE.maxit = 100,
                             MCMLE.effectiveSize = NULL)
                           )
mcmc.diagnostics(mod_edges_2degrees)
summary(mod_edges_2degrees)


# edges + 3 degrees --------------------------------------------------------

summary(nw_ud ~ edges +  degree(3))
# edges degree3 
# 8943     206

set.seed(666)
mod_edges_3degrees <- ergm(nw_ud ~ edges
                           + degree(3),
                           control = control.ergm(
                             parallel = 6,
                             parallel.type = "PSOCK",
                             MCMLE.maxit = 100,
                             MCMLE.effectiveSize = NULL)
                           )
mcmc.diagnostics(mod_edges_3degrees)
summary(mod_edges_3degrees)


# edges + min 2 degrees ---------------------------------------------------

summary(nw_ud ~ edges +  mindegree(2))
# edges mindegree2 
# 8943       1729 

set.seed(666)
mod_edges_min2degrees <- ergm(nw_ud ~ edges
                              + mindegree(2),
                              control = control.ergm(
                                parallel = 6,
                                parallel.type = "PSOCK",
                                MCMLE.maxit = 40,
                                MCMLE.effectiveSize = NULL)
                              )
mcmc.diagnostics(mod_edges_min2degrees)
summary(mod_edges_min2degrees)


# edges + min 3 degrees ---------------------------------------------------

summary(nw_ud ~ edges +  mindegree(3))
# edges mindegree3 
# 8943       1074

set.seed(666)
mod_edges_min3degrees <- ergm(nw_ud ~ edges
                              + mindegree(3),
                              control = control.ergm(
                                parallel = 6,
                                parallel.type = "PSOCK",
                                MCMLE.maxit = 30,
                                MCMLE.effectiveSize = NULL)
                              )
mcmc.diagnostics(mod_edges_min3degrees)
summary(mod_edges_min3degrees)


# mcmc and god diagnostics ------------------------------------------------

mcmc.diagnostics(mod_edges_triangles)
mcmc.diagnostics(mod_edges_gwesp)
mcmc.diagnostics(mod_edges_2degrees)
mcmc.diagnostics(mod_edges_3degrees)
mcmc.diagnostics(mod_edges_min2degrees) 
mcmc.diagnostics(mod_edges_min3degrees)

set.seed(666)
mod_edges_2degrees_gof <- gof(mod_edges_2degrees,
                              control = control.gof.formula(nsim = 10))
plot(mod_edges_2degrees_gof)
set.seed(666)
mod_edges_3degrees_gof <- gof(mod_edges_3degrees,
                              control = control.gof.formula(nsim = 10))
plot(mod_edges_3degrees_gof)
