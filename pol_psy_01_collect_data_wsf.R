
## not sharing the source file
## possible GDPR issues
## source file has info 
## that will reveal
## the names of the coauthors
## although all this is publicly available info
## (names on papers, affiliations etc.)
## not sure if the collected data violates EU privacy regulations

source("source_file.R")

col_dyd <- map_dfr(.x = list_of_coauthors,
                   .f = pull_nw)

nw_df <- col_dyd %>% distinct()

save(nw_df, file = "data/nw_df.RData")