#bbs analysis

# references: 
#https://bbsbayes.github.io/bbsBayes2/articles/bbsBayes2.html#workflow-overview

library(bbsBayes2)

#fetch_bbs_data() 

aou <- bbsBayes2::search_species("Island Scrub-Jay")$aou[1]


search_species("Island Scrub-Jay")
species_forms

# write these out as species lists 
all_species_bbs_database <- load_bbs_data()$species

write_csv(all_species_bbs_database, "sp_key_bbs.csv")




# running a model 

s <- stratify(by = "bbs_cws", sample_data = TRUE)

p <- prepare_data(s)
md <- prepare_model(p, model = "first_diff")
m <- run_model(md, iter_sampling = 100, iter_warmup = 500, chains = 2)

#or read in 
# m <- readRDS("BBS_PacificWren_first_diff_hier_202403071206_STAN.rds")

names(m)

summary_stats <- get_summary(m)

conv <- get_convergence(m)

# summary_stats_n_smooth <- get_summary(m, variables = "n_smooth") %>%
#   arrange(-rhat)
# summary_stats_n_smooth


i <- generate_indices(model_output = m)

p <- plot_indices(indices = i,
                  add_observed_means = TRUE) # optional argument to show raw observed mean counts

print(p[["continent"]])

library(ggplot2)


p1_mod <- p[["continent"]]+
  coord_cartesian(xlim = c(2000,NA))+ #subset of years
  labs(title = "") # remove the title
print(p1_mod)

# generate trends
t <- generate_trends(i)

# generate trends for specific time zone 
t_10 <- generate_trends(i,
                        min_year = 2011,
                        max_year = 2021)

# plot trends: 
trend_map <- plot_map(t)
print(trend_map)

trend_map10 <- plot_map(t_10)
print(trend_map10)






library(ggplot2)

# Get the meta data in trend object - strata information
strata_with_data <- t$meta_strata

# Load the original strata map used in the model fitting
# then filter to just the strata with data for this species
data_bounding_box <- load_map(stratify_by = t$meta_data$stratify_by) %>%
  filter(strata_name %in% strata_with_data$strata_name) %>%
  sf::st_bbox() # create a bounding box for x and y limits

t_mod <- trend_map +
  coord_sf(xlim = data_bounding_box[c("xmin","xmax")],
           ylim = data_bounding_box[c("ymin","ymax")])

print(t_mod)


m$meta_data




s <- stratify(by = "bbs_usgs", species="Scissor-tailed Flycatcher")
#> Using 'bbs_usgs' (standard) stratification
#> Loading BBS data...
#> Filtering to species Scissor-tailed Flycatcher (4430)
#> Stratifying data...
#>   Renaming routes...
p <- prepare_data(s)

