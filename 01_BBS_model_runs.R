#bbs analysis


library(bbsBayes2)

#fetch_bbs_data() 

s <- stratify(by = "bbs_cws", sample_data = TRUE)

aou <- bbsBayes2::search_species("Island Scrub-Jay")$aou[1]


search_species("Island Scrub-Jay")
species_forms

# write these out as species lists 
all_species_bbs_database <- load_bbs_data()$species



p <- prepare_data(s)
md <- prepare_model(p, model = "first_diff")
m <- run_model(md, iter_sampling = 100, iter_warmup = 500, chains = 2)




s <- stratify(by = "bbs_usgs", species="Scissor-tailed Flycatcher")
#> Using 'bbs_usgs' (standard) stratification
#> Loading BBS data...
#> Filtering to species Scissor-tailed Flycatcher (4430)
#> Stratifying data...
#>   Renaming routes...
p <- prepare_data(s)