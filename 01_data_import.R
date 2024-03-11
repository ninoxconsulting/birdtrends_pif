# testing data download and exploration
library(googledrive)
library(googlesheets4)
library(dplyr)
library(readr)
library(janitor)

#
# To authorize the googlesheets download, set your google auth email with:
# options(
#   gargle_oauth_email = "email.which.gives.you.access.to.these.files@gmail.com"
# )
# If this is different from your normal google auth email you can add this to a
# project-specific .Rprofile file to cache



# note - has problems accessing the standard BVS repository so created a copy on Mydrive 


# get list of files 
trends_doc <- drive_find(pattern = "BBS_", type = "csv")      

# get the first record 
alltr <- trends_doc %>%
  filter(name == "BBS_Full_Indices_continent_country_2022.csv")%>% 
  slice_head(n = 1)

#drive_get(as_id(alltr))

tr <- drive_download(as_id(alltr), path = "01_raw_data/BBS_Full_Indices_continent_country_2022.csv", overwrite = TRUE)

head(tr)

trd <- read_csv(file.path("01_raw_data", "All_BBS_Trends_2022.csv"), col_types = "c",
               name_repair = "universal")
               


# unique_sp_list 

sp <- trd |> 
  select(species, Species_ID_core) |> 
  distinct()

write_csv(sp, file.path("01_raw_data", "sp_list.csv"))

## TODO: generate bird list by catergory. 




