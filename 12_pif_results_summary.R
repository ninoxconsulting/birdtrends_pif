# read in libraries 

library(dplyr)
library(readr)
library(janitor)
library(bbsBayes2)

#devtools::install_github("ninoxconsulting/birdtrends", ref= "annual_trend_projections" )
library(birdtrends)
extrafont::loadfonts(device="win")
library(ggplot2)
#library(doParallel)
#library(foreach) 
library(ggrepel)
library(tidyverse)
library(ggtext)
library(ggdist)
library(glue)
library(patchwork)



# read in the final plot data 

outputs <- "02_outputs"

pifs <- read.csv("sp_key_bbs_full.csv") |> 
  filter(pif_rank %in% c("d", "r", "red")) |> 
  dplyr::select(aou, english, unid_combined, pif_rank, spcode,
                st_pop_pc_lower, st_pop_pc_uppper,
                lt_pop_pc_lower, lt_pop_pc_uppper)


outbirds <- as.numeric(list.files(outputs))

raou <- pifs$aou

# shortlist the aous to species have modeled outputs 
mraou <- raou[raou %in% outbirds]
#drip the spotted owl 3690
mraou <- mraou[-55]


mraoul <- as.list(mraou)

pif_rank <- pifs |> select(aou, english, pif_rank)


# get the percentage values of meeting the short-term 
  
target_achieve = purrr::map(mraou, function(i){  
  
  #i = mraou[1]
  
  mtemp = readRDS(file.path(outputs, i , paste0(i , "_outputs.rds"))) 
  st <- mtemp$prob_st
  names(st) <- c("st_lower_pc", "st_upper_pc", "ref_year", "target_yr") 
  st <- st |> 
    select(-ref_year, -target_yr)
  
  lt <- mtemp$prob_lt
  names(lt) <- c("lt_lower_pc", "lt_upper_pc", "ref_year", "target_yr") 
  lt <- lt |> 
    select(-ref_year, -target_yr)
  
  out <- cbind(st, lt)%>%
    mutate(., aou = i)
  #out <- left_join(out, pif_rank) 
  out

}) |> bind_rows()

target_achieve <- left_join(target_achieve, pif_rank)

# basic plot 

#ggplot(target_achieve, aes(y = st_lower_pc, x = lt_lower_pc))+
#  geom_jitter(width = 1, aes(colour = pif_rank), size = 3) + scale_colour_viridis_d()
# 
# ggplot(target_achieve, aes(y = st_lower_pc, x = lt_lower_pc, label = english))+
#   geom_point(aes(colour = pif_rank), alpha = 0.6, size = 2.5) + 
#   #geom_text(hjust=-0.2, vjust=0, colour = "darkgrey", size = 2.5)+
#   scale_colour_viridis_d()+
#   theme_bw()+ 
#   ylab("short term percentage") + 
#   xlab("long term percentage") +
#   geom_text_repel(size = 2, colour = "grey26",min.segment.length = 0, seed = 42, box.padding = 0.5)


# ggsave(file.path("03_summary", "allsp_percent_summary.jpg"),
#        width = 30,
#        height = 20,
#        units = c("cm"),
#        dpi = 300)
# 



## Part 2

## Summarise how the distribution matches to the targets and categorization into groups. 

all_dist = purrr::map(mraou, function(i){  
 # i = mraou[1]
  
  mtemp = readRDS(file.path(outputs, i , paste0(i , "_outputs.rds"))) 
  trends <-  mtemp$trends
  trends
 
}) |> bind_rows()
  
# combine the data with the metadata about the species - short term trend

df_all <- all_dist |>  
  left_join(pifs) |> 
  select(-unid_combined, -spcode) %>% 
  group_by(aou) |> 
  mutate(median = median(st_ch_pc)) %>% 
  ungroup() |> 
  mutate(english_order = fct_reorder(english, desc(median)))
  

# calculate the Percentiles for each species based on entire distributon 


# calculate the distribution parameters : short term trends 

dist_df <- df_all %>% 
  select(st_ch_pc, aou, st_pop_pc_lower, english_order)%>% 
  group_by(aou)%>% 
  mutate(median = median(st_ch_pc),
         sd = sd(st_ch_pc))

p <- c(.05, 0.1, 0.2,0.25, 0.5,0.75,.80, .90, .95)

p_names <- paste0(p*100, "%")
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

dist_df_percentiles <-dist_df %>% 
  group_by(english_order) %>% 
  summarize_at(vars(st_ch_pc), funs(!!!p_funs))%>%
  ungroup()

short_dist_df <- dist_df%>% 
  select(-st_ch_pc)%>% 
  distinct()%>%
  arrange(median)

st_sum <- left_join(short_dist_df, dist_df_percentiles)%>%
  ungroup()

# group into catergories 

st_sum <- st_sum %>%
 dplyr::mutate(st_class_type1 = case_when(
    `5%` <= -30 & `95%` >=43 ~ "uncertain",
    `75%` < st_pop_pc_lower ~ "miss objective - high consistency",
    `25%` > st_pop_pc_lower ~ "meet objective - high consistency", 
    `25%` < st_pop_pc_lower & `50%`> st_pop_pc_lower ~ "meet objective - low consistency", 
    `50%` < st_pop_pc_lower & `75%`> st_pop_pc_lower ~ "miss objective - low consistency",
    TRUE ~ "tbd"
  ))
  
#write.csv(st_sum, file.path("st_sum_test.csv"), row.names = FALSE)

st_cat <- st_sum %>% 
  dplyr::select(aou, st_class_type1)



## Calculate the same for the long term trends 

dist_df_lt <- df_all %>% 
  select(lt_ch_pc, aou, lt_pop_pc_lower, english_order)%>% 
  group_by(aou)%>% 
  mutate(median_lt = median(lt_ch_pc))

dist_df_percentiles_lt <- dist_df_lt %>% 
  group_by(english_order) %>% 
  summarize_at(vars(lt_ch_pc), funs(!!!p_funs))%>%
  ungroup()

long_dist_df <- dist_df_lt %>% 
  select(-lt_ch_pc)%>% 
  distinct()

lt_sum <- left_join(long_dist_df, dist_df_percentiles_lt)%>%
  ungroup()

# group into catergories 

lt_sum <- lt_sum %>%
  mutate(lt_class_type1 = case_when(
    `5%` <= -30 & `95%` >=43 ~ "uncertain",
    `75%` < lt_pop_pc_lower ~ "miss objective - high consistency",
    `25%` > lt_pop_pc_lower ~ "meet objective - high consistency", 
    `25%` < lt_pop_pc_lower & `50%`> lt_pop_pc_lower ~ "meet objective - low consistency",
    `50%` < lt_pop_pc_lower & `75%`> lt_pop_pc_lower ~ "miss objective - low consistency",
    TRUE ~ "tbd"
  ))
  
lt_cat <- lt_sum %>% 
  select(aou, lt_class_type1)

# generate both short and long term with species names
cat <- left_join(st_cat,  lt_cat ) 





# summary by both long and short 
 cat |> 
  group_by(st_class_type1, lt_class_type1) |> 
  count()


# summary by short 

st <- cat |> 
  group_by(st_class_type1) |> 
  count() |> 
  ungroup() |> 
  mutate(tot =sum(n)) |> 
  rowwise() |> 
  mutate(pc = round((n / tot)*100,0)) %>% 
  mutate(type = "short-term") %>% 
  mutate(class = st_class_type1)

# summary by long
lt <- cat |> 
  group_by(lt_class_type1) |> 
  count() |> 
  ungroup() |> 
  mutate(tot =sum(n)) |> 
  rowwise() |> 
  mutate(pc = round((n / tot)*100,0)) %>% 
  mutate(type = "long-term")%>% 
  mutate(class = lt_class_type1)


cat_long <- bind_rows(st, lt) |> 
  select(-st_class_type1, -lt_class_type1) |> 
complete(type, class  , fill = list(n = 0))

write.csv(cat_long, file.path("03_summary", "overall_percentage_no_summary.csv"), row.names = FALSE)

# generate overview summary plots 

cat_long <- cat_long %>%
  mutate(class_name = case_when(
    class == "meet objective - high consistency" ~ "me_h",
    class == "meet objective - low consistency" ~ "me_l",
    class == "miss objective - low consistency" ~ "mi_l",
    class == "miss objective - high consistency" ~ "mi_h",
    class == "uncertain" ~ "un"
  ))
           

p1 <- ggplot(cat_long, aes(y = pc, class, fill = factor(type, levels = c("short-term", "long-term"))))+
  geom_bar(stat = "identity", position = position_dodge(preserve="single"), width = 0.9,)+
  #geom_col(position = position_stack(reverse = TRUE))
  #scale_x_discrete(limits = c("meet objective - high consistency","meet objective - low consistency", "miss objective - low consistency", "miss objective - high consistency", "uncertain" ), labels = label_wrap_gen(10)) +
  #scale_x_discrete(labels = c("meet objective - high consistency" = "meet objective - high",
  #                            "meet objective - low consistency" = "meet objective - low", 
  #                            "miss objective - low consistency" = "miss objective - low",
  #                            "miss objective - high consistency" = "miss objective - high",
  #                            "uncertain" = "un"), default = label_wrap_gen(width = 25, multi_line = TRUE))) +
  scale_x_discrete(labels = c("meet objective - high consistency" = "me_h",
                              "meet objective - low consistency" = "me_l",
                              "miss objective - low consistency" = "mi_l",
                              "miss objective - high consistency" = "mi_h",
                              "uncertain" = "un"),
                   limits = c("meet objective - high consistency","meet objective - low consistency", "miss objective - low consistency", "miss objective - high consistency", "uncertain" )) +
                   
  #scale_x_discrete(limits = c("me_h","me_l", "mi_l", "mi_h", "un" )) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.55)+
  scale_fill_grey()+
  labs(y = "percent of species")+
  ylim(0,57)+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c( 0.12,0.90),
    legend.box.margin = margin(t = 1, l = 1),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    #axis.text.x = element_text(angle = 90, hjust = 1)) #,
    #legend.box.background = element_rect(color = "black")#"top"
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
  )

p1
ggsave(plot =p1, 
       file.path("03_summary", "st_allsp_percent_catergory_paper.jpg"),
       width = 15,
       height = 12,
       units = c("cm"),
       dpi = 300)

paper_filepath <- "C:/Users/genev/OneDrive/Documents/02.Contracts/2024_ECCC_birdtrends/06.Journal_paper/Paper_tables_figures"

ggsave(plot = p1, 
       file.path(paper_filepath, "Figure1.jpg"),
       width = 15,
       height = 12,
       units = c("cm"),
       dpi = 300)


## Get the full list of species and category 

df_all <- left_join(df_all, cat)

# get list of species for each catergory 

sp_class <- df_all |> 
  select(aou, english, st_class_type1, lt_class_type1) |> 
  unique()%>% 
  arrange(st_class_type1, lt_class_type1, aou)

pifs <- read.csv("sp_key_bbs_full.csv")|> 
  select(c("seq","aou","english","genus", "species")) |> 
  distinct()

sp_full <- left_join(sp_class, pifs, )


write.csv(sp_full, file.path("03_summary", "species_class_fulltable.csv"))




#################################################################

## Grahics with full distribution 

######################################################################

#classtype = "meet target - high confidence"   #"miss target - high confidence"  #"exceed"     "falling short"     "ontrack"       "uncertain"    
classtype = "miss objective - low consistency"  
classtype = "meet objective - low consistency"  
classtype = "miss objective - high consistency" 
classtype = "meet objective - high consistency"

df_for_legend <- df_all  %>% 
  filter(english == "Golden-winged Warbler")
df_for_legend <- df_for_legend[1:1000,]

df <- df_all %>%
  #filter(pif_rank == plottype)
  left_join(st_sum) |> 
  filter(st_class_type1 == classtype) 


bg_color <- "grey97"
font_family <- "Fira Sans"


# get subset to test graphics

#df <- df |> 
#  filter(english %in% c("Black-chinned Sparrow", "Prothonotary Warbler"))


### PLots continued 

p <- df %>%
  ggplot(aes(x =  st_ch_pc, y = english_order)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3)+ #, slab_fill = "blue") + 
  ##stat_halfeye(aes(x = lt_ch_pc, y =english_order), slab_fill = "orange", fill_type = "segments", alpha = 0.3) + 
  stat_interval(.width = c(0.5, 0.75, 0.95)) +
  scale_x_continuous(breaks = seq(-100, 100, 100)) +
  #xlim(-100, 250) + # meet target - high confidence
  #xlim(-100, 120) +
  xlim(-100, 75) + # miss target - high consistency
  geom_point(data = df, aes(x = st_pop_pc_lower, y = as.factor(english)))+
  geom_point(data = df, aes(x = st_pop_pc_uppper, y = as.factor(english)))+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  #stat_summary(geom = "point", fun = median) +
  geom_vline(xintercept = 0, col = "grey30", lty = "dashed") +
  guides(col = "none") +
   labs(
     x = "percent change (%)",
     y = NULL
   ) +
  theme_minimal(base_family = font_family)  +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
    plot.title = element_text(family = "Fira Sans SemiBold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 4, b = 16), size = 10),
    plot.caption = element_textbox_simple(
      margin = margin(t = 12), size = 7
    ),
    plot.caption.position = "plot",
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = "Fira Sans SemiBold"),
    plot.margin = margin(4, 4, 4, 4),
    axis.text=element_text(size=12)
    )


if(classtype == "meet objective - high consistency"){
#if(plottype == "red"){

# create the dataframe for the legend (inside plot)
#df_for_legend <- df %>% 
#  filter(english == "Lesser Prairie-Chicken")

p_legend <- df_for_legend %>% 
  ggplot(aes(x = st_ch_pc, y = as.factor(english))) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval( .width = c(0.5, 0.75, 0.95)) +
  #scale_y_discrete(labels = toupper) +
  #scale_x_continuous(breaks = seq(-100, 100,100)) +
  xlim(-60, 150)+
  geom_point(data = df_for_legend, aes(x = st_pop_pc_lower, y = as.factor(english)))+
  geom_point(data = df_for_legend, aes(x = st_pop_pc_uppper, y = as.factor(english)))+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  annotate(
    "richtext",
    y = c(0.93, 0.9, 0.9, 1.18, 1.18, 1.9),
    #x= c(-60, 65, 20, 5, 55, 45),
    x= c(-10, 105, 65, 5, 55, 110),
    label = c("50 % of projections <br>fall within this range", 
              "95 % of <br>projections", 
              "75 % of <br> projections", 
              "lower target", 
              "upper target",
              "Distribution<br>of projections"),
    fill = NA, label.size = NA, family = font_family, size = 3, vjust = 1,
  ) +
  geom_curve(
   # data = data.frame(
  #    y =     c(0.9, 0.9, 0.9,   1.1, 1.82),
  #    yend = c(0.98, 0.98, 0.98,  1.02, 1.82), 
  #    x =    c(-21, 50, 12,  40, 25),
  #    xend = c(-17, 51, 14,  35, 10)),
    data = data.frame(
      y =    c( 0.9, 0.9),
      yend = c( 0.98, 0.98), 
      x =    c( 60, 40),
      xend = c(61, 41)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) + #lower target
  geom_curve(  
    data = data.frame(
      y =      1.1, 
      yend =  1.02,
      x =     20,
      xend =  25), 
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = -0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 40, length = unit(1, "mm"))
  ) + #upper target
  geom_curve(  
    data = data.frame(
      y =      1.1, 
      yend =  1.02,
      x =     40,
      xend =  35), 
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 40, length = unit(1, "mm"))
  ) + # distrbution of projections>
  geom_curve(  
    data = data.frame(
      y =     1.82, 
      yend =  1.82,
      x =    75,
      xend =  60), #60
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 40, length = unit(1, "mm"))
  ) + # 95% projections>
  geom_curve(  
    data = data.frame(
      y =     0.9, 
      yend =  0.98,
      x =   90,
      xend = 85), #60
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 40, length = unit(1, "mm"))
  )+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
 # coord_flip(xlim = c(0.75, 1.3), ylim = c(0, 6000), expand = TRUE) +
  guides(color = "none") +
  labs(title = "Legend") +
  theme_void(base_family = font_family) +
  theme(plot.title = element_text(family = "Fira Sans SemiBold", size = 9,
                                  hjust = 0.075),
        plot.background = element_rect(color = "grey30", size = 0.2, fill = bg_color))


#p_legend


# Insert the custom legend into the plot
p <- p + inset_element(p_legend, l = 0.65, r = 1.0,  t = 0.99, b = 0.80, clip = FALSE)

}

#p


if (classtype == "meet objective - high consistency") { 
  pwidth = 30  
  pheight = 35
}

if (classtype == "miss objective - low consistency") { 
  pwidth = 30  
  pheight = 25
}

if (classtype == "meet objective - low consistency") { 
  pwidth = 30  
  pheight = 15
}

if (classtype == "miss objective - high consistency") { 
  pwidth = 30  
  pheight = 35
}

ggsave(plot = p, file.path("03_summary", paste0(classtype, "_listed_sp.jpg")), 
       width = pwidth,
       height = pheight,
       units = c("cm"),
       dpi = 300)


# 
# ######################################
# 
# ## long term trends 
# 
# ########################
# 
# df_all_lt <- all_dist  %>% 
#   left_join(pifs) %>% 
#   select(-unid_combined, -spcode) %>% 
#   group_by(aou) |> 
#   mutate(median = median(lt_ch_pc)) %>% 
#   ungroup() |> 
#   mutate(english_order = fct_reorder(english, desc(median)))
# #arrange(desc(median))#%>% 
# #select(-median)
# 
# 
# plottype = "red"
# 
# df <- df_all_lt %>% 
#   filter(pif_rank == plottype)
# 
# 
# p <- df %>%
#   ggplot(aes(x =  lt_ch_pc, y = english_order)) +
#   stat_halfeye(fill_type = "segments", alpha = 0.3)+ #, slab_fill = "blue") + 
#   #stat_halfeye(aes(x = lt_ch_pc, y =english_order), slab_fill = "orange", fill_type = "segments", alpha = 0.3) + 
#   stat_interval() +
#   #scale_y_discrete(labels = toupper) +
#   scale_x_continuous(breaks = seq(-100, 100,100)) +
#   xlim(-100, 250) +
#   geom_point(data = df, aes(x = lt_pop_pc_lower, y = as.factor(english)))+
#   geom_point(data = df, aes(x = lt_pop_pc_uppper, y = as.factor(english)))+
#   scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
#   #stat_summary(geom = "point", fun = median) +
#   geom_vline(xintercept = 0, col = "grey30", lty = "dashed") +
#   guides(col = "none") +
#   labs(
#     x = "percent change (%)",
#     y = NULL
#   ) +
#   theme_minimal(base_family = font_family)  +
#   theme(
#     plot.background = element_rect(color = NA, fill = bg_color),
#     panel.grid = element_blank(),
#     panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
#     plot.title = element_text(family = "Fira Sans SemiBold"),
#     plot.title.position = "plot",
#     plot.subtitle = element_textbox_simple(
#       margin = margin(t = 4, b = 16), size = 10),
#     plot.caption = element_textbox_simple(
#       margin = margin(t = 12), size = 7
#     ),
#     plot.caption.position = "plot",
#     axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = "Fira Sans SemiBold"),
#     plot.margin = margin(4, 4, 4, 4)
#   )



p


ggsave(file.path("03_summary", paste0(plottype, "_listed_sp_lt.jpg")), 
       width = 30,
       height = 40,
       units = c("cm"),
       dpi = 300)



###########################################################
# update output-plots

library(jpeg)
library(magrittr)
library(dplyr)
library(stringr)


files <- list.files(file.path("02_outputs"), recursive = T, pattern = "plot.rds")

for(i in files){
   #i = files[1]
  
    filename <- str_sub(i, 1,4)
    
    
    aa <-  readRDS(file.path("02_outputs", i))
    plot(aa)
    
    
    ggsave(file.path("02_outputs", filename, paste0("trend_plot_",filename, "_v2.jpg")), 
           width = 25,
           height = 20,
           units = c("cm"),
           dpi = 300)
    
    
  }







# distribution plot for paper to demonstrate the catergories

p1 <- ggplot(data = data.frame(x = c(-100, 100)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 30)) + ylab("") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +  
  #geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  theme_()


miss <- p1 +
  geom_vline(xintercept = 75, linetype = "longdash", linewidth = 2, color = "blue")+
  ggtitle("Miss target - high confidence")
  
exceed <- p1 +
  geom_vline(xintercept = -75, linetype = "longdash", linewidth = 2, color = "blue")+
  ggtitle("Meet target - high confidence")

falling_short <- p1 +
  geom_vline(xintercept = 25, linetype = "longdash", linewidth = 2, color = "blue")+
  ggtitle("Miss target - low confidence")

ontrack <- p1 +
  geom_vline(xintercept = -25, linetype = "longdash", linewidth = 2, color = "blue")+
  ggtitle("Meet target - low confidence")
  
 #  
 # ggplot(data = data.frame(x = c(-100, 100)), aes(x)) +
 #  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 50)) + ylab("") +
 #  scale_y_continuous(breaks = NULL) +
 #  scale_x_continuous(breaks = NULL) +  
 #  #geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
 #  theme_bw()




  
    
#  https://r-graph-gallery.com/web-ridgeline-plot-with-inside-plot-and-annotations.html
#}


# plots to show the distribution 
# https://r-graph-gallery.com/web-ridgeline-plot-with-inside-plot-and-annotations.html
