

library(dplyr)
library(readr)
library(janitor)
library(bbsBayes2)
library(birdtrends)
library(ggplot2)
library(doParallel)
library(foreach) 
library(ggrepel)


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

mraoul <- as.list(mraou)

pif_rank <- pifs |> select(aou, english, pif_rank)


# get the percentage values of meeting the short-term 

target_achieve = foreach(i= mraou, .combine=rbind )%dopar%{
  #i = mraoul[1]
  
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

}


target_achieve <- left_join(target_achieve, pif_rank)

# basic plot 

#ggplot(target_achieve, aes(y = st_lower_pc, x = lt_lower_pc))+
#  geom_jitter(width = 1, aes(colour = pif_rank), size = 3) + scale_colour_viridis_d()

ggplot(target_achieve, aes(y = st_lower_pc, x = lt_lower_pc, label = english))+
  geom_point(aes(colour = pif_rank), alpha = 0.6, size = 2.5) + 
  #geom_text(hjust=-0.2, vjust=0, colour = "darkgrey", size = 2.5)+
  scale_colour_viridis_d()+
  theme_bw()+ 
  ylab("short term percentage") + 
  xlab("long term percentage") +
  geom_text_repel(size = 2, colour = "grey26",min.segment.length = 0, seed = 42, box.padding = 0.5)


ggsave(file.path("03_summary", "allsp_percent_summary.jpg"),
       width = 30,
       height = 20,
       units = c("cm"),
       dpi = 300)


# ## summary of all the type. 
# # convert values into Quartiles (
# 1 = very low <25
# 2 = low < 25 - 50
# 3 = moderate - 50 - 75
# 4 = high = 75 - 90
# 5 = very high >90
# 
# 

## Lower class limit 

sum <- target_achieve %>%
  mutate(st_class = case_when(
    between(st_lower_pc, 0, 25.999)~ 1,
    between(st_lower_pc, 26, 50.999)~ 2,
    between(st_lower_pc, 51, 75.999)~ 3,
    between(st_lower_pc, 76, 90.999)~ 4,
    between(st_lower_pc, 91, 100)~ 5,
    TRUE ~ 0),
    lt_class = case_when(
      between(lt_lower_pc, 0, 25.999)~ 1,
      between(lt_lower_pc, 26, 50.999)~ 2,
      between(lt_lower_pc, 51, 75.999)~ 3,
      between(lt_lower_pc, 76, 90.999)~ 4,
      between(lt_lower_pc, 91, 100)~ 5,
      TRUE ~ 0)) |>
  select(english, pif_rank, st_class,lt_class)%>%
  arrange( pif_rank)


# histogram summary 

ss <- sum |> 
  group_by(pif_rank, st_class) |> 
  count() |> 
  mutate(code = case_when(
    st_class == 1 ~ "very low" ,
    st_class == 2 ~ "low" ,
    st_class == 3 ~  "moderate" ,
    st_class == 4 ~ "high" ,
    st_class == 5 ~  "very high" 
  )) |> 
  ungroup() |> 
#ss |> 
  tidyr::complete(code, pif_rank)|> 
  mutate(tot = case_when(
    pif_rank == 'd' ~ 45, 
    pif_rank == "r" ~ 5,
    pif_rank == 'red' ~ 12)) |> 
  mutate (pc = (n / tot)*100)



ggplot(ss, aes(y = pc, code, fill = pif_rank))+
  geom_bar(stat = "identity", position = "dodge", width = 0.9,)+
  scale_x_discrete(limits = c("very low", "low", "moderate","high", "very high" )) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.55)+
  scale_fill_grey()+
  labs(y = "percent of species")+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
  )


ggsave(file.path("03_summary", "st_allsp_percent_catergory.jpg"),
       width = 30,
       height = 20,
       units = c("cm"),
       dpi = 300)




## long term  class limit 

ls <- sum |> 
  group_by(pif_rank, lt_class) |> 
  count() |> 
  mutate(code = case_when(
    lt_class == 1 ~ "very low" ,
    lt_class == 2 ~ "low" ,
    lt_class == 3 ~  "moderate" ,
    lt_class == 4 ~ "high" ,
    lt_class == 5 ~  "very high" 
  )) |> 
  ungroup() |> 
  #ss |> 
  tidyr::complete(code, pif_rank)|> 
  mutate(tot = case_when(
    pif_rank == 'd' ~ 45, 
    pif_rank == "r" ~ 5,
    pif_rank == 'red' ~ 12)) |> 
  mutate (pc = (n / tot)*100)



ggplot(ls, aes(y = pc, code, fill = pif_rank))+
  geom_bar(stat = "identity", position = "dodge", width = 0.9,)+
  scale_x_discrete(limits = c("very low", "low", "moderate","high", "very high" )) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.55)+
  scale_fill_grey()+
  labs(y = "percent of species")+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
  )


ggsave(file.path("03_summary", "lt_allsp_percent_catergory.jpg"),
       width = 30,
       height = 20,
       units = c("cm"),
       dpi = 300)







# Detailed plot data 
##########################
source("functions/calculate_all_probs.R")

# keep entire distribution of the plots 

all_dist = foreach(i= mraou, .combine=rbind )%dopar%{
  
  #i = mraou[1]
  
  mtemp = readRDS(file.path(outputs, i , paste0(i , "_outputs.rds"))) 
  
  targ <-  mtemp$targ
  preds_sm <- mtemp$pred_sm
  
  # short term
  probs_st <- calculate_all_probs(predicted_trends = preds_sm, ref_year = 2014, targ_year = 2026) |> 
    select(ch_pc) |> 
    mutate(aou = i) |> 
    rename("st_ch_pc" = ch_pc)
  
  
  # long term trends
  probs_lt <- calculate_all_probs(predicted_trends = preds_sm, ref_year = 2014, targ_year = 2046) |> 
    select(ch_pc) |> 
    mutate(aou = i) |> 
    rename("lt_ch_pc" = ch_pc)
  
  
  probs <- cbind(probs_st , probs_lt ) 
  probs <- probs[,-4]
  probs
  
} 
  
  
bg_color <- "grey97"
font_family <- "Fira Sans"
library(tidyverse)
library(ggtext)
library(ggdist)
library(glue)
library(patchwork)


df_all <- all_dist  %>% 
  left_join(pifs) %>% 
  select(-unid_combined, -spcode) %>% 
  group_by(aou) |> 
  mutate(median = median(st_ch_pc)) %>% 
  ungroup() |> 
  mutate(english_order = fct_reorder(english, desc(median)))
  #arrange(desc(median))#%>% 
  #select(-median)
  

plottype = "red"

df <- df_all %>% 
  filter(pif_rank == plottype)


p <- df %>%
  ggplot(aes(x =  st_ch_pc, y = english_order)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3)+ #, slab_fill = "blue") + 
  #stat_halfeye(aes(x = lt_ch_pc, y =english_order), slab_fill = "orange", fill_type = "segments", alpha = 0.3) + 
  stat_interval() +
  #scale_y_discrete(labels = toupper) +
  scale_x_continuous(breaks = seq(-100, 100,100)) +
  xlim(-100, 250) +
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
    plot.margin = margin(4, 4, 4, 4)
  )



if(plottype == "red"){

# create the dataframe for the legend (inside plot)
df_for_legend <- df %>% 
  filter(english == "Lesser Prairie-Chicken")

p_legend <- df_for_legend %>% 
  ggplot(aes(x = st_ch_pc, y = as.factor(english))) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  #scale_y_discrete(labels = toupper) +
  #scale_x_continuous(breaks = seq(-100, 100,100)) +
  xlim(-90, 100)+
  geom_point(data = df_for_legend, aes(x = st_pop_pc_lower, y = as.factor(english)))+
  geom_point(data = df_for_legend, aes(x = st_pop_pc_uppper, y = as.factor(english)))+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  annotate(
    "richtext",
    y = c(0.93, 0.9, 0.9, 1.18, 1.18, 1.85),
    x= c(-60, 65, 10, 5, 55, 45),
    label = c("50 % of predictions<br>fall within this range", "95 % of prices", 
              "80 % of prices", "lower target", "upper target","Distribution<br>of predictions"),
    fill = NA, label.size = NA, family = font_family, size = 3, vjust = 1,
  ) +
  geom_curve(
    data = data.frame(
      y =     c(0.9, 0.9, 0.9,   1.1, 1.82),
      yend = c(0.98, 0.98, 0.98,  1.02, 1.82), 
      x =    c(-21, 50, 12,  40, 25),
      xend = c(-17, 51, 14,  35, 10)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  geom_curve(
    data = data.frame(
      y =      1.1, 
      yend =  1.02,
      x =     20,
      xend =  25), 
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = -0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 40, length = unit(1, "mm"))
  ) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
 # coord_flip(xlim = c(0.75, 1.3), ylim = c(0, 6000), expand = TRUE) +
  guides(color = "none") +
  labs(title = "Legend") +
  theme_void(base_family = font_family) +
  theme(plot.title = element_text(family = "Fira Sans SemiBold", size = 9,
                                  hjust = 0.075),
        plot.background = element_rect(color = "grey30", size = 0.2, fill = bg_color))



# Insert the custom legend into the plot
p + inset_element(p_legend, l = 0.65, r = 1.0,  t = 0.99, b = 0.75, clip = FALSE)


}

p


ggsave(file.path("03_summary", paste0(plottype, "_listed_sp.jpg")), 
       width = 30,
       height = 40,
       units = c("cm"),
       dpi = 300)










  
  
  
  
  
    
#  https://r-graph-gallery.com/web-ridgeline-plot-with-inside-plot-and-annotations.html
#}


# plots to show the distribution 
# https://r-graph-gallery.com/web-ridgeline-plot-with-inside-plot-and-annotations.html
