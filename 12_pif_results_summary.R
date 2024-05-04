
library(dplyr)
library(readr)
library(janitor)
library(bbsBayes2)
library(birdtrends)
library(ggplot2)
library(doParallel)
library(foreach)  


# read in the final plot data 

outputs <- "outputs"

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

ggplot(target_achieve, aes(y = st_lower_pc, x = as.factor(aou)), color = pif_rank)+
  geom_point(aes(col = pif_rank)) + 
  xlab("Date") + ylab("Tag") 



## summary of all the type. 
# convert values into Quartiles (
1 = very low <25
2 = low < 25 - 50
3 = moderate - 50 - 75
4 = high = 75 - 90
5 = very high >99



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
  
    
library(tidyverse)
library(waffle)   
library(ggtext)

    
p2 <- ggplot( sum, aes(values=st_class, fill=pif_rank))+
  waffle::geom_waffle(
    n_rows = 4,        # Number of squares in each row
    color = "white",   # Border color
    flip = TRUE, na.rm=TRUE
  )+
  facet_grid(~st_class)+
  coord_equal()

p2<-p2+
  # Hide legend
  guides(fill='none',color='none')+
  # Add title and subtitle
  #labs(title=title,subtitle=sub)+
  theme(
    # Enable markdown for title and subtitle
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    # "Clean" facets 
    panel.background=element_rect(fill="white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill="white"),
    strip.background.y = element_rect(fill="dimgrey"),
    strip.text.y = element_text(color="white")
  )



















##########################
# Detailed plot data 
##########################

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
  

df <- df_all %>% 
  filter(pif_rank == "red")


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
     #title = toupper("Probability of meeting short term trends"),
  #   subtitle = "plot sutitle",
  #   caption = "Axis capped at 10,000 USD.<br>
  #   Data: Pennington, Kate (2018). 
  #   Bay Area Craigslist Rental Housing Posts, 2000-2018.<br>
  #   Retrieved from github.com/katepennington/historic_bay_area_craigslist_housing_posts/blob/master/clean_2000_2018.csv.zip.
  #   <br>
  #   Visualization: Ansgar Wolsing",
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

# create the dataframe for the legend (inside plot)
df_for_legend <- df %>% 
  filter(english == "Lesser Prairie-Chicken")

p_legend <- df_for_legend %>% 
  ggplot(aes(x = value, y = as.factor(english))) +
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

ggsave(file.path("summary", "d_listed_sp.jpg"), 
       width = 30,
       height = 40,
       units = c("cm"),
       dpi = 300)
















  
  
  
  
  
    
#  https://r-graph-gallery.com/web-ridgeline-plot-with-inside-plot-and-annotations.html
#}


# plots to show the distribution 
# https://r-graph-gallery.com/web-ridgeline-plot-with-inside-plot-and-annotations.html
