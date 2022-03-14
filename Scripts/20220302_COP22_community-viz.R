## PROJECT:  badboys
## AUTHOR:   A.Chafetz | USAID
## PURPOSE:  TZA COP21 Support - Index Testing
## LICENSE:  MIT
## DATE:     2021-01-26


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ICPIutilities)
library(glamr)
library(glitr)
library(extrafont)
library(scales)
library(glue)
library(ggtext)
library(ggrepel)
library(gisr)
library(sf)
library(waffle)
library(patchwork)

# MUNGE/VIZ ------------------------------------

df <- readxl::read_xlsx("Data/FY22_LTFU_data.xlsx", sheet = "LTFU By Quarter  Summary") %>% clean_names()

#pivot longer
df_long <-df %>% pivot_longer(-c(quarter), names_to = "indicator")

ltfu_viz <- df_long %>% 
  ggplot(aes(quarter)) +
  geom_col(aes(y = value, fill = indicator), position = "dodge") +
  scale_fill_manual(values = c(scooter, golden_sand, trolley_grey_light)) +
  geom_text(data = df_long %>% filter(quarter == "FY22Q1"),
            aes(y = value + 50, label = number(value)),
            family = "Source Sans Pro", size = 9/.pt, hjust = -1) +
  geom_vline(xintercept = "FY20Q3", color = trolley_grey, linetype = "dashed") +
  scale_y_continuous(labels = label_number_si()) +
  si_style_ygrid() +
  labs(x = NULL, y = NULL,
       subtitle = "LTFU Performance by Quarter",
     #  subtitle = "Ethiopia COP22 LTFU Analysis",
       caption = glue("
         Source: United Data Systems Custom Indicator LTFU Data
         USAID SI Analytics | Karishma Srikanth")) +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        legend.position = "None")

#read in prct values
df_prct <- readxl::read_xlsx("Data/FY22_LTFU_data.xlsx", sheet = 2, skip =2, range = "I3:J11") %>% clean_names() %>% 
  rename(value = x2,
         trace_outcome = fy22_q1)

#calculate shares
df_prct <- df_prct %>% 
  mutate(share = (value / sum(df_prct$value))*100,
         period = "FY22Q1")

#Waffle chart
waffle <- df_prct %>% 
 mutate(fill_color = case_when(trace_outcome == "Re engaged in care" ~ "#00354f",
                               trace_outcome == "Unable to be located" ~ trolley_grey_light,
                               trace_outcome == "Active on Treatment" ~ scooter,
                               TRUE ~ scooter_light)) %>% 
  ggplot(aes(fill = fill_color, values = share)) + 
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE,  make_proportional = T, alpha = 0.75) + 
  scale_fill_identity() +
  #scale_fill_manual(values = c(scooter, golden_sand, burnt_sienna, usaid_medblue, genoa, old_rose, trolley_grey_light)) +
  #scale_fill_si(palette = "old_roses", discrete = T, reverse = T) +
  si_style_nolines() + 
  labs(x = NULL, y = NULL,
      # title = "LTFU Performance by Quarter",
       subtitle = "LTFU Tracing by Tracing Outcome",
       caption = glue("
         Source: United Data Systems Custom Indicator LTFU Data
         USAID SI Analytics | Karishma Srikanth")) + 
  theme(axis.text = element_blank())

#patchwork together
ltfu_viz + waffle + plot_layout(widths = c(2, 1)) +
  plot_annotation(
    title = "In FY22Q1, about 62% of clients with IIT and line lists received have been reengaged in care",
    theme = si_style())

si_save("Graphics/ETH-cop22-ltfu-combined.svg")


# REGIONAL VIZ -----

df_region <- readxl::read_xlsx("Data/FY22_LTFU_data.xlsx", skip =1, range = "a2:f5") 

df_region %>% 
  pivot_longer(cols = -c(LTFU), names_to = "Region") %>% 
  ggplot(aes(LTFU)) +
  geom_col(aes(y = value, fill = LTFU), position = "dodge") + 
  facet_wrap(~Region) +
  scale_fill_manual(values = c(scooter, golden_sand, trolley_grey_light)) +
  si_style_ygrid() +
  geom_text(aes(y = value + 20, label = number(value,1)),
            family = "Source Sans Pro", size = 11/.pt, hjust = 0.5, color = grey80k) +
  scale_y_continuous(labels = label_number_si())+
  si_style_ygrid() +
  labs(x = NULL, y = NULL,
       title = "FY22Q1 LTFU Performance by Region",
       subtitle = "Ethiopia COP22 LTFU Analysis",
       caption = glue("
         Source: United Data Systems Custom Indicator LTFU Data
         USAID SI Analytics | Karishma Srikanth")) +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"),
          legend.position = "None")

si_save("Images/ETH-cop22-ltfu-region.png")



  
