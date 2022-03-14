## PROJECT:  ethiopia-cop22
## AUTHOR:   K. Srikanth | USAID
## PURPOSE:  ETH COP 22 Support - Index Testing
## LICENSE:  MIT
## DATE:     2022-03-02
## NOTE: adapted from badboys/Scripts/COP21_TZA_Index_Review.R


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
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

# IMPORT DATA -------------------------------------------------------------

df_eth <- si_path() %>% 
  return_latest("PSNU_IM") %>% 
  read_msd()


# Index Quarterly Trends --------------------------------------------------

df_qtr <- df_eth %>% 
  filter(
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
  mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
  group_by(fiscal_year, indicator, mod_type) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd()

df_qtr %>% 
  mutate(lab = case_when(mod_type == "Index" ~ clean_number(value, 1)),
         fill_color = ifelse(mod_type == "Index", denim, denim_light)) %>% 
  ggplot(aes(period, value, fill = fct_rev(fill_color))) +
  geom_col(alpha = .7) +
  geom_text(aes(label = lab),
            family = "Source Sans Pro", color = "#505050",
            vjust = -.5) +
  facet_grid(indicator ~ ., scales = "free_y") +
  scale_fill_identity() +
 # scale_fill_si("denims") +
  scale_y_continuous(labels = label_number_si(), expand = c(.005, .005)) +
  scale_x_discrete(breaks = c("FY19Q1", "FY19Q3", "FY20Q1", "FY20Q3", "FY21Q1", "FY21Q3")) +
  labs(x = NULL, y = NULL,
       title = "GRADUAL INCREASE IN <span style = 'color:#002065'>INDEX TESTING</span> SHARE OF <span style = 'color:#bfddff'>ALL POSITIVE</span> TEST",
       subtitle = "Ethiopia | PEPFAR",
       caption = glue("Source: {source_info}
         USAID SI Analytics | Karishma Srikanth")) +
  si_style_ygrid() +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_text(face = "bold"))

si_save("COP22_ETH_Index-share.png", path = "Images")

# Are there differences by age/sex?

df_agesex <- df_eth %>% 
  filter(
         indicator %in% c("HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         fiscal_year == 2021) %>% 
  mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
  group_by(fiscal_year, ageasentered, sex, indicator, mod_type) %>% 
  summarise(across(c(cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(mod_type, cumulative)

df_agesex %>% 
  filter(sex != "Unknown Sex",
         ageasentered != "Unknown Age") %>% 
  mutate(value = ifelse(sex == "Male", -Index, Index),
         lab = clean_number(Index,1)) %>% 
  ggplot(aes(value, ageasentered, fill = sex)) +
  geom_col() +
  geom_text(aes(label = lab, hjust = ifelse(sex == "Male", 1.2, -.4)),
            family = "Source Sans Pro", size = 4, color = "#505050") +
  geom_vline(xintercept = 0) +
  expand_limits(x = c(-6000,6000)) +
  labs(x = NULL, y = NULL,
       title = "LESS POSITIVE INDEX TESTS FOR <span style = 'color:#287c6f'>MALES</span> THAN <span style = 'color:#8980cb'>FEMALES</span> in FY21",
       subtitle = "Ethiopia | PEPFAR FY21",
       caption = glue("Source: {source_info} | USAID SI Analytics | Karishma Srikanth")) +
  scale_x_continuous(breaks = seq(-6000,6000, 1000)) +
  scale_fill_manual(values = c(moody_blue, genoa)) +
  si_style() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_markdown()
  )

si_save("COP22_ETH_Index-age-sex.png", path = "Images")

# SHARE OF POS FROM INDEX -------------------------------------------------


df_hts <- df_eth %>% 
  filter(operatingunit == "Ethiopia",
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result")

df_hts <- df_hts %>% 
  mutate(type_loc = ifelse(str_detect(modality, "Mod"), "Community", "Facility"),
         type_mod = ifelse(str_detect(modality, "Index"), "Index", "Other")
  )
df_pos_share <- df_hts %>% 
  bind_rows(df_hts %>% mutate(snu1 = "NATIONAL")) %>% 
  group_by(fiscal_year, snu1, indicator, type_mod) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop_last") %>% 
  filter(indicator == "HTS_TST_POS") %>% 
  mutate(share = cumulative/sum(cumulative)) %>% 
  ungroup()


df_pos_share <- df_pos_share %>% 
  group_by(snu1, fiscal_year) %>% 
  mutate(snu_total = ifelse(fiscal_year == 2022, sum(cumulative), 0)) %>% 
  group_by(snu1) %>% 
  mutate(snu_total = max(snu_total),
         snu_index = ifelse(fiscal_year == 2022 & type_mod == "Index", cumulative, 0),
         snu_index = max(snu_index),
         area_fill = ifelse(snu1 == "NATIONAL", burnt_sienna, scooter),
         facet_title = glue("{snu1}\nFY21Q1: {comma(snu_index)}/{comma(snu_total)}")) %>% 
  ungroup()


df_pos_share %>% 
  filter(type_mod == "Index",
         snu_total > 500) %>% 
  ggplot(aes(fiscal_year, share, fill = area_fill, color = area_fill)) +
  geom_area(alpha = .6, size = 1.1) +
  facet_wrap(~fct_reorder(facet_title, snu_index, median, .desc = TRUE)) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(2020, 2021, 2022)) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL,
       title = "NATIONALLY ALMOST 30% OF FY22Q1 POSITIVES ARE FROM INDEX TESTING",
       subtitle = "Share of positive index test out of all positive tests",
       caption = glue("Note: Plots from the regions with less than 500 positive tests in FY21Q1 are removed
         Source: {source_info}
         USAID SI Analytics | Karishma Srikanth")) +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"))

si_save("Images/COP22_ETH_Index_Share.png")

# SHARE OF COMMUNITY VS FACILITY INDEX POS --------------------------------

df_pos_index_share <- df_hts %>% 
  bind_rows(df_hts %>% mutate(snu1 = "NATIONAL")) %>% 
  group_by(fiscal_year, snu1, indicator, type_mod, type_loc) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop_last") %>% 
  filter(indicator == "HTS_TST_POS",
         type_mod == "Index") %>% 
  mutate(share = cumulative/sum(cumulative),
         share = ifelse(type_loc == "Community", -share, share)) %>% 
  ungroup()

df_pos_index_share <- df_pos_index_share %>% 
  complete(type_loc, nesting(fiscal_year, snu1), fill = list(cumulative = 0, share = 0)) %>% 
  mutate(fiscal_year = as.character(fiscal_year) %>% fct_rev,
         area_fill = ifelse(type_loc == "Community", denim, scooter),
         background = ifelse(type_loc == "Community", -1, 1)) %>% 
  group_by(snu1, fiscal_year) %>% 
  mutate(snu_total = ifelse(fiscal_year == 2022, sum(cumulative), 0)) %>% 
  group_by(snu1) %>% 
  mutate(snu_total = max(snu_total),
         snu_comm = ifelse(fiscal_year == 2022 & type_loc == "Community", cumulative, 0),
         snu_comm = max(snu_comm),
         snu_fac = ifelse(fiscal_year == 2022 & type_loc == "Facility", cumulative, 0),
         snu_fac = max(snu_fac),
         facet_title = glue("{snu1}<br>FY22Q1: <span style = 'color:#2057a7;'>{comma(snu_comm)}</span>/<span style = 'color:#1e87a5;'>{comma(snu_fac)}")) %>% 
  ungroup()

df_pos_index_share %>% 
  filter(snu_total > 200) %>% 
  ggplot(aes(share, fiscal_year, fill = area_fill)) +
  geom_col(aes(background), alpha = .2, fill = "gray80") +
  geom_col(alpha = .8) +
  geom_vline(xintercept = c(-1, 0, 1), color = "gray60") +
  geom_vline(xintercept = c(-.5, .5), color = "gray60", linetype = "dashed") +
  facet_wrap(~fct_reorder(facet_title, snu_total, max, .desc = TRUE)) +
  scale_fill_identity() +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = percent(c(1, 0, 1))) +
  si_style_nolines() +
  labs(x = NULL, y = NULL,
       title = glue("NATIONALLY, IN FY22Q1 ABOUT HALF OF POSITIVE INDEX TESTS ARE IN <span style = 'color:#2057a7;'>COMMUNITIES</span> AND HALF ARE IN <span style = 'color:#1e87a5;'>FACILITIES</span>"),
       subtitle = "Share of positive index test by location",
       caption = glue("Note: Plots from the regions with less than 200 positive index tests in FY21Q1 are removed
         Source: {source_info}
         USAID SI Analytics | Karishma Srikanth")) +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        strip.text = element_markdown(),
        plot.title = element_markdown())

si_save("Images/COP22_ETH_Index_Comm-v-Fac.png")  

# INDEX AGE/SEX POSITIVITY ------------------------------------------------

df_index_positivity <-  df_hts %>% 
  bind_rows(df_hts %>% mutate(snu1 = "NATIONAL")) %>% 
  group_by(fiscal_year, snu1, indicator, ageasentered, sex, type_mod, type_loc) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
  filter(type_mod == "Index",
         fiscal_year == 2022) %>% 
  pivot_wider(names_from = indicator, values_from = cumulative, values_fill = 0) %>% 
  mutate(positivity = HTS_TST_POS/HTS_TST) %>% 
  ungroup()  

df_index_positivity <- df_index_positivity %>% 
  group_by(snu1) %>% 
  mutate(snu_total = sum(HTS_TST_POS)) %>% 
  group_by(snu1, type_loc) %>% 
  mutate(snu_type = sum(HTS_TST_POS),
         facet_title = glue("{snu1}<br>FY22Q1: {comma(snu_type)}")) %>% 
  ungroup()

df_index_positivity %>% 
  filter(type_loc == "Facility",
         !ageasentered %in% c("<01", "01-04", "05-09", "10-14", "Unknown Age"),
         snu_total > 200) %>% 
  ggplot(aes(positivity, ageasentered, color = sex, fill = sex,size = HTS_TST_POS)) +
  geom_point(shape = 21, alpha = .7) +
  facet_wrap(~fct_reorder(facet_title, snu_total, max, .desc = TRUE), ncol = 9) +
  scale_x_continuous(label = percent) +
  scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa),
                    aesthetics = c("colour", "fill")) +
  scale_size_continuous(label = comma) +
  labs(x = NULL, y = NULL, fill = NULL, color = NULL,
       title = "FACILITY INDEX POSITIVITY BY AGE/SEX",
       caption = glue("Note: Plots from the regions with less than 200 index positive tests in FY21Q1 are removed
         Source: {source_info}
         USAID SI Analytics | Karishma Srikanth")) +
  si_style() +
  theme(panel.spacing.x = unit(.5, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        axis.text.x = element_text(size = 7),
        strip.text = element_markdown())

si_save("Images/COP22_ETH_Index_Positivity_Fac.png") 

df_index_positivity %>% 
  filter(type_loc == "Community",
         !ageasentered %in% c("<01", "01-04", "05-09", "10-14", "Unknown Age"),
         snu_total > 200) %>% 
  ggplot(aes(positivity, ageasentered, color = sex, fill = sex, size = HTS_TST_POS)) +
  geom_point(shape = 21, alpha = .7) +
  facet_wrap(~fct_reorder(facet_title, snu_total, max, .desc = TRUE), ncol = 9) +
  scale_x_continuous(label = percent) +
  scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa),
                    aesthetics = c("colour", "fill")) +
  scale_size_continuous(label = comma) +
  labs(x = NULL, y = NULL, fill = NULL, color = NULL,
       title = "COMMUNITY INDEX POSITIVITY BY AGE/SEX",
       caption = glue("Note: Plots from the regions with less than 200 index positive tests in FY21Q1 are removed
         Source: {source_info}
         USAID SI Analytics | Karishma Srikanth")) +
  si_style() +
  theme(panel.spacing.x = unit(.5, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        axis.text.x = element_text(size = 7),
        strip.text = element_markdown())

si_save("Images/COP22_ETH_Index_Positivity_Comm.png")
