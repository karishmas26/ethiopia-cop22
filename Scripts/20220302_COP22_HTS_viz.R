# PROJECT:  eth-cop22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  COP22 Planning Meeting - Case Finding Visuals
# LICENSE:  MIT
# DATE:     2022-03-02

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(glue)
library(janitor)
library(ggtext)
library(extrafont)
library(patchwork)

# IMPORT AND GLOBALS  ------------------------------------------------------------

df <- si_path() %>%
  return_latest("PSNU_IM_FY20-22_20220211_v1_1_Ethiopia") %>% 
  read_msd()

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

source_info <- source_info()


# MUNGE/VIZ -----------------------------------------------------------------------

#By Modality
df_clean_mod <- df %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         fiscal_year >= 2020) %>% 
  group_by(fiscal_year, indicator, modality) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  #reshape_msd() %>% 
  pivot_wider(names_from = indicator,
              values_from = cumulative,
              names_glue = "{tolower(indicator)}") %>% 
  mutate(positivity = hts_tst_pos/hts_tst,
         val_lab = clean_number(hts_tst_pos, 1),
         val_lab_hts = clean_number(hts_tst))

#HTS Modality

hts_pos_modality_viz <- df_clean_mod %>%
  filter(fiscal_year == 2021) %>% 
  ggplot(aes(fct_reorder(modality, hts_tst_pos, .desc = TRUE))) +
  geom_col(aes(y = hts_tst_pos), fill = scooter) +
  geom_hline(yintercept = 0) +
  geom_text(aes(y = hts_tst_pos,label = val_lab, color = "#505050",
            family = "Source Sans Pro SemiBold", vjust = -.5)) +
  # facet_wrap(~ sex) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  scale_y_continuous(labels = label_number_si(), expand = c(.005, .005)) +
  labs(x = NULL) +
  si_style_nolines() +
  theme(axis.text.y = element_blank())

positivity_modality_viz <- df_clean_mod %>% 
  filter(fiscal_year == 2021) %>% 
  ggplot(aes(fct_reorder(modality, hts_tst_pos, .desc = TRUE), positivity, group = fiscal_year)) +
  geom_point(shape = 21, color = denim, fill = denim, size = 11, stroke = 2) +
  geom_line(size = 1.5, color = denim) +
  geom_text(aes(label = percent(positivity, 1)),
            family = "Source Sans Pro", size = 12/.pt, color = "white") +
  expand_limits(y = .2) +
  # labs(subtitle = glue("As of {latest_pd_art}, {percent(latest_stat_art, 1)} of OVC <18 are on ARTs"),
  #      x = NULL, y = NULL) +
  si_style_nolines() +
  scale_y_continuous(labels = percent) +
  labs(subtitle = NULL,
       x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) + coord_cartesian(expand = F, clip = "off")

positivity_modality_viz / hts_pos_modality_viz + plot_annotation(
  title = glue("HTS Pos Yield and Positivity, by Modality (FY21Q4)"),
  caption = glue("USAID SI Analytics | Source: {source_info}"),
  theme = si_style())

si_save("Graphics/06_hts_pos_yield_modality.svg")


 df_clean_mod %>%
  filter(fiscal_year != 2020,
         modality != "OtherMod" & modality != "VCTMod" & modality != "Post ANC1") %>% 
   mutate(fiscal_year = as.factor(fiscal_year)) %>% 
  ggplot(aes(fct_reorder(modality, hts_tst, .desc = FALSE))) +
  geom_col(aes(y = hts_tst, fill = fiscal_year), position = "dodge") +
  geom_hline(yintercept = 0) +
  geom_text(aes(y = hts_tst + 30000,label = val_lab_hts, color = "#505050",
                family = "Source Sans Pro SemiBold", vjust = -0.3)) +
  # facet_wrap(~ sex) +
  scale_fill_manual(values = c(scooter, golden_sand)) +
  scale_color_identity() +
  coord_flip() +
 # coord_cartesian(clip = "off") +
  scale_y_continuous(labels = label_number_si(), expand = c(.005, .005)) +
  labs(x = NULL,
       title = "HTS Performance by Modality, <span style = 'color:#1e87a5'>FY21</span> and <span style = 'color:#f2bc40'>FY22Q1i</span>",
       subtitle = "Ethiopia COP22 Case Finding Analysis",
       caption = glue("Source: {source_info}
                      USAID SI Analytics | Karishma Srikanth")) +
  si_style_xgrid() +
  theme(plot.title = element_markdown(),
        legend.position = "none")
 
 si_save("Graphics/03_hts_modality.svg")
 


#HTS by partner ----------------------------------------
 
df_clean_partner <- df %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         fiscal_year >= 2020) %>% 
  group_by(fiscal_year, indicator, primepartner) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  #reshape_msd() %>% 
  pivot_wider(names_from = indicator,
              values_from = cumulative,
              names_glue = "{tolower(indicator)}") %>% 
  mutate(positivity = hts_tst_pos/hts_tst,
         val_lab = clean_number(hts_tst_pos, 1))

hts_pos_partner <- df_clean_partner %>%
  filter(fiscal_year == 2021,
         primepartner != "Dedup" & primepartner != "Default",
         !is.na(hts_tst_pos),
         hts_tst_pos > 0) %>% 
  ggplot(aes(fct_reorder(primepartner, hts_tst_pos, .desc = FALSE))) +
  geom_col(aes(y = hts_tst_pos), fill = burnt_sienna) +
  geom_hline(yintercept = 0) +
  geom_text(aes(y = hts_tst_pos,label = val_lab, color = "#505050",
                family = "Source Sans Pro SemiBold", vjust = -.5)) +
  coord_flip() +
  # facet_wrap(~ sex) +
  scale_fill_identity() +
  scale_color_identity() +
  #coord_cartesian(clip = "off") +
  scale_y_continuous(labels = label_number_si(), expand = c(.005, .005)) +
  labs(x = NULL) +
  si_style_xgrid() +
  theme()

positivity_partner_viz <- df_clean_partner %>% 
  filter(fiscal_year == 2021,
         primepartner != "Dedup" & primepartner != "Default",
         !is.na(hts_tst_pos)) %>% 
  ggplot(aes(fct_reorder(primepartner, hts_tst_pos, .desc = FALSE), positivity, group = fiscal_year)) +
  geom_point(shape = 21, color = burnt_sienna_light, fill = burnt_sienna_light, size = 11, stroke = 2) +
  geom_line(size = 1.5, color = burnt_sienna_light) +
  geom_text(aes(label = percent(positivity, 1)),
            family = "Source Sans Pro", size = 12/.pt) +
  expand_limits(y = .2) +
  # labs(subtitle = glue("As of {latest_pd_art}, {percent(latest_stat_art, 1)} of OVC <18 are on ARTs"),
  #      x = NULL, y = NULL) +
  si_style_xgrid() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(subtitle = NULL,
       y = "Positivity", x = NULL) +
  theme(
        axis.text.y = element_blank()) 

 hts_pos_partner + positivity_partner_viz + plot_annotation(
  title = glue("HTS Positives and Yield, by Partner (FY21)"),
  subtitle = "Ethiopia COP22 Case Finding Analysis",
  caption = glue("USAID SI Analytics | Source: {source_info}"),
  theme = si_style())

si_save("Graphics/04_hts_pos_yield_partner.svg")

#HTS POS by Age/Sex --------------------------------

df_hts_age_sex <- df %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         fiscal_year >= 2020) %>% 
  group_by(fiscal_year, indicator, sex, ageasentered) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  #reshape_msd() %>% 
  pivot_wider(names_from = indicator,
              values_from = cumulative,
              names_glue = "{tolower(indicator)}") %>% 
  mutate(positivity = hts_tst_pos/hts_tst,
         val_lab = clean_number(hts_tst_pos, 1),
         val_lab_hts = clean_number(hts_tst),
         fill_color = ifelse(sex == "Female", moody_blue, genoa),
         fill_color_pos = ifelse(sex == "Female", moody_blue_light, genoa_light))

hts_age_pos <- df_hts_age_sex %>% 
  filter(!ageasentered %in% c("Unknown Age", "<01", "01-04", "05-09", "10-14"),
         fiscal_year != 2020) %>% 
  #pivot_wider(names_from = sex, values_from = hts_tst_pos) %>% 
  ggplot(aes(x = ageasentered)) +
  geom_col(aes(y = hts_tst_pos, fill = fill_color), position = "dodge") +
  geom_hline(yintercept = 0) +
  facet_wrap(~fiscal_year) + 
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(y = hts_tst_pos,label = val_lab, color = "#505050",
                family = "Source Sans Pro SemiBold", vjust = -.5)) +
  si_style_ygrid() +
  scale_y_continuous(labels = label_number_si()) +
  # labs(x = NULL, y= NULL,
  #      title = "HTS Positives and Yield, by age and sex",
  #      subtitle = "Ethiopia COP22 Case Finding Analysis",
  #      caption = glue("Source: {source_info}
  #                     USAID SI Analytics | Karishma Srikanth")) +
  theme(plot.title = element_markdown())



hts_age_positivity <- df_hts_age_sex %>% 
  filter(!ageasentered %in% c("Unknown Age", "<01", "01-04", "05-09", "10-14"),
         fiscal_year != 2020) %>%  
  ggplot(aes(ageasentered, positivity, group = sex)) +
  geom_point(aes(fill = fill_color_pos, color = fill_color_pos), shape = 21, size = 11, stroke = 2) +
  geom_line(aes(color = fill_color_pos), size = 1.5) +
  facet_wrap(~fiscal_year) + 
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(label = percent(positivity, 1)),
            family = "Source Sans Pro", size = 12/.pt) +
  #expand_limits(y = .2) +
  # labs(subtitle = glue("As of {latest_pd_art}, {percent(latest_stat_art, 1)} of OVC <18 are on ARTs"),
  #      x = NULL, y = NULL) +
  si_style_ygrid() +
  #coord_flip() +
  scale_y_continuous(labels = percent) +
   labs(subtitle = NULL,
        y = "Positivity", x = NULL) +
  theme(
    axis.text.x = element_blank()) 

hts_age_positivity / hts_age_pos + plot_annotation(
  title = glue("HTS Positives and Yield, by age and sex (FY21)"),
  subtitle = "Ethiopia COP22 Case Finding Analysis",
  caption = glue("USAID SI Analytics | Source: {source_info}"),
  theme = si_style())

si_save("Graphics/06_hts_pos_yield_age_sex.svg")

# df_hts_age_sex %>% 
#   filter(!ageasentered %in% c("Unknown Age", "<01", "01-04", "05-09", "10-14"),
#          fiscal_year != 2020) %>% 
#   pivot_wider(names_from = sex, values_from = hts_tst_pos) %>% 
#   ggplot(aes(x = ageasentered)) +
#   geom_col(aes(y = Female, fill = fill_color), position = position_nudge(x = nudge_space)) +
#   geom_col(aes(y = Male, fill = fill_color), position = position_nudge(x = -nudge_space)) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~fiscal_year) + 
#   scale_fill_identity() +
#   scale_color_identity() +
#   si_style_ygrid() +
#   scale_y_continuous(labels = label_number_si()) +
#   labs(x = NULL,
#        title = "HTS Positives and Yield, by age and sex",
#        subtitle = "Ethiopia COP22 Case Finding Analysis",
#        caption = glue("Source: {source_info}
#                       USAID SI Analytics | Karishma Srikanth")) +
#   theme(plot.title = element_markdown(),
#         legend.position = "none")

# +
#   geom_text(aes(y = hts_tst_pos,label = val_lab, color = "#505050",
#                 family = "Source Sans Pro SemiBold", vjust = -.5)) 


#USAID Trends -----------------------------------------------------------

df_usaid <- df %>% 
  filter(fundingagency == "USAID",
         #fiscal_year != 2020,
    indicator %in% c("HTS_TST","HTS_TST_NEG", "HTS_TST_POS"),
         standardizeddisaggregate %in% 
           c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result")) %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  reshape_msd() %>% 
  pivot_wider(names_from = indicator,
              values_from = value,
              names_glue = "{tolower(indicator)}") %>% 
  mutate(positivity = hts_tst_pos/hts_tst,
         val_lab = clean_number(hts_tst_pos, 1),
         val_lab_hts = clean_number(hts_tst,1))

nudge_space <- 0.125 #set nudge for offset bars

usaid_hts_pos_tst <- df_usaid %>% 
  #pivot_wider(names_from = sex, values_from = hts_tst_pos) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = hts_tst), fill= trolley_grey_light, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = hts_tst_pos), fill= scooter, position = position_nudge(x = nudge_space)) +
  geom_hline(yintercept = 0) +
  #facet_wrap(~fiscal_year) + 
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(y = hts_tst_pos,label = val_lab, color = "#505050",
                family = "Source Sans Pro", vjust = -.5)) +
  geom_text(aes(y = hts_tst,label = val_lab_hts, color = "#505050",
                family = "Source Sans Pro", vjust = -.5)) +
  si_style_ygrid() +
  scale_y_continuous(labels = label_number_si()) +
  # labs(x = NULL, y= NULL,
  #      title = "HTS Positives and Yield, by age and sex",
  #      subtitle = "Ethiopia COP22 Case Finding Analysis",
  #      caption = glue("Source: {source_info}
  #                     USAID SI Analytics | Karishma Srikanth")) +
  theme(plot.title = element_markdown()) +
  labs(subtitle = NULL,
       y = NULL, x = NULL)

usaid_hts_positivity <- df_usaid %>% 
  ggplot(aes(period, positivity, group = period_type)) +
  geom_point(fill = denim, color = denim, shape = 21, size = 11, stroke = 2) +
  geom_line(size = 1.5, color = denim) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(label = percent(positivity, 1)),
            family = "Source Sans Pro", size = 12/.pt, color = "white") +
  #expand_limits(y = .2) +
  # labs(subtitle = glue("As of {latest_pd_art}, {percent(latest_stat_art, 1)} of OVC <18 are on ARTs"),
  #      x = NULL, y = NULL) +
  si_style_nolines() +
  #coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(subtitle = NULL,
       y = NULL, x = NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

usaid_hts_positivity /usaid_hts_pos_tst +  plot_layout(heights = c(1,2)) + plot_annotation(
  title = glue("USAID testing sucessfully adapted to COVID-19 and maintained high yields"),
  subtitle = glue("FY20Q1 - FY22Q1 USAID HTS Pos and Yield"),
  caption = glue("USAID SI Analytics | Source: {source_info}"),
  theme = si_style()) 

si_save("Graphics/usaid-hts-pos-yield.svg")


#Select PSNU --------------------------------------

df_woreda <- df %>% 
  filter(fundingagency == "USAID",
         fiscal_year != 2020,
         indicator %in% c("HTS_TST","HTS_TST_NEG", "HTS_TST_POS"),
         standardizeddisaggregate %in% 
           c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result"),
         psnu %in% c("Debre Markos Town", "Kirkos Woreda 9", "Woldiya Town", "Shewa Robit Town")) %>% 
  group_by(operatingunit, snu1, snu1uid, psnu, psnuuid, fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  reshape_msd() %>% 
  pivot_wider(names_from = indicator,
              values_from = value,
              names_glue = "{tolower(indicator)}") %>% 
  mutate(positivity = hts_tst_pos/hts_tst,
         val_lab = clean_number(hts_tst_pos, 1),
         val_lab_hts = clean_number(hts_tst,1))

df_woreda %>% 
  #pivot_wider(names_from = sex, values_from = hts_tst_pos) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = hts_tst), fill= trolley_grey_light, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = hts_tst_pos), fill= scooter, position = position_nudge(x = nudge_space)) +
  # geom_point(aes(y = positivity, group = period_type),fill = denim, color = denim, shape = 21, size = 11, stroke = 2) +
  # geom_line(aes(y = positivity, group = period_type),size = 1.5, color = denim) +
  geom_hline(yintercept = 0) +
  facet_wrap(~psnu) + 
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(y = hts_tst_pos,label = val_lab, color = "#505050",
                family = "Source Sans Pro", vjust = -.5)) +
  geom_text(aes(y = hts_tst,label = val_lab_hts, color = "#505050",
                family = "Source Sans Pro", vjust = -.5)) +
  si_style_ygrid() +
  scale_y_continuous(labels = label_number_si()) +
  labs(x = NULL, y= NULL,
       title = "Select USAID Sites with High Yield, FY21Q1-FY22Q1",
       subtitle = "Ethiopia COP22 Case Finding Analysis",
       caption = glue("Source: {source_info}
                      USAID SI Analytics | Karishma Srikanth")) +
  theme(plot.title = element_markdown()) 

si_save("Graphics/cop22-eth-high-yield-sites.svg")

df_woreda %>% 
  ggplot(aes(period, positivity, group = period_type)) +
  geom_point(fill = denim, color = denim, shape = 21, size = 11, stroke = 2) +
  geom_line(size = 1.5, color = denim) +
  scale_fill_identity() +
  scale_color_identity() +
  facet_wrap(~psnu) +
  geom_text(aes(label = percent(positivity, 1)),
            family = "Source Sans Pro", size = 12/.pt, color = "white") +
  #expand_limits(y = .2) +
  # labs(subtitle = glue("As of {latest_pd_art}, {percent(latest_stat_art, 1)} of OVC <18 are on ARTs"),
  #      x = NULL, y = NULL) +
  si_style_nolines() +
  #coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(subtitle = NULL,
       y = NULL, x = NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

si_save("Graphics/cop22-eth-high-yield-positivity-site.svg")


#Self Testing-----------------------------------------

# df %>% 
#   filter(fundingagency == "USAID",
#          fiscal_year >= 2021,
#          indicator %in% c("HTS_TST_POS", "HTS_SELF")) %>% 
#   count(indicator, standardizeddisaggregate, otherdisaggregate) %>% view()
#    #      standardizeddisaggregate == "Total Numerator") %>%
#   group_by(indicator, fiscal_year, fundingagency) %>% 
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
#   reshape_msd() %>%
#   mutate(fill_color = ifelse(indicator == "HTS_SELF", scooter, denim))
