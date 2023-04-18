# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  USAID / CDC Woreda analysis
# REF ID:   f263ddae 
# LICENSE:  MIT
# DATE:     2022-12-01
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    
    file_path <- return_latest(folderpath = merdata,
      pattern = "MER_Structured_Datasets_PSNU_IM_FY21-23_20230317_v2_1_Ethiopia")
      
  # Grab metadata
   get_metadata(file_path)
  
   ref_id <- "f263ddae"
   
   data_folder <- "Data/"
   
   clean_number <- function(x, digits = 0){
     dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                      x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                      x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                      TRUE ~ glue("{x}"))
   }

# IMPORT ------------------------------------------------------------------
  
 df <-  read_psd(file_path)
   
  usaid_woreda <-  data_folder %>% 
     return_latest("C&T_Woreda_List_COP21") %>% 
     read_xlsx(sheet = "C&T PSNUs List"
     )

# MUNGE -------------------------------------------------------------------
   
clinical_cdc_psnu <- df %>% 
     clean_agency() %>% 
    filter(funding_agency == "CDC",
           indicator %in% c("TX_CURR"),
           fiscal_year == metadata$curr_fy) %>% 
distinct(psnuuid) %>% 
    pull()
  
# DISTRIBUTION OF SITES
  
 df_sites <- df %>% 
    clean_agency() %>% 
    filter(funding_agency == "CDC",
           #indicator %in% c("TX_CURR"),
           fiscal_year == metadata$curr_fy,
           psnuuid %in% clinical_cdc_psnu,
          # standardizeddisaggregate == "Total Numerator"
    ) %>% 
    select(fiscal_year, funding_agency, snu1, psnu, psnuuid) %>% 
    left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("psnuuid" = "datim_uid")) %>% 
    mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported)) %>% 
   distinct()
 
 
 df_site_viz <- df_sites %>% 
   count(snu1, usaid_supported) %>% 
   pivot_wider(names_from = "usaid_supported", values_from = "n") %>% 
   mutate(`USAID Supported` = ifelse(is.na(`USAID Supported`), 0, `USAID Supported`)) %>% 
   mutate(total = `Not USAID supported` + `USAID Supported`,
          usaid_share = `USAID Supported` / total) %>% 
   mutate(usaid_lab = clean_number(`USAID Supported`),
          total_lab = clean_number(total))
 
 df_site_viz %>% 
   ggplot(aes(x = fct_reorder(snu1, total))) +
   geom_col(aes(y = total),alpha = 0.5, fill = scooter_light, width = 0.5,
            position = position_nudge(x = 0)) +
   geom_col(aes(y = `USAID Supported`), fill = denim, width = 0.5) +
   geom_text(aes(y = `USAID Supported`, label = glue("{usaid_lab}/{total_lab}"),
            # size = 11/.pt, 
             family = "Source Sans Pro", 
             color = grey90k,
            hjust = -0.2,
             vjust = 0.5)) +
   coord_flip() +
   #geom_col(aes(y = HTS_TST_cmltv), width = 0.5, fill = grey50k) +
   #geom_col(aes(y = HTS_TST_POS_cmltv), width = 0.5, fill = "#855C75") +
   si_style_xgrid() +
   scale_color_identity()+
   scale_fill_identity() +
   #  facet_wrap(~usaid_supported) +
   scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
   labs(x = NULL, y = NULL, title = "DISTRIBUTION OF USAID-SUPPORTED & NON-USAID SUPPORTED WOREDAS",
        caption = metadata$caption)
 
 si_save("Graphics/00_psnu_dist.svg")
   
# TX CURR -----------------------------------------------------------    
  
df_viz <-  df %>% 
    clean_agency() %>% 
    filter(funding_agency == "CDC",
           indicator %in% c("TX_CURR"),
           fiscal_year == metadata$curr_fy,
           psnuuid %in% clinical_cdc_psnu,
           standardizeddisaggregate == "Total Numerator"
           ) %>% 
    group_by(fiscal_year, funding_agency, indicator, snu1, psnu, psnuuid) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
    reshape_msd() %>% 
    left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("psnuuid" = "datim_uid")) %>% 
    mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))

df_viz %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total)
  
    
df_viz %>% 
  #filter(period == "FY22Q4") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  scale_y_continuous(limits = c(0, 10000)) +
  scale_color_manual(values = c(scooter_light, denim))


df_viz %>% 
  #filter(period == "FY22Q4") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  #facet_wrap(~snu1) +
  scale_y_continuous(limits = c(0, 10000)) +
  scale_color_manual(values = c(scooter_light, denim))


df_viz %>% 
  filter(snu1 == "Oromia") %>% 
  mutate(fill_color = ifelse(usaid_supported == "USAID Supported", denim, scooter_light)) %>% 
  ggplot(aes(x=reorder(psnu, (value)), y = value)) +
  coord_flip() +
  geom_col(aes(fill = fill_color), width =0.8, show.legend = F) +
  geom_hline(yintercept = 0, size = .5) +
  #geom_vline(xintercept = 126, linetype="dashed", size =0.5) +
  # geom_text_repel(aes(label = txcurr_label), na.rm = TRUE,
  #                 hjust = -0.35,
  #                 family = "Source Sans Pro", size = 10/.pt) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~snu1) +
  si_style() +
  si_style_xgrid() +
  scale_fill_identity()


# CERVICAL CANCER -------------------------------------

df_cxca <-  df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "CXCA"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2021, 2022),
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, psnu, psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))


df_cxca %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total)

df_cxca %>% 
  filter(indicator == "CXCA_SCRN",
         period %in% c("FY22Q2", "FY22Q4")) %>% 
  #filter(period == "FY22Q4") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  #facet_wrap(~snu1) +
  scale_y_continuous(limits = c(0, 1500)) +
  scale_color_manual(values = c(scooter_light, denim))


df_cxca_ag <- df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "CXCA"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2021, 2022),
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, psnu, psnuuid) %>% 
  summarise(across(matches("qtr2|qtr4"), sum, na.rm = TRUE)) %>% 
  pivot_longer(cols = matches("qtr"),
               names_to = "period") %>% 
  spread(indicator, value) %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported)) %>% 
  group_by(fiscal_year, funding_agency, period, usaid_supported) %>% 
  summarise(across(starts_with("CXCA"), sum, na.rm = TRUE),.groups = "drop") %>%
  mutate(tx_rate = CXCA_TX / CXCA_SCRN_POS,
         period = paste0("FY", str_sub(fiscal_year, 3, 4), "Q", str_sub(period, 4))) %>% 
  group_by(fiscal_year, funding_agency, usaid_supported) %>% 
  mutate(order_var = sum(CXCA_SCRN, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ou_order = fct_reorder(usaid_supported, order_var, .desc = T)) 


si_blue <- "#4974a5"
nudge_space  <-  0.15

a <-df_cxca_ag %>% 
  mutate(positivity = CXCA_SCRN_POS/CXCA_SCRN) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = CXCA_SCRN), fill = golden_sand_light,
           width = 0.6) +
  geom_col(aes(y = CXCA_SCRN_POS), fill = golden_sand, width = 0.5, 
           position = position_nudge(x = nudge_space)) + 
  geom_col(aes(y = CXCA_TX), fill = si_blue, width = 0.5, position = position_nudge(x = -nudge_space)) +
  geom_text(aes(y = CXCA_SCRN_POS, label = percent(positivity, 1)),
            size = 12/.pt, family = "Source Sans Pro SemiBold", vjust = -0.5, hjust = -0.5) +
  # geom_text(aes(y = CXCA_SCRN, label = percent(positivity, 1)),
  #           size = 12/.pt, family = "Source Sans Pro SemiBold", vjust = -0.5, hjust = -0.5) +
  si_style_xline() +
  facet_wrap(~usaid_supported) +
  geom_hline(yintercept = seq(1e4, 3.5e4, 1e4), color = "white", size = 0.5) +
  #scale_y_continuous(position = "right", labels = label_number()) +
  scale_y_continuous(position = "right", label = label_number(scale_cut = cut_short_scale())) +
  #theme(strip.text = element_blank()) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL)

b <- df_cxca_ag %>% 
  ggplot(aes(x = period, y = tx_rate, group = usaid_supported)) +
  geom_area(aes(y = 1), fill = "#bdcee2", alpha = 0.5)+
  geom_area(fill = si_blue, alpha = 0.5)+
  geom_line(color = si_blue, size = 2) +
  # geom_textpath(aes(label = "Treatment rate"), hjust = 0.95, vjust = -1, include_line = F)+
  geom_text(aes(label = "Treatment rate", y = 1, x = "FY21Q2"), vjust = -1,
            family = "Source Sans Pro", size = 12/.pt) +
  geom_hline(yintercept = 1, size = 0.25, linetype = "dotted") +
  geom_label(aes(label = percent(tx_rate, 1)), size = 12/.pt, family = "Source Sans Pro SemiBold") +
  si_style_xline() +
  facet_wrap(~usaid_supported) +
  # coord_cartesian(expand = F) +
  theme(strip.text = element_blank()) +
  scale_x_discrete(expand = expansion(add = 0.25))+
  scale_y_continuous(expand = expansion(mult = 0), lim = c(0, 1.5)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank()) +
  labs(caption = glue("Source: {metadata$msd_source}}
                     US Agency for International Development"))

a / b + plot_layout(heights = c(8, 2)) +
  plot_annotation(
    title = "USAID-SUPPORTED WOREDAS HAD SLIGHTLY HIGHER NUMBERS OF CERVICAL CANCER SCREENINGS AND RATES OF POSITIVITY THAN NON-USAID SUPPORTED WOREDAS",
    subtitle = "USAID-supported woredas have slightly lower rates of linkage to treatment for cervical cancer than non-USAID supported woredas",
    caption = metadata$caption,
    theme = si_style())

si_save("Graphics/04_cxca.svg")

# TESTING --------------------------------------------------------------

df_hts <- df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "HTS"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2021, 2022),
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, psnu, psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))



df_hts %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total) %>% view()


df_hts_ag <-  df_hts %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
  pivot_wider(names_from = "indicator")

a <- df_hts_ag %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = HTS_TST), fill = "#e0d4db", width = 0.5,
           position = position_nudge(x = 0.1)) +
  geom_col(aes(y = HTS_TST_POS), fill = "#855C75", width = 0.5) +
  geom_text(aes(y = HTS_TST_POS, label = percent(HTS_TST_POS/HTS_TST, 1)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  #geom_col(aes(y = HTS_TST_cmltv), width = 0.5, fill = grey50k) +
  #geom_col(aes(y = HTS_TST_POS_cmltv), width = 0.5, fill = "#855C75") +
  si_style_ygrid() +
  facet_wrap(~usaid_supported) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL, y = NULL, title = "TESTING POSITIVITY TRENDS",
       caption = metadata$caption)+
  coord_cartesian(expand = F)

b <- df_hts %>% 
  filter(indicator == "HTS_TST_POS",
         #  period %in% c("FY22Q2", "FY22Q4")
  ) %>% 
  #filter(period == "FY22Q4") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  # geom_smooth(aes(x = period, y = value,
  #                 weight = value, group = usaid_supported, color = usaid_supported),
  #             method = "loess",
  #             formula = "y ~ x", se = FALSE, na.rm = TRUE,
  #             size = 1.5) +
  #facet_wrap(~snu1) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_color_manual(values = c(scooter_light, denim)) +
  #theme(legend.text = element_blank(),) +
  labs(x = NULL,
       y = NULL,
       title = "USAID supported woredas identify more HIV positives than Non USAID supported woredas" %>% toupper())

b/a 
si_save("Graphics/05_hts.svg")


# RTT ------------------------------------------------------------------------

df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
         #count(indicator, standardizeddisaggregate) %>% view()
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, psnu, psnuuid) %>% 
  #group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator) %>% 
  #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = "indicator",
              names_glue = "{tolower(indicator)}") %>% 
  group_by(snu1, psnu, psnuuid) %>% 
  mutate(tx_curr_lag1 = lag(tx_curr, 1, order_by = period)) %>% 
ungroup() %>% 
  rowwise() %>% 
  mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  ungroup()


df_rtt <-  df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "RTT"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2022, 2023),
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, psnu, psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))

# 
# df %>% 
#   filter(funding_agency == "CDC",
#          indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_CURR_Lag1", "TX_RTT"), 
#          #count(indicator, standardizeddisaggregate) %>% view()
#          standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>% 
#   group_by(fiscal_year, indicator) %>%
#   #group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator) %>% 
#   #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
#   reshape_msd(include_type = FALSE) %>% 
#   pivot_wider(names_from = "indicator",
#               names_glue = "{tolower(indicator)}") %>% 
#   rowwise() %>% 
#   mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
#   ungroup()

df_rtt_agg <-df_rtt %>% 
  group_by(period, funding_agency, snu1, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total) 


rtt_viz <- function(df, snu) {
 
  df %>% 
    filter(snu1 == snu) %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = total ), fill = "#e0d4db", width = 0.5,
             position = position_nudge(x = -0.1)) +
    geom_col(aes(y = `USAID Supported`), fill = "#855C75", width = 0.5) +
    geom_text(aes(y = `USAID Supported`, label = percent(usaid_share, 1)),
              size = 11/.pt, 
              family = "Source Sans Pro", 
              color = grey90k,
              vjust = -0.5) +
    #geom_col(aes(y = HTS_TST_cmltv), width = 0.5, fill = grey50k) +
    #geom_col(aes(y = HTS_TST_POS_cmltv), width = 0.5, fill = "#855C75") +
    si_style_ygrid() +
    #  facet_wrap(~usaid_supported) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    labs(x = NULL, y = NULL, title = glue("USAID-SUPPORTED WOREDAS IN {snu %>% toupper()} ACCOUNT FOR OVER HALF OF PATIENTS RETURNING TO TREATMENT"),
         caption = metadata$caption)+
    coord_cartesian(expand = F)
  
}

rtt_viz(df_rtt_agg, "Addis Ababa")
rtt_viz(df_rtt_agg, "Oromia")
rtt_viz(df_rtt_agg, "Amhara")


si_save("Graphics/07_rtt_bar.svg")


df_rtt_agg %>% 
  filter(snu1 %in% c("Addis Ababa", "Oromia", "Amhara")) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = `USAID Supported`), fill = denim, width = 0.5) +
  geom_col(aes(y = `Not USAID supported`), fill = denim_light, width = 0.5,
           position = position_nudge(x = -0.2)) +
  geom_text(aes(y = `USAID Supported`, label = clean_number(`USAID Supported`)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  geom_text(aes(y = `Not USAID supported`, label = clean_number(`Not USAID supported`)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = "white",
            #position = position_nudge(x = -0.2),
            vjust = -0.5) +
  facet_wrap(~snu1
             #,nrow = 3, scales = "free_y"
             ) +
  #geom_col(aes(y = HTS_TST_cmltv), width = 0.5, fill = grey50k) +
  #geom_col(aes(y = HTS_TST_POS_cmltv), width = 0.5, fill = "#855C75") +
  si_style_ygrid() +
  #  facet_wrap(~usaid_supported) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL, y = NULL, title = glue("USAID-SUPPORTED WOREDAS ACCOUNT FOR OVER HALF OF PATIENTS RETURNING TO TREATMENT"),
       caption = metadata$caption)+
  coord_cartesian(expand = F)


 df_rtt %>% 
  filter(indicator == "TX_RTT"
         # ,
         # period %in% c("FY22Q2", "FY22Q4")
         ) %>% 
  #filter(period == "FY22Q4") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  #facet_wrap(~snu1) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_color_manual(values = c(scooter_light, denim)) +
  labs(x = NULL, y = NULL, title = "USAID-SUPPORTED WOREDAS HAVE MORE ART PATIENTS RETURN TO TREATMENT THAN NON-USAID SUPPORTED WOREDAS",
       caption = metadata$caption)


si_save("Graphics/07b_rtt_scatter.svg")


  
