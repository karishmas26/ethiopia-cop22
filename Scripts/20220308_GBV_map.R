# PROJECT:  eth-cop22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  COP22 Planning Meeting - GBV/OVC Map
# LICENSE:  MIT
# DATE:     2022-03-09

#Dependencies -------------------------------------------

library(glamr)
library(gophr)
library(tidyverse)
library(sf)
library(gisr)
library(glitr)
library(scales)
library(patchwork)
library(glue)
library(here)

load_secrets()

#Directories and globals  ------------------------------------

# Set up paths
merdata <- glamr::si_path("path_msd")
rasdata <- glamr::si_path("path_raster")
shpdata <- glamr::si_path("path_vector")
datim   <- glamr::si_path("path_datim")
dir_terr <- glamr::si_path("path_raster")

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_images <- "Images"

source_info <- source_info()

# IMPORT ------------------------------------------------------------------------

# Load the shapefiles to grab boundaries from below
spdf_pepfar <- get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
cntry <- "Ethiopia"


## Raster data
terr <- gisr::get_raster(path = dir_terr)

# cntry_lvl <- glamr::get_ouorglevel(
#   operatingunit = cntry,
#   country = cntry,
#   org_type = "country"
# ) # Cntry_lvl is 3
cntry_lvl = 3
spdf_cntry <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = cntry_lvl)

snu_lvl = 4

spdf_snu <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = snu_lvl)


# psnu_lvl <- glamr::get_ouorglevel(
#   operatingunit = cntry,
#   country = cntry,
#   org_type = "prioritization"
# )
psnu_lvl = 6

spdf_psnu <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = psnu_lvl)

plot(spdf_psnu) # The sf object inside is your psnu level
cntry_map <- plot(spdf_cntry) # ths sf object inside is your country level


#LOAD MSD
df_site <- si_path() %>% 
  return_latest("MER_Structured_Datasets_Site_IM_FY20-22_20220211_v1_1_Ethiopia") %>% 
  read_msd()

#MUNGE ------------------------------------------------------

#Get number of psnus reporting Fy21 cumulative results > 0 for GBV and OVC <18
df_site_clean <- df_site %>% 
 # count(psnu)
  #count(indicator) %>% view()
  filter(fiscal_year == 2021,
         indicator %in% c("GEND_GBV", "OVC_SERV_UNDER_18"),
         standardizeddisaggregate == "Total Numerator") %>% 
 # count(indicator, standardizeddisaggregate)
 group_by(operatingunit, snu1, snu1uid, psnu, psnuuid, indicator) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = indicator, values_from = cumulative) %>% 
  mutate(psnu_cat = case_when(GEND_GBV > 0 & OVC_SERV_UNDER_18 > 0 ~ "Both",
                              GEND_GBV > 0 & (OVC_SERV_UNDER_18  == 0 | is.na(OVC_SERV_UNDER_18)) ~ "GBV",
                              OVC_SERV_UNDER_18 > 0 & (GEND_GBV  == 0 | is.na(GEND_GBV)) ~ "OVC",
                              TRUE ~ "None"))


# psnu_partner <- df_site %>% 
#   # count(psnu)
#   #count(indicator) %>% view()
#   filter(fiscal_year == 2021,
#          indicator %in% c("GEND_GBV"),
#          standardizeddisaggregate == "Total Numerator") %>%
#   # count(indicator, standardizeddisaggregate)
#   group_by(operatingunit, snu1, snu1uid, psnu, psnuuid, indicator, primepartner) %>% 
#   summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
#   select(psnu, psnuuid, primepartner) %>% 
#   distinct()
# 
# #list of partners by GBV psnu
# 
# partner_list <- df_site_clean %>% 
#   left_join(psnu_partner, by = c('psnu', "psnuuid")) %>%
# #  count(psnu_cat)
#   filter(psnu_cat %in% c("GBV")) %>% 
#   count(snu1, psnu, primepartner, psnu_cat, GEND_GBV) %>% 
#   rename(cumulative_result = GEND_GBV) %>% 
#   select(-c(n))
# 
# setdiff(site_psnu, partner_psnu)
# 
# write_csv(partner_list, "Dataout/GBV-psnu-by-partner.csv")


# Grab all of snu1 info into a separate dataframe
df_sf_all <- spdf_psnu %>% 
  left_join(., df_site_clean %>% distinct(psnuuid, snu1, snu1uid, psnu_cat), by = c("uid" = "psnuuid"))

# Join the reduced PSNU to the spatial file (keeping on columns where PEPFAR has indicator values)
df_sf <- 
  df_site_clean %>% 
  left_join(., spdf_psnu, by = c("psnuuid" = "uid")) 

# MAPPING --------------------------------------------------------------

# maps by region

region_map <- function(df, region) {
  
  viz <- df_sf %>% 
    filter(snu1 == region) %>% 
    ggplot() +
    geom_sf(data = spdf_snu %>% filter(orgunit == region), aes(geometry = geometry), fill = NA, size = 0.7, color = grey90k) +
    geom_sf(data = df_sf_all %>% filter(snu1 == region), aes(geometry = geometry, fill = psnu_cat),
            color = "white", size = 0.1) +
    geom_sf(data = spdf_snu %>% filter(orgunit == region), aes(geometry = geometry), fill = NA, size = 0.7, color = grey90k) +
    scale_fill_manual(values = c(genoa, scooter,trolley_grey, golden_sand)) + 
    facet_wrap(~snu1) +
    #geom_sf(aes(fill = targets, geometry = geometry), color = "white", size = 0.1) +
    # scale_fill_si(palette = "old_roses") +
    labs(x = NULL, y = NULL,
         title = "GBV and OVC_SERV<18 Service Alignment") +
    si_style_map() +
    guides(fill=guide_legend(title="PSNU Categorization",
                             title.theme = element_text(
                               # size = 15,
                               face = "bold",
                               #colour = "red",
                               angle = 0
                             )))
  
  si_save(here(dir_images,
              glue("{region}_gbv-ovc-map.png")),
         plot = viz,
         scale = 1, dpi = 320, width = 10, height = 5.625, units = "in")
  
  
}

region_map(df_sf, "Addis Ababa")
region_map(df_sf, "Tigray")
region_map(df_sf, "Amhara")
region_map(df_sf, "Oromia")
region_map(df_sf, "Sidama")



# Maps with terrain base and bar plot ----

basemap <- terrain_map(countries = spdf_cntry,
                       adm0 = spdf_cntry,
                       adm1 = spdf_psnu,
                       mask = TRUE,
                       terr = terr)

# Viz
gviz <- basemap +
  geom_sf(data = df_sf, aes(geometry = geometry, fill = psnu_cat), color = grey10k, alpha = .8) +
  geom_sf(data = spdf_cntry, fill = NA, size = 1.5, color = grey10k) +
  geom_sf(data = spdf_cntry, fill = NA, size = .3, color = grey90k) +
  scale_fill_manual(values = c(genoa, scooter,trolley_grey, golden_sand)) + 
  labs(x = NULL, y = NULL,
       title = "GBV and OVC_SERV<18 Service Alignment, FY21Q4") +
  si_style_map() +
  guides(fill=guide_legend(title="PSNU Categorization",
                           title.theme = element_text(
                            # size = 15,
                             face = "bold",
                             #colour = "red",
                             angle = 0
                           )))


#SNU categories
df_snu_cat <- df_site_clean %>% 
  count(snu1, psnu_cat) 


snu_plot <- df_snu_cat %>% 
  filter(snu1 != "_Military Ethiopia") %>% 
  mutate(fill_color = case_when(psnu_cat == "GBV" ~ scooter,
                                psnu_cat == "OVC" ~ golden_sand,
                                psnu_cat == "Both" ~ genoa,
                                psnu_cat == "None" ~ trolley_grey),
         psnu_cat = recode(psnu_cat, "GBV" = "Only Reporting GBV",
                           "OVC" = "Only Reporting OVC",
                           "Both" = "Both OVC and GBV Reported",
                           "None" = "Neither reported")) %>% 
  ggplot(aes(x = fct_relevel(psnu_cat, "Neither reported",
                             "Both OVC and GBV Reported",
                             "Only Reporting GBV",
                             "Only Reporting OVC"), y = n, fill = fill_color)) +
  geom_col() +
  facet_wrap(~snu1, scales = "free_y",ncol = 2) +
  geom_text(aes(label = n, color = "#505050",
                family = "Source Sans Pro SemiBold", hjust = -0.2))+
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip() +
  si_style_xgrid() +
  labs(x = NULL, y = NULL,
       title = "Number of PSNUs reporting GEND_GBV and/or OVC_SERV<18 by SNU (FY21Q4)") +
#  si_style_nolines() +
  theme(strip.placement = "outside",
        strip.text.x = element_text(hjust = .5))
 
gviz + snu_plot + 
  plot_layout(widths = c(2:3)) +
  plot_annotation(
  #title = "GBV AND OVC SERVICE ALIGNMENT, FY21Q4",
  caption = glue("{source_info}
                 USAID SI Analytics | Karishma Srikanth"),
  theme = si_style())

si_save("Images/20220309-gbv-ovc-map.png")
