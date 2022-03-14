# PROJECT:  eth-cop22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  COP22 Planning Meeting - OVC Expansion
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
library(googlesheets4)
library(googledrive)
library(janitor)
library(ggtext)

load_secrets()

#Directories and Import ------------------------------------

# Set up paths
merdata <- glamr::si_path("path_msd")
dir_terr <- glamr::si_path("path_raster")
shpdata <- glamr::si_path("path_vector")
datim   <- glamr::si_path("path_datim")

df <- si_path() %>% 
  return_latest("OVC SNUs for_FY23_ to Alex") %>% 
  readxl::read_xlsx(sheet =2) %>% 
  clean_names() %>% 
  mutate(snu = recode(snu, "Addis A." = "Addis Ababa",
                      "Oromiya" = "Oromia"))


df_psnu <- si_path() %>% 
  return_latest("MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1_Ethiopia") %>% 
  read_msd()

#GEODATA ----------------------------------------------------

## Raster data
terr <- gisr::get_raster(path = dir_terr)

# Load the shapefiles to grab boundaries from below
spdf_pepfar <- get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
cntry <- "Ethiopia"

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

plot(spdf_snu)



# MUNGE -------------------------------------------------------------------


# Grab all of snu1 info into a separate dataframe
df_sf_all <- spdf_psnu %>% 
  left_join(., df %>% distinct(), by = c("uid" = "psnuuid"))

# Join the reduced PSNU to the spatial file (keeping on columns where PEPFAR has indicator values)
df_sf <- 
  df %>% 
  left_join(., spdf_psnu, by = c("psnuuid" = "uid")) 

# psnu <- df_psnu %>% 
#   select(psnu, psnuuid)
# 
# df_woreda <- df %>% 
#   left_join(psnu, by = c("psnu")) %>% 
#   distinct() %>% 
#   left_join(spdf_psnu, by = c("psnuuid" = "uid"))
# 
# write_csv(df_woreda, "Dataout/ovc-snu-expansion-eth.csv")


# MAP ----------------------------------------------

region_map <- function(region, save = TRUE) {
  
  map_aa <- df_sf %>% 
    filter(snu == region) %>% 
    ggplot() +
    geom_sf(data = spdf_snu %>% filter(orgunit == region), aes(geometry = geometry), fill = NA, size = 0.7, color = grey90k) +
    geom_sf(data = df_sf_all %>% filter(snu == region), aes(geometry = geometry, fill = ovc_status), color = "white", size = 0.25) +
    geom_sf(data = spdf_snu %>% filter(orgunit == region), aes(geometry = geometry), fill = NA, size = 0.7, color = grey90k) +
    scale_fill_manual(values = c(golden_sand, scooter, old_rose)) + 
    
    #geom_sf(aes(fill = targets, geometry = geometry), color = "white", size = 0.1) +
    # scale_fill_si(palette = "old_roses") +
    labs(x = NULL, y = NULL) +
    si_style_map() +
    guides(fill=guide_legend(title="OVC Program Status",
                             title.theme = element_text(
                               # size = 15,
                               face = "bold",
                               #colour = "red",
                               angle = 0
                             )))
  
  plot_aa <- df %>%
    filter(snu == region) %>%
    group_by(snu) %>%
    count(ovc_status) %>%
    #  filter(snu1 != "_Military Ethiopia") %>%
    mutate(fill_color = case_when(ovc_status == "Transition out OVC" ~ old_rose,
                                  ovc_status == "current operating OVC program" ~ golden_sand,
                                  ovc_status == "SNU for expansion" ~ scooter)) %>%
    ggplot(aes(x = fct_relevel(ovc_status, "Transition out OVC",
                               "SNU for expansion",
                               "current operating OVC program"), y = n, fill = fill_color)) +
    geom_col() +
    facet_wrap(~snu, scales = "free_y",ncol = 2) +
    geom_text(aes(label = n, color = "#505050",
                  family = "Source Sans Pro SemiBold", hjust = -0.2))+
    scale_fill_identity() +
    scale_y_continuous(label = label_number_si()) +
    scale_color_identity() +
    coord_flip() +
    si_style_xgrid() +
    labs(x = NULL, y = NULL) +
    #  si_style_nolines() +
    theme(strip.placement = "outside",
          strip.text.x = element_text(hjust = .5))

 viz <-  map_aa + plot_aa + plot_layout(widths = c(3,2)) +
    plot_annotation(
      title = glue("In FY23, OVC programs will expand to 2 woredas in {region}"),
      caption = glue("
                 USAID SI Analytics | Karishma Srikanth"),
      theme = si_style())
 
 if(save == TRUE){
   si_save(here(dir_graphics,
                glue("ovc-{region}.svg")),
           plot = viz)
 }
  
 return(viz)
}

region_map("Addis Ababa", save = TRUE)
region_map("Tigray", save = TRUE)
region_map("Oromia", save = TRUE)
region_map("Amhara", save = TRUE)

  
#Whole country ------------------
  
  map_eth <- # Viz
    df_sf %>% 
    #  filter(snu == "Oromiya") %>% 
    ggplot() +
    geom_sf(data = spdf_cntry, fill = NA, size = 0.7, color = "white") +
    geom_sf(data = df_sf_all %>% filter(!is.na(psnu)), aes(geometry = geometry, fill = ovc_status), color = "white", alpha = .8) +
    geom_sf(data = spdf_snu, fill = NA, size = 0.7, color = grey90k) +
    geom_sf(data = spdf_cntry, fill = NA, size = 1, color = grey90k) +
    scale_fill_manual(values = c(golden_sand, scooter,old_rose)) + 
    labs(x = NULL, y = NULL) +
    si_style_map() +
    guides(fill=guide_legend(title="OVC Status",
                             title.theme = element_text(
                               # size = 15,
                               face = "bold",
                               #colour = "red",
                               angle = 0
                             )))
  
    plot_eth <- df %>% 
      #filter(snu == "Tigray") %>% 
     # group_by(snu) %>% 
      count(ovc_status) %>% 
      #  filter(snu1 != "_Military Ethiopia") %>% 
      mutate(fill_color = case_when(ovc_status == "Transition out OVC" ~ old_rose,
                                    ovc_status == "current operating OVC program" ~ golden_sand,
                                    ovc_status == "SNU for expansion" ~ scooter)) %>% 
      ggplot(aes(x = fct_relevel(ovc_status, "Transition out OVC",
                                 "SNU for expansion",
                                 "current operating OVC program"), y = n, fill = fill_color)) +
      geom_col() +
      #facet_wrap(~snu, scales = "free_y",ncol = 2) +
      geom_text(aes(label = n, color = "#505050",
                    family = "Source Sans Pro SemiBold", hjust = -0.2))+
      scale_fill_identity() +
      scale_y_continuous(label = label_number_si()) +
      scale_color_identity() +
      coord_flip() +
      si_style_xgrid() +
      labs(x = NULL, y = NULL) +
      #  si_style_nolines() +
      theme(strip.placement = "outside",
            strip.text.x = element_text(hjust = .5))
    
    map_eth + plot_eth + plot_layout(widths = c(3,2)) +
      plot_annotation(
        title = glue("In FY23, OVC programs will expand to 44 woredas across Ethiopia and transition out of 7 woredas"),
        caption = glue("
                 USAID SI Analytics | Karishma Srikanth"),
        theme = si_style())
    
si_save("Graphics/ovc-expansion-eth.svg")
  


