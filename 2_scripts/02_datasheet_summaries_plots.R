library(ggplot2)
library(magrittr)


#####################################################################################
########################### Part 2: create output and plots #########################

output_path <- file.path("3_output")

####################################### MAPS ######################################## 

# set manual shapes + colours for 
# region = number
# taxunit = shape 

datasheet_discreteTS_list <- readRDS(file = file.path("1_data","prepared","datasheet_discreteTS_list.RDS"))

datasheet_discreteTS_list$sites$TaxUnit_shape <- factor(rep(0, times = nrow(datasheet_discreteTS_list$sites)), levels = c(0:15))

for(current_macroregion in unique(datasheet_discreteTS_list$sites$Macro_region_code)) {
  current_macroregion_subset <- subset(datasheet_discreteTS_list$sites, Macro_region_code == current_macroregion)
  current_unique_TaxUnits <- unique(current_macroregion_subset$TaxUnit)
  
  for (current_unique_TaxUnits_index in 1:length(current_unique_TaxUnits)) {
    a <- current_unique_TaxUnits[current_unique_TaxUnits_index]
    datasheet_discreteTS_list$sites[which(datasheet_discreteTS_list$sites[,"Macro_region_code"] == current_macroregion & datasheet_discreteTS_list$sites[,"TaxUnit"] == a),"TaxUnit_shape"] <- 
      factor(current_unique_TaxUnits_index, levels = c(0:15))
  }
}


# get base map data for extent
world <- rgeos::gBuffer(rworldmap::getMap(resolution = "high"), byid=TRUE, width=0)
clipper_europe <- as(raster::extent(-9.969482, 26.97, 35.74455, 57.5), "SpatialPolygons")
sp::proj4string(clipper_europe) <- sp::CRS(sp::proj4string(world))
world_clip <- raster::intersect(world, clipper_europe)
world_clip_f <- ggplot2::fortify(world_clip)

base_map <- 
  ggplot() +
  geom_polygon(data = world_clip_f,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "grey") +
  coord_map() +
  theme_classic() +  
  xlab("Longitude") +
  ylab("Latitude") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = "right",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))


##################### map with sites in macroregion color
map_all_sites_RegionCol <-
  base_map +
  labs(color = "Macroregion") +
  geom_text(data = datasheet_discreteTS_list$sites,
            aes(x = Long, y = Lat,
                label=Macro_region_number),
            color = "black",
            # alpha = 0.7,
            size = 2) +
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

# map_all_sites_RegionCol
# 
# ggsave(plot = map_all_sites_RegionCol,
#        filename = file.path(output_path, "map_all_sites.svg"), 
#        width = 30, height = 16, units = "cm")
# ggsave(plot = map_all_sites_RegionCol,
#        filename = file.path(output_path, "map_all_sites.png"), 
#        width = 30, height = 16, units = "cm")

#####################
# sort macroregions by latitude

Macroregion_mean_coords_df <- readr::read_csv(file = file.path("1_data", "prepared", "Macroregion_mean_coords.csv"))

Macroregion_mean_coords_df$Macro_region_number_text <- 
  factor(paste(Macroregion_mean_coords_df$Macro_region_number,
               " ", 
               Macroregion_mean_coords_df$Macro_region),
         levels = paste(Macroregion_mean_coords_df$Macro_region_number,
                        " ", 
                        Macroregion_mean_coords_df$Macro_region)[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                       decreasing = T)])

map_all_sites_plus_center <-
  map_all_sites_RegionCol + 
  geom_label(data = Macroregion_mean_coords_df,
             aes(x = Macro_region_code_mean_Long, 
                 y = Macro_region_code_mean_Lat,
                 label = Macro_region_number), 
             size = 5) + 
  geom_point(data = Macroregion_mean_coords_df,
             aes(x = Macro_region_code_mean_Long, 
                 y = Macro_region_code_mean_Lat,
                 fill = Macro_region_number_text),
             alpha = 0) +
  labs(fill = "Macroregion") + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

map_all_sites_plus_center

ggsave(plot = map_all_sites_plus_center,
       filename = file.path(output_path, "Fig_1A_map_all_sites_plus_center_numbers.png"), 
       width = 30, height = 16, units = "cm")
svg(filename=file.path(output_path, "Fig_1A_map_all_sites_plus_center_numbers.svg"), width = 30*0.39, height = 16*0.39);map_all_sites_plus_center;dev.off()


##################### map with site types, quality scores
map_all_sites_SiteType_Quali <- 
  base_map + 
  labs(fill = "Quality Score",
       shape = "Site Type") +
  geom_point(data = datasheet_discreteTS_list$sites,
             aes(x = Long, y = Lat,
                 fill = quality_score,
                 shape = Site_type),
             alpha = 0.9,
             size = 3) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "darkgreen",
                       midpoint = 3)+
  scale_shape_manual(values=c(21, 22, 24)) + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

map_all_sites_SiteType_Quali

ggsave(plot = map_all_sites_SiteType_Quali,
       filename = file.path(output_path, "Fig_4A_map_all_sites_siteType_quali.png"), 
       width = 30, height = 24, units = "cm")
svg(filename=file.path(output_path, "Fig_4A_map_all_sites_siteType_quali.svg"), width = 30*0.39, height = 24*0.39);map_all_sites_SiteType_Quali;dev.off()




############################################## COUNTS ############################################## 

# taxunits per timeslice from sites sheet
TaxUnits_per_TS_count_df_list <- list()
for(current_timeslice in unique(datasheet_discreteTS_list$sites$Timeslice_discrete)) {
  
  current_timeslice_subset <- subset(datasheet_discreteTS_list$sites, 
                                     Timeslice_discrete == current_timeslice)
  
  TaxUnits_per_TS_count_df_list[[current_timeslice]] <- 
    data.frame(Timeslice = current_timeslice,
               N_unique_TaxUnits = length(unique(current_timeslice_subset$TaxUnit)), 
               TaxUnits = paste0(unique(current_timeslice_subset$TaxUnit), collapse = ","))
}
TaxUnits_per_TS_count_df <-  do.call(rbind.data.frame, TaxUnits_per_TS_count_df_list)


TaxUnits_per_TS_count_df$Timeslice <- gsub("1", "I", TaxUnits_per_TS_count_df$Timeslice )
TaxUnits_per_TS_count_df$Timeslice  <- gsub("2", "II", TaxUnits_per_TS_count_df$Timeslice )
TaxUnits_per_TS_count_df$Timeslice  <- gsub("3", "III", TaxUnits_per_TS_count_df$Timeslice )
TaxUnits_per_TS_count_df$Timeslice  <- gsub("4", "IV", TaxUnits_per_TS_count_df$Timeslice )

TaxUnits_per_TS_count_df$Timeslice <- factor(TaxUnits_per_TS_count_df$Timeslice, 
                                             levels = c("I", "II", "III", "IV"))

TaxUnits_per_TS_count_df[,c(1,2)]

readr::write_csv(TaxUnits_per_TS_count_df,
                 file = file.path(output_path, "TaxUnits_per_TS.csv"))

TaxUnits_per_TS_plot <- 
  ggplot(data = TaxUnits_per_TS_count_df) +
  geom_col(aes(x = Timeslice, 
               y = N_unique_TaxUnits,
               fill = Timeslice)) +
  ylab("Number of unique NACs") +
  theme_bw() +
  xlab("Time-slice") +
  scale_fill_manual(values = c("I" = "#86b1c0",
                               "II" = "#8694c0",
                               "III" = "#9586c0",
                               "IV" = "#b286c0")) +
  theme(legend.position = "none") + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

TaxUnits_per_TS_plot

ggsave(plot = TaxUnits_per_TS_plot,
       filename = file.path(output_path , "TaxUnits_per_TS.png"), 
       width = 10, height = 10, units = "cm")



# taxunits per macroregion
TaxUnits_per_Region_count_df_list <- list()
for(current_macroregion in unique(datasheet_discreteTS_list$sites$Macro_region_code)) {
  
  current_macroregion_subset <- subset(datasheet_discreteTS_list$sites, 
                                       Macro_region_code == current_macroregion)
  
  TaxUnits_per_Region_count_df_list[[current_macroregion]] <- 
    data.frame(Macroregion = current_macroregion,
               N_unique_TaxUnits = length(unique(current_macroregion_subset$TaxUnit)), 
               TaxUnits = paste0(unique(current_macroregion_subset$TaxUnit), collapse = ","))
}
TaxUnits_per_Region_count_df <-  do.call(rbind.data.frame, TaxUnits_per_Region_count_df_list)


TaxUnits_per_Region_count_df$Macroregion <- factor(TaxUnits_per_Region_count_df$Macroregion,
                                                   levels = Macroregion_mean_coords_df$Macro_region_code[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                                               decreasing = T)])

TaxUnits_per_Region_count_df <- na.omit(TaxUnits_per_Region_count_df)

TaxUnits_per_Region_plot <- 
  ggplot(data = TaxUnits_per_Region_count_df) +
  geom_col(aes(x = Macroregion, 
               y = N_unique_TaxUnits
               )) +
  theme_bw() +
  xlab("Region") + 
  ylab("Number of unique NACs") +
  theme(legend.position = "none") + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

TaxUnits_per_Region_plot

ggsave(plot = TaxUnits_per_Region_plot,
       filename = file.path(output_path, "TaxUnits_per_Region.png"), 
       width = 20, height = 10, units = "cm")


# taxunits per macroregion per timeslice
TaxUnits_per_Region_per_TS_count_df_list <- list()

for(current_timeslice in unique(datasheet_discreteTS_list$sites$Timeslice_discrete)) {
  
  current_timeslice_subset <- subset(datasheet_discreteTS_list$sites, 
                                     Timeslice_discrete == current_timeslice)
  
  TaxUnits_per_Region_count_df_list <- list()
  for(current_macroregion in unique(current_timeslice_subset$Macro_region_code)) {
    
    current_macroregion_subset <- subset(current_timeslice_subset, 
                                         Macro_region_code == current_macroregion)
    
    TaxUnits_per_Region_count_df_list[[current_macroregion]] <- 
      data.frame(Timeslice = current_timeslice,
                 Macroregion = current_macroregion,
                 N_unique_TaxUnits = length(unique(current_macroregion_subset$TaxUnit)), 
                 TaxUnits = paste0(unique(current_macroregion_subset$TaxUnit), collapse = ","))
  }
  TaxUnits_per_Region_per_TS_count_df_list[[current_timeslice]] <-  do.call(rbind.data.frame, TaxUnits_per_Region_count_df_list)
  
  
}
TaxUnits_per_Region_per_TS_count_df <-  do.call(rbind.data.frame, TaxUnits_per_Region_per_TS_count_df_list)

TaxUnits_per_Region_per_TS_count_df$Macroregion <- factor(TaxUnits_per_Region_per_TS_count_df$Macroregion,
                                                          levels = Macroregion_mean_coords_df$Macro_region_code[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                                                      decreasing = T)])
TaxUnits_per_Region_per_TS_count_df <- na.omit(TaxUnits_per_Region_per_TS_count_df)

TaxUnits_per_Region_per_TS_count_df$Timeslice <- gsub("1", "I", TaxUnits_per_Region_per_TS_count_df$Timeslice )
TaxUnits_per_Region_per_TS_count_df$Timeslice  <- gsub("2", "II", TaxUnits_per_Region_per_TS_count_df$Timeslice )
TaxUnits_per_Region_per_TS_count_df$Timeslice  <- gsub("3", "III", TaxUnits_per_Region_per_TS_count_df$Timeslice )
TaxUnits_per_Region_per_TS_count_df$Timeslice  <- gsub("4", "IV", TaxUnits_per_Region_per_TS_count_df$Timeslice )

TaxUnits_per_Region_per_TS_count_df$Timeslice <- factor(TaxUnits_per_Region_per_TS_count_df$Timeslice, 
                                             levels = c("I", "II", "III", "IV"))

TaxUnits_per_Region_per_TS_plot <- 
  ggplot(data = TaxUnits_per_Region_per_TS_count_df) +
  geom_col(aes(x = Macroregion, 
               y = N_unique_TaxUnits,
               fill = Timeslice
               )) +
  facet_wrap(~Timeslice) +
  theme_bw() +
  ylab("Number of unique NACs") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("I" = "#86b1c0",
                               "II" = "#8694c0",
                               "III" = "#9586c0",
                               "IV" = "#b286c0")) + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

TaxUnits_per_Region_per_TS_plot

ggsave(plot = TaxUnits_per_Region_per_TS_plot,
       filename = file.path(output_path, "Fig_1C_TaxUnits_per_Region_per_TS.png"), 
       width = 30, height = 15, units = "cm")
svg(filename=file.path(output_path, "Fig_1C_TaxUnits_per_Region_per_TS.svg"), width = 30*0.39, height = 15*0.39);TaxUnits_per_Region_per_TS_plot;dev.off()



#################### quality score by Region by Timeslice
quality_score_by_ts_by_region_list <- list()
for (ts_index in unique(datasheet_discreteTS_list[["sites"]]$Timeslice_discrete)) {
  subset_by_ts <- subset(datasheet_discreteTS_list[["sites"]], Timeslice_discrete == ts_index)
  subset_by_ts <- unique(subset_by_ts)
  
  quality_score_by_region_list <- list()
  for (current_macroregion in unique(subset_by_ts$Macro_region_code)) {
    subset_by_region <- subset(subset_by_ts, Macro_region_code == current_macroregion)
    subset_by_region <- unique(subset_by_region)
    
    quality_score_by_region_list[[current_macroregion]] <-
      data.frame(Macro_region_code = factor(current_macroregion, 
                                            levels =  Macroregion_mean_coords_df$Macro_region_code[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, decreasing = T)]),
                 Macro_region_code_n = factor(paste0(current_macroregion, 
                                                     "\n(n=", 
                                                     nrow(subset_by_region), 
                                                     ")")),
                 TS_discrete = ts_index,
                 quality_median_score = round(median(subset_by_region$quality_score), 
                                   digits = 2)) # quality score
    
  }
  temp_df <- do.call(rbind.data.frame, quality_score_by_region_list)
  quality_score_by_ts_by_region_list[[ts_index]] <- temp_df
}
quality_score_by_ts_by_region_df <- do.call(rbind.data.frame, quality_score_by_ts_by_region_list)

quality_score_by_ts_by_region_df <- dplyr::left_join(quality_score_by_ts_by_region_df,
                                                     datasheet_discreteTS_list$sites[,c("Macro_region_code", "Macro_region_number", "quality_score", "Timeslice_discrete")],
                                                     by = c("Macro_region_code", "TS_discrete" = "Timeslice_discrete"))

quality_score_by_ts_by_region_df$TS_discrete <- gsub(1, "I", quality_score_by_ts_by_region_df$TS_discrete)
quality_score_by_ts_by_region_df$TS_discrete <- gsub(2, "II", quality_score_by_ts_by_region_df$TS_discrete)
quality_score_by_ts_by_region_df$TS_discrete <- gsub(3, "III", quality_score_by_ts_by_region_df$TS_discrete)
quality_score_by_ts_by_region_df$TS_discrete <- gsub(4, "IV", quality_score_by_ts_by_region_df$TS_discrete)

quality_score_by_ts_by_region_plot <- 
ggplot(data = quality_score_by_ts_by_region_df) + 
  ggpointgrid::geom_pointrect(aes(x = Macro_region_code,
                                  y = quality_score,
                                  fill = quality_score),
                              shape = 21,
                              # size = 1,
                              scale_x = 0.2,
                              scale_y = 0.1) +
  geom_point(aes(x = Macro_region_code,
                 y = quality_median_score),
             color = "black",
             shape = 21,
             size = 8) +
  theme_bw() +
  ylab("Quality score") +
  xlab("Macroregion") +
  facet_wrap(~TS_discrete) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "darkgreen",
                       midpoint = 3) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks=c(0:6), limits = c(0, 6)) + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

quality_score_by_ts_by_region_plot

quality_score_by_ts_by_region_df_medians <- unique(quality_score_by_ts_by_region_df %>% 
                                                     dplyr::select(-c("Macro_region_number", "quality_score")))
quality_score_by_ts_by_region_df_medians
readr::write_csv(quality_score_by_ts_by_region_df_medians,
                 file = file.path(output_path, "quality_score_by_ts_by_region_df_medians.csv"))

ggsave(plot = quality_score_by_ts_by_region_plot,
       filename = file.path(output_path, "Fig_4B_quality_score_by_ts_by_region.png"), 
       width = 30, height = 20, units = "cm")
svg(filename=file.path(output_path, "Fig_4B_quality_score_by_ts_by_region.svg"), width = 30*0.39, height = 20*0.39);quality_score_by_ts_by_region_plot;dev.off()



############################################## tile grid binary data ############################################## 

tools <- subset(datasheet_discreteTS_list$tools,
                Macro_region_code == "PL")
technology <- subset(datasheet_discreteTS_list$technology,
                     Macro_region_code == "PL")

# technology

clipped_technology_sorted <- 
technology[,c(which(names(technology) == "LP_reduction_strat_1"):which(names(technology) == "Microburin"), which(names(technology) == "Timeslice_discrete"),which(names(technology) == "TaxUnit_unique_TS"))] 

library(magrittr)
library(dplyr)
library(ggplot2)

# bring into long-format
clipped_technology_sorted_tbl <- 
data.table::melt(data.table::setDT(clipped_technology_sorted), id.vars = c("TaxUnit_unique_TS", "Timeslice_discrete")) %>% 
  as_tibble() 

grid_clipped_technology_sorted_tbl <- 
clipped_technology_sorted_tbl %>%
  mutate(., Timeslice_discrete_factor = factor(Timeslice_discrete, levels = (4:1))) %>% 
  mutate(., TaxUnit_unique_TS_factor = forcats::fct_reorder(clipped_technology_sorted_tbl$TaxUnit_unique_TS, 
                                                            clipped_technology_sorted_tbl$Timeslice_discrete)) %>%
  mutate(., Timeslice_discrete_factor = gsub("1", "I", Timeslice_discrete_factor )) %>%
  mutate(., Timeslice_discrete_factor = gsub("2", "II", Timeslice_discrete_factor )) %>%
  mutate(., Timeslice_discrete_factor = gsub("3", "III", Timeslice_discrete_factor )) %>%
  mutate(., Timeslice_discrete_factor = gsub("4", "IV", Timeslice_discrete_factor )) %>% 
  arrange(., Timeslice_discrete) %>% 
  ggplot(.,
         aes(y = TaxUnit_unique_TS_factor, 
             x = variable,
             fill = value)) +
  geom_tile(color = "black") +
  coord_fixed() +
  labs(x = "Trait variables",
       y = "NACs") +
  theme(axis.text.x = element_blank(),
        # axis.ticks.x = element_blank()
        ) +
  scale_fill_manual(values = c("1" = "grey50", 
                               "0"= "white",
                               # "?"="grey80", 
                               "na"="red")) + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

grid_clipped_technology_sorted_tbl

grid_rect_clipped_technology_sorted_tbl <- 
  grid_clipped_technology_sorted_tbl +
  ggforce::geom_mark_rect(aes(group = Timeslice_discrete_factor,
                              fill = Timeslice_discrete_factor),
                          expand = 0,
                          radius = 0) +
  scale_fill_manual(values = c("1" = "grey50", 
                               "0"= "white",
                               # "?"="grey80", 
                               "na"="red",
                               "I" = "#86b1c0","II" = "#8694c0","III" = "#9586c0","IV" = "#b286c0"))
grid_rect_clipped_technology_sorted_tbl

ggsave(plot = grid_clipped_technology_sorted_tbl,
       filename = file.path(output_path, "Fig_5_3A_grid_clipped_technology_sorted_tbl.png"), 
       width = 20, height = 15, units = "cm")
svg(filename = file.path(output_path, "Fig_5_3A_grid_clipped_technology_sorted_tbl.svg"),width=20*0.39,height=15*0.39);grid_clipped_technology_sorted_tbl;dev.off()

ggsave(plot = grid_rect_clipped_technology_sorted_tbl,
       filename = file.path(output_path, "Fig_5_3A_grid_rect_clipped_technology_sorted_tbl.png"), 
       width = 20, height = 15, units = "cm")
svg(filename = file.path(output_path, "Fig_5_3A_grid_rect_clipped_technology_sorted_tbl.svg"),width=20*0.39,height=15*0.39);grid_rect_clipped_technology_sorted_tbl;dev.off()


# tools

clipped_tools_sorted <- 
tools[,c(which(names(tools) == "A_p"):which(names(tools) == "D_adze_axe"), which(names(tools) == "Timeslice_discrete"),which(names(tools) == "TaxUnit_unique_TS"))]

clipped_tools_sorted_tbl <- 
  data.table::melt(data.table::setDT(clipped_tools_sorted), id.vars = c("TaxUnit_unique_TS", "Timeslice_discrete")) %>% 
  as_tibble() 

grid_clipped_tools_sorted_tbl <- 
clipped_tools_sorted_tbl %>%
  mutate(., Timeslice_discrete_factor = factor(Timeslice_discrete, levels = (1:4))) %>% 
  mutate(., TaxUnit_unique_TS_factor = forcats::fct_reorder(clipped_tools_sorted_tbl$TaxUnit_unique_TS, 
                                                            clipped_tools_sorted_tbl$Timeslice_discrete)) %>%
  mutate(., Timeslice_discrete_factor = gsub("1", "I", Timeslice_discrete_factor )) %>%
  mutate(., Timeslice_discrete_factor = gsub("2", "II", Timeslice_discrete_factor )) %>%
  mutate(., Timeslice_discrete_factor = gsub("3", "III", Timeslice_discrete_factor )) %>%
  mutate(., Timeslice_discrete_factor = gsub("4", "IV", Timeslice_discrete_factor )) %>% 
  arrange(., Timeslice_discrete) %>% 
  ggplot(.,aes(y = TaxUnit_unique_TS_factor, x = variable,
               fill = value)) +
  geom_tile( 
            color = "black") +
  scale_fill_manual(values = c("1" = "grey50", 
                               "0"= "white"#, 
                               # "na"="red"
                               )
                    ) +
  coord_fixed() +
  labs(x = "Trait variables",
       y = "NACs") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        ) + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

grid_clipped_tools_sorted_tbl

grid_rect_clipped_tools_sorted_tbl <- 
  grid_clipped_tools_sorted_tbl +
  ggforce::geom_mark_rect(aes(group = Timeslice_discrete_factor,
                              fill = Timeslice_discrete_factor),
                          expand = 0,
                          radius = 0) +
  scale_fill_manual(values = c("1" = "grey50", "0"= "white",
                               # "na"="red",
                               "I" = "#86b1c0","II" = "#8694c0","III" = "#9586c0","IV" = "#b286c0"))

grid_rect_clipped_tools_sorted_tbl

ggsave(plot = grid_clipped_tools_sorted_tbl,
       filename = file.path(output_path, "Fig_5_3B_grid_clipped_tools_sorted_tbl.png"), 
       width = 15, height = 10, units = "cm")
svg(filename = file.path(output_path, "Fig_5_3B_grid_clipped_tools_sorted_tbl.svg"),width=15*0.39,height=10*0.39);grid_clipped_tools_sorted_tbl;dev.off()

ggsave(plot = grid_rect_clipped_tools_sorted_tbl,
       filename = file.path(output_path, "Fig_5_3B_grid_rect_clipped_tools_sorted_tbl.png"), 
       width = 15, height = 10, units = "cm")
svg(filename = file.path(output_path, "Fig_5_3B_grid_rect_clipped_tools_sorted_tbl.svg"),width=15*0.39,height=10*0.39);grid_rect_clipped_tools_sorted_tbl;dev.off()





