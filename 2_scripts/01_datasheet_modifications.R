# datasheet
library(ggplot2)
library(dplyr)
library(magrittr)


######################## SITES 
datasheet_sites_raw_loaded <- readr::read_csv(file.path("1_data", "raw", "MASTERTABLE_v7_all_revised_sites.csv"), 
                                       skip = 2)
# datasheet_sites_raw$Unique_row_ID <- c(1:nrow(datasheet_sites_raw_loaded))

datasheet_sites_Abgleich_missingSites <- readr::read_csv(file.path("1_data", "raw", "MASTERTABLE_v7_all_revised_sites_Abgleich_missingSites.csv"))

datasheet_sites_raw <- dplyr::left_join(datasheet_sites_raw_loaded, 
                                    datasheet_sites_Abgleich_missingSites,
                                    by = c("Key_site","Level_layer_concentration"))

# quality score
datasheet_sites_raw$quality_score <- 0
datasheet_sites_raw$quality_rank <- factor(rep(3, times = nrow(datasheet_sites_raw)), levels = c(1,2,3))

for (i in 1:nrow(datasheet_sites_raw)) {
  
  current_score_row <- datasheet_sites_raw[i,]
  
  current_score <- c()
 
  if (current_score_row$Dating_qual == "Reliable" && !is.na(current_score_row$Dating_qual)) {
    current_score <- c(current_score, 1)
  } else {
    current_score <- c(current_score, 0)
  }  
  
  if (current_score_row$Site_strat == "Stratified" && !is.na(current_score_row$Site_strat)) {
    current_score <- c(current_score, 1)
  } else {
    current_score <- c(current_score, 0)
  }  
  
  if (current_score_row$Ass_pos == "Primary/insitu" && !is.na(current_score_row$Ass_pos)) {
    current_score <- c(current_score, 1)
  } else {
    current_score <- c(current_score, 0)
  }  
  
  if (current_score_row$Ass_coh == "Homogeneous" && !is.na(current_score_row$Ass_coh)) {
    current_score <- c(current_score, 1)
  } else {
    current_score <- c(current_score, 0)
  }  
  
  if (current_score_row$Site_excav == "Before WW2" && !is.na(current_score_row$Site_excav)) {
    current_score <- c(current_score, 0)
  } else if (current_score_row$Site_excav == "1980-2000" && !is.na(current_score_row$Site_excav)) {
    current_score <- c(current_score, 1)
  } else if (current_score_row$Site_excav == "After 2000" && !is.na(current_score_row$Site_excav)) {
    current_score <- c(current_score, 2)
  } else {
    current_score <- c(current_score, 0)
  }

  datasheet_sites_raw[i,"quality_score"] <- sum(current_score)
  
  # rank based on score
  if(sum(current_score) >= 5){
    datasheet_sites_raw[i,"quality_rank"] <- factor(1, levels = c(1,2,3))
  } else if (sum(current_score) >= 3){
    datasheet_sites_raw[i,"quality_rank"] <- factor(2, levels = c(1,2,3))
  } else if (sum(current_score) < 3){
    datasheet_sites_raw[i,"quality_rank"] <- factor(3, levels = c(1,2,3))
  }
  
}

readr::write_csv(datasheet_sites_raw,
                 path = file.path("1_data", "prepared", "MASTERTABLE_v7_all_revised_sites_with_outline_siteID.csv"))
datasheet_sites <- datasheet_sites_raw
# datasheet_sites <- readr::read_csv(file = file.path("1_data", "prepared", "MASTERTABLE_v7_all_revised_sites_with_outline_siteID.csv"))


# discretize Site_ID with coordinates to make them fit to the artefact names
datasheet_sites_SiteID_coords <- na.omit(unique(datasheet_sites[,c("Site_ID", 
                                                                   "Macro_region_code", 
                                                                   "Long", 
                                                                   "Lat")]))
datasheet_sites_SiteID_coords$Site_ID <- gsub(" ", "", datasheet_sites_SiteID_coords$Site_ID)

datasheet_sites_SiteID_coords_list <- list()
for(row_index in 1:nrow(datasheet_sites_SiteID_coords)) {
  
  current_row <- as.data.frame(datasheet_sites_SiteID_coords)[row_index,]
  
  current_siteID <- 
  strsplit(current_row$Site_ID, split = ",")[[1]]
  
 if(length(current_siteID) > 1){
   
   current_row_Site_ID_discrete_list <- list()
   for (current_siteID_index in current_siteID) {
     current_row$Site_ID_discrete <- current_siteID_index
     current_row_Site_ID_discrete_list[[current_siteID_index]] <- current_row
   } 
   datasheet_sites_SiteID_coords_list[[row_index]] <- do.call(rbind.data.frame, 
                                                              current_row_Site_ID_discrete_list)
     
  } else {
    current_row$Site_ID_discrete <- current_siteID
    datasheet_sites_SiteID_coords_list[[row_index]] <- current_row
    
 }
  
}
datasheet_sites_SiteID_coords <- do.call(rbind.data.frame, datasheet_sites_SiteID_coords_list)
datasheet_sites_SiteID_coords <- datasheet_sites_SiteID_coords[-which(duplicated(datasheet_sites_SiteID_coords$Site_ID_discrete) == TRUE),]

readr::write_csv(x = datasheet_sites_SiteID_coords[,c("Site_ID_discrete",
                                                      "Long", 
                                                      "Lat")],
                 path = file.path("1_data", "prepared", "Artefact_Site_IDs_discrete_coordinates.csv"))
# datasheet_sites_SiteID_coords <- readr::read_csv(file = file.path("1_data", "prepared", "Artefact_Site_IDs_discrete_coordinates.csv"))


# Macroregion_mean_coords to order macroregions by latitude
Macroregion_mean_coords_list <- list()
for(current_macroregion_index in unique(datasheet_sites$Macro_region_code)) {
  coords <- subset(unique(datasheet_sites),
                   Macro_region_code == current_macroregion_index)
  Macroregion_mean_coords_list[[current_macroregion_index]] <- 
  unique(data.frame(Macro_region = coords$Macro_region,
                    Macro_region_code = current_macroregion_index,
                    Macro_region_code_n = paste0(current_macroregion_index, 
                                                 "\n(n=", 
                                                 nrow(coords), 
                                                 ")"), 
                    Macro_region_code_mean_Long = mean(coords$Long),
                    Macro_region_code_mean_Lat = mean(coords$Lat)))
}
Macroregion_mean_coords_df <- do.call(rbind.data.frame, Macroregion_mean_coords_list)

# order Macroregions by LATITUDE
Macroregion_mean_coords_df$Macro_region_code <- factor(Macroregion_mean_coords_df$Macro_region_code,
                                                       levels = Macroregion_mean_coords_df$Macro_region_code[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                                                   decreasing = T)])
Macroregion_mean_coords_df$Macro_region <- factor(Macroregion_mean_coords_df$Macro_region,
                                                       levels = Macroregion_mean_coords_df$Macro_region[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                                              decreasing = T)])

Macroregion_mean_coords_df <- dplyr::left_join(Macroregion_mean_coords_df, # data sheet
                                               data.frame(Macro_region = levels(Macroregion_mean_coords_df$Macro_region), # df containing the macro region levels and resp. numbers as factors
                                                          Macro_region_number = factor(1:length(levels(Macroregion_mean_coords_df$Macro_region)))),
                                               by = "Macro_region")


readr::write_csv(x = Macroregion_mean_coords_df,
                 path = file.path("1_data", "prepared", "Macroregion_mean_coords.csv"))
# Macroregion_mean_coords_df <- readr::read_csv(file = file.path("1_data", "prepared", "Macroregion_mean_coords.csv"))


datasheet_sites$Macro_region_code <- factor(datasheet_sites$Macro_region_code,
                                            levels = Macroregion_mean_coords_df$Macro_region_code[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                                        decreasing = T)])
datasheet_sites$Macro_region <- factor(datasheet_sites$Macro_region,
                                       levels = Macroregion_mean_coords_df$Macro_region[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                              decreasing = T)])

datasheet_sites <- dplyr::left_join(datasheet_sites, # data sheet
                                    data.frame(Macro_region = levels(datasheet_sites$Macro_region), # df containing the macro region levels and resp. numbers as factors
                                               Macro_region_number = factor(1:length(levels(datasheet_sites$Macro_region)))),
                                    by = "Macro_region")



######################## TECHNOLOGY
datasheet_technology <- readr::read_csv(file.path("1_data","raw","MASTERTABLE_v7_all_revised_technology.csv"), 
                                        skip = 2)

datasheet_technology$Timeslice_range_from <- 999
datasheet_technology$Timeslice_range_to <- 999
datasheet_technology$TaxUnit_unique_TS <- "EMPTY"

for(i in 1:nrow(datasheet_technology)) {
  # add from-to range
  current_Timeslice_split <- 
    strsplit(as.character(datasheet_technology[i, "Timeslice"]), 
             split = "")[[1]]
  
  datasheet_technology[i, "Timeslice_range_from"] <- as.integer(current_Timeslice_split[1])
  datasheet_technology[i, "Timeslice_range_to"] <- as.integer(current_Timeslice_split[length(current_Timeslice_split)])
  
  # add a really _UNIQUE_ TaxUnit name
  if(datasheet_technology[i, "Timeslice_range_from"] != datasheet_technology[i, "Timeslice_range_to"]){
    datasheet_technology[i, "TaxUnit_unique_TS"] <- paste0(datasheet_technology[i, "TaxUnit_unique"], 
                                                           "_", 
                                                           datasheet_technology[i, "Timeslice_range_from"], 
                                                           datasheet_technology[i, "Timeslice_range_to"])
  } else {
    datasheet_technology[i, "TaxUnit_unique_TS"] <- paste0(datasheet_technology[i, "TaxUnit_unique"], 
                                                           "_", 
                                                           datasheet_technology[i, "Timeslice_range_from"])
    }
}

readr::write_csv(datasheet_technology,
                 file.path("1_data","prepared","MASTERTABLE_v7_all_revised_technology_with_TSrange_uniqueID.csv"))



######################## TOOLS
datasheet_tools <- readr::read_csv(file.path("1_data","raw","MASTERTABLE_v7_all_revised_tools.csv"), 
                                   skip = 2)

datasheet_tools$Timeslice_range_from <- 999
datasheet_tools$Timeslice_range_to <- 999
datasheet_tools$TaxUnit_unique_TS <- "EMPTY"

for(i in 1:nrow(datasheet_tools)) {
  # add from-to range
  current_Timeslice_split <- 
    strsplit(as.character(datasheet_tools[i, "Timeslice"]), 
             split = "")[[1]]
  
  datasheet_tools[i, "Timeslice_range_from"] <- as.integer(current_Timeslice_split[1])
  datasheet_tools[i, "Timeslice_range_to"] <- as.integer(current_Timeslice_split[length(current_Timeslice_split)])
  
  # add a really _UNIQUE_ TaxUnit name
  if(datasheet_tools[i, "Timeslice_range_from"] != datasheet_tools[i, "Timeslice_range_to"]){
    datasheet_tools[i, "TaxUnit_unique_TS"] <- paste0(datasheet_tools[i, "TaxUnit_unique"], 
                                                           "_", 
                                                      datasheet_tools[i, "Timeslice_range_from"], 
                                                      datasheet_tools[i, "Timeslice_range_to"])
  } else {
    datasheet_tools[i, "TaxUnit_unique_TS"] <- paste0(datasheet_tools[i, "TaxUnit_unique"], 
                                                           "_", 
                                                           datasheet_tools[i, "Timeslice_range_from"])
  }
}

readr::write_csv(datasheet_tools,
                 file.path("1_data","prepared","MASTERTABLE_v7_all_revised_tools_with_TSrange_uniqueID.csv"))




####################################
# discretize datasheets by timeslice

datasheet_list <- list(sites = datasheet_sites,
                       technology = datasheet_technology,
                       tools = datasheet_tools)
datasheet_discreteTS_list <- list()

for (current_sheet_name in names(datasheet_list)) {
  
  current_datasheet <- datasheet_list[[current_sheet_name]]
  current_datasheet$Timeslice <- gsub(" ", "", current_datasheet$Timeslice) # remove empty spaces 
  current_datasheet$Timeslice_discrete <- 9999 # create new column with placeholder values
  
  current_datasheet_discretizedTS_list <- list()
  
  for (current_sheet_row in 1:nrow(current_datasheet)) {
    
    current_row <- current_datasheet[current_sheet_row,]
    
    if(nchar(current_row$Timeslice) > 1){ # if there is more than one timeslice, ...
      
      current_row_all_timeslices <- strsplit(current_row$Timeslice, 
                                             split = ",")[[1]]
      if(nchar(current_row_all_timeslices[1])>1){
        current_row_all_timeslices <- strsplit(current_row$Timeslice, 
                                               split = "")[[1]]
      }
      
      new_row_with_discrete_ts_list <- list()
      for(current_timeslice in 1:length(current_row_all_timeslices)) {
        current_row$Timeslice_discrete <- as.numeric(current_row_all_timeslices[current_timeslice])
        
        new_row_with_discrete_ts_list[[current_timeslice]] <- current_row
      }
      
      current_datasheet_discretizedTS_list[[current_sheet_row]] <- do.call(rbind.data.frame, 
                                                                           new_row_with_discrete_ts_list)
      
      
    } else {
      current_row$Timeslice_discrete <- as.numeric(current_row$Timeslice)
      current_datasheet_discretizedTS_list[[current_sheet_row]] <- current_row
    }
    
    
  }
  current_df <- do.call(rbind.data.frame, current_datasheet_discretizedTS_list)
  
  current_df$Macro_region_code <- factor(current_df$Macro_region_code,
                                              levels = Macroregion_mean_coords_df$Macro_region_code[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                                          decreasing = T)])
  if(current_sheet_name == "sites"){
    current_df$Macro_region_number <- current_df$Macro_region_number
  }
  current_df$Macro_region <- factor(current_df$Macro_region,
                                         levels = Macroregion_mean_coords_df$Macro_region[order(Macroregion_mean_coords_df$Macro_region_code_mean_Lat, 
                                                                                                decreasing = T)])
  
  
  datasheet_discreteTS_list[[current_sheet_name]] <- current_df
  
  readr::write_csv(x = current_df,
                   path = file.path("1_data","prepared",paste0("datasheet_discreteTS_list_", current_sheet_name, ".csv")))
}
saveRDS(datasheet_discreteTS_list,
        file = file.path("1_data","prepared","datasheet_discreteTS_list.RDS"))

# datasheet_discreteTS_list <- readRDS(file = file.path("1_data","prepared","datasheet_discreteTS_list.RDS"))




# table_1_expert_region_TS_TaxUnit_table <- datasheet_discreteTS_list$sites[,c("Macro_region", 
#                                                                              "TaxUnit", 
#                                                                              "Timeslice", 
#                                                                              "Timeslice_discrete")]
# for(i in 1:nrow(table_1_expert_region_TS_TaxUnit_table)) {
#   
#   if(nchar(table_1_expert_region_TS_TaxUnit_table[i, "Timeslice"][[1]]) > 1){
#     split_TS_range <- 
#     strsplit(table_1_expert_region_TS_TaxUnit_table[i, "Timeslice"][[1]],
#              split = "")[[1]]
#     
#     table_1_expert_region_TS_TaxUnit_table[i, "Timeslice"] <- paste0(split_TS_range[1], 
#                                                                      "-", 
#                                                                      split_TS_range[length(split_TS_range)])
#   }
#   
# }
# 
# 
# table_1_expert_region_TS_TaxUnit_table %>% 
#   dplyr::arrange(Macro_region, Timeslice_discrete) %>% 
#   dplyr::select(Macro_region, TaxUnit,Timeslice) %>% 
#   unique() %>% 
#   write.table(file = file.path("1_data","prepared","table_1_expert_region_TS_TaxUnit_table.csv"),
#               sep = ",")


