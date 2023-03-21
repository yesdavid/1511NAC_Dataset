# file for
# (1) data extraction from prepared images using the outlineR package
# (2) outline metrics extraction


if (!require("remotes")){
  install.packages("remotes")
} 
if (!require("outlineR")){
  remotes::install_github("yesdavid/outlineR",
                          dependencies = T)
  library(outlineR)
} 




####################
# outline extraction
####################


artefact_images_by_region <- 
  list.files(file.path("1_data", "artefact_images_by_region"))

current_region_AR_list <- list()
current_region_BR_list <- list()
current_region_ES_list <- list()
for(current_region in artefact_images_by_region){
  
  
  current_region_folder_path <- file.path("1_data", "artefact_images_by_region", current_region)
  
  tool_types <- list.files(current_region_folder_path)

  
  for(current_tool_type in tool_types){
    
      tps_df <- outlineR::tps_to_df(list.files(file.path(current_region_folder_path, current_tool_type),
                                               pattern = "*.tps",
                                               ignore.case = T,
                                               full.names = T))
      
      single_outlines_list <- outlineR::get_outlines(outpath = file.path(current_region_folder_path,
                                                                         current_tool_type,
                                                                         "single"),
                                                     tps_file_rescale = tps_df)
      
      
      current_Out <- outlineR::combine_outlines(single_outlines_list)
      
      names <- names(current_Out$coo)
      
      
      # # correct wrong image TaxUnit names in ARTEFACTNAME (i.e. the picture's name), as well as in the .TPS scaling factor file!
      # # order: c("wrong", "correct")
      # list_TaxUnits_to_correct <- 
      #   list(c("LAzilian_CH", "LAzil_CH"),
      #        c("BeuroA_SG", "BeurA_SG"),
      #        c("Feder_BOMO", "ABPFeder_BOMO"),
      #        c("ABPTechnocomplex_BOMO", "ABPTishn_BOMO"),
      #        c("FederBrom_NG", "Feder_NG"),
      #        c("LEpipgrav_NEI", "LEpigrav_NEI"),
      #        c("Kami_PL", "Kam_PL"))
      # 
      # for(taxunit_to_correct in 1:length(list_TaxUnits_to_correct)){
      #   names <- gsub(pattern = list_TaxUnits_to_correct[[taxunit_to_correct]][1], 
      #                 replacement = list_TaxUnits_to_correct[[taxunit_to_correct]][2], 
      #                 x = names)
      #   
      #   tps_df$IMAGE <- gsub(pattern = list_TaxUnits_to_correct[[taxunit_to_correct]][1], 
      #                        replacement = list_TaxUnits_to_correct[[taxunit_to_correct]][2], 
      #                        x = tps_df$IMAGE)
      #   
      # }
      # 
      # names(current_Out$coo) <- names
      
      
      current_df <- list()
      current_Out_smoothed_list <- list()
      for(i in 1:length(names)){
        current_img_elements <- strsplit(names[[i]], split = "_")[[1]]
        
        current_df[[i]] <- 
          data.frame(Timeslice = current_img_elements[1],
                     TaxUnit = current_img_elements[2],
                     Region = current_img_elements[3],
                     Site = current_img_elements[4],
                     ShortImageRef = current_img_elements[5],
                     ToolClass = toupper(current_img_elements[6]),
                     KnappingDirection = factor(toupper(current_img_elements[7]),
                                                levels = c("P", "D")),
                     IMAGE = paste0(current_img_elements[1:8], collapse = "_"), # name of the plate
                     ARTEFACTNAME = names[[i]]) # name of the individual artefact on the plate
        
        # 1. smooth curve to avoid digitization noise
        # 2. interpolate semilandmarks to 500 landmarks in total
          current_single_shape <- Momocs::coo_interpolate(Momocs::coo_smoothcurve(current_Out$coo[[i]], 
                                                                                  n = 10),
                                                          n = 500)
        # 3. set the starting landmark up to the tip of the artefact
        current_Out_smoothed_list[[names[[i]]]] <- Momocs::coo_slide(current_single_shape,
                                                                     id = which.max(current_single_shape[,2]))
        
        
      }
      current_Out_df <- do.call(rbind.data.frame, current_df)
      current_Out_df <- dplyr::left_join(current_Out_df, tps_df[,c("IMAGE","SCALE")], by = "IMAGE")
      
      current_Out_df$KnappingDirection <- factor(toupper(current_Out_df$KnappingDirection),
                                                 levels = c("P", "D"))
      
      #re-adjust TS labels
      current_Out_df$Timeslice <- as.character(current_Out_df$Timeslice)
      if(length(current_Out_df[which(current_Out_df$Timeslice == "TS0"),]$Timeslice) > 0){
        current_Out_df[which(current_Out_df$Timeslice == "TS0"),]$Timeslice <- "TS1"
      }
      if(length(current_Out_df[which(current_Out_df$Timeslice == "TS01"),]$Timeslice) > 0){
        current_Out_df[which(current_Out_df$Timeslice == "TS01"),]$Timeslice <- "TS1"
      }
      if(length(current_Out_df[which(current_Out_df$Timeslice == "TS45"),]$Timeslice) > 0){
        current_Out_df[which(current_Out_df$Timeslice == "TS45"),]$Timeslice <- "TS4"
      }
      if(length(current_Out_df[which(current_Out_df$Timeslice == "TS5"),]$Timeslice) > 0){
        current_Out_df[which(current_Out_df$Timeslice == "TS5"),]$Timeslice <- "TS4"
      }
      
      # create TS ranges
      current_Out_df$Timeslice_range_from <- 999
      current_Out_df$Timeslice_range_to <- 999
      
      for(i in 1:nrow(current_Out_df)) {
        current_Timeslice_split <- 
          strsplit(strsplit(
            as.character(current_Out_df[i, "Timeslice"]), 
            split = "TS")[[1]][2], 
            split = "")[[1]]
      
        current_Out_df[i, "Timeslice_range_from"] <- current_Timeslice_split[1]
        current_Out_df[i, "Timeslice_range_to"] <- current_Timeslice_split[length(current_Timeslice_split)]
      }
      
      current_Out_with_df <- Momocs::Out(current_Out_smoothed_list,  # current_Out$coo,
                                         fac = current_Out_df)
    
    if(current_tool_type == "Armature"){
      current_region_AR_list[[current_region]] <- current_Out_with_df
    } else if(current_tool_type == "Borer"){
      current_region_BR_list[[current_region]] <- current_Out_with_df
    } else if(current_tool_type == "Endscraper"){
      current_region_ES_list[[current_region]] <- current_Out_with_df
    }  
    
  }
}

# add meta-data to outlines
Artefact_Site_IDs_discrete_coordinates <- unique(readr::read_csv(file.path("1_data","prepared","Artefact_Site_IDs_discrete_coordinates.csv")))
Macroregion_mean_coords <- readr::read_csv(file.path("1_data","prepared","Macroregion_mean_coords.csv"))

outlines_AR <- outlineR::combine_outlines(current_region_AR_list)
outlines_AR$fac <- dplyr::left_join(outlines_AR$fac, Artefact_Site_IDs_discrete_coordinates, 
                                    by = c("Site" = "Site_ID_discrete")) # add site coordinates
outlines_AR$fac$Region <- factor(outlines_AR$fac$Region,
       levels = Macroregion_mean_coords$Macro_region_code[order(Macroregion_mean_coords$Macro_region_code_mean_Lat, 
                                                                decreasing = T)]) # add region factor, sorted by latitude

outlines_BR <- outlineR::combine_outlines(current_region_BR_list)
outlines_BR$fac <- dplyr::left_join(outlines_BR$fac, Artefact_Site_IDs_discrete_coordinates, 
                                    by = c("Site" = "Site_ID_discrete")) # add site coordinates
outlines_BR$fac$Region <- factor(outlines_BR$fac$Region,
                                 levels = Macroregion_mean_coords$Macro_region_code[order(Macroregion_mean_coords$Macro_region_code_mean_Lat, decreasing = T)]) # add region factor, sorted by latitude

outlines_ES <- outlineR::combine_outlines(current_region_ES_list)
outlines_ES$fac <- dplyr::left_join(outlines_ES$fac, Artefact_Site_IDs_discrete_coordinates, 
                                    by = c("Site" = "Site_ID_discrete")) # add site coordinates
outlines_ES$fac$Region <- factor(outlines_ES$fac$Region,
                                 levels = Macroregion_mean_coords$Macro_region_code[order(Macroregion_mean_coords$Macro_region_code_mean_Lat, 
                                                                                          decreasing = T)]) # add region factor, sorted by latitude


# save outlines + metadata as RDS files; can be read with readRDS()
saveRDS(outlines_AR,
        file = file.path("1_data", "prepared", "outlines_AR.RDS"))
saveRDS(outlines_BR,
        file = file.path("1_data", "prepared", "outlines_BR.RDS"))
saveRDS(outlines_ES,
        file = file.path("1_data", "prepared", "outlines_ES.RDS"))



#############################
# extract metric measurements
#############################

outlines_list <- list(outlines_AR = outlines_AR, 
                      outlines_BR = outlines_BR,
                      outlines_ES = outlines_ES)

for(current_outlines_index in names(outlines_list)){
  current_outlines <- outlines_list[[current_outlines_index]]
  
  
  # center outlines
  current_outlines_centered <- Momocs::coo_center(current_outlines)
  
  
  # select outlines with scaling factor
  current_outlines_centered_wSCALE <- Momocs::filter(current_outlines_centered, 
                                                     !SCALE %in% NA)
  
  # derive metric measurements
  ## length + width; see ?Momocs::coo_lw() for details
  lw <- Momocs::measure(Momocs::Out(current_outlines_centered_wSCALE), 
                        coo_lw)$coe
  
  ## area; see ?Momocs::coo_area() for details
  area <- Momocs::measure(Momocs::Out(current_outlines_centered_wSCALE), 
                          coo_area)$coe
  
  ## perimeter; see ?Momocs::coo_perim() for details
  perimeter <- Momocs::measure(Momocs::Out(current_outlines_centered_wSCALE), 
                               coo_perim)$coe 
  
  ## Haralick's circularity which is less sensible to digitalization noise; see ?Momocs::coo_circularityharalick() for details
  circularityharalick <- Momocs::measure(Momocs::Out(current_outlines_centered_wSCALE), 
                                         coo_circularityharalick)$coe
  
  ## eccentricity of a shape; see ?Momocs::coo_eccentricityeigen() for details
  eccentricityeigen <- Momocs::measure(Momocs::Out(current_outlines_centered_wSCALE), 
                                       coo_eccentricityeigen)$coe 
  
  ## calliper = the longest distance between two points of the shape provided; see ?Momocs::coo_calliper() for details
  calliper <- Momocs::measure(Momocs::Out(current_outlines_centered_wSCALE), 
                              coo_calliper)$coe 
  
  metric_measures_artefactnames_df_list <- list()
  for (i in 1:length(current_outlines_centered_wSCALE)){
    metric_measures_artefactnames_df_list[[i]] <- 
      data.frame(ARTEFACTNAME = strsplit(names(lw[i]),
                                         split = "lw.")[[1]][2],
                 length_cm = round(lw[[i]][1], 
                                   digits = 2),
                 width_cm = round(lw[[i]][2], 
                                  digits = 2),
                 area_cm2 = round(area[i,][[1]], 
                                  digits = 2),
                 perimeter_cm = round(perimeter[i,][[1]], 
                                      digits = 2),
                 calliper_cm = round(calliper[i,][[1]], 
                                     digits = 2),
                 circularity = round(circularityharalick[i,][[1]], 
                                     digits = 2),
                 eccentricityeigen = round(eccentricityeigen[i,][[1]], 
                                           digits = 2))
    
  }
  
  metric_measures_artefactnames_df <- do.call(rbind.data.frame, metric_measures_artefactnames_df_list)
  metric_measures_artefactnames_df$ARTEFACTNAME <- as.character(metric_measures_artefactnames_df$ARTEFACTNAME)
  
  current_outlines_centered_fac <- current_outlines_centered$fac
  current_outlines_centered_fac$ARTEFACTNAME <- as.character(current_outlines_centered_fac$ARTEFACTNAME)
  
  
  ##### symmetry
  current_outlines_centered_scaled <- Momocs::coo_scale(current_outlines_centered)
  
  # harmonic calibration. Estimates the number of harmonics required for the Fourier methods implemented in Momocs. This is the only step in this section that produces data we need in the subsequent step.
  current_outlines_centered_scaled_harmonics <- Momocs::calibrate_harmonicpower_efourier(current_outlines_centered_scaled, plot = F)
  
  # efourier
  current_outlines_centered_scaled_efourier <- Momocs::efourier(current_outlines_centered_scaled,
                                                                nb.h = as.matrix(current_outlines_centered_scaled_harmonics[["minh"]])[[4,1]], # chooses number of harmonics for 99.0%
                                                                norm = F) 
  
  # Momocs::symmetry()
  # calculates several indices on the matrix of coefficients: 
  # AD, the sum of absolute values of harmonic coefficients A and D; 
  # BC same thing for B and C; 
  # amp the sum of the absolute value of all harmonic coefficients and sym which is the ratio of AD over amp. 
  # See ?Momocs::symmetry()  for more details.
  symmetry_df <- as.data.frame(Momocs::symmetry(current_outlines_centered_scaled_efourier))
  
  symmetry_df$ARTEFACTNAME <- rownames(symmetry_df)
  
  metric_measures_artefactnames_df <- dplyr::left_join(metric_measures_artefactnames_df,
                                                       symmetry_df,
                                                       by = "ARTEFACTNAME")
  
  # join with fac
  current_outlines_fac_artefactnames_w_metric_measures <- dplyr::left_join(current_outlines_centered_fac, 
                                                                           metric_measures_artefactnames_df,
                                                                           by = "ARTEFACTNAME")
  
  # save
  write.csv(current_outlines_fac_artefactnames_w_metric_measures,
            file = file.path("1_data", "prepared", paste0(current_outlines_index, "_fac_artefactnames_w_metric_measures.csv")))
  
  current_outlines_centered_w_metric_measures <- Momocs::Out(current_outlines_centered$coo, 
                                                             fac = current_outlines_fac_artefactnames_w_metric_measures)
  saveRDS(current_outlines_centered_w_metric_measures,
          file = file.path("1_data", "prepared", paste0(current_outlines_index, "_centered_w_metric_measures.RDS")))
}









