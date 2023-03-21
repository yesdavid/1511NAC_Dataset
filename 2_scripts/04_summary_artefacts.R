library(Momocs)
library(ggplot2)

output_path <- file.path("3_output")

# read outlines
outlines_AR <- readRDS(file.path("1_data", "prepared", "outlines_AR.RDS"))
outlines_BR <- readRDS(file.path("1_data", "prepared", "outlines_BR.RDS"))
outlines_ES <- readRDS(file.path("1_data", "prepared", "outlines_ES.RDS"))


#################
outlines_list <- list(outlines_AR = outlines_AR, 
                      outlines_BR = outlines_BR, 
                      outlines_ES = outlines_ES)


outlines_fac_discretised_ts <- list()
for(current_outlines_name in names(outlines_list)){
  
  current_outlines <- outlines_list[[current_outlines_name]]
  
  # some artefacts fall into a times-lice range
  # we have to discretize the range into the separate time-slices and duplicate the affected rows
  discretized_by_ts_list <- list()
  for(i in 1:nrow(current_outlines$fac)){
    
    current_row <- current_outlines$fac[i,]
    current_row_timeslice_split <- strsplit(as.character(current_row$Timeslice), 
                                            split = "")[[1]]
    ts_range <- c(current_row_timeslice_split[3]:current_row_timeslice_split[length(current_row_timeslice_split)])
    
    list_list <- list()
    for(ts_index in 1:length(ts_range)){
      current_row_modified_ts <- current_row
      current_row_modified_ts$Timeslice_discrete <- paste0("TS", ts_range[ts_index])
      list_list[[ts_index]] <- current_row_modified_ts
    }
    discretized_by_ts_list[[i]] <- do.call(rbind.data.frame, list_list)
  }
  
  discretized_by_ts <- as.data.frame(do.call(rbind.data.frame, discretized_by_ts_list))
  discretized_by_ts$Timeslice_discrete <- factor(discretized_by_ts$Timeslice_discrete,
                                                 levels = c(unique(discretized_by_ts$Timeslice_discrete)))
  
  discretized_by_ts$Timeslice_discrete_roman <- discretized_by_ts$Timeslice_discrete
  discretized_by_ts$Timeslice_discrete_roman <- gsub("TS1", "I", discretized_by_ts$Timeslice_discrete_roman)
  discretized_by_ts$Timeslice_discrete_roman  <- gsub("TS2", "II", discretized_by_ts$Timeslice_discrete_roman )
  discretized_by_ts$Timeslice_discrete_roman  <- gsub("TS3", "III", discretized_by_ts$Timeslice_discrete_roman )
  discretized_by_ts$Timeslice_discrete_roman  <- gsub("TS4", "IV", discretized_by_ts$Timeslice_discrete_roman )
  
  discretized_by_ts$Timeslice_discrete_roman <- factor(discretized_by_ts$Timeslice_discrete_roman , 
                                               levels = c("I", "II", "III", "IV"))
  
  discretized_by_ts$Timeslice_discrete_counts <- "NA"
  for(current_ts_discrete in unique(discretized_by_ts$Timeslice_discrete_roman)){
    discretized_by_ts[discretized_by_ts$Timeslice_discrete_roman == current_ts_discrete, "Timeslice_discrete_counts"] <- 
      paste0(current_ts_discrete, 
             "\n(n=", 
             summary(discretized_by_ts$Timeslice_discrete_roman)[current_ts_discrete][[1]], 
             ")")
  }
  
  discretized_by_ts$Region_counts <- "NA"
  outlines_list[[current_outlines_name]]$fac$Region_counts <- "NA"
  Region_counts_levels_vector_discretized_by_ts <- c()
  Region_counts_levels_vector_outlines_list <- c()
  for(current_region in levels(unique(current_outlines$fac$Region))){
    discretized_by_ts[which(discretized_by_ts$Region == current_region), "Region_counts"] <- 
      paste0(current_region, 
             "\n(n=", 
             summary(discretized_by_ts$Region)[current_region][[1]], 
             ")")
    Region_counts_levels_vector_discretized_by_ts <- c(Region_counts_levels_vector_discretized_by_ts,
                                                       paste0(current_region, 
                                                              "\n(n=", 
                                                              summary(discretized_by_ts$Region)[current_region][[1]], 
                                                              ")"))
    
    
    outlines_list[[current_outlines_name]]$fac[which(outlines_list[[current_outlines_name]]$fac$Region == current_region),"Region_counts"] <- 
      paste0(current_region, 
             "\n(n=", 
             summary(outlines_list[[current_outlines_name]]$fac$Region)[current_region][[1]], 
             ")")
    Region_counts_levels_vector_outlines_list <- c(Region_counts_levels_vector_outlines_list, 
                                                   paste0(current_region, 
                                                          "\n(n=", 
                                                          summary(outlines_list[[current_outlines_name]]$fac$Region)[current_region][[1]], 
                                                          ")"))
  }
  
  outlines_list[[current_outlines_name]]$fac$Region_counts <- factor(outlines_list[[current_outlines_name]]$fac$Region_counts, 
                                                                     levels = Region_counts_levels_vector_outlines_list)
  
  discretized_by_ts$Region_counts <- factor(discretized_by_ts$Region_counts, 
                                            levels = Region_counts_levels_vector_discretized_by_ts)
  
  
  
  outlines_fac_discretised_ts[[current_outlines_name]] <- discretized_by_ts
  
}



# save outlines + metadata as RDS files; can be read with readRDS()
readr::write_csv(outlines_fac_discretised_ts$outlines_AR,
        file = file.path("1_data", "prepared", "outlines_AR_fac_discretised_ts.csv"))
readr::write_csv(outlines_fac_discretised_ts$outlines_BR,
        file = file.path("1_data", "prepared", "outlines_BR_fac_discretised_ts.csv"))
readr::write_csv(outlines_fac_discretised_ts$outlines_ES,
        file = file.path("1_data", "prepared", "outlines_ES_fac_discretised_ts.csv"))



##############################################################################################
# counts per region 
# template
counts_per_region <- 
  ggplot(data = outlines_fac_discretised_ts[["outlines_AR"]]) +
  geom_bar(aes(x = Region_counts, 
               fill = Timeslice_discrete_roman),
           na.rm = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(label = "Region") +
  ylab(label = "Number of unique artefacts") +
  labs(fill = "Time-slice")+
  scale_fill_manual(values = c("I" = "#86b1c0",
                               "II" = "#8694c0",
                               "III" = "#9586c0",
                               "IV" = "#b286c0")) + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

# counts per region: ARMATURE
counts_per_region_AR <-
counts_per_region %+% outlines_fac_discretised_ts[["outlines_AR"]] +
  ggtitle(paste0("Armatures", 
                 " (N=",
                 length(unique(outlines_fac_discretised_ts[["outlines_AR"]]$ARTEFACTNAME)),
                 ")")) 
counts_per_region_AR

# counts per region: BORER
counts_per_region_BR <-
counts_per_region %+% outlines_fac_discretised_ts[["outlines_BR"]] +
  ggtitle(paste0("Borers", 
                 " (N=",
                 length(unique(outlines_fac_discretised_ts[["outlines_BR"]]$ARTEFACTNAME)),
                 ")"))
counts_per_region_BR

# counts per region: ENDSCRAPER
counts_per_region_ES <-
counts_per_region %+% outlines_fac_discretised_ts[["outlines_ES"]] +
  ggtitle(paste0("Endscrapers", 
                 " (N=",
                 length(unique(outlines_fac_discretised_ts[["outlines_ES"]]$ARTEFACTNAME)),
                 ")")) 
counts_per_region_ES

ggsave(plot = counts_per_region_AR,
       filename = file.path(output_path , "Fig_3ALeft_counts_per_region_AR.png"), 
       width = 25, height = 10, units = "cm")
svg(filename= file.path(output_path , "Fig_3ALeft_counts_per_region_AR.svg"),width = 25*0.39, height = 10*0.39);counts_per_region_AR;dev.off()

ggsave(plot = counts_per_region_BR,
       filename = file.path(output_path , "Fig_3CLeft_counts_per_region_BR.png"), 
       width = 25, height = 10, units = "cm")
svg(filename= file.path(output_path , "Fig_3CLeft_counts_per_region_BR.svg"),width = 25*0.39, height = 10*0.39);counts_per_region_BR;dev.off()

ggsave(plot = counts_per_region_ES,
       filename = file.path(output_path , "Fig_3BLeft_counts_per_region_ES.png"), 
       width = 25, height = 10, units = "cm")
svg(filename= file.path(output_path , "Fig_3BLeft_counts_per_region_ES.svg"),width = 25*0.39, height = 10*0.39);counts_per_region_ES;dev.off()
##############################################################################################

# counts per Timeslice_discrete_roman 

# template
counts_per_Timeslice_discrete <- 
  ggplot(data = outlines_fac_discretised_ts[["outlines_AR"]]) +
  geom_bar(aes(x = Timeslice_discrete_counts, 
               fill = Timeslice_discrete_roman),
           na.rm = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(label = "Time-slice") +
  ylab(label = "Number of artefacts falling into discrete time-slices") +
  labs(fill = "Time-slice") +
  scale_fill_manual(values = c("I" = "#86b1c0",
                               "II" = "#8694c0",
                               "III" = "#9586c0",
                               "IV" = "#b286c0")) +
  theme(legend.position = "none") + 
  theme(text = element_text(family = "-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"))

# counts per counts_per_Timeslice_discrete: ARMATURE
counts_per_Timeslice_discrete_AR <-
counts_per_Timeslice_discrete %+% outlines_fac_discretised_ts[["outlines_AR"]] +
  ggtitle(paste0("Number of unique armatures: N=",
                 length(unique(outlines_fac_discretised_ts[["outlines_AR"]]$ARTEFACTNAME))))
counts_per_Timeslice_discrete_AR

# counts per counts_per_Timeslice_discrete: BORER
counts_per_Timeslice_discrete_BR <-
counts_per_Timeslice_discrete %+% outlines_fac_discretised_ts[["outlines_BR"]] +
  ggtitle(paste0("Number of unique borers: N=",
                 length(unique(outlines_fac_discretised_ts[["outlines_BR"]]$ARTEFACTNAME))))
counts_per_Timeslice_discrete_BR

# counts per counts_per_Timeslice_discrete: ENDSCRAPER
counts_per_Timeslice_discrete_ES <-
counts_per_Timeslice_discrete %+% outlines_fac_discretised_ts[["outlines_ES"]] +
  ggtitle(paste0("Number of unique endscrapers: N=",
                 length(unique(outlines_fac_discretised_ts[["outlines_ES"]]$ARTEFACTNAME)))) 
counts_per_Timeslice_discrete_ES

# save
ggsave(plot = counts_per_Timeslice_discrete_AR,
       filename = file.path(output_path , "Fig_3ARight_counts_per_Timeslice_discrete_AR.png"), 
       width = 15, height = 15, units = "cm")
svg(filename=file.path(output_path , "Fig_3ARight_counts_per_Timeslice_discrete_AR.svg"), width=15*0.39, height=15*0.39);counts_per_Timeslice_discrete_AR;dev.off()

ggsave(plot = counts_per_Timeslice_discrete_BR,
       filename = file.path(output_path , "Fig_3CRight_counts_per_Timeslice_discrete_BR.png"), 
       width = 15, height = 15, units = "cm")
svg(filename=file.path(output_path , "Fig_3CRight_counts_per_Timeslice_discrete_BR.svg"), width=15*0.39, height=15*0.39);counts_per_Timeslice_discrete_BR;dev.off()

ggsave(plot = counts_per_Timeslice_discrete_ES,
       filename = file.path(output_path , "Fig_3BRight_counts_per_Timeslice_discrete_ES.png"), 
       width = 15, height = 15, units = "cm")
svg(filename=file.path(output_path , "Fig_3BRight_counts_per_Timeslice_discrete_ES.svg"), width=15*0.39, height=15*0.39);counts_per_Timeslice_discrete_ES;dev.off()




##############################################################################################

# outlines_AR
summary(subset(outlines_fac_discretised_ts$outlines_AR, Timeslice_discrete == "TS1")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_AR, Timeslice_discrete == "TS2")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_AR, Timeslice_discrete == "TS3")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_AR, Timeslice_discrete == "TS4")$Region)

# outlines_BR
summary(subset(outlines_fac_discretised_ts$outlines_BR, Timeslice_discrete == "TS1")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_BR, Timeslice_discrete == "TS2")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_BR, Timeslice_discrete == "TS3")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_BR, Timeslice_discrete == "TS4")$Region)

# outlines_ES
summary(subset(outlines_fac_discretised_ts$outlines_ES, Timeslice_discrete == "TS1")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_ES, Timeslice_discrete == "TS2")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_ES, Timeslice_discrete == "TS3")$Region)
summary(subset(outlines_fac_discretised_ts$outlines_ES, Timeslice_discrete == "TS4")$Region)


##############################################################################################

summary(factor(subset(outlines_fac_discretised_ts$outlines_AR, Timeslice_discrete == "TS4" & Region == "PL")$Site))


svg(filename=file.path(output_path , "Fig_5_4_outlines_TS4_PL_WojnowoAcutIII75_panel.svg"), width = 8, height = 8)
Momocs::panel(Momocs::slice(outlines_AR, ARTEFACTNAME %in% subset(outlines_fac_discretised_ts$outlines_AR, 
                                                                  Timeslice_discrete == "TS4" & Region == "PL" & Site == "WojnowoAcutIII75")$ARTEFACTNAME),
              cols = "#b286c0")
dev.off()


svg(filename=file.path(output_path , "Fig_5_4_outlines_TS4_PL_panel.svg"), width = 8, height = 8)
Momocs::panel(Momocs::slice(outlines_AR, ARTEFACTNAME %in% subset(outlines_fac_discretised_ts$outlines_AR, 
                                                                  Timeslice_discrete == "TS4" & Region == "PL")$ARTEFACTNAME),
              cols = "#b286c0")
dev.off()





