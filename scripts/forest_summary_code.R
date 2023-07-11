#-------------------------------------
# Forest Data Summaries: Regeneration
#-------------------------------------

#---- Functions ----
# Write dataframe to shapefile using common settings
write_to_shp <- function(data, x = "X", y = "Y", shp_name){
  st_write(st_as_sf(data, coords = c(x, y), crs = park_crs),
           shp_name, delete_layer = TRUE)#FALSE)
}

#---- Plot event lists ----
plotevs <- do.call(joinLocEvent, args_all)
plotevs_vs <- do.call(joinLocEvent, args_vs)
plotevs_4yr <- plotevs %>% filter(between(SampleYear, from_4yr, to))

#---- Table 1. Regen densities by plot and year ----
reg <- do.call(joinRegenData, 
               args = c(args_all, speciesType = 'native', 
                        canopyForm = 'canopy', units = 'sq.m'))


reg_cycle_table <- reg %>% group_by(Plot_Name, cycle) %>% 
                           summarize(seed_den = round(sum(seed_den, na.rm = TRUE), 2),
                                     sap_den = round(sum(sap_den, na.rm = TRUE), 2),
                                     stock = round(sum(stock, na.rm = TRUE), 2), 
                                     .groups = 'drop') %>% 
                           left_join(plotevs %>% 
                                       select(Plot_Name, cycle, Plot = PlotCode, Panel = PanelCode),
                                     ., by = c("Plot_Name", "cycle"))

reg_cols <- c("seed_den", "sap_den", "stock")
reg_cycle_table[, reg_cols][is.na(reg_cycle_table[, reg_cols])] <- 0 

reg_cycle_wide <- reg_cycle_table %>% 
  pivot_wider(names_from = cycle, values_from = c(seed_den, sap_den, stock))

head(reg_cycle_wide)

write.csv(reg_cycle_wide, 
          paste0(new_path, "tables/", "Table_1_", park, "_regen_by_cycle.csv"), row.names = FALSE)

#---- Map 1 regen by cycle ----
reg_cycle <- reg %>% group_by(Plot_Name, cycle) %>% 
                     summarize(regen_den = sum(regen_den, na.rm = TRUE), .groups = 'drop') %>% 
                     left_join(plotevs %>% select(Plot_Name, xCoordinate, yCoordinate, cycle), ., 
                               by = c("Plot_Name", "cycle")) %>% 
                     arrange(Plot_Name, cycle) %>% 
                     pivot_wider(names_from = cycle, values_from = regen_den, 
                                 names_prefix = "cycle_", values_fill = 0) %>% 
                     rename(X = xCoordinate, Y = yCoordinate) #abbr for shapefile

max(reg_cycle[,4:ncol(reg_cycle)])

write_to_shp(reg_cycle, 
             shp_name = paste0(new_path, "shapefiles/", park, "_regen_by_cycle_", to, ".shp" ))

#---- Map 2 regen by size class ----
reg_sz_cols <- c("seed_15_30cm", "seed_30_100cm", "seed_100_150cm", "seed_p150cm", "sap_den") 

reg_size <- reg %>% group_by(Plot_Name, SampleYear) |> 
  summarize_at(vars(all_of(reg_sz_cols)), sum, na.rm = T)

reg_size_4yr <- reg_size %>% filter(between(SampleYear, from_4yr, to)) %>% 
                             left_join(plotevs_4yr %>% select(Plot_Name, xCoordinate, yCoordinate),
                                       ., by = 'Plot_Name')

reg_size_4yr[, reg_sz_cols][reg_size_4yr[is.na(reg_size_4yr[,reg_sz_cols])]] <- 0
head(reg_size_4yr)

colnames(reg_size_4yr) <- c("Plot_Name", "X", "Y", "SampleYear", 
                             "s15_30", "s30_100", "s100_150", "s150p", "sap") #abbr. for shapefile

reg_size_4yr$total <- rowSums(reg_size_4yr[,5:ncol(reg_size_4yr)])

write_to_shp(reg_size_4yr, 
             shp_name = paste0(new_path, "shapefiles/", park, 
                               "_regen_by_size_class_cycle_", cycle_latest, ".shp"))

#---- Figure 1A Regen trends by size class ----
# Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
reg_vs <- do.call(joinRegenData, 
                  args = c(args_vs, speciesType = 'native', 
                           canopyForm = 'canopy', units = 'sq.m'))

reg_size_cy <- reg_vs %>% group_by(Plot_Name, cycle) %>% 
                          summarize_at(vars(all_of(reg_sz_cols)), sum, na.rm = TRUE) %>% 
                          left_join(plotevs_vs %>% select(Plot_Name, cycle),
                                    ., by = c("Plot_Name", "cycle")) 

reg_size_cy[reg_sz_cols][reg_size_cy[is.na(reg_sz_cols)]] <- 0

# Use forestTrends to generate loess-smoothed CIs
reg_smooth <- map_dfr(reg_sz_cols, 
                      ~case_boot_loess(reg_size_cy, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                      mutate(size_class = .x)
)

# For plot labels
reg_colors <- c("seed_15_30cm" = "#D6D6FF", 
                "seed_30_100cm" = "#8F97E3", 
                "seed_100_150cm" = "#556CC9", 
                "seed_p150cm" = "#244EAD", 
                "sap_den" = "#05e646")

reg_smooth$size_class <- factor(reg_smooth$size_class, 
                                levels = c("seed_15_30cm", "seed_30_100cm", 
                                           "seed_100_150cm", "seed_p150cm", 
                                           "sap_den"))

cycle_labs = c("1" = "Cycle 1: 2006 \u2013 2009",
               "2" = "Cycle 2: 2010 \u2013 2013", 
               "3" = "Cycle 3: 2014 \u2013 2017", 
               "4" = "Cycle 4: 2018 \u2013 2022",
               "5" = "Cycle 5: 2023")

reg_labels <- c("15 \u2013 30 cm", "30 \u2013 100 cm", "100 \u2013 150 cm",
                ">150 cm & < 1 cm DBH", "Saplings: 1 \u2013 9.9 cm DBH")

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = size_class, y = estimate, color = size_class,#linetype = sign, 
                         group = size_class))+ theme_FHM()+
  geom_bar(stat = 'identity', aes(fill = size_class, color = 'DimGrey'))+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = bquote(Stems/m^2))+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs), ncol = 5)+
  scale_color_manual(values = reg_colors, name = "Size Class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "Size Class",
                    labels = reg_labels)+
  scale_x_discrete(name = "Size Class",
                   labels = reg_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

reg_trend_plot
  
ggsave(paste0(new_path, "figures/", "Figure_1A_", park, "_regen_by_size_class_by_cycle.svg"),
       height = 5, width = 7.5, units = 'in')

#---- Figure 1B Diam. dist. trends by size class ----
  # Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
  # Including all species and canopy forms
tree_dd <- do.call(sumTreeDBHDist, args = c(args_vs, status = 'live'))

dbh_cols <- c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9', 
              'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
              'dens_90_99.9', 'dens_100p')

tree_dbh_sm <- map_dfr(dbh_cols, 
                    ~case_boot_loess(tree_dd, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                     mutate(dbh_class = .x)
)

tree_dbh_sm$estimate[tree_dbh_sm$estimate < 0] <- 0

tree_dbh_sm$dbh_class <- factor(tree_dbh_sm$dbh_class, 
                                levels = dbh_cols)
levels(tree_dbh_sm$dbh_class)

dbh_labels <- c("10 \u2013 19.9 cm", 
                "20 \u2013 29.9 cm", 
                "30 \u2013 39.9 cm", 
                "40 \u2013 49.9 cm", 
                "50 \u2013 59.9 cm", 
                "60 \u2013 69.9 cm", 
                "70 \u2013 79.9 cm", 
                "80 \u2013 89.9 cm", 
                "90 \u2013 99.9 cm",
                ">= 100 cm")

dbh_labels <- c("10", 
                "20", 
                "30", 
                "40", 
                "50", 
                "60", 
                "70", 
                "80", 
                "90",
                "100+")

# check flat diameter distribution
head(tree_dbh_sm)

e <- min(tree_dbh_sm$estimate[tree_dbh_sm$estimate > 0])

tree_dbh_sm <- suppressWarnings(# warning is for 'c', which we're not using
  tree_dbh_sm %>% 
  mutate(dbh_class2 = gsub("p", "", dbh_class)) %>% 
  separate(dbh_class2, into = c("a", "b", "c"), sep = "_", remove = FALSE) %>% 
  mutate(class = as.numeric(b)) %>% select(-a, -b, -c, -dbh_class2))

AIC_test <- map_dfr(seq_along(1:5), function(x){
  df <- tree_dbh_sm %>% filter(cycle == x)
  lin_mod <- lm(estimate ~ class, data = df)
  exp_mod <- lm(log(estimate + e) ~ class, data = df)
  aic_check <- data.frame(cycle = x,
                          linear = AIC(lin_mod),
                          exp = AIC(exp_mod) + sum(2*log(df$estimate + e)))
})

AIC_test$best_mod <- ifelse(AIC_test$linear < AIC_test$exp + 4, "linear", "log-linear")
# added 4 to exp because linear is the simpler model. The log-linear model needs
# to be < 4 points lower than linear AIC to count.

cycle_labs_tr = c("1" = ifelse(AIC_test$best_mod[AIC_test$cycle == 1] == 'linear',
                               paste0(cycle_labs[1], "*"), cycle_labs[1]),
                  "2" = ifelse(AIC_test$best_mod[AIC_test$cycle == 2] == 'linear',
                               paste0(cycle_labs[2], "*"), cycle_labs[2]),
                  "3" = ifelse(AIC_test$best_mod[AIC_test$cycle == 3] == 'linear',
                               paste0(cycle_labs[3], "*"), cycle_labs[3]),
                  "4" = ifelse(AIC_test$best_mod[AIC_test$cycle == 4] == 'linear',
                               paste0(cycle_labs[4], "*"), cycle_labs[4]),
                  "5" = ifelse(AIC_test$best_mod[AIC_test$cycle == 5] == 'linear',
                               paste0(cycle_labs[5], "*"), cycle_labs[5]))

dbh_trend_plot <- 
  ggplot(tree_dbh_sm, aes(x = dbh_class, y = estimate))+ theme_FHM()+
  geom_bar(stat = 'identity', fill = "#81B082" , color = 'DimGrey')+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = "Stems/ha")+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs_tr), ncol = 5)+
  scale_color_manual(values = reg_colors, name = "DBH Size Class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "DBH Size Class",
                    labels = reg_labels)+
  scale_x_discrete(name = "DBH Size Class",
                   labels = dbh_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

dbh_trend_plot

ggsave(paste0(new_path, "figures/", "Figure_1B_", park, "_tree_dbh_dist_by_cycle.svg"),
       height = 4.6, width = 7.8, units = 'in')

#---- Map 3 Regen by composition ----
reg_all <- do.call(joinRegenData, args = c(args_4yr, units = 'sq.m'))

reg_spp <- do.call(joinRegenData, args = list(park = 'all', from = 2019, to = 2022))

table(reg_spp$ScientificName, reg_spp$ParkUnit)
table(reg_all$ScientificName)

dom_regspp <- reg_all %>% group_by(Plot_Name, ScientificName) %>% summarize(reg = sum(regen_den, na.rm = T)) %>% 
  group_by(ScientificName) %>% summarize(reg = sum(reg, na.rm = T)) %>% arrange(desc(reg))

dom_regspp

Acer <- c('Acer rubrum', 'Acer saccharinum', 'Acer negundo')#, 'Acer saccharum') # Drop Acer saccharum for MABI
Betula <- c('Betula','Betula alleghaniensis', 'Betula lenta',  'Betula X cearulea ',
            'Betula papyrifera', 'Betula populifolia', 'Betula cordifolia')
Carya <- c('Carya', 'Carya cordiformis', 'Carya glabra', 'Carya ovata', 'Carya tomentosa')
Fraxinus <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', "Fraxinus nigra")
Pinus <- c("Pinus resinosa", "Pinus strobus", "Pinus")
Populus <- c('Populus', 'Populus deltoides', 'Populus grandidentata', 'Populus tremuloides')
Prunus <- c('Prunus', 'Prunus serotina', 'Prunus virginiana')
Quercus <- c('Quercus', 'Quercus (Red group)', 'Quercus (White group)',
             'Quercus alba', 'Quercus bicolor', 'Quercus coccinea',
             'Quercus montana', 'Quercus palustris', 'Quercus rubra',
             'Quercus velutina')
Ulmus <- c("Ulmus", "Ulmus americana", "Ulmus rubra")

Other_Native <- c('Amelanchier',  'Amelanchier arborea', 'Amelanchier laevis',
                  'Celtis occidentalis', 'Cladrastis kentukea', 'Juglans nigra',
                  'Nyssa sylvatica', 'Tilia americana', 'Picea rubens', 'Platanus occidentalis',
                  'Salix', 'Unknown Conifer', 'Sorbus decora',
                  'Unknown Hardwood', 'Unknown Tree - 01', 'Unknown Tree - 03')

Subcanopy <- c('Acer spicatum', 'Acer pensylvanicum',
               'Carpinus caroliniana', 'Cornus florida', 
                'Ilex opaca', 'Juniperus virginiana', 
               'Ostrya virginiana',
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')

if(park == "ROVA"){
Exotic_spp <- c('Aesculus hippocastanum', 'Crataegus', 'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 'Picea abies', "Pinus sylvestris",
                'Rhamnus cathartica', 'Salix alba')
} else if(park %in% c("MABI", "SAGA")){ # so Acer platanoides is split
Exotic_spp <- c('Aesculus hippocastanum', 'Ailanthus altissima', 
                "Alnus serrulata", "Cladrastis kentukea", 
                'Crataegus', 'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus',  'Robinia pseudoacacia',
                'Rhamnus cathartica', 'Salix alba')
} else if(park %in% c("MIMA")){ # so Acer platanoides is split
  Exotic_spp <- c('Aesculus hippocastanum', 'Ailanthus altissima', 
                  "Alnus serrulata", "Cladrastis kentukea", 
                  'Crataegus', 'Malus', 'Malus pumila', 'Morus alba',
                  'Photinia villosa', 'Prunus avium', 'Pyrus',  
                  'Rhamnus cathartica', 'Salix alba')
} else {
Exotic_spp <- c('Acer platanoides', 'Aesculus hippocastanum', 'Ailanthus altissima',
                'Crataegus', 'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 'Picea abies', "Pinus sylvestris",
                'Rhamnus cathartica', 'Salix alba')
  
}
table(reg_all$ScientificName)

if(park == "ROVA"){
  reg_all <- reg_all %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                               ScientificName %in% Betula ~ "BETSPP",
                               ScientificName %in% Carya ~ "CARSPP",
                               ScientificName %in% Fraxinus ~ "FRASPP",
                               ScientificName %in% Pinus ~ "PINSPP",
                               ScientificName %in% Populus ~ "POPSPP",
                               ScientificName %in% Prunus ~ "PRUSPP",
                               ScientificName %in% Quercus ~ "QUESPP",
                               ScientificName %in% c(Other_Native, Ulmus) ~ "OTHNAT",
                               ScientificName %in% c(Exotic_spp, "Robinia pseudoacacia") ~ "EXOTIC",
                               ScientificName %in% Subcanopy ~ "SUBCAN",
                               TRUE ~ toupper(paste0(
                                 substr(word(ScientificName, 1), 1, 3), 
                                 substr(word(ScientificName, 2), 1, 3))))) 
} else if(park == "MABI"){
  reg_all <- reg_all %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                               ScientificName %in% Betula ~ "BETSPP",
                               ScientificName %in% Fraxinus ~ "FRASPP",
                               ScientificName %in% Pinus ~ "PINSPP",
                               ScientificName %in% Prunus ~ "PRUSPP",
                               ScientificName %in% Quercus ~ "QUESPP",
                               ScientificName %in% c(Other_Native, Ulmus, Populus, Carya) ~ "OTHNAT",
                               ScientificName %in% c(Exotic_spp, "Robinia pseudoacacia") ~ "EXOTIC",
                               ScientificName %in% Subcanopy ~ "SUBCAN",
                               TRUE ~ toupper(paste0(
                                 substr(word(ScientificName, 1), 1, 3), 
                                 substr(word(ScientificName, 2), 1, 3))))) 
} else if(park == "SAGA"){
  reg_all <- reg_all %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                               ScientificName %in% Betula ~ "BETSPP",
                               ScientificName %in% Fraxinus ~ "FRASPP",
                               ScientificName %in% Pinus ~ "PINSPP",
                               ScientificName %in% Prunus ~ "PRUSPP",
                               ScientificName %in% Quercus ~ "QUESPP",
                               ScientificName %in% c(Other_Native, Populus, Prunus) ~ "OTHNAT",
                               ScientificName %in% c(Exotic_spp, "Robinia pseudoacacia") ~ "EXOTIC",
                               ScientificName %in% Subcanopy ~ "SUBCAN",
                               TRUE ~ toupper(paste0(
                                 substr(word(ScientificName, 1), 1, 3), 
                                 substr(word(ScientificName, 2), 1, 3)))))  
  
} else if(park == "MIMA"){
  reg_all <- reg_all %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                               ScientificName %in% Betula ~ "BETSPP",
                               ScientificName %in% Carya ~ "CARSPP",
                               ScientificName %in% Fraxinus ~ "FRASPP",
                               ScientificName %in% Pinus ~ "PINSPP",
                               ScientificName %in% Prunus ~ "PRUSPP",
                               ScientificName %in% Quercus ~ "QUESPP",
                               ScientificName %in% c(Other_Native, Populus) ~ "OTHNAT",
                               ScientificName %in% c(Exotic_spp) ~ "EXOTIC",
                               ScientificName %in% Subcanopy ~ "SUBCAN",
                               ScientificName %in% Ulmus ~ "ULMSPP",
                               TRUE ~ toupper(paste0(
                                 substr(word(ScientificName, 1), 1, 3), 
                                 substr(word(ScientificName, 2), 1, 3)))))  
} else {
reg_all <- reg_all %>% 
  mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                             ScientificName %in% Betula ~ "BETSPP",
                             ScientificName %in% Carya ~ "CARSPP",
                             ScientificName %in% Fraxinus ~ "FRASPP",
                             ScientificName %in% Pinus ~ "PINSPP",
                             ScientificName %in% Populus ~ "POPSPP",
                             ScientificName %in% Prunus ~ "PRUSPP",
                             ScientificName %in% Quercus ~ "QUESPP",
                             ScientificName %in% Ulmus ~ "ULMSPP",
                             ScientificName %in% Other_Native ~ "OTHNAT",
                             ScientificName %in% Exotic_spp ~ "EXOTIC",
                             ScientificName %in% Subcanopy ~ "SUBCAN",
                             TRUE ~ toupper(paste0(
                               substr(word(ScientificName, 1), 1, 3), 
                               substr(word(ScientificName, 2), 1, 3))))) 
}
table(reg_all$spp_grp)

reg_wide <- reg_all %>% group_by(Plot_Name, spp_grp) %>% 
  summarize(regen_den = sum(regen_den, na.rm = TRUE), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = "Plot_Name") %>% arrange(spp_grp) %>% 
  pivot_wider(names_from = spp_grp, values_from = regen_den, values_fill = 0) %>% 
  arrange(Plot_Name)

table(reg_all$ScientificName, reg_all$spp_grp)

reg_wide <- if("NONPRE" %in% names(reg_wide)){reg_wide %>% select(-NONPRE)}else{reg_wide} 

reg_wide$total <- rowSums(reg_wide[,4:ncol(reg_wide)])
reg_wide$logtot <- log(reg_wide$total + 1)

names(sort(desc(colSums(reg_wide[,c(4:(ncol(reg_wide)-2))]))))


write_to_shp(reg_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_regen_by_spp_cycle", cycle_latest, ".shp"))

#---- Map 4 Tree canopy composition ----
trees_4yr <- do.call(joinTreeData, args = c(args_4yr, status = 'live'))

#table(joinTreeData(from = 2019, to = 2022)$ScientificName, joinTreeData(from = 2019, to = 2022)$ParkUnit)

table(trees_4yr$ScientificName)

dom_trspp <- trees_4yr %>% group_by(Plot_Name, ScientificName) %>% summarize(ba = sum(BA_cm2, na.rm = T)) %>% 
  group_by(ScientificName) %>% summarize(ba = sum(ba, na.rm = T)) %>% arrange(desc(ba))

dom_trspp

if(park == "ROVA"){
trees_4yr <- trees_4yr %>% 
  mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                             ScientificName %in% Betula ~ "BETSPP",
                             ScientificName %in% Carya ~ "CARSPP",
                             ScientificName %in% Fraxinus ~ "FRASPP",
                             ScientificName %in% Pinus ~ "PINSPP",
                             ScientificName %in% Populus ~ "POPSPP",
#                             ScientificName %in% Prunus ~ "PRUSPP",
                             ScientificName %in% Quercus ~ "QUESPP",
                             ScientificName %in% c(Other_Native, Ulmus, Prunus) ~ "OTHNAT",
                             ScientificName %in% c(Exotic_spp, "Robinia pseudoacacia") ~ "EXOTIC",
                             ScientificName %in% Subcanopy ~ "SUBCAN",
                             TRUE ~ toupper(paste0(
                               substr(word(ScientificName, 1), 1, 3), 
                               substr(word(ScientificName, 2), 1, 3))))) 
} else if(park == "MABI"){
  trees_4yr <- trees_4yr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                               ScientificName %in% Betula ~ "BETSPP",
                               ScientificName %in% Carya ~ "CARSPP",
                               ScientificName %in% Fraxinus ~ "FRASPP",
                               ScientificName %in% Pinus ~ "PINSPP",
                               ScientificName %in% Quercus ~ "QUESPP",
                               ScientificName %in% c(Other_Native, Ulmus, Prunus, Populus,
                                                     "Tilia americana", "Picea rubens") ~ "OTHNAT",
                               ScientificName %in% 
                                 c(Exotic_spp, "Robinia pseudoacacia", 
                                   "Larix decidua", "Pinus sylvestris") ~ "EXOTIC",
                               ScientificName %in% Subcanopy ~ "SUBCAN",
                               TRUE ~ toupper(paste0(
                                 substr(word(ScientificName, 1), 1, 3), 
                                 substr(word(ScientificName, 2), 1, 3))))) 
} else if(park == "SAGA"){
  trees_4yr <- trees_4yr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                               ScientificName %in% Betula ~ "BETSPP",
                               ScientificName %in% Fraxinus ~ "FRASPP",
                               ScientificName %in% Pinus ~ "PINSPP",
                               ScientificName %in% Prunus ~ "PRUSPP",
                               ScientificName %in% Quercus ~ "QUESPP",
                               ScientificName %in% c(Other_Native, Populus, Prunus) ~ "OTHNAT",
                               ScientificName %in% c(Exotic_spp, "Robinia pseudoacacia") ~ "EXOTIC",
                               ScientificName %in% Subcanopy ~ "SUBCAN",
                               TRUE ~ toupper(paste0(
                                 substr(word(ScientificName, 1), 1, 3), 
                                 substr(word(ScientificName, 2), 1, 3)))))  
} else if(park == "MIMA"){
  trees_4yr <- trees_4yr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                               ScientificName %in% Betula ~ "BETSPP",
                               ScientificName %in% Carya ~ "CARSPP",
                               ScientificName %in% Fraxinus ~ "FRASPP",
                               ScientificName %in% Pinus ~ "PINSPP",
                               ScientificName %in% Prunus ~ "PRUSPP",
                               ScientificName %in% Quercus ~ "QUESPP",
                               ScientificName %in% c(Other_Native, Populus) ~ "OTHNAT",
                               ScientificName %in% c(Exotic_spp, "Alnus serrulata", 
                                                     "Catalpa speciosa") ~ "EXOTIC",
                               ScientificName %in% Subcanopy ~ "SUBCAN",
                               ScientificName %in% Ulmus ~ "ULMSPP",
                               TRUE ~ toupper(paste0(
                                 substr(word(ScientificName, 1), 1, 3), 
                                 substr(word(ScientificName, 2), 1, 3)))))  
} else {
trees_4yr <- trees_4yr %>% 
  mutate(spp_grp = case_when(ScientificName %in% Acer ~ "ACESPP",
                             ScientificName %in% Betula ~ "BETSPP",
                             ScientificName %in% Carya ~ "CARSPP",
                             ScientificName %in% Fraxinus ~ "FRASPP",
                             ScientificName %in% Pinus ~ "PINSPP",
                             ScientificName %in% Populus ~ "POPSPP",
                             ScientificName %in% Prunus ~ "PRUSPP",
                             ScientificName %in% Quercus ~ "QUESPP",
                             ScientificName %in% Ulmus ~ "ULMSPP",
                             ScientificName %in% Other_Native ~ "OTHNAT",
                             ScientificName %in% Exotic_spp ~ "EXOTIC",
                             ScientificName %in% Subcanopy ~ "SUBCAN",
                             TRUE ~ toupper(paste0(
                               substr(word(ScientificName, 1), 1, 3), 
                               substr(word(ScientificName, 2), 1, 3))))) 
}

tree_wide <- trees_4yr %>% group_by(Plot_Name, spp_grp) %>% 
  summarize(BAm2ha = sum(BA_cm2, na.rm = TRUE)/400, .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = "Plot_Name") %>% arrange(spp_grp) %>% 
  pivot_wider(names_from = spp_grp, values_from = BAm2ha, values_fill = 0) 

tree_wide$total <- rowSums(tree_wide[,4:ncol(tree_wide)])
tree_wide$logtot <- log(tree_wide$total + 1)

names(tree_wide)
write_to_shp(tree_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_tree_by_spp_cycle", cycle_latest, ".shp"))

#---- Map 5 Regen stocking index ----
reg_4yr <- do.call(joinRegenData,
                   args = c(args_4yr, speciesType = 'native',
                            canopyForm = 'canopy', units = 'sq.m'))

reg_4yr_stock <- reg_4yr %>% group_by(Plot_Name) %>% 
                             summarize(stock = sum(stock, na.rm = T)) %>% 
                 left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
                           ., by = "Plot_Name")

write_to_shp(reg_4yr_stock, shp_name = 
         paste0(new_path, "shapefiles/", park, "_stocking_index_cycle_",
                cycle_latest, ".shp"))

#---- Map 6 Deer Browse Index ----
stand_4yr <- do.call(joinStandData, args_4yr) 

dbi <- left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
                 stand_4yr, by = "Plot_Name") %>% 
       select(Plot_Name, X, Y, DBI = Deer_Browse_Index)

mean(dbi$DBI) #SAGA = 3.67 

write_to_shp(dbi, shp_name = 
         paste0(new_path, "shapefiles/", park, "_dbi_cycle_", cycle_latest, ".shp"))


#---- Map 7 Invasive % Cover by Cycle ----
invcov <- do.call(joinQuadSpecies, args = c(args_all, speciesType = 'invasive')) %>% 
  select(Plot_Name, cycle, ScientificName, quad_avg_cov) %>% 
  group_by(Plot_Name, cycle) %>% summarize(quad_cov = sum(quad_avg_cov), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate, cycle),
            ., by = c("Plot_Name", "cycle")) %>% 
  mutate(quad_cov = replace_na(quad_cov, 0))

invcov_wide <- invcov %>% pivot_wider(names_from = cycle, values_from = quad_cov)

write_to_shp(invcov_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_inv_cover_by_cycle.shp"))

allcov <- do.call(joinQuadSpecies, args = c(args_4yr, speciesType = 'all')) %>% 
  select(Plot_Name, cycle, ScientificName, quad_avg_cov, Exotic)

invspp <- prepTaxa() %>% select(ScientificName, InvasiveNETN)

covsum <- left_join(allcov, invspp, by = "ScientificName") %>% 
  filter(!(InvasiveNETN == FALSE & Exotic == TRUE)) %>% # native vs. invasive
  group_by(Plot_Name, InvasiveNETN) %>% 
  summarize(avgcov = sum(quad_avg_cov, na.rm = TRUE), .groups = 'drop') %>% 
  group_by(InvasiveNETN) %>% summarize(avg_cov = sum(avgcov)/29)

#---- Map 8 Invasive % Cover by Species ----
# Lump some species in the same genus
invspp_4yr <- joinQuadSpecies(from = from_4yr, to = to, speciesType = 'invasive') %>% 
  select(ScientificName) %>% unique() %>% arrange(ScientificName)

Ligustrum = c("Ligustrum", "Ligustrum obtusifolium", "Ligustrum vulgare")
Lonicera = c("Lonicera - Exotic", "Lonicera morrowii", "Lonicera X bella")
Vincetoxicum = c("Vincetoxicum", "Vincetoxicum hirundinaria", "Vincetoxicum nigrum",
                 "Vincetoxicum rossicum")

#sort(unique(invspp_4yr$ScientificName))

invspp1 <- do.call(joinQuadSpecies, 
                   #args = list(from = 2017, to = 2022, speciesType = 'invasive')) %>% 
                   args = c(args_4yr, speciesType = 'invasive')) %>% 
  select(Plot_Name, ScientificName, quad_avg_cov) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name), ., by = "Plot_Name") %>% 
  mutate(quad_avg_cov = replace_na(quad_avg_cov, 0),
         present = ifelse(quad_avg_cov > 0, 1, 0),
         ScientificName = case_when(
           ScientificName %in% Ligustrum ~ "Ligustrum",
           ScientificName %in% Lonicera ~ "Lonicera - Exotic",
           ScientificName %in% Vincetoxicum ~ "Vincetoxicum",
           TRUE ~ ScientificName)) %>% 
  arrange(Plot_Name, ScientificName)

# Determine 12 most common invasives in a park by cover
plotspp_df <- data.frame(expand.grid(unique(invspp1$Plot_Name), 
                                     unique(invspp1$ScientificName), 
                                     stringsAsFactors = FALSE)) %>% 
  rename(Plot_Name = Var1, ScientificName = Var2) %>% 
  filter(!is.na(ScientificName))

topspp <- invspp1 %>% left_join(plotspp_df, ., by = c('Plot_Name', 'ScientificName')) %>% 
  arrange(Plot_Name, ScientificName) %>% 
  mutate(quad_avg_cov = replace_na(quad_avg_cov, 0),
         present = replace_na(present, 0)) %>% 
  group_by(ScientificName) %>% 
  summarize(avg_cov = mean(quad_avg_cov, na.rm = T),
            num_plots = sum(present), .groups = 'drop') %>% 
  arrange(desc(avg_cov)) %>% slice(1:12) %>% select(ScientificName)

# Prep for shapefile
invspp <- invspp1 %>% 
  mutate(ScientificName = case_when(
    ScientificName %in% topspp$ScientificName ~ ScientificName, 
    ScientificName == "None present" ~ "None present", 
    TRUE ~ "Other invasive")) %>% 
  group_by(Plot_Name, ScientificName) %>% 
  summarize(quad_cov = sum(quad_avg_cov), .groups = 'drop') %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  #left_join(., prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName") %>% 
  select(Plot_Name, X, Y, ScientificName, quad_cov) %>% 
  mutate(quad_cov = replace_na(quad_cov, 0),
         sppcode = 
           case_when(ScientificName == "Lonicera - Exotic" ~ "LONEXO",
                     is.na(word(ScientificName, 2)) ~ 
                       toupper(paste0(substr(word(ScientificName, 1), 1, 3), "SPP")),
                     TRUE ~ toupper(paste0(substr(word(ScientificName, 1), 1, 3), 
                       substr(word(ScientificName, 2), 1, 3))))) %>% 
  arrange(sppcode, Plot_Name) %>% 
  select(Plot_Name, X, Y, sppcode, quad_cov) %>% 
  pivot_wider(names_from = sppcode, values_from = quad_cov, values_fill = 0) %>% 
  select(-NONPRE)

invspp$totcov = rowSums(invspp[,4:ncol(invspp)])

write_to_shp(invspp, shp_name = 
               paste0(new_path, "shapefiles/", park, "_inv_cover_by_species.shp"))

#---- Map 9 Tree Pests/Diseases ----
# First compile plot-level disturbances that may include priority pests/pathogens
disturb <- do.call(joinStandDisturbance, args = args_4yr) %>% 
  filter(DisturbanceLabel != "None") %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", DisturbanceNote) ~ "BLD",
                          grepl("Emerald|emerald|EAB", DisturbanceNote) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", DisturbanceNote) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                DisturbanceNote) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", DisturbanceNote) ~ "BBD",
                          grepl("GM|spongy|gypsy", DisturbanceNote) ~ "GM",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()
#mutate(detect = 'plot_dist')

# Next compile pest/pathogens from Tree Conditions
treecond_4yr <- do.call(joinTreeConditions, args = c(args_4yr, status = 'live'))

pests <- c("ALB", "BBD", "BLD", "BC", "BWA", "DOG", "EAB", "EHS", "GM", "HWA", "RPS", 
           "SB", "SLF", "SOD", "SPB", "SW")


treepests <- treecond_4yr %>% select(Plot_Name, all_of(pests)) %>% 
  #group_by(Plot_Name) %>% summarize(across(all_of(pests), ~ifelse(sum(.x) > 0, 1, 0))) %>% 
  group_by(Plot_Name) %>% summarize_at(vars(all_of(pests)), ~ifelse(sum(.x) > 0, 1, 0)) %>% 
  pivot_longer(-Plot_Name, names_to = "pest", values_to = 'tree_cond') %>% 
  filter(tree_cond > 0) %>% 
  arrange(Plot_Name) %>% unique() %>% select(Plot_Name, pest)


# Compile notes from visit that could contain mentions of pests

vnotes <- do.call(joinVisitNotes, args = args_4yr) %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", Notes) ~ "BLD",
                          grepl("Emerald|emerald|EAB", Notes) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", Notes) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                Notes) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", Notes) ~ "BBD",
                          grepl("GM|spongy|gypsy", Notes) ~ "GM",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()

# Combine detections to 1 shapefile
pest_detects <- rbind(treepests, disturb, vnotes) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, SampleYear, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  select(Plot_Name, SampleYear, X, Y, everything()) %>% unique() %>%  
  mutate(pest = replace_na(pest, "None"),
         detect = ifelse(pest == "None", 0, 1))

pests_wide <- pest_detects %>% 
  pivot_wider(names_from = pest, values_from = detect, values_fill = 0) %>% 
  select(-None)

if(park == "MABI"){
pests_wide$EAB[pests_wide$Plot_Name == "MABI-013" & pests_wide$SampleYear == 2022] <- 0
# Tree note said "No EAB", but was picked up in query for positive EAB detections
}

if(park == "MABI"){
  pests_wide$EAB[pests_wide$Plot_Name == "MABI-005" & pests_wide$SampleYear == 2023] <- 0
  # Tree note said "No sign of EAB", but was picked up in query for positive EAB detections
}

if(park == "SAGA" & to == 2022){
  pests_wide$EAB[pests_wide$Plot_Name == "SAGA-017" & pests_wide$SampleYear == 2022] <- 0
  # Tree note said "No EAB", but was picked up in query for positive EAB detections
}

pests_wide$none <- rowSums(pests_wide[,5:ncol(pests_wide)])

if(park %in% c("MABI", "SAGA")){
  worms <- do.call(joinStandData, args = args_vs) %>% select(Plot_Name, cycle, Earthworms) %>% 
    mutate(Earthworms = ifelse(Earthworms == 1, 1, 0)) %>% 
    group_by(Plot_Name) %>% 
    summarize(worms = ifelse(sum(Earthworms, na.rm = T) > 0, 1, 0))
  
  pests_wide <- left_join(pests_wide, worms, by = "Plot_Name")
  
  }

write_to_shp(pests_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_pest_detections_", cycle_latest, ".shp"))


#---- Map 10 Canopy Cover ----
cancov <- do.call(joinStandData, args = args_all) %>% 
  select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate, CrownClos = Pct_Crown_Closure)

cancov_wide <- cancov %>% pivot_wider(names_from = cycle, values_from = CrownClos)

write_to_shp(cancov_wide, shp_name = paste0(new_path, "shapefiles/", park, "_canopy_cover.shp"))

#---- Table 2 Average Invasive cover by plot and cycle ----
inv_plots <- do.call(sumSpeciesList, args = c(args_all, speciesType = "invasive")) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% 
  group_by(Plot_Name, PlotCode, cycle, PanelCode) %>% 
  summarize(inv_cov = sum(quad_avg_cov, na.rm = T),
            numspp = sum(present), .groups = 'drop')

inv_plots_wide <- inv_plots %>% pivot_wider(names_from = cycle, 
                                            values_from = c(inv_cov, numspp)) %>% 
  select(-Plot_Name) %>% mutate(PanelCode = ifelse(PanelCode %in% c(1:2), 1, 2))

write.csv(inv_plots_wide, paste0(new_path, "tables/", "Table_2_", park, 
                                 "_invasives_by_plot_cycle.csv"), row.names = FALSE)

#---- Table 3 Invasive species by number of plots cycle
inv_spp1 <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% arrange(cycle) %>% 
  group_by(ScientificName, cycle) %>% summarize(num_plots = sum(present), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_")

centaurea <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Centaurea", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(centaurea[,-1])

lonicera <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Lonicera", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(lonicera[,-1])

euonymus <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Euonymus", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(euonymus[,-1])

ligustrum <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Ligustrum", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(ligustrum[,-1])

vincetoxicum <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Vincetoxicum", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(vincetoxicum[,-1])


inv_spp <- left_join(inv_spp1, prepTaxa() %>% select(ScientificName, CommonName),
                     by = "ScientificName") %>% select(ScientificName, CommonName, everything())

write.csv(inv_spp, paste0(new_path, "tables/", "Table_3_", park,
                          "_num_invspp_by_cycle.csv"), row.names = FALSE)

#---- Early Detections -----
taxa <- prepTaxa()
spp_all <- do.call(sumSpeciesList, args = c(args_4yr))

# Need to import ParkTaxonProtectedStatus table from local database until it's added to the taxon view
connect <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=NETN_Forest;trusted_connection=TRUE;ReadOnly=True"

con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1)
xref_taxon <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[xrefCOMN].[ParkTaxonProtectedStatus]"))
tlu_park <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[tluCOMN].[Park]"))
RODBC::odbcClose(con)

tlu_park2 <- tlu_park %>% select(ID, Unit) %>% 
  unique() %>% 
  rename(ParkID = ID) %>% 
  filter(Unit %in% park)

ised_taxon1 <- xref_taxon %>% select(ParkID, TaxonID, IsEarlyDetection) %>% 
  filter(IsEarlyDetection == 1) %>% 
  unique()

ised_taxon2 <- inner_join(ised_taxon1, tlu_park2, by = "ParkID") %>% select(-ParkID) %>% unique()

ised_taxon <- left_join(ised_taxon2, taxa %>% select(TaxonID, TSN, ScientificName), 
                        by = "TaxonID")

ised_join <- left_join(spp_all, ised_taxon, by = c("TSN", "ScientificName", "ParkUnit" = "Unit")) %>% 
  filter(IsEarlyDetection == 1) %>% 
  select(-SampleYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
         -quad_pct_freq, -IsEarlyDetection) %>% 
  arrange(Plot_Name, ScientificName) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% filter(!is.na(ScientificName)) %>% 
  select(Plot_Name, X, Y, ScientificName, quad_avg_cov)

write.csv(ised_join, paste0(new_path, "tables/", park, "_early_detection_plant_species.csv"),
          row.names = FALSE)

#---- Invasive Detections for MABI/SAGA/ACAD ---- 
taxa <- prepTaxa()

spp_inv <- do.call(sumSpeciesList, args = c(args_4yr, speciesType = 'invasive')) |> 
  select(Plot_Name, SampleYear, quad_avg_cov, ScientificName) |> 
  filter(!ScientificName %in% "None present")

spp_inv2 <- left_join(spp_inv, plotevs_4yr |> select(Plot_Name, X = xCoordinate, Y = yCoordinate),
                      by = "Plot_Name")

#---- ED Pests ----
priority_pests <- c("ALB", "BLD", "EAB", "EHS", "HWA", "RPS", "SLF", "SOD", "SPB", "SW")

pest_eds <- pests_wide %>% select(Plot_Name, X, Y, any_of(priority_pests)) %>% 
  mutate(pres = ifelse(ncol(.) > 3, rowSums(.[4:ncol(.)]), 0)) %>% filter(pres > 0)

if(nrow(pest_eds) > 0){
write.csv(pest_eds, paste0(new_path, 'tables/', park, "_pest_detections.csv"), row.names = F)
}

#---- Rubus cover for MABI -----
if(park == "MABI"){
rubus <- do.call(joinQuadSpecies, args = args_vs) %>% filter(grepl("Rubus", ScientificName))

rubus2 <- rubus %>% group_by(Plot_Name, cycle) %>% 
  summarize(avg_cov = sum(quad_avg_cov, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name", "cycle")) 

rubus2$avg_cov[is.na(rubus2$avg_cov)] <- 0

rubus_wide <- rubus2 %>% pivot_wider(names_from = cycle, values_from = avg_cov, values_fill = 0)

write_to_shp(rubus_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_Rubus_cover_", ".shp"))

}

#---- CWD by cycle
if(park %in% c("MABI", "SAGA", "MIMA")){
cwd1 <- do.call(joinCWDData, args = args_vs) %>% select(Plot_Name, cycle, CWD_Vol)

cwd <- cwd1 %>% group_by(Plot_Name, cycle) %>% 
  summarize(cwd_vol = sum(CWD_Vol, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name", "cycle"))
  
cwd$cwd_vol[is.na(cwd$cwd_vol)] <- 0

cwd_wide <- cwd %>% pivot_wider(names_from = cycle, 
                                values_from = cwd_vol, 
                                names_prefix = "cycle_",
                                values_fill = 0) 


apply(cwd_wide[,4:7], 2, mean)

write_to_shp(cwd_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_CWD_vol_by_cycle_", ".shp"))

}

