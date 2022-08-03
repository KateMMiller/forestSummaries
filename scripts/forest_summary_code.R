#-------------------------------------
# Forest Data Summaries: Regeneration
#-------------------------------------

#---- Functions ----
# Write dataframe to shapefile using common settings
write_to_shp <- function(data, x = "X", y = "Y", shp_name){
  st_write(st_as_sf(data, coords = c(x, y), crs = park_crs),
           shp_name, delete_layer = TRUE)
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

write.csv(reg_cycle_table, 
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

reg_size <- reg %>% group_by(Plot_Name, cycle) %>% 
                    summarize_at(all_of(reg_sz_cols), sum, na.rm = TRUE) 


reg_size_4yr <- reg_size %>% filter(cycle == cycle_latest) %>% 
                             left_join(plotevs_4yr %>% select(Plot_Name, xCoordinate, yCoordinate),
                                       ., by = 'Plot_Name')
reg_size_4yr[, reg_sz_cols][reg_size_4yr[is.na(reg_size_4yr[,reg_sz_cols])]] <- 0

colnames(reg_size_4yr) <- c("Plot_Name", "X", "Y", "cycle", 
                             "s15_30", "s30_100", "s100_150", "s150p", "sap") #abbr. for shapefile

write_to_shp(reg_size_4yr, 
             shp_name = paste0(new_path, "shapefiles/", park, 
                               "_regen_by_size_class_cycle_", cycle_latest, ".shp"))

#---- Figure 1A Regen trends by size class ----
# Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
reg_vs <- do.call(joinRegenData, 
                  args = c(args_vs, speciesType = 'native', 
                           canopyForm = 'canopy', units = 'sq.m'))

reg_size_cy <- reg_vs %>% group_by(Plot_Name, cycle) %>% 
                          summarize_at(all_of(reg_sz_cols), sum, na.rm = TRUE) %>% 
                          left_join(plotevs_vs %>% select(Plot_Name, cycle),
                                    ., by = c("Plot_Name", "cycle")) 

reg_size_cy[reg_sz_cols][reg_size_cy[is.na(reg_sz_cols)]] <- 0

# Use forestTrends to generate loess-smoothed CIs
reg_smooth <- map_dfr(reg_sz_cols, 
                      ~case_boot_loess(reg_size_cy, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 1000, chatty = TRUE) %>% 
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
               "4" = "Cycle 4: 2018 \u2013 2022")

reg_labels <- c("15 \u2013 30 cm", "30 \u2013 100 cm", "100 \u2013 150 cm",
                ">150 cm & < 1 cm DBH", "Saplings: 1 \u2013 9.9 cm DBH")

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = size_class, y = estimate, color = size_class,#linetype = sign, 
                         group = size_class))+ theme_FHM()+
  geom_bar(stat = 'identity', aes(fill = size_class, color = 'DimGrey'))+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, size = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = bquote(Stems/m^2))+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs), ncol = 4)+
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
tree_dd <- do.call(sumTreeDBHDist, args = c(args_vs, status = 'live') )

dbh_cols <- c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9', 
              'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
              'dens_90_99.9', 'dens_100p')

tree_dbh_sm <- map_dfr(dbh_cols, 
                    ~case_boot_loess(tree_dd, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 1000, chatty = TRUE) %>% 
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

# check flat diameter distribution
head(tree_dbh_sm)
e <- min(tree_dbh_sm$estimate[tree_dbh_sm$estimate > 0])

tree_dbh_sm <- suppressWarnings(# warning is for 'c', which we're not using
  tree_dbh_sm %>% 
  mutate(dbh_class2 = gsub("p", "", dbh_class)) %>% 
  separate(dbh_class2, into = c("a", "b", "c"), sep = "_", remove = FALSE) %>% 
  mutate(class = as.numeric(b)) %>% select(-a, -b, -c, -dbh_class2))

AIC_test <- map_dfr(seq_along(1:4), function(x){
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
                               paste0(cycle_labs[4], "*"), cycle_labs[4]))
cycle_labs_tr

dbh_trend_plot <- 
  ggplot(tree_dbh_sm, aes(x = dbh_class, y = estimate))+ theme_FHM()+
  geom_bar(stat = 'identity', fill = "#81B082" , color = 'DimGrey')+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, size = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = "Stems/ha")+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs_tr), ncol = 4)+
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
       height = 5, width = 7.5, units = 'in')

#---- Map 3 Regen by composition ----
reg_all <- do.call(joinRegenData, args = c(args_4yr, units = 'sq.m'))

reg_spp <- do.call(joinRegenData, args = list(park = 'all', from = 2019, to = 2022))

table(reg_spp$ScientificName, reg_spp$ParkUnit)

Acer <- c('Acer rubrum', 'Acer saccharum', 'Acer saccharinum', 'Acer negundo')
Betula <- c('Betula','Betula alleghaniensis', 'Betula lenta',  'Betula X cearulea ',
            'Betula papyrifera', 'Betula populifolia', 'Betula cordifolia')
Carya <- c('Carya', 'Carya cordiformis', 'Carya glabra', 'Carya ovata', 'Carya tomentosa')
Fraxinus <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica')
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
                  'Nyssa sylvatica', 'Tilia americana', 'Picea rubens', ' Platanus occidentalis',
                  'Salix', 'Unknown Conifer',
                  'Unknown Hardwood', 'Unknown Tree - 01', 'Unknown Tree - 03')
Subcanopy <- c('Acer spicatum', 'Acer pensylvanicum',
               'Carpinus caroliniana', 'Cornus florida', 
                'Ilex opaca', 'Juniperus virginiana', 
               'Ostrya virginiana',
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')
Exotic_spp <- c('Acer platanoides', 'Aesculus hippocastanum', 'Ailanthus altissima',
                'Crataegus', 'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 'Picea abies', "{inus sylvestris",
                'Rhamnus cathartica', 'Salix alba')

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

reg_wide <- reg_all %>% group_by(Plot_Name, spp_grp) %>% 
  summarize(regen_den = sum(regen_den, na.rm = TRUE), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = "Plot_Name") %>% arrange(spp_grp) %>% 
  pivot_wider(names_from = spp_grp, values_from = regen_den, values_fill = 0) %>% 
  arrange(Plot_Name)

reg_wide <- if("NONPRE" %in% names(reg_wide)){reg_wide %>% select(-NONPRE)}else{reg_wide} 

write_to_shp(reg_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_regen_by_spp_cycle", cycle_latest, ".shp"))

#---- Map 4 Tree canopy composition ----
trees_4yr <- do.call(joinTreeData, args = c(args_4yr, status = 'live'))

#table(joinTreeData(from = 2019, to = 2022)$ScientificName, joinTreeData(from = 2019, to = 2022)$ParkUnit)

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

tree_wide <- trees_4yr %>% group_by(Plot_Name, spp_grp) %>% 
  summarize(BAm2ha = sum(BA_cm2, na.rm = TRUE)/400, .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = "Plot_Name") %>% arrange(spp_grp) %>% 
  pivot_wider(names_from = spp_grp, values_from = BAm2ha, values_fill = 0) 

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
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()
#mutate(detect = 'plot_dist')

# Next compile pest/pathogens from Tree Conditions
treecond_4yr <- do.call(joinTreeConditions, args = c(args_4yr, status = 'live'))

pests <- c("ALB", "BBD", "BLD", "BC", "BWA", "DOG", "EAB", "EHS", "GM", "HWA", "RPS", 
           "SB", "SLF", "SOD", "SPB", "SW")

treepests <- treecond_4yr %>% select(Plot_Name, all_of(pests)) %>% 
  group_by(Plot_Name) %>% summarize(across(all_of(pests), ~ifelse(sum(.x) > 0, 1, 0))) %>% 
  #mutate(detect = rowSums(.[, pests])) %>% 
  pivot_longer(all_of(pests), names_to = "pest", values_to = 'tree_cond') %>% 
  filter(tree_cond > 0) %>% 
  arrange(Plot_Name) %>% unique() %>% select(Plot_Name, pest)

# Combine detections to 1 shapefile
pest_detects <- rbind(treepests, disturb) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  select(Plot_Name, X, Y, everything()) %>% unique() %>%  
  mutate(pest = replace_na(pest, "None"),
         detect = ifelse(pest == "None", 0, 1))

pests_wide <- pest_detects %>% 
  pivot_wider(names_from = pest, values_from = detect, values_fill = 0) %>% 
  select(-None)

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
inv_spp1 <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'invasive')) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% arrange(cycle) %>% 
  group_by(ScientificName, cycle) %>% summarize(num_plots = sum(present), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_")

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

priority_pests <- c("ALB", "BLD", "EAB", "EHS", "HWA", "RPS", "SLF", "SOD", "SPB", "SW")

pest_eds <- pests_wide %>% select(Plot_Name, X, Y, any_of(priority_pests)) %>% 
  mutate(pres = ifelse(ncol(.) > 3, rowSums(.[4:ncol(.)]), 0)) %>% filter(pres > 0)

if(nrow(pest_eds) > 0){
write.csv(pest_eds, paste0(new_path, 'tables/', park, "_pest_detections.csv"), row.names = F)
}
