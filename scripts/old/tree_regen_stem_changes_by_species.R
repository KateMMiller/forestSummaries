#---- Figure 2 Recruitment and Mortality ----
library(grid)
tree_mr1 <- do.call(joinTreeData, args = args_vs)

plot_evs <- do.call(joinLocEvent, args = args_vs) %>% select(Plot_Name, cycle) %>% 
  group_by(cycle) %>% mutate(num_plots = sum(!is.na(Plot_Name)))

tree_mr <- left_join(tree_mr1, plot_evs, by = c("Plot_Name", "cycle"))

# tree_mr <- joinTreeData(park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))

alive_stat <- c("1", "AB","AF", "AL", "AS")
recruit_stat <- c("RB","RF","RL","RS")
dead_stat <- c("2", "DB", "DF", "DL", "DS")
exclude_stat <- c("DC","DM","ES","EX","NL","XO","XS","0", "XP")
missed_stat <- "AM"

tree_mr <- tree_mr %>% mutate(status = case_when(TreeStatusCode %in% alive_stat ~ "alive",
                                                 TreeStatusCode %in% dead_stat ~ "dead",
                                                 TreeStatusCode %in% recruit_stat ~ "recruit",
                                                 TreeStatusCode %in% exclude_stat ~ "exclude",
                                                 TreeStatusCode %in% missed_stat ~ 'missed',
                                                 TRUE ~ "UNK"),
                              BA_m2ha = BA_cm2/ifelse(ParkUnit == "ACAD", 225, 400),
                              stems = 1) %>%
  select(Plot_Name, ParkUnit, TagCode, PanelCode, cycle, num_plots, ScientificName, 
         status, DBHcm, stems, BA_m2ha) %>%
  arrange(Plot_Name, cycle) %>% 
  left_join(., prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName")

table(tree_mr$status)
table(tree_mr$ScientificName)

Acer_spp <- c('Acer', 'Acer rubrum', 'Acer saccharum', 'Acer saccharinum', 'Acer negundo')
Betula_spp <- c('Betula','Betula alleghaniensis', 'Betula lenta',  'Betula X cearulea ',
            'Betula papyrifera', 'Betula populifolia', 'Betula cordifolia')
Carya_spp <- c('Carya', 'Carya cordiformis', 'Carya glabra', 'Carya ovata', 'Carya tomentosa')
Fraxinus_spp <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', 'Fraxinus nigra')
Pinus_spp <- c("Pinus resinosa", "Pinus strobus", "Pinus", "Pinus rigida")
Populus_spp <- c('Populus', 'Populus deltoides', 'Populus grandidentata', 'Populus tremuloides')
Prunus_spp <- c('Prunus', 'Prunus serotina', 'Prunus virginiana')
Quercus_spp <- c('Quercus', 'Quercus (Red group)', 'Quercus (White group)',
             'Quercus alba', 'Quercus bicolor', 'Quercus coccinea',
             'Quercus montana', 'Quercus palustris', 'Quercus rubra',
             'Quercus velutina')
Ulmus_spp <- c("Ulmus", "Ulmus americana", "Ulmus rubra")

Other_Native <- c('Amelanchier',  'Amelanchier arborea', 'Amelanchier laevis',
                  'Celtis occidentalis', 'Cladrastis kentukea', 'Juglans nigra',
                  'Nyssa sylvatica', 
                  'Tilia americana',# Turned off for MABI & SAGA
                  #'Picea rubens', 
                  'Platanus occidentalis', 
                  'Salix', 'Unknown Conifer',
                  'Unknown Hardwood', 'Unknown Tree - 01', 'Unknown Tree - 03')
Subcanopy <- c('Acer spicatum', 'Acer pensylvanicum',
               'Carpinus caroliniana', 'Cornus florida', 
               'Ilex opaca', 'Juniperus virginiana', 
               'Ostrya virginiana',
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')

Exotic_spp <- c(#'Acer platanoides',  #Turned off for SAGA ROVA
                'Aesculus hippocastanum', 'Ailanthus altissima',
                #'Crataegus',  # turned off for SARA
                'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 
                'Picea abies', "Pinus sylvestris",  # turned off for MABI
                'Alnus serrulata', 'Catalpa speciosa',
                #'Rhamnus cathartica', # turned off for SARA
                'Salix alba')

if(park == "MORR"){
  tree_mr <- tree_mr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                               ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                               ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                               ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                               #ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                               ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                               ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                               ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                               ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                               ScientificName %in% 
                                 c(Other_Native, Pinus_spp, Populus_spp, Prunus_spp) ~ "Other Native spp.",
                               ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                               ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                               TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
} else if(park == "MABI") {
  tree_mr <- tree_mr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                               ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                               ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                               ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                               ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                               #ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                               #ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                               ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                               ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                               ScientificName %in% c(Other_Native, Prunus_spp, Populus_spp) ~ "Other Native spp.",
                               ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                               ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                               TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
} else if(park == "SAGA") {
  tree_mr <- tree_mr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                               ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                               ScientificName %in% Carya_spp ~ "Carya cordiformis (bitternut hickory)",
                               ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                               ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                               ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                               #ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                               ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                               ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                               ScientificName %in% c(Other_Native, Prunus_spp, Populus_spp) ~ "Other Native spp.",
                               ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                               ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                               TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
} else if(park %in% c("MIMA", "WEFA")) {
  tree_mr <- tree_mr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                               ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                               ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                               ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                               ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                               #ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                               ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                               ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                               ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                               ScientificName %in% c(Other_Native, Populus_spp) ~ "Other Native spp.",
                               ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                               ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                               TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
} else {
tree_mr <- tree_mr %>% 
  mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                             ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                             ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                             ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                             ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                             ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                             ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                             ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                             ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                             ScientificName %in% Other_Native ~ "Other Native spp.",
                             ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                             ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
}                             
table(tree_mr$spp_grp)

tree_mr$stems[tree_mr$status == "exclude"] <- -999
tree_mr$BA_m2ha[tree_mr$status == "exclude"] <- -999
table(tree_mr$ScientificName, tree_mr$spp_grp)

full_names <- c("DBHcm_2_missed", 'DBHcm_3_missed', 'DBHcm_4_missed',  "DBHcm_5_missed",
                "stems_1_alive", "stems_1_dead",  
                "stems_2_alive", "stems_2_dead", "stems_2_exclude", "stems_2_recruit", "stems_2_missed",
                "stems_3_alive", "stems_3_dead", "stems_3_exclude", "stems_3_recruit", "stems_3_missed",   
                "stems_4_alive", "stems_4_dead", "stems_4_exclude", "stems_4_recruit", "stems_4_missed",
                "stems_2_cut", "stems_3_cut", "stems_4_cut", 'stems_5_cut',
                "stems_5_alive", "stems_5_dead", "stems_5_exclude", "stems_5_recruit", "stems_5_missed",  
                "BA_m2ha_1_alive", "BA_m2ha_1_dead",  
                "BA_m2ha_2_alive", "BA_m2ha_2_dead", "BA_m2ha_2_exclude", "BA_m2ha_2_recruit", "BA_m2ha_2_missed",
                "BA_m2ha_3_alive", "BA_m2ha_3_dead", "BA_m2ha_3_exclude", "BA_m2ha_3_recruit", "BA_m2ha_3_missed",   
                "BA_m2ha_4_alive", "BA_m2ha_4_dead", "BA_m2ha_4_exclude", "BA_m2ha_4_recruit", "BA_m2ha_4_missed",
                "BA_m2ha_2_cut", "BA_m2ha_3_cut", "BA_m2ha_4_cut", 'BA_m2ha_5_cut',
                "BA_m2ha_5_alive", "BA_m2ha_5_dead", "BA_m2ha_5_exclude", "BA_m2ha_5_recruit", "BA_m2ha_5_missed"
)


trees_wide <- tree_mr %>% arrange(Plot_Name, TagCode, cycle, status) %>% 
  pivot_wider(names_from = c(cycle, status),
              values_from = c(stems, BA_m2ha, DBHcm),
              values_fill = 0) %>%
  arrange(Plot_Name, TagCode) #%>%

names(trees_wide)

# Next steps are if a combination of status and cycle aren't represented
new_names <- c(full_names[!full_names %in% names(trees_wide)])
new_df <- as_tibble(matrix(rep(0, nrow(trees_wide) * length(new_names)), 
                           nrow = nrow(trees_wide), ncol = length(new_names)), 
                    .name_repair = ~ new_names)

new_names

#trees_wide2 <- cbind(trees_wide, new_df)
trees_wide2 <- trees_wide %>% bind_cols(., new_df) %>%                           
  mutate(exclude = case_when(stems_1_dead == 1 ~ 1,
                             is.na(stems_1_dead) ~ 1,
                             stems_1_exclude == -999 ~ 1,
                             stems_2_exclude == -999 ~ 1,
                             stems_3_exclude == -999 ~ 1,
                             stems_4_exclude == -999 ~ 1,
                             stems_5_exclude == -999 ~ 1,
                             TRUE ~ 0)) %>%
  filter(exclude != 1)

trees_wide3 <- trees_wide2 %>%
  select(ParkUnit, Plot_Name, PanelCode, TagCode, num_plots, spp_grp, 
         DBHcm_2_missed, DBHcm_3_missed, DBHcm_4_missed, DBHcm_5_missed,
         stems_1_alive, 
         stems_2_alive, stems_2_dead, stems_2_recruit, stems_2_missed, stems_2_cut,
         stems_3_alive, stems_3_dead, stems_3_recruit, stems_3_missed, stems_3_cut,
         stems_4_alive, stems_4_dead, stems_4_recruit, stems_4_missed, stems_4_cut,
         stems_5_alive, stems_5_dead, stems_5_recruit, stems_5_missed, stems_5_cut,
         
         BA_m2ha_1_alive, 
         BA_m2ha_2_alive, BA_m2ha_2_dead, BA_m2ha_2_recruit, BA_m2ha_2_missed, BA_m2ha_2_cut,
         BA_m2ha_3_alive, BA_m2ha_3_dead, BA_m2ha_3_recruit, BA_m2ha_3_missed, BA_m2ha_3_cut,
         BA_m2ha_4_alive, BA_m2ha_4_dead, BA_m2ha_4_recruit, BA_m2ha_4_missed, BA_m2ha_4_cut,
         BA_m2ha_5_alive, BA_m2ha_5_dead, BA_m2ha_5_recruit, BA_m2ha_5_missed, BA_m2ha_5_cut
  )

# Add stems that were missed in earlier visits and added later. Use 12.0 as cutoff for whether a
# stem should be a recruit and new to that visit or missed with stem added to previous visits.
# Each subsequent visit adds 2cm to DBH cutoff to determine RS or missed. The allows for a stem
# to be added to previous visits, but only if > the cutoff for RS. The stems_#_alive_adj includes
# stems that are recruits
trees_wide3 <- trees_wide3 %>% mutate(
  stems_1_alive_adj = ifelse(stems_2_missed == 1 | 
                               (stems_3_missed == 1 & DBHcm_3_missed > 14.0) | 
                               (stems_4_missed == 1 & DBHcm_4_missed > 16.0) |
                               (stems_5_missed == 1 & DBHcm_5_missed > 18.0), # for cycle 5 
                             1, stems_1_alive), 
  stems_2_alive_adj = ifelse(stems_2_missed == 1 |
                               stems_3_missed == 1 | 
                               (stems_4_missed == 1 & DBHcm_4_missed > 14.0) | 
                               (stems_5_missed == 1 & DBHcm_5_missed > 16.0) | # for cycle 5 
                               stems_2_recruit == 1, 
                             1, stems_2_alive), 
  stems_3_alive_adj = ifelse(stems_3_missed == 1 |
                               stems_4_missed == 1 |
                               (stems_5_missed == 1 & DBHcm_5_missed > 14.0) | # for cycle 5 
                               stems_3_recruit == 1, 
                             1, stems_3_alive),
  stems_4_alive_adj = ifelse(stems_4_missed == 1 | 
                               (stems_5_missed == 1) | # for cycle 5  
                               stems_4_recruit == 1, 
                             1, stems_4_alive),
  stems_5_alive_adj = ifelse(stems_5_missed == 1 | stems_5_recruit == 1, 1, stems_5_alive),
  
  # Use adjusted stem values for live ba
  BA_m2ha_1_alive_adj = ifelse(stems_1_alive_adj == 1, BA_m2ha_1_alive, 0),
  BA_m2ha_2_alive_adj = ifelse(stems_2_alive_adj == 1, BA_m2ha_2_alive, 0),
  BA_m2ha_3_alive_adj = ifelse(stems_3_alive_adj == 1, BA_m2ha_3_alive, 0), 
  BA_m2ha_4_alive_adj = ifelse(stems_4_alive_adj == 1, BA_m2ha_4_alive, 0), 
  BA_m2ha_5_alive_adj = ifelse(stems_5_alive_adj == 1, BA_m2ha_5_alive, 0), 
  
  # Determine stems that died between cycles         
  c1_2_dead = ifelse(stems_1_alive_adj == 1 & stems_2_dead == 1, 1, 0), # must be alive first
  c2_3_dead = ifelse(stems_2_alive_adj == 1 & stems_3_dead == 1, 1, 0),
  c3_4_dead = ifelse(stems_3_alive_adj == 1 & stems_4_dead == 1, 1, 0),
  c4_5_dead = ifelse(stems_4_alive_adj == 1 & stems_5_dead == 1, 1, 0),
  
  # Determine stems that recruited between cycles  
  c1_2_recr = ifelse(stems_2_recruit == 1, 1, 0),
  c2_3_recr = ifelse(stems_3_recruit == 1, 1, 0),
  c3_4_recr = ifelse(stems_4_recruit == 1, 1, 0),
  c4_5_recr = ifelse(stems_5_recruit == 1, 1, 0),
  
  # For trees that are DF the first time they're observed as dead, use previous visit's alive BA
  c1_2_dead_ba = case_when(c1_2_dead == 1 & BA_m2ha_2_dead > 0 ~ BA_m2ha_2_dead,
                           c1_2_dead == 1 & is.na(BA_m2ha_2_dead) ~ BA_m2ha_1_alive_adj,
                           TRUE ~ 0),
  c2_3_dead_ba = case_when(c2_3_dead == 1 & BA_m2ha_3_dead > 0 ~ BA_m2ha_3_dead,
                           c2_3_dead == 1& is.na(BA_m2ha_3_dead) ~ BA_m2ha_2_alive_adj,
                           TRUE ~ 0),
  c3_4_dead_ba = case_when(c3_4_dead == 1 & BA_m2ha_4_dead > 0 ~ BA_m2ha_4_dead,
                           c3_4_dead == 1 & is.na(BA_m2ha_4_dead) ~ BA_m2ha_3_alive_adj,
                           TRUE ~ 0),
  c4_5_dead_ba = case_when(c4_5_dead == 1 & BA_m2ha_5_dead > 0 ~ BA_m2ha_5_dead,
                           c4_5_dead == 1 & is.na(BA_m2ha_5_dead) ~ BA_m2ha_4_alive_adj,
                           TRUE ~ 0),
  
  c1_2_recr_ba = ifelse(stems_2_recruit == 1, BA_m2ha_2_recruit, 0),
  c2_3_recr_ba = ifelse(stems_3_recruit == 1, BA_m2ha_3_recruit, 0),
  c3_4_recr_ba = ifelse(stems_4_recruit == 1, BA_m2ha_4_recruit, 0),
  c4_5_recr_ba = ifelse(stems_5_recruit == 1, BA_m2ha_5_recruit, 0),
  
  # Calculate BA gained through stem growth of existing trees. Calculation doesn't include recruits
  c1_2_grow_ba = ifelse(stems_2_alive_adj == 1 & stems_1_alive_adj == 1,
                        BA_m2ha_2_alive - BA_m2ha_1_alive, 0),
  c2_3_grow_ba = ifelse(stems_3_alive_adj == 1 & stems_2_alive_adj == 1,
                        BA_m2ha_3_alive - BA_m2ha_2_alive, 0),
  c3_4_grow_ba = ifelse(stems_4_alive_adj == 1 & stems_3_alive_adj == 1, 
                        BA_m2ha_4_alive - BA_m2ha_3_alive, 0),
  c4_5_grow_ba = ifelse(stems_5_alive_adj == 1 & stems_4_alive_adj == 1,
                        BA_m2ha_5_alive - BA_m2ha_4_alive, 0)
)

# check <- trees_wide3 %>% filter(Plot_Name == "MORR-001" & stems_2_dead == 1)

# Summarize species-level mortality and recruitment at plot level, converted to annual rate. 
# Note that live_stem and live_BA values include recruits for that visit. 
mor_rec <- trees_wide3 %>% group_by(ParkUnit, PanelCode, num_plots, spp_grp) %>%
  summarize(
    # Calculate denominator for extended cycle 4 intervals
    c4_denom = case_when(first(ParkUnit) %in% c("MABI", "MIMA", "SAGA", "SARA") & first(PanelCode) == 3 ~ 6,
                         first(ParkUnit) %in% c("MORR", "ROVA", "WEFA") & first(PanelCode) == 4 ~ 5,
                         first(ParkUnit) == 'ACAD' & first(PanelCode) == 3 ~ 5,
                         TRUE ~ 4),
    
    c1_live_stem = sum(stems_1_alive_adj, na.rm = T),
    c2_live_stem = sum(stems_2_alive_adj, na.rm = T),
    c3_live_stem = sum(stems_3_alive_adj, na.rm = T),
    c4_live_stem = sum(stems_4_alive_adj, na.rm = T),
    c5_live_stem = sum(stems_5_alive_adj, na.rm = T),
    
    c1_2_mort_stem_yr = sum(c1_2_dead, na.rm = T)/4,
    c2_3_mort_stem_yr = sum(c2_3_dead, na.rm = T)/4,
    c3_4_mort_stem_yr = sum(c3_4_dead, na.rm = T)/c4_denom,
    c4_5_mort_stem_yr = sum(c4_5_dead, na.rm = T),
    
    c1_2_recr_stem_yr = sum(c1_2_recr, na.rm = T)/4,
    c2_3_recr_stem_yr = sum(c2_3_recr, na.rm = T)/4,
    c3_4_recr_stem_yr = sum(c3_4_recr, na.rm = T)/c4_denom,
    c4_5_recr_stem_yr = sum(c4_5_recr, na.rm = T),
    
    c1_live_BA = sum(BA_m2ha_1_alive_adj, na.rm = T), 
    c2_live_BA = sum(BA_m2ha_2_alive_adj, na.rm = T),
    c3_live_BA = sum(BA_m2ha_3_alive_adj, na.rm = T),
    c4_live_BA = sum(BA_m2ha_4_alive_adj, na.rm = T),
    c5_live_BA = sum(BA_m2ha_5_alive_adj, na.rm = T),
    
    c1_2_mort_BA_yr = sum(c1_2_dead_ba, na.rm = T)/4,
    c2_3_mort_BA_yr = sum(c2_3_dead_ba, na.rm = T)/4,
    c3_4_mort_BA_yr = sum(c3_4_dead_ba, na.rm = T)/c4_denom,
    c4_5_mort_BA_yr = sum(c4_5_dead_ba, na.rm = T),
    
    c1_2_recr_BA_yr = sum(c1_2_recr_ba, na.rm = T)/4,
    c2_3_recr_BA_yr = sum(c2_3_recr_ba, na.rm = T)/4,
    c3_4_recr_BA_yr = sum(c3_4_recr_ba, na.rm = T)/c4_denom,
    c4_5_recr_BA_yr = sum(c4_5_recr_ba, na.rm = T),
    
    c1_2_incgr_BA_yr = sum(c1_2_grow_ba, na.rm = T)/4,
    c2_3_incgr_BA_yr = sum(c2_3_grow_ba, na.rm = T)/4,
    c3_4_incgr_BA_yr = sum(c3_4_grow_ba, na.rm = T)/c4_denom,
    c4_5_incgr_BA_yr = sum(c4_5_grow_ba, na.rm = T),
    
    .groups = 'drop') %>% select(-c4_denom)

  # write.csv(mor_rec, "MORT_REC_check.csv", row.names = F)

# Calculate % annual mortality and recruitment rates based on number added/lost divided by 
# previous visit's value for each panel and species combination. Have to use summarize
# to summarize across panels
# write.csv(mor_rec, "MORR_mor_rec_check.csv", row.names = F)
head(mor_rec)

mor_rec2 <- mor_rec %>% group_by(spp_grp) %>% 
  summarize(
  mortBA12 = -1 * sum(c1_2_mort_BA_yr)/sum(c1_live_BA) * 100,
  mortStem12 = -1 * sum(c1_2_mort_stem_yr)/sum(c1_live_stem) * 100,
  recrBA12 = sum(c1_2_recr_BA_yr)/sum(c1_live_BA) * 100,
  recrStem12 = sum(c1_2_recr_stem_yr)/sum(c1_live_stem) * 100,
  growBA12 = sum(c1_2_incgr_BA_yr)/sum(c1_live_BA) * 100,
  ElevMort12 = ifelse(mortStem12 > 1.6, 1, 0),
  
  mortBA23 = -1 * sum(c2_3_mort_BA_yr)/sum(c2_live_BA) * 100,
  mortStem23 = -1 * sum(c2_3_mort_stem_yr)/sum(c2_live_stem) * 100,
  recrBA23 = sum(c2_3_recr_BA_yr)/sum(c2_live_BA) * 100,
  recrStem23 = sum(c2_3_recr_stem_yr)/sum(c2_live_stem) * 100,
  growBA23 = sum(c2_3_incgr_BA_yr)/sum(c2_live_BA) * 100,
  ElevMort23 = ifelse(mortStem23 > 1.6, 1, 0),
  
  mortBA34 = -1 * sum(c3_4_mort_BA_yr)/sum(c3_live_BA) * 100,
  mortStem34 = -1 * sum(c3_4_mort_stem_yr)/sum(c3_live_stem) * 100,
  recrBA34 = sum(c3_4_recr_BA_yr)/sum(c3_live_BA) * 100,
  recrStem34 = sum(c3_4_recr_stem_yr)/sum(c3_live_stem) * 100,
  growBA34 = sum(c3_4_incgr_BA_yr)/sum(c3_live_BA) * 100,
  ElevMort34 = ifelse(mortStem34 > 1.6, 1, 0),
  
  mortBA45 = -1 * sum(c4_5_mort_BA_yr)/sum(c4_live_BA) * 100,
  mortStem45 = -1 * sum(c4_5_mort_stem_yr)/sum(c5_live_stem) * 100,
  recrBA45 = sum(c4_5_recr_BA_yr)/sum(c4_live_BA) * 100,
  recrStem45 = sum(c4_5_recr_stem_yr)/sum(c4_live_stem) * 100,
  growBA45 = sum(c4_5_incgr_BA_yr)/sum(c4_live_BA) * 100,
  ElevMort45 = ifelse(mortStem45 > 1.6, 1, 0),
  
  c1_live_BA = sum(c1_live_BA, na.rm = T),
  c1_live_stem = sum(c1_live_stem, na.rm = T),
  c2_live_BA = sum(c2_live_BA, na.rm = T),
  c2_live_stem = sum(c2_live_stem, na.rm = T),
  c3_live_BA = sum(c3_live_BA, na.rm = T),
  c3_live_stem = sum(c3_live_stem, na.rm = T),
  c4_live_BA = sum(c4_live_BA, na.rm = T),
  c4_live_stem = sum(c4_live_stem, na.rm = T),
  c5_live_BA = sum(c5_live_BA, na.rm = T),
  c5_live_stem = sum(c5_live_stem, na.rm = T),
  
  .groups = 'drop'
  # Add cycle 5 for ACAD
  ) 

# Code until now matches rates calculated by hand
plots <- unique(plot_evs[,c('cycle', 'num_plots')])

# Create figures that show how BA and stem density changed over time for species
mor_rec_overall <- mor_rec2 %>% group_by(spp_grp) %>%                         
  summarize(corr = ifelse(park == "ACAD", 44.444, 25),
            c1_live_BAm2ha = sum(c1_live_BA, na.rm = TRUE)/plots$num_plots[plots$cycle == 1], #already m2/ha
            c1_live_stemha = sum(c1_live_stem, na.rm = TRUE)*corr/plots$num_plots[plots$cycle == 1],
            c2_live_BAm2ha = sum(c2_live_BA, na.rm = TRUE)/plots$num_plots[plots$cycle == 2],
            c2_live_stemha = sum(c2_live_stem, na.rm = TRUE)*corr/plots$num_plots[plots$cycle == 2],
            c3_live_BAm2ha = sum(c3_live_BA, na.rm = TRUE)/plots$num_plots[plots$cycle == 3],
            c3_live_stemha = sum(c3_live_stem, na.rm = TRUE)*corr/plots$num_plots[plots$cycle == 3],
            c4_live_BAm2ha = sum(c4_live_BA, na.rm = TRUE)/plots$num_plots[plots$cycle == 4],
            c4_live_stemha = sum(c4_live_stem, na.rm = TRUE)*corr/plots$num_plots[plots$cycle == 4],
            c5_live_BAm2ha = sum(c5_live_BA, na.rm = TRUE)/plots$num_plots[plots$cycle == 5],
            c5_live_stemha = sum(c5_live_stem, na.rm = TRUE)*corr/plots$num_plots[plots$cycle == 5],
            .groups = 'drop') %>% 
  select(-corr) %>% 
  data.frame() 

mor_rec_ov_long <- mor_rec_overall %>% 
  pivot_longer(-c(spp_grp), names_to = 'metric', values_to = 'net_total') %>% 
  mutate(cycle = as.numeric(substr(metric, 2, 2)),
         unit_type = ifelse(grepl("BA", metric), "BA", "stem"))

mor_rec_tot <- mor_rec_ov_long %>% group_by(metric, cycle, unit_type) %>% 
  summarize(park_total = sum(net_total, na.rm = T), .groups = 'drop')

table(mor_rec_ov_long$spp_grp)

net_ba <- 
  ggplot(mor_rec_ov_long %>% filter(unit_type == "BA"), 
       aes(x = cycle, y = net_total)) +
    geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
    labs(x = 'Cycle', y = "Average Basal Area (sq.m/ha)") +
    # geom_line(data = mor_rec_tot %>% filter(unit_type == "BA"), 
    #           aes(x = cycle, y = park_total), color = 'black') + 
    theme_FHM()+
    # may have to update for different parks
    scale_color_manual(values = c(
                                  #"Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
                                  "Acer spp. (maple)" = "#54FF00",
                                  "Betula spp. (birch)" = "#38A800",
                                  "Carya spp. (hickory)" = "#FFFF00",
                                  "Crataegus (hawthorns)" = "#9D0909", #SARA only
                                  #"Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
                                  "Fagus grandifolia (American beech)" = "#FFAA00",
                                  "Fraxinus spp. (ash)" = "#A87000",
                                  #"Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
                                  #"Larix decidua (European larch)" = "#C8CE00", # MABI only
                                  "Other Exotic spp." = "#FF0000", # off for MABI
                                  "Other Native spp." = "#828282",
                                  #"Picea abies (Norway spruce)" = "#C2037A",
                                  "Pinus spp. (pine)" = "#1D5104", # off for MORR 
                                  #"Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
                                  "Populus spp. (poplar)" = "#CBCC7E", # off for MABI
                                  "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
                                  "Quercus spp. (oak)" = "#C500FF",
                                  "Rhamnus cathartica (common buckthorn)" = "#FF7854", # SARA only
                                  #"Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
                                  "Subcanopy spp." = "#FFBEE8",
                                  #"Tilia americana (American basswood)" = "#53CEF2", # MABI only
                                  #"Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
                                  "Ulmus spp. (elm)" = "#59538A" # off for MABI
                                  ),
                    name = NULL) +
  scale_linetype_manual(values = c(
                                  #"Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
                                  "Acer spp. (maple)" = "solid",
                                  "Betula spp. (birch)" = "solid",
                                  "Carya spp. (hickory)" = "solid",
                                  "Crataegus (hawthorns)" = "dotdash", #SARA only
                                  #"Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
                                  "Fagus grandifolia (American beech)" = "solid",
                                  "Fraxinus spp. (ash)" = "solid",
                                  #"Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
                                  #"Larix decidua (European larch)" = "solid", # MABI only
                                  "Other Exotic spp." = "dotdash", # off for MABI
                                  "Other Native spp." = "solid",
                                  #"Picea abies (Norway spruce)" = "solid",
                                  "Pinus spp. (pine)" = "solid", # off for MORR 
                                  #"Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
                                  "Populus spp. (poplar)" = "solid", # off for MABI
                                  "Prunus spp. (cherry)" ="solid", # off for MORR & MABI 
                                  "Quercus spp. (oak)" = "solid",
                                  "Rhamnus cathartica (common buckthorn)" = "dotdash", # SARA only
                                  #"Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
                                  "Subcanopy spp." = "solid",
                                  #"Tilia americana (American basswood)" = "solid", # MABI only
                                  #"Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
                                  "Ulmus spp. (elm)" = "solid" # off for MABI
  ),
  name = NULL)

svg(paste0(new_path, "figures/", "Figure_3_", park, "_net_BA_by_species_cycle.svg"),
    height = 5.6, width = 9.5)
  net_ba
dev.off()

net_stems <- 
  ggplot(mor_rec_ov_long %>% filter(unit_type == "stem"), 
         aes(x = cycle, y = net_total)) +
    geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
    labs(x = 'Cycle', y = "Average Stem Density (stems/ha)") +
    # geom_line(data = mor_rec_tot %>% filter(unit_type == "stem"),
    #           aes(x = cycle, y = park_total), color = 'black') +
    theme_FHM()+
    # may have to update for different parks
  scale_color_manual(values = c(
    #"Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
    "Acer spp. (maple)" = "#54FF00",
    "Betula spp. (birch)" = "#38A800",
    "Carya spp. (hickory)" = "#FFFF00",
    "Crataegus (hawthorns)" = "#9D0909", #SARA only
    #"Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
    "Fagus grandifolia (American beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#A87000",
    #"Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
    #"Larix decidua (European larch)" = "#C8CE00", # MABI only
    "Other Exotic spp." = "#FF0000", # off for MABI
    "Other Native spp." = "#828282",
    #"Picea abies (Norway spruce)" = "#C2037A",
    "Pinus spp. (pine)" = "#1D5104", # off for MORR 
    #"Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "#CBCC7E", # off for MABI
    "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
    "Quercus spp. (oak)" = "#C500FF",
    "Rhamnus cathartica (common buckthorn)" = "#FF7854", # SARA only
    #"Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
    "Subcanopy spp." = "#FFBEE8",
    #"Tilia americana (American basswood)" = "#53CEF2", # MABI only
    #"Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "#59538A" # off for MABI
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    #"Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
    "Acer spp. (maple)" = "solid",
    "Betula spp. (birch)" = "solid",
    "Carya spp. (hickory)" = "solid",
    "Crataegus (hawthorns)" = "dotdash", #SARA only
    #"Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
    "Fagus grandifolia (American beech)" = "solid",
    "Fraxinus spp. (ash)" = "solid",
    #"Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
    #"Larix decidua (European larch)" = "solid", # MABI only
    "Other Exotic spp." = "dotdash", # off for MABI
    "Other Native spp." = "solid",
    #"Picea abies (Norway spruce)" = "solid",
    "Pinus spp. (pine)" = "solid", # off for MORR 
    #"Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "solid", # off for MABI
    "Prunus spp. (cherry)" ="solid", # off for MORR & MABI 
    "Quercus spp. (oak)" = "solid",
    "Rhamnus cathartica (common buckthorn)" = "dotdash", # SARA only
    #"Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
    "Subcanopy spp." = "solid",
    #"Tilia americana (American basswood)" = "solid", # MABI only
    #"Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "solid" # off for MABI
  ),
  name = NULL)
       
svg(paste0(new_path, "figures/", "Figure_3_", park, "_net_stems_by_species_cycle.svg"),
    height = 5.6, width = 9.5)
  net_stems
dev.off()

#----- Similar figures for seedlings and saplings -----
reg <- do.call(joinRegenData, c(args_all, units = 'sq.m')) |> 
  filter(!ScientificName %in% "Rhamnus cathartica") # b/c treat as shrub until tree-size
head(reg)

#rhacat <- reg |> filter(ScientificName == "Rhamnus cathartica") |> select(Plot_Name, SampleYear, cycle, ScientificName)

regtab <- as.data.frame(table(reg$ScientificName)) |> arrange(desc(Freq))
regtab

Acer_spp <- c('Acer', 'Acer saccharinum', 'Acer negundo', 'Acer rubrum', 'Acer saccharum')
Betula_spp <- c('Betula','Betula alleghaniensis', 'Betula lenta',  'Betula X cearulea',
                'Betula papyrifera', 'Betula populifolia', 'Betula cordifolia')
Carya_spp <- c('Carya', 'Carya cordiformis', 'Carya glabra', 'Carya ovata', 'Carya tomentosa')
Fraxinus_spp <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', 'Fraxinus nigra')
Pinus_spp <- c("Pinus resinosa", "Pinus strobus", "Pinus", "Pinus rigida")
Populus_spp <- c('Populus', 'Populus deltoides', 'Populus grandidentata', 'Populus tremuloides')
Prunus_spp <- c('Prunus', 'Prunus serotina', 'Prunus virginiana')
Quercus_spp <- c('Quercus', 'Quercus (Red group)', 'Quercus (White group)',
                 'Quercus alba', 'Quercus bicolor', 'Quercus coccinea',
                 'Quercus montana', 'Quercus palustris', 'Quercus rubra',
                 'Quercus velutina')
Ulmus_spp <- c("Ulmus", "Ulmus americana", "Ulmus rubra")

Other_Native <- c('Amelanchier',  'Amelanchier arborea', 'Amelanchier laevis',
                  'Celtis occidentalis', 'Cladrastis kentukea', 'Juglans nigra',
                  'Nyssa sylvatica', 'Unknown species',
                  'Tilia americana',
                  'Pinus', 'Picea rubens',
                  'Platanus occidentalis', 
                  'Salix', 'Unknown Conifer', 'Unknown Tree - 02',
                  'Unknown Hardwood', 'Unknown Tree - 01', 'Unknown Tree - 03')

Subcanopy <- c('Acer spicatum', 'Acer pensylvanicum',
               'Carpinus caroliniana', 'Cornus florida', 
               'Ilex opaca', 'Juniperus virginiana', 
               'Ostrya virginiana',
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')
Exotic_spp <- c('Acer platanoides',  #Turned off for SAGA ROVA
                'Aesculus hippocastanum', 'Ailanthus altissima',
                #'Crataegus', ' # Turned off for SARA
                'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 
                'Picea abies', "Pinus sylvestris",  # turned off for MABI
                'Alnus serrulata', 'Catalpa speciosa',
                'Rhamnus cathartica',  
                'Salix alba')

head(reg)
tlu_plants <- prepTaxa() %>% select(TSN, ScientificName, CommonName)
head(tlu_plants)
head(plot_evs)

reg_spp <- reg %>% left_join(., tlu_plants, by = c("TSN", "ScientificName")) %>%
  mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                             ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                             ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                             ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                             ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                             ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                             ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                             ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                             ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                             ScientificName %in% c(Other_Native, Prunus_spp, Populus_spp) ~ "Other Native spp.",
                             ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                             ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

head(reg_spp)

reg_spp$ScientificName[reg_spp$ScientificName %in% c("Quercus (Red group)", "Quercus (White group)")] <- "Quercus"

reg_sum <- reg_spp %>% left_join(., plots, by = 'cycle') %>%
  group_by(Plot_Name, SampleYear, cycle, spp_grp, num_plots) %>%
  summarize(num_seeds = sum(seed_den), num_saps = sum(sap_den), .groups = 'keep') %>%
  group_by(cycle, spp_grp) %>%
  summarize(avg_seeds_m2 = sum(num_seeds)/first(num_plots), 
            avg_saps_m2 = sum(num_saps)/first(num_plots), .groups = 'keep') |> 
  filter(!spp_grp == "None present (NA)")


# Shift to loess smoother with case bootstrap. Need a matrix of site x species x year
head(plot_evs)
table(plot_spp_yr$spp_grp)

plot_spp_yr <- expand.grid(Plot_Name = unique(plot_evs$Plot_Name), SampleYear = unique(reg_spp$SampleYear),
                           spp_grp = unique(reg_spp$spp_grp)) |> 
               filter(spp_grp != "None present (NA)") |> 
               mutate(species = word(spp_grp, 1),
                      genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
                      sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |> 
               select(Plot_Name, SampleYear, spp_grp, sppcode)

plot_spp_yr$sppcode[plot_spp_yr$ScientificName == "Acer saccharum"] <- "ACESAC3"
plot_spp_yr$sppcode[plot_spp_yr$sppcode == "QUE(RE"] <- "QUESPP"
plot_spp_yr$sppcode[plot_spp_yr$sppcode == "QUE(WH"] <- "QUESPP"
plot_spp_yr$sppcode[plot_spp_yr$sppcode == "CRA(HA"] <- "CRASPP"

dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))

reg_spp_smooth <- left_join(plot_spp_yr, reg_spp |> select(Plot_Name, SampleYear, spp_grp, seed_den, sap_den), 
                            by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
                 # filter(SampleYear > 2006) #dropped first year b/c only 1 microplot

reg_spp_smooth[,c("seed_den", "sap_den")][is.na(reg_spp_smooth[,c("seed_den", "sap_den")])] <- 0

spp_list <- sort(unique(reg_spp_smooth$sppcode))

span = 8/length(unique(reg_spp_smooth$SampleYear))

seed_smooth <- purrr::map_dfr(spp_list, 
                              function(spp){
                              df <- reg_spp_smooth |> filter(sppcode %in% spp)
                              case_boot_loess(df, x = "SampleYear", y = "seed_den", ID = "Plot_Name",
                                              group = "sppcode", 
                                              span = span, num_reps = 1000) |>
                               mutate(sppcode = spp)
                              }
                              )

sap_smooth <- purrr::map_dfr(spp_list, 
                              function(spp){
                                df <- reg_spp_smooth |> filter(sppcode %in% spp)
                                case_boot_loess(df, x = "SampleYear", y = "sap_den", ID = "Plot_Name",
                                                group = "sppcode", 
                                                span = span, num_reps = 1000) |>
                                  mutate(sppcode = spp)
                              }
)

# Determine if significant based on whether first and last year CIs overlap
seed_smooth2 <- 
  left_join(seed_smooth, 
            seed_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
                           summarize(up_first = first(upper95), up_last = last(upper95),
                                     lo_first = first(lower95), lo_last = last(lower95),
                                     sign = case_when(up_first < lo_last ~ "signinc",
                                                      lo_first > up_last ~ "signdec",
                                                      is.na(up_first) ~ "notmod",
                                                      TRUE ~ "nonsign")) |> 
                           select(sppcode, sign),
  by = "sppcode")

# Join full group names back into dataframe
seed_smooth3 <- left_join(seed_smooth2,
                          plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                          by = c('sppcode'), 
                          relationship = 'many-to-many') |> 
                mutate(spp_grp = as.character(spp_grp)) |> 
                arrange(spp_grp)

# Plotting trends by species group facet
seed_trends <- 
  ggplot(seed_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
                           color = sign, fill = sign)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2) +
  geom_line(linewidth = 0.5) +
  scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
                                   "signinc" = 'solid', "signdec" = 'solid'), drop = FALSE) +
  scale_fill_manual(values = c("notmod" = "white", "nonsign" =  "#696969",
                               "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
                                "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE) +
  facet_wrap(~spp_grp, scales = 'free_y') + 
  labs(y = "Seedlings (stems/sq.m)", x = "Year") +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 3), 2023), 
                     limits = c(2005, 2024)) +
  theme_FHM() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        legend.position = 'bottom')

seed_trends

svg(paste0(new_path, "figures/", "Figure_XA_", park, "_smoothed_seedlings_by_species_cycle.svg"),
    height = 8, width = 7)
seed_trends
dev.off()

# Saplings
sap_smooth2 <- 
  left_join(sap_smooth, 
            sap_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

sap_smooth3 <- left_join(sap_smooth2,
                         plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                         by = c('sppcode'), 
                         relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |>
  group_by(sppcode) |> mutate(drop = ifelse(sum(estimate) == 0, "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  arrange(spp_grp)


sap_trends <- 
  ggplot(sap_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
                           color = sign, fill = sign)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2) +
  geom_line(linewidth = 0.5) +
  scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
                                   "signinc" = 'solid', "signdec" = 'solid'), drop = FALSE) +
  scale_fill_manual(values = c("notmod" = "white", "nonsign" =  "#696969",
                               "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
                                "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE) +
  facet_wrap(~spp_grp, scales = 'free_y') + 
  labs(y = "Saplings (stems/sq.m)", x = "Year") +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 3), 2023), 
                     limits = c(2005, 2024)) +
  theme_FHM() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom')

sap_trends

svg(paste0(new_path, "figures/", "Figure_XA_", park, "_smoothed_saplings_by_species_cycle.svg"),
    height = 8, width = 7)
sap_trends
dev.off()
  
#
head(sap_smooth3)

net_seeds <- 
  ggplot(seed_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = 'Year', y = "Average Seedling Density (stems/sq.m)") +
  theme_FHM()+
  scale_color_manual(values = c(
    #"Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
    "Acer spp. (maple)" = "#54FF00",
    "Betula spp. (birch)" = "#38A800",
    "Carya spp. (hickory)" = "#FFFF00",
    "Crataegus (hawthorns)" = "#9D0909", #SARA only
    #"Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
    "Fagus grandifolia (American beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#A87000",
    #"Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
    #"Larix decidua (European larch)" = "#C8CE00", # MABI only
    "Other Exotic spp." = "#FF0000", # off for MABI
    "Other Native spp." = "#828282",
    #"Picea abies (Norway spruce)" = "#C2037A",
    "Pinus spp. (pine)" = "#1D5104", # off for MORR 
    #"Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "#CBCC7E", # off for MABI
    "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
    "Quercus spp. (oak)" = "#C500FF",
    #"Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
    "Subcanopy spp." = "#FFBEE8",
    #"Tilia americana (American basswood)" = "#53CEF2", # MABI only
    #"Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "#59538A" # off for MABI
  ),
  name = NULL) +
    scale_linetype_manual(values = c(
      #"Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
      "Acer spp. (maple)" = "solid",
      "Betula spp. (birch)" = "solid",
      "Carya spp. (hickory)" = "solid",
      "Crataegus (hawthorns)" = "dotdash", #SARA only
      #"Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
      "Fagus grandifolia (American beech)" = "solid",
      "Fraxinus spp. (ash)" = "solid",
      #"Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
      #"Larix decidua (European larch)" = "solid", # MABI only
      "Other Exotic spp." = "dotdash", # off for MABI
      "Other Native spp." = "dotdash",
      #"Picea abies (Norway spruce)" = "solid",
      "Pinus spp. (pine)" = "solid", # off for MORR 
      #"Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
      "Populus spp. (poplar)" = "solid", # off for MABI
      "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
      "Quercus spp. (oak)" = "solid",
      #"Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
      "Subcanopy spp." = "solid",
      #"Tilia americana (American basswood)" = "solid", # MABI only
      #"Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
      "Ulmus spp. (elm)" = "solid" # off for MABI
    ),
    name = NULL) +
    scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                       limits = c(2006, 2023)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = 'bottom', 
          legend.key.width = unit(1, 'cm'))



net_seeds

svg(paste0(new_path, "figures/", "Figure_4A_", park, "_net_seedlings_by_species_cycle.svg"),
    height = 5.6, width = 9.5)
net_seeds
dev.off()

net_saps <- 
  ggplot(sap_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = 'Year', y = "Average Seedling Density (stems/sq.m)") +
  theme_FHM()+
  scale_color_manual(values = c(
    #"Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
    "Acer spp. (maple)" = "#54FF00",
    "Betula spp. (birch)" = "#38A800",
    "Carya spp. (hickory)" = "#FFFF00",
    "Crataegus (hawthorns)" = "#9D0909", #SARA only
    #"Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
    "Fagus grandifolia (American beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#A87000",
    #"Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
    #"Larix decidua (European larch)" = "#C8CE00", # MABI only
    "Other Exotic spp." = "#FF0000", # off for MABI
    "Other Native spp." = "#828282",
    #"Picea abies (Norway spruce)" = "#C2037A",
    "Pinus spp. (pine)" = "#1D5104", # off for MORR 
    #"Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "#CBCC7E", # off for MABI
    "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
    "Quercus spp. (oak)" = "#C500FF",
    #"Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
    "Subcanopy spp." = "#FFBEE8",
    #"Tilia americana (American basswood)" = "#53CEF2", # MABI only
    #"Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "#59538A" # off for MABI
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    #"Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
    "Acer spp. (maple)" = "solid",
    "Betula spp. (birch)" = "solid",
    "Carya spp. (hickory)" = "solid",
    "Crataegus (hawthorns)" = "dotdash", #SARA only
    #"Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
    "Fagus grandifolia (American beech)" = "solid",
    "Fraxinus spp. (ash)" = "solid",
    #"Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
    #"Larix decidua (European larch)" = "solid", # MABI only
    "Other Exotic spp." = "dotdash", # off for MABI
    "Other Native spp." = "dotdash",
    #"Picea abies (Norway spruce)" = "solid",
    "Pinus spp. (pine)" = "solid", # off for MORR 
    #"Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "solid", # off for MABI
    "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
    "Quercus spp. (oak)" = "solid",
    #"Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
    "Subcanopy spp." = "solid",
    #"Tilia americana (American basswood)" = "solid", # MABI only
    #"Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "solid" # off for MABI
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm'))

net_saps

svg(paste0(new_path, "figures/", "Figure_2_", park, "_net_saplings_by_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
net_saps
dev.off()

