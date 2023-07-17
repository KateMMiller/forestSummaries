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
exclude_stat <- c(#"DC",
  "DM","ES","EX","NL","XO","XS","0", "XP")
missed_stat <- "AM"

tree_mr <- tree_mr %>% mutate(status = case_when(TreeStatusCode %in% alive_stat ~ "alive",
                                                 TreeStatusCode %in% dead_stat ~ "dead",
                                                 TreeStatusCode %in% recruit_stat ~ "recruit",
                                                 TreeStatusCode %in% exclude_stat ~ "exclude",
                                                 TreeStatusCode %in% missed_stat ~ 'missed',
                                                 TreeStatusCode %in% "DC" ~ "cut",
                                                 TRUE ~ "UNK"),
                              BA_m2ha = BA_cm2/ifelse(ParkUnit == "ACAD", 225, 400),
                              stems = 1) %>%
  select(Plot_Name, ParkUnit, TagCode, PanelCode, cycle, num_plots, ScientificName, 
         status, DBHcm, stems, BA_m2ha) %>%
  arrange(Plot_Name, cycle) %>% 
  left_join(., prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName")



table(tree_mr$status)
treetab <- as.data.frame(table(tree_mr$ScientificName)) |> arrange(desc(Freq))
treetab

Acer_spp <- c('Acer', 'Acer saccharinum', 'Acer negundo', 'Acer rubrum')
Betula_spp <- c('Betula','Betula alleghaniensis', 'Betula lenta',  'Betula X cearulea',
            'Betula papyrifera', 'Betula populifolia', 'Betula cordifolia')
Carya_spp <- c('Carya', 'Carya cordiformis', 'Carya glabra', 'Carya ovata', 'Carya tomentosa')
Fraxinus_spp <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', 'Fraxinus nigra')
#Pinus_spp <- c("Pinus resinosa", "Pinus strobus", "Pinus", "Pinus rigida")
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
               #'Ostrya virginiana', 
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')
Exotic_spp <- c('Acer platanoides',  #Turned off for SAGA ROVA
                'Aesculus hippocastanum', 'Ailanthus altissima',
                'Crataegus', 'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 
                #'Picea abies', "Pinus sylvestris",  # turned off for MABI
                'Alnus serrulata', 'Catalpa speciosa',
                'Rhamnus cathartica', 'Salix alba')


tree_mr <- tree_mr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                               ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                               ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                               ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                               #ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                               #ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                               ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                               ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                               ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                               ScientificName %in% c(Other_Native, Prunus_spp, Populus_spp) ~ "Other Native spp.",
                               ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                               ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                               TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

as.data.frame(table(tree_mr$spp_grp)) |> arrange(desc(Freq))

tree_mr$stems[tree_mr$status == "exclude"] <- -999
tree_mr$BA_m2ha[tree_mr$status == "exclude"] <- -999
head(tree_mr)

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
names(trees_wide3)

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
    geom_line(aes(color = spp_grp, linetype = spp_grp), size = 1.5) +
    labs(x = 'Cycle', y = "Average Basal Area (sq.m/ha)") +
    theme_FHM()+
    scale_color_manual(values = c("Acer saccharum (sugar maple)" = "#54FF00",
                                  "Acer spp. (maple)" = "#338A06",
                                  "Betula spp. (birch)" = "#F6EA42",
                                  "Fagus grandifolia (American beech)" = "#FFAA00",
                                  "Fraxinus spp. (ash)" = "#A87000",
                                  "Larix decidua (European larch)" = "#9778B1", 
                                  "Ostrya virginiana (eastern hophornbeam)" = "#FF77F7",
                                  "Other Native spp." = "#828282",
                                  "Picea abies (Norway spruce)" = "#C2037A",
                                  "Pinus resinosa (red pine)" = "#E36B02",
                                  "Pinus strobus (eastern white pine)" = "#745C02",
                                  "Pinus sylvestris (Scots pine)" = '#F57A7A',
                                  #"Populus spp. (poplar)" = "#F6EA42", 
                                  "Prunus spp. (cherry)" ="#00E6A9", 
                                  "Quercus spp. (oak)" = "#C500FF",
                                  "Subcanopy spp." = "#FFBEE8",
                                  "Tsuga canadensis (eastern hemlock)" = "#005CE6"), name = NULL) +
  scale_linetype_manual(values = c("Acer saccharum (sugar maple)" = "solid",
                                   "Acer spp. (maple)" = "solid",
                                   "Betula spp. (birch)" = "solid",
                                   "Fagus grandifolia (American beech)" = "solid",
                                   "Fraxinus spp. (ash)" = "solid",
                                   "Larix decidua (European larch)" = "dotdash",
                                   "Ostrya virginiana (eastern hophornbeam)" = "solid", 
                                   "Other Native spp." = "solid",
                                   "Picea abies (Norway spruce)" = "dotdash",
                                   "Pinus resinosa (red pine)" = "solid",
                                   "Pinus strobus (eastern white pine)" = "solid",
                                   "Pinus sylvestris (Scots pine)" = 'dotdash',
                                   #"Populus spp. (poplar)" = "solid", 
                                   "Prunus spp. (cherry)" ="solid", 
                                   "Quercus spp. (oak)" = "solid",
                                   "Subcanopy spp." = "solid",
                                   "Tsuga canadensis (eastern hemlock)" = "solid"), name = NULL)

net_ba
svg(paste0(new_path, "figures/sppcomp/", "Figure_1_", park, "_net_BA_by_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
  net_ba
dev.off()

net_stems <- 
  ggplot(mor_rec_ov_long %>% filter(unit_type == "stem"), 
         aes(x = cycle, y = net_total)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), size = 1.5) +
  labs(x = 'Cycle', y = "Average Stem Density (stems/ha)") +
  theme_FHM()+
  scale_color_manual(values = c("Acer saccharum (sugar maple)" = "#54FF00",
                                "Acer spp. (maple)" = "#338A06",
                                "Betula spp. (birch)" = "#F6EA42",
                                "Fagus grandifolia (American beech)" = "#FFAA00",
                                "Fraxinus spp. (ash)" = "#A87000",
                                "Larix decidua (European larch)" = "#9778B1", 
                                "Ostrya virginiana (eastern hophornbeam)" = "#FF77F7",
                                "Other Native spp." = "#828282",
                                "Picea abies (Norway spruce)" = "#C2037A",
                                "Pinus resinosa (red pine)" = "#E36B02",
                                "Pinus strobus (eastern white pine)" = "#745C02",
                                "Pinus sylvestris (Scots pine)" = '#F57A7A',
                                #"Populus spp. (poplar)" = "#F6EA42", 
                                "Prunus spp. (cherry)" ="#00E6A9", 
                                "Quercus spp. (oak)" = "#C500FF",
                                "Subcanopy spp." = "#FFBEE8",
                                "Tsuga canadensis (eastern hemlock)" = "#005CE6"), name = NULL) +
  scale_linetype_manual(values = c("Acer saccharum (sugar maple)" = "solid",
                                   "Acer spp. (maple)" = "solid",
                                   "Betula spp. (birch)" = "solid",
                                   "Fagus grandifolia (American beech)" = "solid",
                                   "Fraxinus spp. (ash)" = "solid",
                                   "Larix decidua (European larch)" = "dotdash", 
                                   "Ostrya virginiana (eastern hophornbeam)" = "solid", 
                                   "Other Native spp." = "solid",
                                   "Picea abies (Norway spruce)" = "dotdash",
                                   "Pinus resinosa (red pine)" = "solid",
                                   "Pinus strobus (eastern white pine)" = "solid",
                                   "Pinus sylvestris (Scots pine)" = 'dotdash',
                                   #"Populus spp. (poplar)" = "solid", 
                                   "Prunus spp. (cherry)" ="solid", 
                                   "Quercus spp. (oak)" = "solid",
                                   "Subcanopy spp." = "solid",
                                   "Tsuga canadensis (eastern hemlock)" = "solid"), name = NULL)
  
net_stems
svg(paste0(new_path, "figures/sppcomp/", "Figure_1_", park, "_net_stems_by_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
  net_stems
dev.off()

#----- Similar figures for seedlings and saplings -----
reg <- do.call(joinRegenData, c(args_all, units = 'sq.m'))
head(reg)

regtab <- as.data.frame(table(reg$ScientificName)) |> arrange(desc(Freq))
regtab

Acer_spp <- c('Acer', 'Acer saccharinum', 'Acer negundo')#, 'Acer rubrum')
Betula_spp <- c('Betula','Betula alleghaniensis', 'Betula lenta',  'Betula X cearulea',
                'Betula papyrifera', 'Betula populifolia', 'Betula cordifolia')
Carya_spp <- c('Carya', 'Carya cordiformis', 'Carya glabra', 'Carya ovata', 'Carya tomentosa')
Fraxinus_spp <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', 'Fraxinus nigra')
#Pinus_spp <- c("Pinus resinosa", "Pinus strobus", "Pinus", "Pinus rigida")
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
               #'Ostrya virginiana', 
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')
Exotic_spp <- c('Acer platanoides',  #Turned off for SAGA ROVA
                'Aesculus hippocastanum', 'Ailanthus altissima',
                'Crataegus', 'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 
                #'Picea abies', "Pinus sylvestris",  # turned off for MABI
                'Alnus serrulata', 'Catalpa speciosa',
                'Rhamnus cathartica', 'Salix alba')

head(reg)
tlu_plants <- prepTaxa() %>% select(TSN, ScientificName, CommonName)
head(tlu_plants)
head(plot_evs)

reg_spp <- reg %>% left_join(., tlu_plants, by = c("TSN", "ScientificName")) %>%
  mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                             ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                             ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                             ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                             #ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                             #ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                             ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                             ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                             ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                             ScientificName %in% c(Other_Native, Prunus_spp, Populus_spp) ~ "Other Native spp.",
                             ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                             ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

head(reg_spp)

plots

reg_sum <- reg_spp %>% left_join(., plots, by = 'cycle') %>%
  group_by(Plot_Name, SampleYear, cycle, spp_grp, num_plots) %>%
  summarize(num_seeds = sum(seed_den), num_saps = sum(sap_den), .groups = 'keep') %>%
  group_by(cycle, spp_grp) %>%
  summarize(avg_seeds_m2 = sum(num_seeds)/first(num_plots), 
            avg_saps_m2 = sum(num_saps)/first(num_plots), .groups = 'keep')

net_seeds <- 
  ggplot(reg_sum, 
         aes(x = cycle, y = avg_seeds_m2)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), size = 1.5) +
  labs(x = 'Cycle', y = "Average Seedling Density (stems/sq.m)") +
  theme_FHM()+
  scale_color_manual(values = c("Acer rubrum (red maple)" = "#B0CB83",
                                "Acer saccharum (sugar maple)" = "#54FF00",
                                "Acer spp. (maple)" = "#338A06",
                                "Betula spp. (birch)" = "#F6EA42",
                                "Fagus grandifolia (American beech)" = "#FFAA00",
                                "Fraxinus spp. (ash)" = "#A87000",
                                "Larix decidua (European larch)" = "#9778B1", 
                                "Ostrya virginiana (eastern hophornbeam)" = "#FF77F7",
                                "Other Native spp." = "#828282",
                                "Picea abies (Norway spruce)" = "#C2037A",
                                "Pinus resinosa (red pine)" = "#E36B02",
                                "Pinus strobus (eastern white pine)" = "#745C02",
                                "Pinus sylvestris (Scots pine)" = '#F57A7A',
                                #"Populus spp. (poplar)" = "#F6EA42", 
                                "Prunus spp. (cherry)" ="#00E6A9", 
                                "Quercus spp. (oak)" = "#C500FF",
                                "Subcanopy spp." = "#FFBEE8",
                                "Tsuga canadensis (eastern hemlock)" = "#005CE6"), name = NULL) +
  scale_linetype_manual(values = c("Acer rubrum (red maple)" = "solid",
                                   "Acer saccharum (sugar maple)" = "solid",
                                   "Acer spp. (maple)" = "solid",
                                   "Betula spp. (birch)" = "solid",
                                   "Fagus grandifolia (American beech)" = "solid",
                                   "Fraxinus spp. (ash)" = "solid",
                                   "Larix decidua (European larch)" = "dotdash", 
                                   "Ostrya virginiana (eastern hophornbeam)" = "solid",
                                   "Other Native spp." = "solid",
                                   "Picea abies (Norway spruce)" = "dotdash",
                                   "Pinus resinosa (red pine)" = "solid",
                                   "Pinus strobus (eastern white pine)" = "solid",
                                   "Pinus sylvestris (Scots pine)" = 'dotdash',
                                   #"Populus spp. (poplar)" = "solid", 
                                   "Prunus spp. (cherry)" ="solid", 
                                   "Quercus spp. (oak)" = "solid",
                                   "Subcanopy spp." = "solid",
                                   "Tsuga canadensis (eastern hemlock)" = "solid"), name = NULL)

net_seeds

svg(paste0(new_path, "figures/sppcomp/", "Figure_2_", park, "_net_seedlings_by_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
net_seeds
dev.off()

net_saps <- 
  ggplot(reg_sum, 
         aes(x = cycle, y = avg_saps_m2)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), size = 1.5) +
  labs(x = 'Cycle', y = "Average Sapling Density (stems/sq.m)") +
  theme_FHM()+
  scale_color_manual(values = c("Acer rubrum (red maple)" = "#B0CB83",
                                "Acer saccharum (sugar maple)" = "#54FF00",
                                "Acer spp. (maple)" = "#338A06",
                                "Betula spp. (birch)" = "#F6EA42",
                                "Fagus grandifolia (American beech)" = "#FFAA00",
                                "Fraxinus spp. (ash)" = "#A87000",
                                "Larix decidua (European larch)" = "#9778B1", 
                                "Ostrya virginiana (eastern hophornbeam)" = "#FF77F7",
                                "Other Native spp." = "#828282",
                                "Picea abies (Norway spruce)" = "#C2037A",
                                "Pinus resinosa (red pine)" = "#E36B02",
                                "Pinus strobus (eastern white pine)" = "#745C02",
                                "Pinus sylvestris (Scots pine)" = '#F57A7A',
                                #"Populus spp. (poplar)" = "#F6EA42", 
                                "Prunus spp. (cherry)" ="#00E6A9", 
                                "Quercus spp. (oak)" = "#C500FF",
                                "Subcanopy spp." = "#FFBEE8",
                                "Tsuga canadensis (eastern hemlock)" = "#005CE6"), name = NULL) +
  scale_linetype_manual(values = c("Acer rubrum (red maple)" = "solid",
                                   "Acer saccharum (sugar maple)" = "solid",
                                   "Acer spp. (maple)" = "solid",
                                   "Betula spp. (birch)" = "solid",
                                   "Fagus grandifolia (American beech)" = "solid",
                                   "Fraxinus spp. (ash)" = "solid",
                                   "Larix decidua (European larch)" = "dotdash", 
                                   "Ostrya virginiana (eastern hophornbeam)" = "solid",
                                   "Other Native spp." = "solid",
                                   "Picea abies (Norway spruce)" = "dotdash",
                                   "Pinus resinosa (red pine)" = "solid",
                                   "Pinus strobus (eastern white pine)" = "solid",
                                   "Pinus sylvestris (Scots pine)" = 'dotdash',
                                   #"Populus spp. (poplar)" = "solid", 
                                   "Prunus spp. (cherry)" ="solid", 
                                   "Quercus spp. (oak)" = "solid",
                                   "Subcanopy spp." = "solid",
                                   "Tsuga canadensis (eastern hemlock)" = "solid"), name = NULL)

net_saps

svg(paste0(new_path, "figures/sppcomp/", "Figure_2_", park, "_net_saplings_by_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
net_saps
dev.off()

#----- Understory composition -----
tlu_plants <- prepTaxa() %>% select(TSN, ScientificName, CommonName, Genus, Family)
head(tlu_plants)

quads <- do.call(joinQuadSpecies, args_all) %>% filter(IsGerminant == FALSE) %>%
  left_join(., tlu_plants, by = c("TSN", "ScientificName")) %>% 
  left_join(., plots, by = 'cycle') %>%
  select(Plot_Name, cycle, Family, Genus, ScientificName, CommonName, quad_avg_cov, quad_pct_freq, 
         Tree, TreeShrub, Shrub, Vine, Herbaceous, Graminoid, FernAlly, InvasiveNETN, Exotic, num_plots)

domspp <- quads %>% 
  group_by(ScientificName, cycle, num_plots) %>% 
  summarize(avg_cover = sum(quad_avg_cov)/num_plots, avg_freq = sum(quad_pct_freq)/num_plots, .groups = 'drop') %>%
  arrange(desc(avg_cover), ScientificName)

sort(unique(domspp$ScientificName))

head(quads)

domgen <- quads %>% 
  group_by(Family, Genus, Tree, TreeShrub, Shrub, Vine, Herbaceous, Graminoid, FernAlly, Exotic, InvasiveNETN, num_plots) %>% 
  summarize(avg_cover = sum(quad_avg_cov)/num_plots, avg_freq = sum(quad_pct_freq)/num_plots, .groups = 'drop') %>%
  arrange(desc(avg_cover), Genus) %>%
  mutate(rank = rank(avg_cover))

quad_grps <- quads |> mutate(spp_grp = case_when(InvasiveNETN == TRUE & (Tree == 1 | TreeShrub == 1 | 
                                                                           Shrub == 1 | Vine == 1) ~ "Invasive woody spp.",
                                                 Exotic == TRUE & InvasiveNETN == FALSE & 
                                                   (Tree == 1 | TreeShrub == 1 | Shrub == 1 | Vine == 1) ~ "Exotic woody spp.",
                                                 Genus == "Rubus" ~ "Rubus spp. (brambles)",
                                                 Exotic == FALSE & (TreeShrub == 1 | Shrub == 1 | Vine == 1) ~ "Native shrub spp.",
                                                 Genus == "Vinca" ~ "Exotic woody spp.",
                                                 #InvasiveNETN == TRUE & (Herbaceous == TRUE | Graminoid == TRUE) ~ "Invasive forb spp.",
                                                 ScientificName == "Alliaria petiolata" ~ "Alliaria petiolata (garlic mustard)",
                                                 InvasiveNETN == FALSE & Exotic == TRUE & 
                                                   (Herbaceous == TRUE | Graminoid == TRUE) ~ "Exotic herb. spp.",
                                                 #Family == "Poaceae" ~ "Poaceae (grasses)", 
                                                 Exotic == FALSE & Graminoid == TRUE ~ "Native graminoids",
                                                 Genus == "Carex" ~ "Carex spp. (sedges)",
                                                 Family %in% c("Juncaceae", "Cyperaceae") & 
                                                   !Genus %in% "Carex" ~ "Other graminoids",
                                                 ScientificName == "Ageratina altissima" ~ "Ageratina altissima (white snakeroot)",
                                                 Family == "Asteraceae" ~ "Asteraceae (asters and goldenrods)",
                                                 FernAlly == TRUE ~ "Ferns",
                                                 Genus == "Arisaema" ~ "Arisaema triphyllum (jack in the pulpit)",
                                                 Genus == "Viola" ~ "Viola spp. (violets)",
                                                 #Genus == "Galium" ~ "Galium spp. (bedstraw)",
                                                 #ScientificName %in% c("Rubus (dewberry)", "Rubus pubescens") ~ "Rubus (dewberry)",
                                                 #Family == "Saxifragaceae" ~ "Saxifragaceae (saxifrages)",
                                                 Family == "Saxifragaceae" ~ "Native perennial herb. spp.",
                                                 Family %in% c("Asparagaceae", "Liliaceae", 
                                                               "Melanthiaceae", "Colchicaceae") ~ #"Liliales (lily superfamily)",
                                                   "Native perennial herb. spp.",
                                                 ScientificName %in% c("Actaea", "Actaea pachypoda", "Actaea rubra",
                                                                       "Anemone canadensis", "Anemone quinquefolia",
                                                                       "Aralia nudicaulis", "Aralia racemosa", "Cardamine", 
                                                                       "Cardamine pensylvanica", "Caulophyllum thalictroides",
                                                                       "Circaea", "Circaea lutetiana", "Circaea canadensis", 
                                                                       "Geranium maculatum", "Geranium", "Goodyera", "Laportea canadensis",
                                                                       "Mitella diphylla", "Mitella nuda", "Osmorhiza claytonii", 
                                                                       "Oxalis montana", "Phryma leptostachya", "Pyrola", "Pyrola elliptica",
                                                                       "Ranunculus abortivus", "Ranunculus hispidus", "Ranunculus recurvatus",
                                                                       "Sanicula", "Sanicula marilandica", "Paronychia canadensis", 
                                                                       "Mitchella repens", "Rubus (dewberry)", "Rubus pubescens") ~ 
                                                   "Native perennial herb. spp.",
                                                 Genus %in% c("Asarum", "Claytonia", "Dicentra", "Erythronium") ~ 
                                                   "Native perennial herb. spp.",
                                                 ScientificName %in% c("Allium tricoccum", "Cardamine diphylla") ~ 
                                                   "Native perennial herb. spp.",
                                                 ScientificName %in% c("Asclepias syriaca", "Chelone glabra", "Epilobium", 
                                                                       "Fragaria vesca", "Fragaria virginiana", "Geum", 
                                                                       "Impatiens", "Impatiens capensis", "Impatiens pallida",
                                                                       "Oxalis", "Oxalis stricta", "Pilea pumila", "Polygonum achoreum",
                                                                       "Potentilla simplex", "Potentilla norvegica", "Ranunculus", 
                                                                       "Rumex", "Scutellaria lateriflora",
                                                                       "Sisyrinchium angustifolium", "Veronica") ~
                                                   "Native weedy herb. spp.",
                                                 Genus == "Galium" ~ "Native weedy herb. spp.",
                                                 ScientificName == "Fagus grandifolia" ~ "Fagus grandifolia (American beech)",
                                                 ScientificName == "Ostrya virginiana" ~ "Ostrya virginiana (eastern hophornbeam)",
                                                 Genus == 'Fraxinus' ~ "Fraxinus spp. (ash)",
                                                 ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                                 Exotic == FALSE & Tree == 1 ~ "Native tree spp.",
                                                 TRUE ~ "Other")) |> 
  filter(! ScientificName %in% c("Unknown Herb - 01", "Unknown Herb - 02"))

as.data.frame(table(quad_grps$spp_grp)) |> arrange(desc(Freq))

table(quad_grps$spp_grp)

quad_sum <- quad_grps |> group_by(cycle, spp_grp, num_plots) |> 
  summarize(avg_cov = (sum(quad_avg_cov))/num_plots, avg_freq = (sum(quad_pct_freq))/num_plots)

head(quad_sum)

cover_plot <- 
  ggplot(quad_sum, 
         aes(x = cycle, y = avg_cov)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), size = 1.5) +
  labs(x = 'Cycle', y = "Average % Quadrat Cover") +
  theme_FHM()+
  scale_color_manual(values = c(
                                "Alliaria petiolata (garlic mustard)" = "#C2037A",
                                "Exotic herb. spp." = "#F981FF", 
                                "Exotic woody spp." = "#B559BA", 
                                "Invasive woody spp." = "#FF0000", 
                                "Ageratina altissima (white snakeroot)" = "#F96262",
                                "Arisaema triphyllum (jack in the pulpit)" = "#2FBDFF",
                                "Asteraceae (asters and goldenrods)" = "#F6EA42",
                                "Ferns" = "#B0CB83",
                                "Rubus spp. (brambles)" = "#C500FF",
                                "Viola spp. (violets)" = "#FFBEE8",
                                #"Liliales (lily superfamily)" = "#0071E1",
                                "Native perennial herb. spp." = "#3AB000",
                                "Native graminoids" = "#995902",
                                "Native shrub spp." = "#946DAB",
                                "Native tree spp." = "#338A06",
                                "Acer saccharum (sugar maple)" = "#54FF00",
                                "Fagus grandifolia (American beech)" = "#FFAA00",
                                "Fraxinus spp. (ash)" = "#A87000",
                                "Ostrya virginiana (eastern hophornbeam)" = "#FF77F7",
                                "Native weedy herb. spp." = "#FFD277"
                                ), name = NULL) +
  scale_linetype_manual(values = c("Alliaria petiolata (garlic mustard)" = "dotdash",
                                   "Exotic herb. spp." = "dotdash", 
                                   "Exotic woody spp." = "dotdash", 
                                   "Invasive woody spp." = "dotdash", 
                                   "Ageratina altissima (white snakeroot)" = "solid",
                                   "Arisaema triphyllum (jack in the pulpit)" = "solid",
                                   "Asteraceae (asters and goldenrods)" = "solid",
                                   "Ferns" = "solid",
                                   "Rubus spp. (brambles)" = "solid",
                                   "Viola spp. (violets)" = "solid",
                                   #"Liliales (lily superfamily)" = "solid",
                                   "Native perennial herb. spp." = "dotdash",
                                   "Native graminoids" = "dotdash",
                                   "Native shrub spp." = "solid",
                                   "Native tree spp." = "solid",
                                   "Acer saccharum (sugar maple)" = "solid",
                                   "Fagus grandifolia (American beech)" = "solid",
                                   "Fraxinus spp. (ash)" = "solid",
                                   "Ostrya virginiana (eastern hophornbeam)" = "solid",
                                   "Native weedy herb. spp." = "dotdash"
                                   ), name = NULL)

cover_plot

svg(paste0(new_path, "figures/sppcomp/", "Figure_3_", park, "_quad_cover_by_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
cover_plot
dev.off()
table(quad_sum$spp_grp)

lessdom <- c("Ageratina altissima (white snakeroot)", "Alliaria petiolata (garlic mustard)",
             "Arisaema triphyllum (jack in the pulpit)", "Asteraceae (asters and goldenrods)",
             "Exotic herb. spp.", "Exotic woody spp.", "Invasive woody spp.", 
             "Native graminoids", "Native perennial herb. spp.", 
             "Native shrub spp.", "Native weedy herb. spp.")#, "Viola spp. (violets)")

cover_plot_lessdom <- 
  ggplot(quad_sum |> 
           filter(spp_grp %in% lessdom), 
         aes(x = cycle, y = avg_cov)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), size = 1.5) +
  labs(x = 'Cycle', y = "Average % Quadrat Cover") +
  theme_FHM()+
  scale_color_manual(values = c(
    "Alliaria petiolata (garlic mustard)" = "#C2037A",
    "Exotic herb. spp." = "#F981FF", 
    "Exotic woody spp." = "#B559BA", 
    "Invasive woody spp." = "#FF0000", 
    "Ageratina altissima (white snakeroot)" = "#F96262",
    "Arisaema triphyllum (jack in the pulpit)" = "#2FBDFF",
    "Asteraceae (asters and goldenrods)" = "#F6EA42",
    #"Ferns" = "#B0CB83",
    #"Rubus spp. (brambles)" = "#C500FF",
    #"Viola spp. (violets)" = "#FFBEE8",
    #"Liliales (lily superfamily)" = "#0071E1",
    "Native perennial herb. spp." = "#3AB000",
    "Native graminoids" = "#995902",
    "Native shrub spp." = "#946DAB",
    #"Native tree spp." = "#338A06",
    "Native weedy herb. spp." = "#FFD277"
  ), name = NULL) +
  scale_linetype_manual(values = c("Alliaria petiolata (garlic mustard)" = "dotdash",
                                   "Exotic herb. spp." = "dotdash", 
                                   "Exotic woody spp." = "dotdash", 
                                   "Invasive woody spp." = "dotdash", 
                                   "Ageratina altissima (white snakeroot)" = "solid",
                                   "Arisaema triphyllum (jack in the pulpit)" = "solid",
                                   "Asteraceae (asters and goldenrods)" = "solid",
                                   #"Ferns" = "solid",
                                   #"Rubus spp. (brambles)" = "solid",
                                   #"Viola spp. (violets)" = "solid",
                                   #"Liliales (lily superfamily)" = "solid",
                                   "Native perennial herb. spp." = "dotdash",
                                   "Native graminoids" = "dotdash",
                                   "Native shrub spp." = "solid",
                                   #"Native tree spp." = "solid",
                                   "Native weedy herb. spp." = "solid"
  ), name = NULL)

cover_plot_lessdom

svg(paste0(new_path, "figures/sppcomp/", "Figure_3_", park, "_quad_cover_by_lessdom_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
cover_plot_lessdom
dev.off()

# Native shrubs are cornus alternifolia, Sambucus spp., PARQUI and Vitis
#--- Need to fix this so sums to 100, currently sums a hit for every species per group.
freq_plot <- 
  ggplot(quad_sum, 
         aes(x = cycle, y = avg_freq)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), size = 1.5) +
  labs(x = 'Cycle', y = "Average % Quadrat Frequency") +
  theme_FHM()+
  scale_color_manual(values = c(
    "Alliaria petiolata (garlic mustard)" = "#C2037A",
    "Exotic herb. spp." = "#F981FF", 
    "Exotic woody spp." = "#B559BA", 
    "Invasive woody spp." = "#FF0000", 
    "Ageratina altissima (white snakeroot)" = "#F96262",
    "Arisaema triphyllum (jack in the pulpit)" = "#2FBDFF",
    "Asteraceae (asters and goldenrods)" = "#F6EA42",
    "Ferns" = "#B0CB83",
    "Rubus spp. (brambles)" = "#C500FF",
    "Viola spp. (violets)" = "#FFBEE8",
    #"Liliales (lily superfamily)" = "#0071E1",
    "Native perennial herb. spp." = "#54FF00",
    "Native graminoids" = "#995902",
    "Native shrub spp." = "#946DAB",
    "Native tree spp." = "#338A06",
    "Native weedy herb. spp." = "#FFAA00"
  ), name = NULL) +
  scale_linetype_manual(values = c("Alliaria petiolata (garlic mustard)" = "dotdash",
                                   "Exotic herb. spp." = "dotdash", 
                                   "Exotic woody spp." = "dotdash", 
                                   "Invasive woody spp." = "dotdash", 
                                   "Ageratina altissima (white snakeroot)" = "solid",
                                   "Arisaema triphyllum (jack in the pulpit)" = "solid",
                                   "Asteraceae (asters and goldenrods)" = "solid",
                                   "Ferns" = "solid",
                                   "Rubus spp. (brambles)" = "solid",
                                   "Viola spp. (violets)" = "solid",
                                   #"Liliales (lily superfamily)" = "solid",
                                   "Native perennial herb. spp." = "solid",
                                   "Native graminoids" = "solid",
                                   "Native shrub spp." = "solid",
                                   "Native tree spp." = "solid",
                                   "Native weedy herb. spp." = "solid"
  ), name = NULL)

freq_plot

svg(paste0(new_path, "figures/sppcomp/", "Figure_3_", park, "_quad_freq_by_species_cycle_20230620.svg"),
    height = 5.6, width = 9.5)
freq_plot
dev.off()


