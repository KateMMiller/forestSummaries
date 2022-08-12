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
Fraxinus_spp <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica')
Pinus_spp <- c("Pinus resinosa", "Pinus strobus", "Pinus")
Populus_spp <- c('Populus', 'Populus deltoides', 'Populus grandidentata', 'Populus tremuloides')
Prunus_spp <- c('Prunus', 'Prunus serotina', 'Prunus virginiana')
Quercus_spp <- c('Quercus', 'Quercus (Red group)', 'Quercus (White group)',
             'Quercus alba', 'Quercus bicolor', 'Quercus coccinea',
             'Quercus montana', 'Quercus palustris', 'Quercus rubra',
             'Quercus velutina')
Ulmus_spp <- c("Ulmus", "Ulmus americana", "Ulmus rubra")

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

if(park == "MORR"){
  tree_mr <- tree_mr %>% 
    mutate(spp_grp = case_when(ScientificName %in% Acer_spp ~ "Acer spp. (maple)",
                               ScientificName %in% Betula_spp ~ "Betula spp. (birch)",
                               ScientificName %in% Carya_spp ~ "Carya spp. (hickory)",
                               ScientificName %in% Fraxinus_spp ~ "Fraxinus spp. (ash)",
                               #ScientificName %in% Pinus_spp ~ "Pinus spp. (pine)",
                               #ScientificName %in% Populus_spp ~ "Populus spp. (poplar)",
                               #ScientificName %in% Prunus_spp ~ "Prunus spp. (cherry)",
                               ScientificName %in% Quercus_spp ~ "Quercus spp. (oak)",
                               ScientificName %in% Ulmus_spp ~ "Ulmus spp. (elm)",
                               ScientificName %in% 
                                 c(Other_Native, Pinus_spp, Populus_spp, Prunus_spp) ~ "Other Native spp.",
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

tree_mr$stems[tree_mr$status == "exclude"] <- -999
tree_mr$BA_m2ha[tree_mr$status == "exclude"] <- -999

full_names <- c("DBHcm_2_missed", 'DBHcm_3_missed', 'DBHcm_4_missed', # "DBHcm_5_missed",
                "stems_1_alive", "stems_1_dead",  
                "stems_2_alive", "stems_2_dead", "stems_2_exclude", "stems_2_recruit", "stems_2_missed",
                "stems_3_alive", "stems_3_dead", "stems_3_exclude", "stems_3_recruit", "stems_3_missed",   
                "stems_4_alive", "stems_4_dead", "stems_4_exclude", "stems_4_recruit", "stems_4_missed",  
#                "stems_5_alive", "stems_5_dead", "stems_5_exclude", "stems_5_recruit", "stems_5_missed",  
                "BA_m2ha_1_alive", "BA_m2ha_1_dead",  
                "BA_m2ha_2_alive", "BA_m2ha_2_dead", "BA_m2ha_2_exclude", "BA_m2ha_2_recruit", "BA_m2ha_2_missed",
                "BA_m2ha_3_alive", "BA_m2ha_3_dead", "BA_m2ha_3_exclude", "BA_m2ha_3_recruit", "BA_m2ha_3_missed",   
                "BA_m2ha_4_alive", "BA_m2ha_4_dead", "BA_m2ha_4_exclude", "BA_m2ha_4_recruit", "BA_m2ha_4_missed" #,   
                #"stems_5_alive", "stems_5_dead", "stems_5_exclude", "stems_5_recruit", "stems_5_missed"  
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
                                                     #stems_5_exclude == -999 ~ 1,
                                                     TRUE ~ 0)) %>%
                          filter(exclude != 1)

trees_wide3 <- trees_wide2 %>%
  select(ParkUnit, Plot_Name, PanelCode, TagCode, num_plots, spp_grp, 
         DBHcm_2_missed, DBHcm_3_missed, DBHcm_4_missed, #DBHcm_5_missed,
         stems_1_alive, 
         stems_2_alive, stems_2_dead, stems_2_recruit, stems_2_missed,
         stems_3_alive, stems_3_dead, stems_3_recruit, stems_3_missed,
         stems_4_alive, stems_4_dead, stems_4_recruit, stems_4_missed,
         #stems_5_alive, stems_5_dead, stems_5_recruit, stems_5_missed,
         
         BA_m2ha_1_alive, 
         BA_m2ha_2_alive, BA_m2ha_2_dead, BA_m2ha_2_recruit, BA_m2ha_2_missed,
         BA_m2ha_3_alive, BA_m2ha_3_dead, BA_m2ha_3_recruit, BA_m2ha_3_missed,
         BA_m2ha_4_alive, BA_m2ha_4_dead, BA_m2ha_4_recruit, BA_m2ha_4_missed#,
         #BA_m2ha_5_alive, BA_m2ha_5_dead, BA_m2ha_5_recruit, BA_m2ha_5_missed
         )

# Add stems that were missed in earlier visits and added later. Use 12.0 as cutoff for whether a
# stem should be a recruit and new to that visit or missed with stem added to previous visits.
# Each subsequent visit adds 2cm to DBH cutoff to determine RS or missed. The allows for a stem
# to be added to previous visits, but only if > the cutoff for RS. The stems_#_alive_adj includes
# stems that are recruits
trees_wide3 <- trees_wide3 %>% mutate(
  stems_1_alive_adj = ifelse(stems_2_missed == 1 | 
                            (stems_3_missed == 1 & DBHcm_3_missed > 14.0) | 
                            (stems_4_missed == 1 & DBHcm_4_missed > 16.0), #|
                            #(stems_5_missed == 1 & DBHcm > 18.0), # for cycle 5 in ACAD
                              1, stems_1_alive), 
  stems_2_alive_adj = ifelse(stems_2_missed == 1 |
                             stems_3_missed == 1 | 
                            (stems_4_missed == 1 & DBHcm_4_missed > 14.0) | 
                            #(stems_5_missed == 1 & DBHcm > 16.0) | # for cycle 5 in ACAD
                             stems_2_recruit == 1, 
                               1, stems_2_alive), 
  stems_3_alive_adj = ifelse(stems_3_missed == 1 |
                             stems_4_missed == 1 |
                            #(stems_5_missed == 1 & DBHcm > 14.0) | # for cycle 5 in ACAD
                             stems_3_recruit == 1, 
                             1, stems_3_alive),
  stems_4_alive_adj = ifelse(stems_4_missed == 1 | 
                            #(stems_5_missed == 1) | # for cycle 5 in ACAD  
                             stems_4_recruit == 1, 
                            1, stems_4_alive),
# stems_5_alive_adj = ifelse(stems_5_missed == 1 | stems_5_recruit == 1, 1, stems_5_alive),

# Use adjusted stem values for live ba
  BA_m2ha_1_alive_adj = ifelse(stems_1_alive_adj == 1, BA_m2ha_1_alive, 0),
  BA_m2ha_2_alive_adj = ifelse(stems_2_alive_adj == 1, BA_m2ha_2_alive, 0),
  BA_m2ha_3_alive_adj = ifelse(stems_3_alive_adj == 1, BA_m2ha_3_alive, 0), 
  BA_m2ha_4_alive_adj = ifelse(stems_4_alive_adj == 1, BA_m2ha_4_alive, 0), 
  # BA_m2ha_5_alive_adj = ifelse(stems_5_alive_ajd == 1, BA_m2ha_5_alive, 0), 

# Determine stems that died between cycles         
  c1_2_dead = ifelse(stems_1_alive_adj == 1 & stems_2_dead == 1, 1, 0), # must be alive first
  c2_3_dead = ifelse(stems_2_alive_adj == 1 & stems_3_dead == 1, 1, 0),
  c3_4_dead = ifelse(stems_3_alive_adj == 1 & stems_4_dead == 1, 1, 0),
  #c4_5_dead = ifelse(stems_4_alive_adj == 1 & stems_5_dead == 1, 1, 0),
         
# Determine stems that recruited between cycles  
  c1_2_recr = ifelse(stems_2_recruit == 1, 1, 0),
  c2_3_recr = ifelse(stems_3_recruit == 1, 1, 0),
  c3_4_recr = ifelse(stems_4_recruit == 1, 1, 0),
  #c4_5_recr = ifelse(stems_5_recruit == 1, 1, 0),
        
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
  # c4_5_dead_ba = case_when(c4_5_dead == 1 & BA_m2ha_5_dead > 0 ~ BA_m2ha_5_dead,
  #                          c4_5_dead == 1 & is.na(BA_m2ha_5_dead) ~ BA_m2ha_4_alive_adj,
  #                          TRUE ~ 0),

  c1_2_recr_ba = ifelse(stems_2_recruit == 1, BA_m2ha_2_recruit, 0),
  c2_3_recr_ba = ifelse(stems_3_recruit == 1, BA_m2ha_3_recruit, 0),
  c3_4_recr_ba = ifelse(stems_4_recruit == 1, BA_m2ha_4_recruit, 0),
  #c4_5_recr_ba = ifelse(stems_5_recruit == 1, BA_m2ha_5_recruit, 0),
         
# Calculate BA gained through stem growth of existing trees. Calculation doesn't include recruits
  c1_2_grow_ba = ifelse(stems_2_alive_adj == 1 & stems_1_alive_adj == 1,
                        BA_m2ha_2_alive - BA_m2ha_1_alive, 0),
  c2_3_grow_ba = ifelse(stems_3_alive_adj == 1 & stems_2_alive_adj == 1,
                        BA_m2ha_3_alive - BA_m2ha_2_alive, 0),
  c3_4_grow_ba = ifelse(stems_4_alive_adj == 1 & stems_3_alive_adj == 1, 
                        BA_m2ha_4_alive - BA_m2ha_3_alive, 0) # ,
  #c4_5_grow_ba = ifelse(stems_5_alive_adj == 1 & stems_4_alive_adj == 1, 
  #                      BA_m2ha_5_alive - BA_m2ha_4_alive, 0))
)

  # check <- trees_wide3 %>% filter(Plot_Name == "MORR-001" & stems_2_dead == 1)

# Summarize mortality and recruitment at plot level, converted to annual rate. 
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
    #c5_live_stem = sum(stems_5_alive_adj, na.rm = T),
            
    c1_2_mort_stem_yr = sum(c1_2_dead, na.rm = T)/4,
    c2_3_mort_stem_yr = sum(c2_3_dead, na.rm = T)/4,
    c3_4_mort_stem_yr = sum(c3_4_dead, na.rm = T)/c4_denom,
    #c4_5_mort_stem = sum(c4_5_dead, na.rm = T),
            
    c1_2_recr_stem_yr = sum(c1_2_recr, na.rm = T)/4,
    c2_3_recr_stem_yr = sum(c2_3_recr, na.rm = T)/4,
    c3_4_recr_stem_yr = sum(c3_4_recr, na.rm = T)/c4_denom,
    #c4_5_recr_stem = sum(c4_5_recr, na.rm = T),
            
    c1_live_BA = sum(BA_m2ha_1_alive_adj, na.rm = T), 
    c2_live_BA = sum(BA_m2ha_2_alive_adj, na.rm = T),
    c3_live_BA = sum(BA_m2ha_3_alive_adj, na.rm = T),
    c4_live_BA = sum(BA_m2ha_4_alive_adj, na.rm = T),
    #c5_live_BA = sum(BA_m2ha_5_alive_adj, na.rm = T),
            
    c1_2_mort_BA_yr = sum(c1_2_dead_ba, na.rm = T)/4,
    c2_3_mort_BA_yr = sum(c2_3_dead_ba, na.rm = T)/4,
    c3_4_mort_BA_yr = sum(c3_4_dead_ba, na.rm = T)/c4_denom,
    #c4_5_mort_BA = sum(c4_5_dead_ba, na.rm = T),
            
    c1_2_recr_BA_yr = sum(c1_2_recr_ba, na.rm = T)/4,
    c2_3_recr_BA_yr = sum(c2_3_recr_ba, na.rm = T)/4,
    c3_4_recr_BA_yr = sum(c3_4_recr_ba, na.rm = T)/c4_denom,
    #c4_5_recr_BA = sum(c4_5_recr_ba, na.rm = T),
            
    c1_2_incgr_BA_yr = sum(c1_2_grow_ba, na.rm = T)/4,
    c2_3_incgr_BA_yr = sum(c2_3_grow_ba, na.rm = T)/4,
    c3_4_incgr_BA_yr = sum(c3_4_grow_ba, na.rm = T)/c4_denom,
    #c4_5_incgr_BA = sum(c4_5_grow_ba, na.rm = T),
            
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
  
  c1_live_BA = sum(c1_live_BA, na.rm = T),
  c1_live_stem = sum(c1_live_stem, na.rm = T),
  c2_live_BA = sum(c2_live_BA, na.rm = T),
  c2_live_stem = sum(c2_live_stem, na.rm = T),
  c3_live_BA = sum(c3_live_BA, na.rm = T),
  c3_live_stem = sum(c3_live_stem, na.rm = T),
  c4_live_BA = sum(c4_live_BA, na.rm = T),
  c4_live_stem = sum(c4_live_stem, na.rm = T),
  
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
            .groups = 'drop') %>% 
  select(-corr) %>% 
  data.frame() 

mor_rec_ov_long <- mor_rec_overall %>% 
  pivot_longer(-c(spp_grp), names_to = 'metric', values_to = 'net_total') %>% 
  mutate(cycle = as.numeric(substr(metric, 2, 2)),
         unit_type = ifelse(grepl("BA", metric), "BA", "stem"))

mor_rec_tot <- mor_rec_ov_long %>% group_by(metric, cycle, unit_type) %>% 
  summarize(park_total = sum(net_total, na.rm = T), .groups = 'drop')

net_ba <- 
  ggplot(mor_rec_ov_long %>% filter(unit_type == "BA"), 
       aes(x = cycle, y = net_total)) +
    geom_line(aes(color = spp_grp), size = 1.5) +
    labs(x = 'Cycle', y = "Average Basal Area (sq.m/ha)") +
    # geom_line(data = mor_rec_tot %>% filter(unit_type == "BA"), 
    #           aes(x = cycle, y = park_total), color = 'black') + 
    theme_FHM()+
    # may have to update for different parks
    scale_color_manual(values = c("Acer spp. (maple)" = "#54FF00",
                                  "Betula spp. (birch)" = "#38A800",
                                  "Carya spp. (hickory)" = "#FFFF00",
                                  "Fagus grandifolia (American beech)" = "#FFAA00",
                                  "Fraxinus spp. (ash)" = "#A87000",
                                  "Liriodendron tulipifera (tuliptree)" = "#73AAFF",
                                  "Other Exotic spp." = "#FF0000",
                                  "Other Native spp." = "#828282",
                                  #"Pinus spp. (pine)" = "#286F05",
                                  #"Prunus spp. (cherry)" ="#00E6A9",  
                                  "Quercus spp. (oak)" = "#C500FF",
                                  "Robinia pseudoacacia (black locust)" = "#CBCC7E",
                                  "Subcanopy spp." = "#FFBEE8",
                                  "Ulmus spp. (elm)" = "#59538A"),
                    name = NULL)

svg(paste0(new_path, "figures/", "Figure_3_", park, "_net_BA_by_species_cycle.svg"),
    height = 5.6, width = 9.5)
  net_ba
dev.off()

net_stems <- 
  ggplot(mor_rec_ov_long %>% filter(unit_type == "stem"), 
         aes(x = cycle, y = net_total)) +
    geom_line(aes(color = spp_grp), size = 1.5) +
    labs(x = 'Cycle', y = "Average Stem Density (stems/ha)") +
    # geom_line(data = mor_rec_tot %>% filter(unit_type == "stem"),
    #           aes(x = cycle, y = park_total), color = 'black') +
    theme_FHM()+
    # may have to update for different parks
    scale_color_manual(values = c("Acer spp. (maple)" = "#54FF00",
                                  "Betula spp. (birch)" = "#38A800",
                                  "Carya spp. (hickory)" = "#FFFF00",
                                  "Fagus grandifolia (American beech)" = "#FFAA00",
                                  "Fraxinus spp. (ash)" = "#A87000",
                                  "Liriodendron tulipifera (tuliptree)" = "#73AAFF",
                                  "Other Exotic spp." = "#FF0000",
                                  "Other Native spp." = "#828282",
                                  #"Pinus spp. (pine)" = "#286F05",
                                  #"Prunus spp. (cherry)" ="#00E6A9",  
                                  "Quercus spp. (oak)" = "#C500FF",
                                  "Robinia pseudoacacia (black locust)" = "#CBCC7E",
                                  "Subcanopy spp." = "#FFBEE8",
                                  "Ulmus spp. (elm)" = "#59538A"),
                     name = NULL)
       
svg(paste0(new_path, "figures/", "Figure_3_", park, "_net_stems_by_species_cycle.svg"),
    height = 5.6, width = 9.5)
  net_stems
dev.off()

# Set up data for traditional mort/recr plots
mor_rec_spp_long <- mor_rec2 %>% 
  select(spp_grp, 
         mortBA12, mortBA23, mortBA34, mortStem12, mortStem23, mortStem34,
         recrBA12, recrBA23, recrBA34, recrStem12, recrStem23, recrStem34
         ) %>% 
  pivot_longer(-c(spp_grp), names_to = 'metric', values_to = 'rate') %>% 
  mutate(cycle = as.numeric(substr(metric, nchar(metric), nchar(metric))),
         rate_type = substr(metric, 1, 4),
         unit_type = ifelse(grepl("BA", metric), "BA", "stem"))

mor_rec_spp_long$rate[is.na(mor_rec_spp_long$rate)] <- 0
head(mor_rec_spp_long)

BA_max = max(ceiling(abs(mor_rec_spp_long$rate[mor_rec_spp_long$unit_type == "BA"]))) + 1
stem_max = max(ceiling(abs(mor_rec_spp_long$rate[mor_rec_spp_long$unit_type == "stem"]))) + 1
BA_max
stem_max

# For ggplot order
mor_rec_spp_long$met_fac <- factor(mor_rec_spp_long$metric, 
                                   levels = c(
                                     "recrBA34",
                                     "recrBA23", 
                                     "recrBA12", 
                                     "mortBA34",
                                     "mortBA23", 
                                     "mortBA12", 
                                     "recrStem34",
                                     "recrStem23", 
                                     "recrStem12", 
                                     "mortStem34",
                                     "mortStem23", 
                                     "mortStem12" 
                                   ))

# Drop species that didn't experience mortality or recruitment
mor_rec_spp_nz <- mor_rec_spp_long %>% group_by(spp_grp) %>% 
  mutate(nonzero = sum(rate != 0)) %>% filter(nonzero > 0) %>% 
  droplevels()

setdiff(unique(mor_rec_spp_long$spp_grp), unique(mor_rec_spp_nz$spp_grp))
head(mor_rec_spp_nz)

# Basal Area plots
p1 <- ggplot(mor_rec_spp_nz %>% filter(unit_type == "BA" & rate_type == 'recr'), 
             aes(x = spp_grp, y = rate, group = factor(cycle), fill = factor(cycle)))+ 
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 5, size = 0.2)+
  coord_flip() +  
  scale_x_discrete(limits = rev(mor_rec_spp_nz$spp_grp)) +
  scale_y_continuous(limits = c(0, BA_max), 
                     #breaks = c(1, 2, 3), 
                     name = "Annual Recruitment (% Basal Area)") +
  theme_FHM()+ labs(x = NULL) + 
  theme(legend.position = 'bottom',
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(t=0.5,r=0.75,b=2,l=-0.87),"cm"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual(values = c("2" = "#CAD7D0", 
                               "3" = "#79A98E", 
                               "4" = "#4D6A5A"),
                    labels = c("1 \U2013 2", "2 \U2013 3", "3 \U2013 4"),
                    name = 'Recr. by cycle')

p2 <- ggplot(mor_rec_spp_nz %>% filter(unit_type == "BA" & rate_type == 'mort'), 
             aes(x = spp_grp, y = rate, group = factor(cycle), fill = factor(cycle)))+ 
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 5, size = 0.2)+
  coord_flip() + 
  geom_hline(aes(yintercept = 0))+
  scale_x_discrete(limits = rev(mor_rec_spp_nz$spp_grp)) +
  theme_FHM()+ labs(x = NULL) +
  theme(legend.position = 'bottom', 
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(t=0.5,r=0,b=2,l=0),"cm"))+
  #ylim(round(-BA_max, digits = 0), 0) +
  scale_fill_manual(values = c("2" = "#F5BBBB", 
                               "3" = "#CD5C5C", 
                               "4" = "#6E3838"),
                    labels = c("1 \U2013 2", "2 \U2013 3", "3 \U2013 4"),
                    name = 'Mort. by cycle')+
  scale_y_continuous(limits = c(-BA_max,0), 
                     #breaks = c(0, -1.0, -2.0, -3.0), 
                     name = "Annual Mortality (% Basal Area)")

fig_layout = grid.layout(nrow = 1, ncol = 2, 
                         respect = TRUE,
                         heights = unit(c(6, 6), c('in', 'in')),
                         widths = unit(c(5.5, 3.25), c('in', 'in')))

fig_vp <- viewport(name = "fig_vp", layout = fig_layout,
                   width = unit(8, 'in'), height = unit(6, 'in'))


svg(paste0(new_path, "figures/", "Figure_2_", park, "_treeBA_by_species_cycle.svg"),
    height = 5.2, width = 9.5)

grid.newpage()
pushViewport(fig_vp)
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
popViewport()

dev.off()

# Stem plots
p1s <- ggplot(mor_rec_spp_nz %>% filter(unit_type == "stem" & rate_type == 'recr'), 
              aes(x = spp_grp, y = rate, group = factor(cycle), fill = factor(cycle)))+ 
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 5, size = 0.2)+
  coord_flip() +  
  scale_x_discrete(limits = rev(mor_rec_spp_nz$spp_grp)) +
  scale_y_continuous(limits = c(0, round(stem_max, digits = 0)), 
                     #breaks = c(1, 2, 3), 
                     name = "Annual Recruitment (% stems)") +
  theme_FHM()+ labs(x = NULL) + 
  theme(legend.position = 'bottom',
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(t=0.5,r=0.75,b=2,l=-0.87),"cm"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual(values = c("2" = "#CAD7D0", 
                               "3" = "#79A98E", 
                               "4" = "#4D6A5A"),
                    labels = c("1 \U2013 2", "2 \U2013 3", "3 \U2013 4"),
                    name = 'Recr. by cycle')

p1s
p2s <- ggplot(mor_rec_spp_nz %>% filter(unit_type == "stem" & rate_type == 'mort'), 
              aes(x = spp_grp, y = rate, group = factor(cycle), fill = factor(cycle)))+ 
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 5, size = 0.2)+
  coord_flip() + 
  geom_hline(aes(yintercept = 0))+
  scale_x_discrete(limits = rev(mor_rec_spp_nz$spp_grp)) +
  theme_FHM()+ labs(x = NULL) +
  theme(legend.position = 'bottom', 
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(t=0.5,r=0,b=2,l=0),"cm"))+
  #ylim(round(-BA_max, digits = 0), 0) +
  scale_fill_manual(values = c("2" = "#F5BBBB", 
                               "3" = "#CD5C5C", 
                               "4" = "#6E3838"),
                    labels = c("1 \U2013 2", "2 \U2013 3", "3 \U2013 4"),
                    name = "Mort. by cycle")+
  scale_y_continuous(limits = c(-round(stem_max, digits = 0),0), 
                     #breaks = c(0, -1.0, -2.0, -3.0),
                     name = 'Annual Mortality (% stems)')

fig_layout = grid.layout(nrow = 1, ncol = 2, 
                         respect = TRUE,
                         heights = unit(c(6, 6), c('in', 'in')),
                         widths = unit(c(5.5, 3.25), c('in', 'in')))

fig_vp <- viewport(name = "fig_vp", layout = fig_layout,
                   width = unit(8, 'in'), height = unit(6, 'in'))


svg(paste0(new_path, "figures/", "Figure_2_", park, "_treeStem_by_species_cycle.svg"),
    height = 5.2, width = 9.5)

grid.newpage()
pushViewport(fig_vp)
print(p2s, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p1s, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
popViewport()

dev.off()

# Park level averaging by cycle
names(trees_wide3)

mor_rec_long <- trees_wide3 %>% group_by(ParkUnit, Plot_Name) %>% 
  summarize(c1_live_BA = sum(BA_m2ha_1_alive, na.rm = T), 
            c2_live_BA = sum(BA_m2ha_2_alive, na.rm = T), 
            c3_live_BA = sum(BA_m2ha_3_alive, na.rm = T), 
            c4_live_BA = sum(BA_m2ha_4_alive, na.rm = T), 
            .groups = 'drop') %>% 
  pivot_longer(-c(ParkUnit, Plot_Name), names_to = 'tree_cycle', values_to = 'BAm2ha') %>% 
  mutate(cycle = as.numeric(substr(tree_cycle, 2, 2)))

head(mor_rec_long)

# Calculate average BAm2/ha per cycle using loess smoother
# BA_smooth <- case_boot_loess(mor_rec_long, x = 'cycle', y = 'BAm2ha', ID = 'Plot_Name', 
#                              span = 8/4, num_reps = 1000, chatty = TRUE)
# 
# BA_smooth
# 
# ggplot(BA_smooth, aes(x = cycle, y = estimate))+
#   geom_line() + theme_FHM() + ylim(0, 40) +
#   geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2)+
#   labs(y = expression("Basal Area ("*m^2*")/ha)"))
# 
# ggsave(paste0(new_path, "figures/", "Figure_X_", park, "_treeBA_by_cycle.svg"),
#        height = 8, width = 11, units = 'in')
# Turns out these aren't that helpful

# Total BA lost in last cycle
mor_rec_all <- mor_rec %>% group_by(ParkUnit) %>% 
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
    
    c1_live_BA = sum(c1_live_BA, na.rm = T),
    c1_live_stem = sum(c1_live_stem, na.rm = T),
    c2_live_BA = sum(c2_live_BA, na.rm = T),
    c2_live_stem = sum(c2_live_stem, na.rm = T),
    c3_live_BA = sum(c3_live_BA, na.rm = T),
    c3_live_stem = sum(c3_live_stem, na.rm = T),
    c4_live_BA = sum(c4_live_BA, na.rm = T),
    c4_live_stem = sum(c4_live_stem, na.rm = T),
    
    .groups = 'drop'
    # Add cycle 5 for ACAD
  ) 
head(mor_rec_all)

write.csv(mor_rec_all, "D:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/MORR/2022/overall_mort_recr.csv",
          row.names = F)
