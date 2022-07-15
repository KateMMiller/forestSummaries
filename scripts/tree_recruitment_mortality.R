#---- Figure 2 Recruitment and Mortality ----
library(grid)
tree_mr <- do.call(joinTreeData, args = args_vs)
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
  select(Plot_Name, ParkUnit, TagCode, PanelCode, cycle, ScientificName, 
         status, DBHcm, stems, BA_m2ha) %>%
  arrange(Plot_Name, cycle) %>% 
  left_join(., prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName")

head(tree_mr)

table(tree_mr$status)

Acer <- c('Acer', 'Acer rubrum', 'Acer saccharum', 'Acer saccharinum', 'Acer negundo')
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

tree_mr <- tree_mr %>% 
  mutate(spp_grp = case_when(ScientificName %in% Acer ~ "Acer spp. (maple)",
                             ScientificName %in% Betula ~ "Betula spp. (birch)",
                             ScientificName %in% Carya ~ "Carya spp. (hickory)",
                             ScientificName %in% Fraxinus ~ "Fraxinus spp. (ash)",
                             ScientificName %in% Pinus ~ "Pinus spp. (pine)",
                             ScientificName %in% Populus ~ "Populus spp. (poplar)",
                             ScientificName %in% Prunus ~ "Prunus spp. (cherry)",
                             ScientificName %in% Quercus ~ "Quercus spp. (oak)",
                             ScientificName %in% Ulmus ~ "Ulmus spp. (elm)",
                             ScientificName %in% Other_Native ~ "Other Native spp.",
                             ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                             ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
                             # TRUE ~ toupper(paste0(
                             #   substr(word(ScientificName, 1), 1, 3), 
                             #   substr(word(ScientificName, 2), 1, 3))))) 
head(tree_mr)

tree_mr$stems[tree_mr$status == "exclude"] <- -999
tree_mr$BA_m2ha[tree_mr$status == "exclude"] <- -999

full_names <- c("stems_1_alive", "stems_1_dead",  
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


trees_wide <- tree_mr %>% pivot_wider(names_from = c(cycle, status),
                                      values_from = c(stems, BA_m2ha),
                                      values_fill = 0) %>%
  arrange(Plot_Name, TagCode) #%>%

# Next steps are if a combination of status and cycle aren't represented
new_names <- c(full_names[!full_names %in% names(trees_wide)])
new_df <- as_tibble(matrix(rep(0, nrow(trees_wide) * length(new_names)), nrow = nrow(trees_wide), ncol = length(new_names)), 
                    .name_repair = ~ new_names)

head(new_df)

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
names(trees_wide)

trees_wide3 <- trees_wide2 %>%
  select(ParkUnit, Plot_Name, PanelCode, TagCode, DBHcm, spp_grp,
         stems_1_alive, 
         stems_2_alive, stems_2_dead, stems_2_recruit, stems_2_missed,
         stems_3_alive, stems_3_dead, stems_3_recruit, stems_3_missed,
         stems_4_alive, stems_4_dead, stems_4_recruit, stems_4_missed,
         #stems_5_alive, stems_5_dead, stems_5_recruit, stems_5_missed,
         
         BA_m2ha_1_alive, 
         BA_m2ha_2_alive, BA_m2ha_2_dead, BA_m2ha_2_recruit, BA_m2ha_2_missed,
         BA_m2ha_3_alive, BA_m2ha_3_dead, BA_m2ha_3_recruit, BA_m2ha_3_missed,
         BA_m2ha_4_alive, BA_m2ha_4_dead, BA_m2ha_4_recruit, BA_m2ha_4_missed,
         #BA_m2ha_5_alive, BA_m2ha_5_dead, BA_m2ha_5_recruit, BA_m2ha_5_missed,

         ) %>%
  mutate(stems_1_alive_adj = ifelse(stems_2_missed == 1 | 
                                   (stems_3_missed == 1 & DBHcm > 14.0) | # adjust for AMs using RS cutoff + 2cm 
                                   (stems_4_missed == 1 & DBHcm > 16.0), #|
                                   #(stems_5_missed == 1 & DBHcm > 18.0), # for cycle 5 in ACAD
                                    1, stems_1_alive), 
         stems_2_alive_adj = ifelse(stems_2_missed == 1 |
                                    stems_3_missed == 1 | 
                                   (stems_4_missed == 1 & DBHcm > 14.0) | # adjust for AMs using RS cutoff + 2cm 
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
                                    stems_4_recruit == 1, 1, stems_4_alive),
#        stems_5_alive_adj = ifelse(stems_5_missed == 1 | stems_5_recruit == 1, 1, stems_5_alive),
         
         BA_m2ha_1_alive_adj = case_when(BA_m2ha_2_missed > 0 ~ BA_m2ha_2_missed,
                                         BA_m2ha_3_missed > 0 & DBHcm > 14.0 ~ BA_m2ha_3_missed, # adjust for AMs like above
                                         BA_m2ha_4_missed > 0 & DBHcm > 14.0 ~ BA_m2ha_4_missed,
                                         #BA_m2ha_5_missed > 0 & DBHcm > 18.0 ~ BA_m2ha_5_missed,
                                         TRUE ~ BA_m2ha_1_alive),
         
         BA_m2ha_2_alive_adj = case_when(BA_m2ha_2_missed > 0 ~ BA_m2ha_2_missed,
                                         BA_m2ha_3_missed > 0 ~ BA_m2ha_3_missed,
                                         BA_m2ha_4_missed > 0 & DBHcm > 14.0 ~ BA_m2ha_4_missed,
                                         #BA_m2ha_5_missed > 0 & DBHcm > 16.0 ~ BA_m2ha_5_missed,
                                         BA_m2ha_2_recruit > 0 ~ BA_m2ha_2_recruit,
                                         TRUE ~ BA_m2ha_2_alive), # adjust for AMs and recrs
         
         BA_m2ha_3_alive_adj = case_when(BA_m2ha_3_missed > 0 ~ BA_m2ha_3_missed,
                                         BA_m2ha_4_missed > 0 ~ BA_m2ha_4_missed,
                                         #BA_m2ha_5_missed > 0 & DBHcm > 14.0 ~ BA_m2ha_5_missed,
                                         BA_m2ha_3_recruit > 0 ~ BA_m2ha_3_recruit,
                                         TRUE ~ BA_m2ha_3_alive), # adjust for AMs and recrs
         
         BA_m2ha_4_alive_adj = case_when(BA_m2ha_4_missed > 0 ~ BA_m2ha_4_missed,
                                         #BA_m2ha_5_missed > 0 ~ BA_m2ha_5_missed,
                                         BA_m2ha_4_recruit > 0 ~ BA_m2ha_4_recruit,
                                         TRUE ~ BA_m2ha_4_alive), # adjust for AMs and recrs

         # BA_m2ha_5_alive_adj = case_when(BA_m2ha_5_missed > 0 ~ BA_m2ha_5_missed,
         #                                 BA_m2ha_5_recruit > 0 ~ BA_m2ha_5_recruit,
         #                                 TRUE ~ BA_m2ha_5_alive),
         
         c1_2_dead = ifelse(stems_1_alive_adj == 1 & stems_2_dead == 1, 1, 0), # must be alive first
         c2_3_dead = ifelse(stems_2_alive_adj == 1 & stems_3_dead == 1, 1, 0),
         c3_4_dead = ifelse(stems_3_alive_adj == 1 & stems_4_dead == 1, 1, 0),
         #c4_5_dead = ifelse(stems_4_alive_adj == 1 & stems_5_dead == 1, 1, 0),
         
         c1_2_recr = ifelse(stems_2_recruit == 1, 1, 0),
         c2_3_recr = ifelse(stems_3_recruit == 1, 1, 0),
         c3_4_recr = ifelse(stems_4_recruit == 1, 1, 0),
         #c4_5_recr = ifelse(stems_5_recruit == 1, 1, 0),
          
         c1_2_dead_ba = ifelse(stems_1_alive_adj == 1 & stems_2_dead == 1, BA_m2ha_2_dead, 0),
         c2_3_dead_ba = ifelse(stems_2_alive_adj == 1 & stems_3_dead == 1, BA_m2ha_3_dead, 0),
         c3_4_dead_ba = ifelse(stems_3_alive_adj == 1 & stems_4_dead == 1, BA_m2ha_4_dead, 0),
         #c4_5_dead_ba = ifelse(stems_4_alive_adj == 1 & stems_5_dead == 1, BA_m2ha_5_dead, 0),

         c1_2_recr_ba = ifelse(stems_2_recruit == 1, BA_m2ha_2_recruit, 0),
         c2_3_recr_ba = ifelse(stems_3_recruit == 1, BA_m2ha_3_recruit, 0),
         c3_4_recr_ba = ifelse(stems_4_recruit == 1, BA_m2ha_4_recruit, 0),
         #c4_5_recr_ba = ifelse(stems_5_recruit == 1, BA_m2ha_5_recruit, 0),
         
         c1_2_grow_ba = ifelse(stems_2_alive_adj == 1 & stems_1_alive_adj == 1, # calc isn't including recruits
                               BA_m2ha_2_alive - BA_m2ha_1_alive, 0),
         c2_3_grow_ba = ifelse(stems_3_alive_adj == 1 & stems_2_alive_adj == 1, # calc isn't including recruits
                               BA_m2ha_3_alive - BA_m2ha_2_alive, 0),
         c3_4_grow_ba = ifelse(stems_4_alive_adj == 1 & stems_3_alive_adj == 1, # calc isn't including recruits
                               BA_m2ha_4_alive - BA_m2ha_3_alive, 0) # ,
         #c4_5_grow_ba = ifelse(stems_5_alive_adj == 1 & stems_4_alive_adj == 1, # calc isn't including recruits
         #                      BA_m2ha_5_alive - BA_m2ha_4_alive, 0))

)

names(trees_wide3)
mor_rec <- trees_wide3 %>% group_by(ParkUnit, Plot_Name, PanelCode, spp_grp) %>%
  summarize(c1_live_stem = sum(stems_1_alive_adj, na.rm = T),
            c2_live_stem = sum(stems_2_alive_adj, na.rm = T),
            c3_live_stem = sum(stems_3_alive_adj, na.rm = T),
            c4_live_stem = sum(stems_4_alive_adj, na.rm = T),
            #c5_live_stem = sum(stems_5_alive_adj, na.rm = T),
            
            c1_2_mort_stem = sum(c1_2_dead, na.rm = T),
            c2_3_mort_stem = sum(c2_3_dead, na.rm = T),
            c3_4_mort_stem = sum(c3_4_dead, na.rm = T),
            #c4_5_mort_stem = sum(c4_5_dead, na.rm = T),
            
            c1_2_recr_stem = sum(c1_2_recr, na.rm = T),
            c2_3_recr_stem = sum(c2_3_recr, na.rm = T),
            c3_4_recr_stem = sum(c3_4_recr, na.rm = T),
            #c4_5_recr_stem = sum(c4_5_recr, na.rm = T),
            
            c1_live_BA = sum(BA_m2ha_1_alive_adj, na.rm = T), #total live BA inc. recr.
            c2_live_BA = sum(BA_m2ha_2_alive_adj, na.rm = T),
            c3_live_BA = sum(BA_m2ha_3_alive_adj, na.rm = T),
            c4_live_BA = sum(BA_m2ha_4_alive_adj, na.rm = T),
            #c5_live_BA = sum(BA_m2ha_5_alive_adj, na.rm = T),
            
            c1_2_mort_BA = sum(c1_2_dead_ba, na.rm = T),
            c2_3_mort_BA = sum(c2_3_dead_ba, na.rm = T),
            c3_4_mort_BA = sum(c3_4_dead_ba, na.rm = T),
            #c4_5_mort_BA = sum(c4_5_dead_ba, na.rm = T),
            
            c1_2_recr_BA = sum(c1_2_recr_ba, na.rm = T),
            c2_3_recr_BA = sum(c2_3_recr_ba, na.rm = T),
            c3_4_recr_BA = sum(c3_4_recr_ba, na.rm = T),
            #c4_5_recr_BA = sum(c4_5_recr_ba, na.rm = T),
            
            c1_2_incgr_BA = sum(c1_2_grow_ba, na.rm = T),
            c2_3_incgr_BA = sum(c2_3_grow_ba, na.rm = T),
            c3_4_incgr_BA = sum(c3_4_grow_ba, na.rm = T),
            #c4_5_incgr_BA = sum(c4_5_grow_ba, na.rm = T),
            
            .groups = 'drop')

names(mor_rec)

mor_rec2 <- mor_rec %>% mutate(

  mortBA12 = (c1_2_mort_BA * 100)/(c1_live_BA * 4),
  mortStem12 = (c1_2_mort_stem * 100)/(c1_live_stem * 4),
  recrBA12 = (c1_2_recr_BA * 100)/(c1_live_BA * 4),
  recrStem12 = (c1_2_recr_stem * 100)/(c1_live_stem * 4),
  growBA12 = (c1_2_incgr_BA * 100)/(c1_live_BA * 4),
  netBA12 = growBA12 + recrBA12 - mortBA12,
  netStem12 = recrStem12 - mortStem12,
  ElevMort12 = ifelse(mortStem12 > 1.6, 1, 0),
  
  mortBA23 = (c2_3_mort_BA * 100)/(c2_live_BA * 4),
  mortStem23 = (c2_3_mort_stem * 100)/(c2_live_stem * 4),
  recrBA23 = (c2_3_recr_BA * 100)/(c2_live_BA * 4),
  recrStem23 = (c2_3_recr_stem * 100)/(c2_live_stem * 4),
  growBA23 = (c2_3_incgr_BA * 100)/(c2_live_BA * 4),
  netBA23 = growBA23 + recrBA23 - mortBA23,
  netStem23 = recrStem23 - mortStem23,
  ElevMort23 = ifelse(mortStem23 > 1.6, 1, 0),
  
  c4_denom = case_when(ParkUnit %in% c("MABI", "MIMA", "SAGA", "SARA") & PanelCode == 3 ~ 6,
                       ParkUnit %in% c("MORR", "ROVA", "WEFA") & PanelCode == 4 ~ 5,
                       ParkUnit == 'ACAD' & PanelCode == 3 ~ 5,
                       TRUE ~ 4),
  
  mortBA34 = (c3_4_mort_BA * 100)/(c3_live_BA * c4_denom),
  mortStem34 = (c3_4_mort_stem * 100)/(c3_live_stem * c4_denom),
  recrBA34 = (c3_4_recr_BA * 100)/(c3_live_BA * c4_denom),
  recrStem34 = (c3_4_recr_stem * 100)/(c3_live_stem * c4_denom),
  growBA34 = (c3_4_incgr_BA * 100)/(c3_live_BA * c4_denom),
  netBA34 = growBA34 + recrBA34 - mortBA34,
  netStem34 = recrStem34 - mortStem34,
  ElevMort34 = ifelse(mortStem34 > 1.6, 1, 0),
  
  # Add cycle 5 for ACAD
  
  ) 
  
head(mor_rec2)

mor_rec_spp <- mor_rec2 %>% group_by(spp_grp) %>% 
  summarize(c1_2_recr_stem = sum(c1_2_recr_stem, na.rm = TRUE),
            c2_3_recr_stem = sum(c2_3_recr_stem, na.rm = TRUE),
            c3_4_recr_stem = sum(c3_4_recr_stem, na.rm = TRUE),
            c1_2_mort_stem = sum(c1_2_mort_stem, na.rm = TRUE) * -1,
            c2_3_mort_stem = sum(c2_3_mort_stem, na.rm = TRUE) * -1,
            c3_4_mort_stem = sum(c3_4_mort_stem, na.rm = TRUE) * -1, 
            
            c1_2_recr_BA = sum(c1_2_recr_BA, na.rm = TRUE),
            c2_3_recr_BA = sum(c2_3_recr_BA, na.rm = TRUE),
            c3_4_recr_BA = sum(c3_4_recr_BA, na.rm = TRUE),
            c1_2_mort_BA = sum(c1_2_mort_BA, na.rm = TRUE) * -1,
            c2_3_mort_BA = sum(c2_3_mort_BA, na.rm = TRUE) * -1,
            c3_4_mort_BA = sum(c3_4_mort_BA, na.rm = TRUE) * -1, 
            num_plots = n())

mor_rec_spp_long <- mor_rec_spp %>% 
  pivot_longer(-c(spp_grp, num_plots), names_to = 'metric', values_to = 'rate') %>% 
  mutate(cycle = as.numeric(substr(metric, 4, 4)),
         rate_type = substr(metric, 6, 9),
         unit_type = ifelse(grepl("BA", metric), "BA", "stem"))


head(mor_rec_spp_long)

BA_max = max(abs(mor_rec_spp_long$rate[mor_rec_spp_long$unit_type == "BA"]), na.rm = T)

# For ggplot order
mor_rec_spp_long$met_fac <- factor(mor_rec_spp_long$metric, 
                                   levels = c(
                                     "c3_4_recr_BA",
                                     "c2_3_recr_BA", 
                                     "c1_2_recr_BA", 
                                     "c3_4_mort_BA",
                                     "c2_3_mort_BA", 
                                     "c1_2_mort_BA", 
                                     "c3_4_recr_stem",
                                     "c2_3_recr_stem", 
                                     "c1_2_recr_stem", 
                                     "c3_4_mort_stem",
                                     "c2_3_mort_stem", 
                                     "c1_2_mort_stem" 
                                   ))
mor_rec_spp_nz <- mor_rec_spp_long %>% group_by(spp_grp) %>% 
  mutate(nonzero = sum(rate != 0)) %>% filter(nonzero > 0) %>% 
  droplevels()

head(mor_rec_spp_nz)

p1 <- ggplot(mor_rec_spp_nz %>% filter(unit_type == "BA" & rate_type == 'recr'), 
             aes(x = spp_grp, y = rate, group = factor(cycle), fill = factor(rev(cycle))))+ 
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 5, size = 0.2)+
  coord_flip() +  
  scale_x_discrete(limits = rev(sort(mor_rec_spp_nz$spp_grp))) +
  scale_y_continuous(limits = c(0, round(BA_max, digits = 0)), 
                     breaks = c(1, 2, 3)) +
  theme_FHM()+ labs(x = NULL) + 
  theme(legend.position = 'bottom',
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(t=0.5,r=0.75,b=2,l=-0.87),"cm"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual(values = c("2" = "#AECBBA", 
                               "3" = "#79A98E", 
                               "4" = "#537A64"),
                    labels = c("1 \U2013 2", "2 \U2013 3", "3 \U2013 4"),
                    name = 'Recruitment by cycle')

p1
p2 <- ggplot(mor_rec_spp_nz %>% filter(unit_type == "BA" & rate_type == 'mort'), 
             aes(x = spp_grp, y = rate, group = factor(cycle), fill = factor(rev(cycle))))+ 
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 5, size = 0.2)+
  coord_flip() + 
  geom_hline(aes(yintercept = 0))+
  scale_x_discrete(limits = rev(sort(mor_rec_spp_nz$spp_grp))) +
  theme_FHM()+ labs(x = NULL) +
  theme(legend.position = 'bottom', 
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(t=0.5,r=0,b=2,l=0),"cm"))+
  #ylim(round(-BA_max, digits = 0), 0) +
  scale_fill_manual(values = c("2" = "#F19898", 
                               "3" = "#CD5C5C", 
                               "4" = "#7F3D3D"),
                    labels = c("1 \U2013 2", "2 \U2013 3", "3 \U2013 4"),
                    name = 'Mortality by cycle')+
  scale_y_continuous(limits = c(-round(BA_max, digits = 0),0), 
                     breaks = c(0, -1.0, -2.0, -3.0))

fig_layout = grid.layout(nrow = 1, ncol = 2, 
                         respect = TRUE,
                         heights = unit(c(6, 6), c('in', 'in')),
                         widths = unit(c(5.5, 3.25), c('in', 'in')))

fig_vp <- viewport(name = "fig_vp", layout = fig_layout,
                   width = unit(8, 'in'), height = unit(6, 'in'))


svg(paste0(new_path, "figures/", "Figure_2_", park, "_treeBA_by_species_cycle.svg"),
    height = 6, width = 9)

grid.newpage()
pushViewport(fig_vp)
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
popViewport()

dev.off()

# Park level averaging by cycle
head(mor_rec2)
mor_rec_long <- mor_rec2 %>% group_by(ParkUnit, Plot_Name) %>% 
  summarize(c1_live_BA = sum(c1_live_BA, na.rm = T), 
            c2_live_BA = sum(c2_live_BA, na.rm = T), 
            c3_live_BA = sum(c3_live_BA, na.rm = T), 
            c4_live_BA = sum(c4_live_BA, na.rm = T), 
            .groups = 'drop') %>% 
  pivot_longer(-c(ParkUnit, Plot_Name), names_to = 'tree_cycle', values_to = 'BAm2ha') %>% 
  mutate(cycle = as.numeric(substr(tree_cycle, 2, 2)))


head(mor_rec_long)
# Calculate average BAm2/ha per cycle using loess smoother
BA_smooth <- case_boot_loess(mor_rec_long, x = 'cycle', y = 'BAm2ha', ID = 'Plot_Name', 
                             span = 8/4, num_reps = 1000, chatty = TRUE)

BA_smooth

ggplot(BA_smooth, aes(x = cycle, y = estimate))+
  geom_line() + theme_FHM() + ylim(0, 40) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2)+
  labs(y = expression("Basal Area ("*m^2*")/ha)"))

ggsave(paste0(new_path, "figures/", "Figure_X_", park, "_treeBA_by_cycle.svg"),
       height = 8, width = 11, units = 'in')
# Turns out these aren't that helpful

