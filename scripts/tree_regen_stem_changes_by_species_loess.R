#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time
# ++++++++ MUST RUN source_script.R FIRST ++++++++
#-------------------------------------------------------------------

#---- Tree trends by species ----
trees1 <- do.call(joinTreeData, args = c(args_vs, status = 'live'))

plot_evs <- do.call(joinLocEvent, args = args_vs) |> select(Plot_Name, SampleYear, cycle) %>% 
  group_by(SampleYear, cycle) |>  mutate(num_plots = sum(!is.na(Plot_Name)))

trees <- left_join(trees1, plot_evs, by = c("Plot_Name", "cycle", "SampleYear")) |> 
         mutate(BA_m2ha = BA_cm2/ifelse(ParkUnit == "ACAD", 225, 400),
                stems = 1) |> 
         select(Plot_Name, SampleYear, cycle, num_plots, ScientificName, 
                stems, BA_m2ha) |> 
  arrange(Plot_Name, SampleYear)  
  
trees <- left_join(trees, prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName")

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
  trees <- trees %>% 
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
  trees <- trees %>% 
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
  trees <- trees %>% 
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
  trees <- trees %>% 
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
trees <- trees %>% 
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
table(trees$spp_grp)
table(trees$ScientificName, trees$spp_grp)

plot_spp_yr <- expand.grid(Plot_Name = unique(plot_evs$Plot_Name), 
                           SampleYear = unique(trees$SampleYear),
                           spp_grp = unique(trees$spp_grp)) |> 
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

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all tree species have the same frequency in expand grid. Check for duplicate species codes."))

tree_spp_sum1 <- left_join(plot_spp_yr, 
                           trees |> select(Plot_Name, SampleYear, spp_grp, stems, BA_m2ha), 
                           by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
# filter(SampleYear > 2006) #dropped first year b/c only 1 microplot

tree_spp_sum1[,c("stems", "BA_m2ha")][is.na(tree_spp_sum1[,c("stems", "BA_m2ha")])] <- 0

conv_to_ha <- ifelse(park == "ACAD", 10000/225, 10000/400)

tree_spp_sum <- tree_spp_sum1 |> group_by(Plot_Name, SampleYear, spp_grp, sppcode) |> 
  summarize(stems_ha = (sum(stems)) * conv_to_ha,
            BA_m2ha = sum(BA_m2ha), .groups = 'drop')

head(tree_spp_sum)
spp_list <- sort(unique(tree_spp_sum$sppcode))

span = 8/length(unique(tree_spp_sum$SampleYear)) #9
length(unique(tree_spp_sum$spp_grp))
table(tree_spp_sum$spp_grp)

tree_stem_smooth <- purrr::map_dfr(spp_list, 
                                   function(spp){
                                     df <- tree_spp_sum |> filter(sppcode %in% spp)
                                     case_boot_loess(df, x = "SampleYear", y = "stems_ha", ID = "Plot_Name",
                                                     group = "sppcode", 
                                                     span = span, num_reps = 1000) |>
                                       mutate(sppcode = spp)
                                   }
)

tree_BA_smooth <- purrr::map_dfr(spp_list, 
                                 function(spp){
                                   df <- tree_spp_sum |> filter(sppcode %in% spp)
                                   case_boot_loess(df, x = "SampleYear", y = "BA_m2ha", ID = "Plot_Name",
                                                   group = "sppcode", 
                                                   span = span, num_reps = 1000) |>
                                     mutate(sppcode = spp)
                                 }
)

#---- Tree Stems
# Determine if significant based on whether first and last year CIs overlap
tree_stem_smooth2 <- 
  left_join(tree_stem_smooth, 
            tree_stem_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
tree_stem_smooth3 <- left_join(tree_stem_smooth2,
                               plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                               by = c('sppcode'), 
                               relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |> 
  arrange(spp_grp)

# Plotting trends by species group facet
tree_stem_trends <- 
  ggplot(tree_stem_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
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
  labs(y = "Trees (stems/ha)", x = "Year") +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 3), 2023), 
                     limits = c(2005, 2024)) +
  theme_FHM() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        legend.position = 'bottom')

tree_stem_trends

svg(paste0(new_path, "figures/", "Figure_XA_", park, "_smoothed_Tree_stems_by_species_cycle.svg"),
    height = 8, width = 7)
tree_stem_trends
dev.off()

#--- Tree BA
tree_BA_smooth <- purrr::map_dfr(spp_list, 
                                 function(spp){
                                   df <- tree_spp_sum |> filter(sppcode %in% spp)
                                   case_boot_loess(df, x = "SampleYear", y = "BA_m2ha", ID = "Plot_Name",
                                                   group = "sppcode", 
                                                   span = span, num_reps = 1000) |>
                                     mutate(sppcode = spp)
                                 }
)

tree_BA_smooth2 <- 
  left_join(tree_BA_smooth, 
            tree_BA_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
tree_BA_smooth3 <- left_join(tree_BA_smooth2,
                             plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                             by = c('sppcode'), 
                             relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |> 
  arrange(spp_grp)

# Plotting trends by species group facet
tree_BA_trends <- 
  ggplot(tree_BA_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
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
  labs(y = "Tree Basal Area (sq.m/ha)", x = "Year") +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 3), 2023), 
                     limits = c(2005, 2024)) +
  theme_FHM() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        legend.position = 'bottom')

tree_BA_trends

svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_Tree_BA_by_species_cycle.svg"),
    height = 8, width = 7)
tree_BA_trends
dev.off()

#---- Net stem/BA plots by species
net_stems <- 
  ggplot(tree_stem_smooth3, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Tree Density (stems/ha)") +
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
    "Other Native spp." = "dotdash",
    #"Picea abies (Norway spruce)" = "solid",
    "Pinus spp. (pine)" = "solid", # off for MORR 
    #"Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "solid", # off for MABI
    "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
    "Quercus spp. (oak)" = "solid",
    "Rhamnus cathartica (common buckthorn)" = "dotted", # SARA only
    #"Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
    "Subcanopy spp." = "solid",
    #"Tilia americana (American basswood)" = "solid", # MABI only
    #"Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "dotted" # off for MABI
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'none', # b/c shares page with 4B 
        legend.key.width = unit(1, 'cm'))


svg(paste0(new_path, "figures/", "Figure_3A_", park, "_smoothed_tree_stems_by_species_cycle.svg"),
    height = 4.6, width = 8)
net_stems
dev.off()

net_ba <- 
  ggplot(tree_BA_smooth3, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Tree Basal Area (sq.m/ha)") +
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
    "Other Native spp." = "dotdash",
    #"Picea abies (Norway spruce)" = "solid",
    "Pinus spp. (pine)" = "solid", # off for MORR 
    #"Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "solid", # off for MABI
    "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
    "Quercus spp. (oak)" = "solid",
    "Rhamnus cathartica (common buckthorn)" = "dotted", # SARA only
    #"Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
    "Subcanopy spp." = "solid",
    #"Tilia americana (American basswood)" = "solid", # MABI only
    #"Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "dotted" # off for MABI
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 5))

svg(paste0(new_path, "figures/", "Figure_3B_", park, "_smoothed_BA_by_species_cycle.svg"),
    height = 6.15, width = 8)
  net_ba
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

# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
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

svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_seedlings_by_species_cycle.svg"),
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
  
net_seeds <- 
  ggplot(seed_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Seedling Density (stems/sq.m)") +
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
      "Ulmus spp. (elm)" = "dotted" # off for MABI
    ),
    name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 4))

net_seeds

svg(paste0(new_path, "figures/", "Figure_4B_", park, "_net_seedlings_by_species_cycle.svg"),
    height = 6.15, width = 8)
net_seeds
dev.off()

net_saps <- 
  ggplot(sap_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Sapling Density (stems/sq.m)") +
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
    "Ulmus spp. (elm)" = "dotted" # off for MABI
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'none', 
        legend.key.width = unit(1, 'cm'))

net_saps

svg(paste0(new_path, "figures/", "Figure_4A_", park, "_net_saplings_by_species_cycle.svg"),
    height = 4.6, width = 8)
net_saps
dev.off()

