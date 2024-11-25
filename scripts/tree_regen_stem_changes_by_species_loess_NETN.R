#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time: NETN
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
table(trees$ScientificName)

Acer_spp <- c('Acer', 'Acer rubrum', 'Acer saccharinum', 'Acer negundo', 'Acer saccharum') # take Acer saccharum out for MABI
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
                  'Tilia americana',# Take Tilia out for for MABI & SAGA
                  'Picea rubens', # take PICRUB out for ACAD
                  'Platanus occidentalis', 
                  'Salix', 'Unknown Conifer',
                  'Unknown Hardwood', 'Unknown Tree - 01', 'Unknown Tree - 03')
Subcanopy <- c('Acer spicatum', 'Acer pensylvanicum',
               'Carpinus caroliniana', 'Cornus florida', 
               'Ilex opaca', 'Juniperus virginiana', 
               'Ostrya virginiana',
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')

Exotic_spp <- c('Acer platanoides',  #Take out  ACEPLA for SAGA ROVA
                'Aesculus hippocastanum', 'Ailanthus altissima',
                'Crataegus',  # Take out Crateagus for SARA
                'Malus', 'Malus pumila', 'Morus alba',
                'Photinia villosa', 'Prunus avium', 'Pyrus', 
                'Picea abies', "Pinus sylvestris",  # Take out PICABI and PINSYL for MABI
                'Alnus serrulata', 'Catalpa speciosa',
                'Rhamnus cathartica', # Take out RHACAT for SARA
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

plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                            SampleYear = unique(plot_yr$SampleYear),
                            spp_grp = unique(trees$spp_grp)) |> 
  filter(spp_grp != "None present (NA)") |> 
  mutate(species = word(spp_grp, 1),
         genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
         sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |> 
  select(Plot_Name, SampleYear, spp_grp, sppcode)

#plot_spp_yr1$sppcode[plot_spp_yr1$ScientificName == "Acer saccharum"] <- "ACESAC3"

plot_spp_yr2 <- left_join(plot_spp_yr1, trees |> select(spp_grp) |> unique(), 
                          by = "spp_grp", relationship = 'many-to-many')

plot_spp_yr <- left_join(plot_yr, plot_spp_yr2, by = c("Plot_Name", "SampleYear"))

# plot_spp_yr$sppcode[plot_spp_yr$sppcode == "QUE(RE"] <- "QUESPP"
# plot_spp_yr$sppcode[plot_spp_yr$sppcode == "QUE(WH"] <- "QUESPP"
# plot_spp_yr$sppcode[plot_spp_yr$sppcode == "CRA(HA"] <- "CRASPP"

dup_spp_check <- as.data.frame(table(plot_spp_yr1$sppcode))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all tree species have the same frequency in expand grid. Check for duplicate species codes."))

head(plot_spp_yr)
head(trees)
tree_spp_sum1 <- left_join(plot_spp_yr, 
                           trees |> select(Plot_Name, SampleYear, spp_grp, stems, BA_m2ha), 
                           by = c("Plot_Name", "SampleYear", "spp_grp"),
                           relationship = 'many-to-many') #|> 

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

#---- Tree Stems
tree_stem_smooth <- purrr::map_dfr(spp_list, 
                                   function(spp){
                                     df <- tree_spp_sum |> filter(sppcode %in% spp)
                                     case_boot_loess(df, x = "SampleYear", y = "stems_ha", ID = "Plot_Name",
                                                     group = "sppcode", 
                                                     span = span, num_reps = 1000) |>
                                       mutate(sppcode = spp)
                                   }
)
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
  scale_x_continuous(breaks = c(seq(from, to, by = 3), to), 
                     limits = c(2005, to)) +
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

net_ba_year <- tree_BA_smooth3 |> group_by(term, SampleYear) |> summarize(net_ba = sum(estimate))
net_ba_year # No decline in BA over time

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
  scale_x_continuous(breaks = c(seq(from, to, by = 3), to), 
                     limits = c(2005, to)) +
  theme_FHM() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        legend.position = 'bottom')

tree_BA_trends

svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_Tree_BA_by_species_cycle.svg"),
    height = 8, width = 7)
tree_BA_trends
dev.off()

table(tree_stem_smooth3$spp_grp)

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
    "Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
    "Acer saccharum (sugar maple)" = "#94da2f", #SAGA MABI only
    "Acer spp. (maple)" = "#54FF00",
    "Betula spp. (birch)" = "#38A800",
    "Carya spp. (hickory)" = "#FFFF00",
    "Crataegus (hawthorns)" = "#9D0909", #SARA only
    "Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
    "Fagus grandifolia (American beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#A87000",
    "Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
    "Larix decidua (European larch)" = "#C8CE00", # MABI only
    "Other Exotic spp." = "#FF0000", # off for MABI
    "Other Native spp." = "#828282",
    "Picea abies (Norway spruce)" = "#C2037A",
    "Pinus spp. (pine)" = "#704489", #"#1D5104", # off for MORR 
    "Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "#CBCC7E", # off for MABI
    "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
    "Quercus spp. (oak)" = "#C500FF",
    "Rhamnus cathartica (common buckthorn)" = "#FF7854", # SARA only
    "Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
    "Subcanopy spp." = "#FFBEE8",
    "Tilia americana (American basswood)" = "#53CEF2", # MABI only
    "Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "#59538A" # off for MABI
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
    "Acer saccharum (sugar maple)" = "solid", #SAGA MABI only
    "Acer spp. (maple)" = "solid",
    "Betula spp. (birch)" = "dotted",
    "Carya spp. (hickory)" = "solid",
    "Crataegus (hawthorns)" = "dotdash", #SARA only
    "Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
    "Fagus grandifolia (American beech)" = "solid",
    "Fraxinus spp. (ash)" = "solid",
    "Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
    "Larix decidua (European larch)" = "solid", # MABI only
    "Other Exotic spp." = "dotdash", # off for MABI
    "Other Native spp." = "dotdash",
    "Picea abies (Norway spruce)" = "solid",
    "Pinus spp. (pine)" = "solid", # off for MORR 
    "Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "solid", # off for MABI
    "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
    "Quercus spp. (oak)" = "solid",
    "Rhamnus cathartica (common buckthorn)" = "dotted", # SARA only
    "Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
    "Subcanopy spp." = "solid",
    "Tilia americana (American basswood)" = "solid", # MABI only
    "Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
     "Ulmus spp. (elm)" = "dotted" # off for MABI
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'none', # b/c shares page with 4B 
        legend.key.width = unit(1, 'cm'))


svg(paste0(new_path, "figures/", "Figure_4A_", park, "_smoothed_tree_stems_by_species_cycle.svg"),
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
    "Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
    "Acer saccharum (sugar maple)" = "#94da2f", #SAGA MABI only
    "Acer spp. (maple)" = "#54FF00",
    "Betula spp. (birch)" = "#38A800",
    "Carya spp. (hickory)" = "#FFFF00",
    "Crataegus (hawthorns)" = "#9D0909", #SARA only
    "Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
    "Fagus grandifolia (American beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#A87000",
    "Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
    "Larix decidua (European larch)" = "#C8CE00", # MABI only
    "Other Exotic spp." = "#FF0000", # off for MABI
    "Other Native spp." = "#828282",
    "Picea abies (Norway spruce)" = "#C2037A",
    "Pinus spp. (pine)" = "#704489", #"#1D5104", # off for MORR 
    "Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "#CBCC7E", # off for MABI
    "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
    "Quercus spp. (oak)" = "#C500FF",
    "Rhamnus cathartica (common buckthorn)" = "#FF7854", # SARA only
    "Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
    "Subcanopy spp." = "#FFBEE8",
    "Tilia americana (American basswood)" = "#53CEF2", # MABI only
    "Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "#59538A" # off for MABI
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
    "Acer saccharum (sugar maple)" = "solid", #SAGA MABI only
    "Acer spp. (maple)" = "solid",
    "Betula spp. (birch)" = "dotted",
    "Carya spp. (hickory)" = "solid",
    "Crataegus (hawthorns)" = "dotdash", #SARA only
    "Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
    "Fagus grandifolia (American beech)" = "solid",
    "Fraxinus spp. (ash)" = "solid",
    "Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
    "Larix decidua (European larch)" = "solid", # MABI only
    "Other Exotic spp." = "dotdash", # off for MABI
    "Other Native spp." = "dotdash",
    "Picea abies (Norway spruce)" = "solid",
    "Pinus spp. (pine)" = "solid", # off for MORR 
    "Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "solid", # off for MABI
    "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
    "Quercus spp. (oak)" = "solid",
    "Rhamnus cathartica (common buckthorn)" = "dotted", # SARA only
    "Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
    "Subcanopy spp." = "solid",
    "Tilia americana (American basswood)" = "solid", # MABI only
    "Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "dotted" # off for MABI
  ), name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 5))

svg(paste0(new_path, "figures/", "Figure_4B_", park, "_smoothed_BA_by_species_cycle.svg"),
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

Acer_spp <- c('Acer', 'Acer saccharinum', 'Acer negundo', 'Acer rubrum', 'Acer saccharum') # remove ACE SAC for MABI
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
                  'Tilia americana', # Remove Tilia for MABI
                  'Pinus', 'Picea rubens', # Remove PICRUB for ACAD
                  'Platanus occidentalis', 
                  'Salix', 'Unknown Conifer', 'Unknown Tree - 02',
                  'Unknown Hardwood', 'Unknown Tree - 01', 'Unknown Tree - 03')

Subcanopy <- c('Acer spicatum', 'Acer pensylvanicum',
               'Carpinus caroliniana', 'Cornus florida', 
               'Ilex opaca', 'Juniperus virginiana', 
               'Ostrya virginiana',
               'Sassafras albidum', 'Salix discolor', 'Viburnum prunifolium')
Exotic_spp <- c('Acer platanoides',  #Take out ACEPLA for SAGA ROVA
                'Aesculus hippocastanum', 'Ailanthus altissima',
                'Crataegus',  # Turned off for SARA
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
                             # ScientificName %in% c(Other_Native, Prunus_spp, Populus_spp) ~ "Other Native spp.", #if need to reduce number of species
                             ScientificName %in% c(Other_Native, Populus_spp) ~ "Other Native spp.",
                             ScientificName %in% Exotic_spp ~ "Other Exotic spp.",
                             ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

head(reg_spp)
table(reg_spp$spp_grp)

#reg_spp$ScientificName[reg_spp$ScientificName %in% c("Quercus (Red group)", "Quercus (White group)")] <- "Quercus"

# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                            SampleYear = unique(plot_yr$SampleYear),
                            spp_grp = unique(reg_spp$spp_grp)) |> 
  filter(spp_grp != "None present (NA)") |> 
  mutate(species = word(spp_grp, 1),
         genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
         sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |> 
  select(Plot_Name, SampleYear, spp_grp, sppcode)

plot_spp_yr <- left_join(plot_yr, plot_spp_yr1, by = c("Plot_Name", "SampleYear"))

#plot_spp_yr$sppcode[plot_spp_yr$ScientificName == "Acer saccharum"] <- "ACESAC3"
# plot_spp_yr$sppcode[plot_spp_yr$sppcode == "QUE(RE"] <- "QUESPP"
# plot_spp_yr$sppcode[plot_spp_yr$sppcode == "QUE(WH"] <- "QUESPP"
#plot_spp_yr$sppcode[plot_spp_yr$sppcode == "CRA(HA"] <- "CRASPP"
table(plot_spp_yr$sppcode)

dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))

reg_spp_smooth <- left_join(plot_spp_yr, reg_spp |> select(Plot_Name, SampleYear, spp_grp, seed_den, sap_den), 
                            by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
                 # filter(SampleYear > 2006) #dropped first year b/c only 1 microplot

reg_spp_smooth[,c("seed_den", "sap_den")][is.na(reg_spp_smooth[,c("seed_den", "sap_den")])] <- 0

spp_list <- sort(unique(reg_spp_smooth$sppcode))

span = 8/length(unique(reg_spp_smooth$SampleYear))
table(reg_spp_smooth$SampleYear, reg_spp_smooth$Plot_Name)

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
  scale_x_continuous(breaks = c(seq(from, to, by = 3), to), 
                     limits = c(2005, to)) +
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
  scale_x_continuous(breaks = c(seq(from, to, by = 3), to), 
                     limits = c(2005, to)) +
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
    "Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
    "Acer saccharum (sugar maple)" = "#94da2f", #SAGA MABI only
    "Acer spp. (maple)" = "#54FF00",
    "Betula spp. (birch)" = "#38A800",
    "Carya spp. (hickory)" = "#FFFF00",
    "Crataegus (hawthorns)" = "#9D0909", #SARA only
    "Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
    "Fagus grandifolia (American beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#A87000",
    "Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
    "Larix decidua (European larch)" = "#C8CE00", # MABI only
    "Other Exotic spp." = "#FF0000", # off for MABI
    "Other Native spp." = "#828282",
    "Picea abies (Norway spruce)" = "#C2037A",
    "Pinus spp. (pine)" = "#704489", #"#1D5104", # off for MORR 
    "Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "#CBCC7E", # off for MABI SAGA
    "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
    "Quercus spp. (oak)" = "#C500FF",
    "Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
    "Subcanopy spp." = "#FFBEE8",
    "Tilia americana (American basswood)" = "#53CEF2", # MABI only
    "Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "#59538A" # off for MABI
  ),
  name = NULL) +
    scale_linetype_manual(values = c(
      "Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
      "Acer saccharum (sugar maple)" = 'solid',
      "Acer spp. (maple)" = "solid",
      "Betula spp. (birch)" = "dotted",
      "Carya spp. (hickory)" = "solid",
      "Crataegus (hawthorns)" = "dotdash", #SARA only
      "Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
      "Fagus grandifolia (American beech)" = "solid",
      "Fraxinus spp. (ash)" = "solid",
      "Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
      "Larix decidua (European larch)" = "solid", # MABI only
      "Other Exotic spp." = "dotdash", # off for MABI
      "Other Native spp." = "dotdash",
      "Picea abies (Norway spruce)" = "solid",
      "Pinus spp. (pine)" = "solid", # off for MORR 
      "Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
      "Populus spp. (poplar)" = "solid", # off for MABI
      "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
      "Quercus spp. (oak)" = "solid",
      "Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
      "Subcanopy spp." = "solid",
      "Tilia americana (American basswood)" = "solid", # MABI only
      "Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
      "Ulmus spp. (elm)" = "dotted" # off for MABI
    ),
    name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 4))

net_seeds

svg(paste0(new_path, "figures/", "Figure_3B_", park, "_net_seedlings_by_species_cycle.svg"),
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
    "Acer platanoides (Norway maple)" = "#A80000", #SAGA and MIMA only
    "Acer saccharum (sugar maple)" = "#94da2f", #SAGA MABI only
    "Acer spp. (maple)" = "#54FF00",
    "Betula spp. (birch)" = "#38A800",
    "Carya spp. (hickory)" = "#FFFF00",
    "Crataegus (hawthorns)" = "#9D0909", #SARA only
    "Carya cordiformis (bitternut hickory)" = "#FFFF00", #SAGA only
    "Fagus grandifolia (American beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#A87000",
    "Liriodendron tulipifera (tuliptree)" = "#73AAFF", # off for MABI & SAGA
    "Larix decidua (European larch)" = "#C8CE00", # MABI only
    "Other Exotic spp." = "#FF0000", # off for MABI
    "Other Native spp." = "#828282",
    "Picea abies (Norway spruce)" = "#C2037A",
    "Pinus spp. (pine)" = "#704489", #"#1D5104", # off for MORR 
    "Pinus sylvestris (Scots pine)" = '#F57A7A', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "#CBCC7E", # off for MABI SAGA
    "Prunus spp. (cherry)" ="#00E6A9", # off for MORR & MABI 
    "Quercus spp. (oak)" = "#C500FF",
    "Robinia pseudoacacia (black locust)" = "#CBCC7E", #off for MABI
    "Subcanopy spp." = "#FFBEE8",
    "Tilia americana (American basswood)" = "#53CEF2", # MABI only
    "Tsuga canadensis (eastern hemlock)" = "#005CE6", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "#59538A" # off for MABI
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Acer platanoides (Norway maple)" = "dotdash", #SAGA and MIMA only
    "Acer saccharum (sugar maple)" = 'solid',
    "Acer spp. (maple)" = "solid",
    "Betula spp. (birch)" = "dotted",
    "Carya spp. (hickory)" = "solid",
    "Crataegus (hawthorns)" = "dotdash", #SARA only
    "Carya cordiformis (bitternut hickory)" = "solid", #SAGA only
    "Fagus grandifolia (American beech)" = "solid",
    "Fraxinus spp. (ash)" = "solid",
    "Liriodendron tulipifera (tuliptree)" = "solid", # off for MABI & SAGA
    "Larix decidua (European larch)" = "solid", # MABI only
    "Other Exotic spp." = "dotdash", # off for MABI
    "Other Native spp." = "dotdash",
    "Picea abies (Norway spruce)" = "solid",
    "Pinus spp. (pine)" = "solid", # off for MORR 
    "Pinus sylvestris (Scots pine)" = 'dotdash', #"#D94600", # MABI & MIMA only
    "Populus spp. (poplar)" = "solid", # off for MABI
    "Prunus spp. (cherry)" ="dotdash", # off for MORR & MABI 
    "Quercus spp. (oak)" = "solid",
    "Robinia pseudoacacia (black locust)" = "dotdash", #off for MABI
    "Subcanopy spp." = "solid",
    "Tilia americana (American basswood)" = "solid", # MABI only
    "Tsuga canadensis (eastern hemlock)" = "solid", # MABI/SAGA ROVA only
    "Ulmus spp. (elm)" = "dotted" # off for MABI
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'none', 
        legend.key.width = unit(1, 'cm'))

net_saps

svg(paste0(new_path, "figures/", "Figure_3A_", park, "_net_saplings_by_species_cycle.svg"),
    height = 4.6, width = 8)
net_saps
dev.off()


#---- Shrub cover -----
# 
shrubs <- do.call(joinMicroShrubData, args_vs) |> filter(SampleYear >= 2010)
table(shrubs$ScientificName, shrubs$SampleYear)

other_native <- c("Amphicarpaea bracteata", "Aronia melanocarpa", "Gaylussacia baccata",
                  "Hamamelis virginiana", # off for SAGA
                  "Ilex verticillata", "Lyonia ligustrina", "Rosa",
                  "Spiraea alba", "Vaccinium angustifolium", "Vaccinium pallidum", 
                  "Vaccinium stamineum", "Zanthoxylum americanum", 'Rhus typhina', 
                  'Vaccinium corymbosum') # adjust per park
corylus = c("Corylus americana", "Corylus cornuta")
other_exotic <- c("Crataegus", "Viburnum lantana", "Rhodotypos scandens", 
                  'Viburnum dilatatum', 'Viburnum sieboldii',
                  'Euonymus alatus')# adjust per park
cornus <- c("Cornus", "Cornus amomum", "Cornus racemosa")
rubus <- c("Rubus", "Rubus allegheniensis", "Rubus idaeus", "Rubus occidentalis", "Rubus odoratus")
viburnum <- c("Viburnum dentatum", "Viburnum lentago", 'Viburnum lantanoides')
vitis <- c("Vitis", "Vitis aestivalis", "Vitis riparia")
lonicera <- c("Lonicera", "Lonicera - Exotic", "Lonicera morrowii")
parth <- c("Parthenocissus", "Parthenocissus quinquefolia")
natvines <- c(parth, vitis, "Toxicodendron radicans")
ligustrum <- c("Ligustrum", "Ligustrum vulgare")

table(shrubs$ScientificName)

shrubs <- left_join(shrubs, prepTaxa() |> select(ScientificName, CommonName), by = "ScientificName")

shrubs <- shrubs %>% 
  mutate(spp_grp = case_when(ScientificName %in% c(other_native, viburnum, parth, natvines) ~ "Other Native spp.",
                             ScientificName %in% other_exotic ~ "Other Exotic spp.", 
                             ScientificName %in% cornus ~ "Cornus spp. (dogwood)",
                             ScientificName %in% corylus ~ "Corylus spp. (hazelnut)",
                             ScientificName %in% rubus ~ "Rubus spp. (brambles)",
                             #ScientificName %in% viburnum ~ "Viburnum spp. (arrowwood)", # included in other_native
                             #ScientificName %in% vitis ~ "Vitis spp. (grape)",#  included in other_native
                             ScientificName %in% lonicera ~ "Lonicera spp. (exotic honeysuckle)",
                            # ScientificName %in% natvines ~ "Native vine spp.", # included in other_native
                             ScientificName %in% ligustrum ~ "Ligustrum spp. (privet)",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

shrub_sum <- shrubs |> group_by(Plot_Name, SampleYear, spp_grp) |> 
  summarize(avg_cov = sum(shrub_avg_cov, na.rm = T), .groups = 'drop')

table(shrub_sum$spp_grp)
head(shrubs)

# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> filter(SampleYear >= 2010) |> unique()

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                           SampleYear = unique(plot_yr$SampleYear),
                           spp_grp = unique(shrub_sum$spp_grp)) |> 
  filter(spp_grp != "None present (NA)") |> 
  mutate(species = word(spp_grp, 1),
         genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
         sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |> 
  select(Plot_Name, SampleYear, spp_grp, sppcode)

plot_spp_yr <- left_join(plot_yr, plot_spp_yr1, by = c("Plot_Name", "SampleYear"))

dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))

shrub_smooth <- left_join(plot_spp_yr, shrub_sum |> select(Plot_Name, SampleYear, spp_grp, avg_cov), 
                            by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 

shrub_smooth[,c("avg_cov")][is.na(shrub_smooth[,c("avg_cov")])] <- 0
table(shrub_smooth$sppcode)

spp_list <- sort(unique(shrub_smooth$sppcode))

span = 8/length(unique(shrub_smooth$SampleYear))

head(shrub_smooth)
shrub_smooth <- purrr::map_dfr(spp_list, 
                              function(spp){
                                df <- shrub_smooth |> filter(sppcode %in% spp)
                                case_boot_loess(df, x = "SampleYear", y = "avg_cov", ID = "Plot_Name",
                                                group = "sppcode", 
                                                span = span, num_reps = 1000) |>
                                  mutate(sppcode = spp)
                              }
)

# Determine if significant based on whether first and last year CIs overlap
shrub_smooth2 <- 
  left_join(shrub_smooth, 
            shrub_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
shrub_smooth3 <- left_join(shrub_smooth2,
                          plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                          by = c('sppcode'), 
                          relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |> 
  arrange(spp_grp)

# Plotting trends by species group facet
shrub_trends <- 
  ggplot(shrub_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
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
  labs(y = "Shrub % Cover", x = "Year") +
  scale_x_continuous(breaks = c(seq(2010, to, by = 3), to), 
                     limits = c(2009, to)) +
  theme_FHM() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        legend.position = 'bottom')

shrub_trends

svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
    height = 8, width = 7)
shrub_trends
dev.off()

table(shrub_smooth3$spp_grp)

net_shrubs <- 
  ggplot(shrub_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Shrub % Cover") +
  theme_FHM()+
  scale_color_manual(values = c(
  "Berberis thunbergii (Japanese barberry)" = "#4CE600",
   "Celastrus orbiculatus (oriental bittersweet)" = "#FFAA00",
    "Cornus spp. (dogwood)" = "#95DE34",
    "Cornus alternifolia (alternate-leaf dogwood)" = "#95de34",
    "Corylus spp. (hazelnut)" = "#57A588",
    "Hamamelis virginiana (American witchhazel)" = "#FFF000",
    "Lonicera spp. (exotic honeysuckle)" = "#0070FF",
    "Native vine spp." = "#308E33",
    "Other Exotic spp." = "#F9CF36",
    "Other Native spp." = "#828282",
    "Rhamnus cathartica (common buckthorn)" = "#FF7854",
    "Rosa multiflora (multiflora rose)" = "#BB3636",
    "Rubus spp. (brambles)" = "#DC91F6",
    "Toxicodendron radicans (eastern poison ivy)" = "#937648",
    "Viburnum spp. (arrowwood)" = "#57A588",
    "Vitis spp. (grape)" = "#8C62B4",
    "Zanthoxylum americanum (Common pricky-ash)" = "#B9C63A",
    "Viburnum lantanoides (hobblebush)" = "#DC91F6",
    "Ligustrum spp. (privet)" = "#B9C63A",
    "Lindera benzoin (northern spicebush)" = "#95de34",
    "Lonicera japonica (Japanese honeysuckle)" = "#0070FF",
    "Rubus phoenicolasius (wine raspberry)" = "#937648"
  ), name = NULL) +
  scale_linetype_manual(values = c(
   "Berberis thunbergii (Japanese barberry)" = "solid",
   "Celastrus orbiculatus (oriental bittersweet)" = "solid",
    "Cornus spp. (dogwood)" = "dotdash",
    "Cornus alternifolia (alternate-leaf dogwood)" = "solid",
    "Corylus spp. (hazelnut)" = "dotdash",
    "Hamamelis virginiana (American witchhazel)" = 'dotdash',
    "Lonicera spp. (exotic honeysuckle)" = "solid",
    "Native vine spp." = "dashed",
    "Other Exotic spp." = "solid",
    "Other Native spp." = "dotdash",
    "Parthenocissus spp. (Virginia creeper)" = "dotted",
    "Rhamnus cathartica (common buckthorn)" = "solid",
    "Rosa multiflora (multiflora rose)" = "solid",
    "Rubus spp. (brambles)" = "dotdash",
    "Toxicodendron radicans (eastern poison ivy)" = "dotted",
    "Viburnum spp. (arrowwood)" = "dotdash",
    "Vitis spp. (grape)" = "dotdash",
     "Viburnum lantanoides (hobblebush)" = "solid",
   "Zanthoxylum americanum (Common pricky-ash)" = "dotdash",
   "Ligustrum spp. (privet)" = "solid",
   "Lindera benzoin (northern spicebush)" = "dotted",
   "Lonicera japonica (Japanese honeysuckle)" = "dotted",
   "Rubus phoenicolasius (wine raspberry)" = "dotdash"
  ), name = NULL) +
  scale_x_continuous(breaks = c(seq(2010, to, by = 2), to), 
                     limits = c(2009.9, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 3))

net_shrubs

svg(paste0(new_path, "figures/", "Figure_5_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
    height = 6, width = 8)
net_shrubs
dev.off()
