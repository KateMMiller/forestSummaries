#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time
# ++++++++ MUST RUN source_script.R FIRST ++++++++
#-------------------------------------------------------------------
#load(paste0("./rdata/ACAD_loess_data.RData"))

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
trees$CommonName[trees$ScientificName == "Pinus strobus"] <- "white pine"
trees$CommonName[trees$ScientificName == "Fagus grandifolia"] <- "beech"
trees$CommonName[trees$ScientificName == "Quercus rubra"] <- "red oak"
trees$CommonName[trees$ScientificName == "Thuja occidentalis"] <- "white cedar"

Betula <- c('Betula',
            'Betula X cearulea', #'Betula lenta',  #Drop for MIMA
            'Betula papyrifera', 'Betula populifolia', 'Betula cordifolia')
Fraxinus <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', "Fraxinus nigra")
Pinus <- c("Pinus resinosa", "Pinus", "Pinus banksiana", "Pinus rigida") #, "Pinus strobus") # drop for MIMA
Picea <- c("Picea", "Picea mariana", "Picea glauca") #"Picea rubens", 
Populus <- c('Populus', 'Populus grandidentata', 'Populus tremuloides')
Subcanopy <- c('Acer spicatum', 'Acer pensylvanicum', 'Crataegus', 'Sorbus decora', 'Sorbus americana')
Other_Hardwood <- c('Amelanchier', 'Amelanchier laevis','Acer saccharum', 'Betula alleghaniensis', 'Salix')
Other_Conifer <- c("Larix laricina", #"Thuja occidentalis", 
                   "Tsuga canadensis", 'Unknown Tree - 01')

trees <- trees %>% 
    mutate(spp_grp = case_when(ScientificName %in% Betula ~ "Betula spp. (white birches)",
                               ScientificName %in% Fraxinus ~ "Fraxinus spp. (ash)",
                               ScientificName %in% Picea ~ "Picea spp. (other spruce)",
                               ScientificName %in% Pinus ~ "Pinus spp. (other pine)",
                               ScientificName %in% Populus ~ "Populus spp. (aspen)",
                               ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                               ScientificName %in% Other_Hardwood ~ "Other hardwoods",
                               ScientificName %in% Other_Conifer ~ "Other conifers",
                               TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

table(trees$spp_grp)

table(trees$ScientificName, trees$spp_grp)

conv_to_ha <- ifelse(park == "ACAD", 10000/225, 10000/400)

tree_spp_check <- trees |> filter(SampleYear > 2020) |> group_by(Plot_Name, spp_grp) |> 
  summarize(tot_stems = sum(stems), tot_BA = sum(BA_m2ha)) |> 
  group_by(spp_grp) |> summarize(avg_stems = (sum(tot_stems)/num_plots)*conv_to_ha,
                                 avg_BA = sum(tot_BA)/num_plots) |> 
  arrange(avg_stems)

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
  summarize(tot_stems = sum(stems),
            tot_BA = sum(BA_m2ha),
            stems_ha = (sum(stems)) * conv_to_ha,
            BA_m2ha = sum(BA_m2ha), .groups = 'drop')

head(tree_spp_sum)
spp_list <- sort(unique(tree_spp_sum$sppcode))

span = 4/5 # 4 #panels and 5 is number of cycles
length(unique(tree_spp_sum$spp_grp))
table(tree_spp_sum$spp_grp)

# tree_spp <- tree_spp_sum |> filter(SampleYear > 2020) |> group_by(spp_grp) |>
#   summarize(avg_stems = mean(stems_ha),
#             avg_BA = mean(BA_m2ha)) |> arrange(avg_stems)

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
    "Abies balsamea (balsam fir)" = '#005ce6',
    "Acer rubrum (red maple)" = "#28C225",
    "Betula spp. (white birches)" = "#A8F312",
    "Fagus grandifolia (beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#F3F012",
    "Other conifers" = "#71CEFF",
    "Other hardwoods" = "#008A13",
    "Picea rubens (red spruce)" = "#A80000",
    "Picea spp. (other spruce)" = "#704489",
    "Pinus strobus (white pine)" = "#A38410",
    "Pinus spp. (other pine)" = "#6A5503", 
    "Populus spp. (aspen)" = "#2BCFB1",
    "Quercus rubra (red oak)" = "#C500FF",
    "Subcanopy spp." = "#FFBEE8",
    "Thuja occidentalis (white cedar)" = "#A76CCB"
      ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Abies balsamea (balsam fir)" = 'solid',
    "Acer rubrum (red maple)" = "solid",
    "Betula spp. (white birches)" = "dashed",
    "Fagus grandifolia (beech)" = "dashed",
    "Fraxinus spp. (ash)" = "dotdash",
    "Other conifers" = "solid",
    "Other hardwoods" = "dotted",
    "Picea rubens (red spruce)" = "solid",
    "Picea spp. (other spruce)" = "dotdash",
    "Pinus strobus (white pine)" = "solid",
    "Pinus spp. (other pine)" = "dotted", 
    "Populus spp. (aspen)" = "dotted",
    "Quercus rubra (red oak)" = "dotted",
    "Subcanopy spp." = "dotdash",
    "Thuja occidentalis (white cedar)" = 'solid'
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 4))

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
    "Abies balsamea (balsam fir)" = '#005ce6',
    "Acer rubrum (red maple)" = "#28C225",
    "Betula spp. (white birches)" = "#A8F312",
    "Fagus grandifolia (beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#F3F012",
    "Other conifers" = "#71CEFF",
    "Other hardwoods" = "#008A13",
    "Picea rubens (red spruce)" = "#A80000",
    "Picea spp. (other spruce)" = "#704489",
    "Pinus strobus (white pine)" = "#A38410",
    "Pinus spp. (other pine)" = "#6A5503", 
    "Populus spp. (aspen)" = "#2BCFB1",
    "Quercus rubra (red oak)" = "#C500FF",
    "Subcanopy spp." = "#FFBEE8",
    "Thuja occidentalis (white cedar)" = "#A76CCB"
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Abies balsamea (balsam fir)" = 'solid',
    "Acer rubrum (red maple)" = "solid",
    "Betula spp. (white birches)" = "dashed",
    "Fagus grandifolia (beech)" = "dashed",
    "Fraxinus spp. (ash)" = "dotdash",
    "Other conifers" = "solid",
    "Other hardwoods" = "dotted",
    "Picea rubens (red spruce)" = "solid",
    "Picea spp. (other spruce)" = "dotdash",
    "Pinus strobus (white pine)" = "solid",
    "Pinus spp. (other pine)" = "dotted", 
    "Populus spp. (aspen)" = "dotted",
    "Quercus rubra (red oak)" = "dotted",
    "Subcanopy spp." = "dotdash",
    "Thuja occidentalis (white cedar)" = 'solid'
  ),
  name = NULL) +
  
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 4))

svg(paste0(new_path, "figures/", "Figure_4B_", park, "_smoothed_BA_by_species_cycle.svg"),
    height = 6.15, width = 8)
  net_ba
dev.off()

#----- Similar figures for seedlings and saplings -----
reg <- do.call(joinRegenData, c(args_all, units = 'sq.m')) |> 
  filter(!ScientificName %in% "Rhamnus cathartica") # b/c treat as shrub until tree-size
head(reg)

regtab <- as.data.frame(table(reg$ScientificName)) |> arrange(desc(Freq))
regtab

tlu_plants <- prepTaxa() %>% select(TSN, ScientificName, CommonName)
tlu_plants$CommonName[tlu_plants$ScientificName == "Pinus strobus"] <- "white pine"
tlu_plants$CommonName[tlu_plants$ScientificName == "Fagus grandifolia"] <- "beech"
tlu_plants$CommonName[tlu_plants$ScientificName == "Quercus rubra"] <- "red oak"
tlu_plants$CommonName[tlu_plants$ScientificName == "Thuja occidentalis"] <- "white cedar"

head(tlu_plants)
head(plot_evs)

reg_spp <- reg %>% left_join(., tlu_plants, by = c("TSN", "ScientificName")) %>%
  mutate(spp_grp = case_when(ScientificName %in% Betula ~ "Betula spp. (white birches)",
                             ScientificName %in% Fraxinus ~ "Fraxinus spp. (ash)",
                             ScientificName %in% Picea ~ "Picea spp. (other spruce)",
                             ScientificName %in% Pinus ~ "Pinus spp. (other pine)",
                             ScientificName %in% Populus ~ "Populus spp. (aspen)",
                             ScientificName %in% Subcanopy ~ "Subcanopy spp.",
                             ScientificName %in% Other_Hardwood ~ "Other hardwoods",
                             ScientificName %in% Other_Conifer ~ "Other conifers",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

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

table(plot_spp_yr$sppcode)

dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))

reg_spp_smooth <- left_join(plot_spp_yr, reg_spp |> select(Plot_Name, SampleYear, spp_grp, seed_den, sap_den), 
                            by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
                 # filter(SampleYear > 2006) #dropped first year b/c only 1 microplot

reg_spp_smooth[,c("seed_den", "sap_den")][is.na(reg_spp_smooth[,c("seed_den", "sap_den")])] <- 0

spp_list <- sort(unique(reg_spp_smooth$sppcode))

span = 4/5
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

head(seed_smooth3)
table(seed_smooth3$spp_grp)
  
net_seeds <- 
  ggplot(seed_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Seedling Density (stems/sq.m)") +
  theme_FHM()+
  scale_color_manual(values = c(
    "Abies balsamea (balsam fir)" = '#005ce6',
    "Acer rubrum (red maple)" = "#28C225",
    "Betula spp. (white birches)" = "#A8F312",
    "Fagus grandifolia (beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#F3F012",
    "Other conifers" = "#71CEFF",
    "Other hardwoods" = "#008A13",
    "Picea rubens (red spruce)" = "#A80000",
    "Picea spp. (other spruce)" = "#704489",
    "Pinus strobus (white pine)" = "#A38410",
    "Pinus spp. (other pine)" = "#6A5503", 
    "Populus spp. (aspen)" = "#2BCFB1",
    "Quercus rubra (red oak)" = "#C500FF",
    "Subcanopy spp." = "#FFBEE8",
    "Thuja occidentalis (white cedar)" = "#A76CCB"
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Abies balsamea (balsam fir)" = 'solid',
    "Acer rubrum (red maple)" = "solid",
    "Betula spp. (white birches)" = "dashed",
    "Fagus grandifolia (beech)" = "dashed",
    "Fraxinus spp. (ash)" = "dotdash",
    "Other conifers" = "solid",
    "Other hardwoods" = "dotted",
    "Picea rubens (red spruce)" = "solid",
    "Picea spp. (other spruce)" = "dotdash",
    "Pinus strobus (white pine)" = "solid",
    "Pinus spp. (other pine)" = "dotted", 
    "Populus spp. (aspen)" = "dotted",
    "Quercus rubra (red oak)" = "dotted",
    "Subcanopy spp." = "dotdash",
    "Thuja occidentalis (white cedar)" = 'solid'
  ),
    name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
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
    "Abies balsamea (balsam fir)" = '#005ce6',
    "Acer rubrum (red maple)" = "#28C225",
    "Betula spp. (white birches)" = "#A8F312",
    "Fagus grandifolia (beech)" = "#FFAA00",
    "Fraxinus spp. (ash)" = "#F3F012",
    "Other conifers" = "#71CEFF",
    "Other hardwoods" = "#008A13",
    "Picea rubens (red spruce)" = "#A80000",
    "Picea spp. (other spruce)" = "#704489",
    "Pinus strobus (white pine)" = "#A38410",
    "Pinus spp. (other pine)" = "#6A5503", 
    "Populus spp. (aspen)" = "#2BCFB1",
    "Quercus rubra (red oak)" = "#C500FF",
    "Subcanopy spp." = "#FFBEE8",
    "Thuja occidentalis (white cedar)" = "#A76CCB"
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Abies balsamea (balsam fir)" = 'solid',
    "Acer rubrum (red maple)" = "solid",
    "Betula spp. (white birches)" = "dashed",
    "Fagus grandifolia (beech)" = "dashed",
    "Fraxinus spp. (ash)" = "dotdash",
    "Other conifers" = "solid",
    "Other hardwoods" = "dotted",
    "Picea rubens (red spruce)" = "solid",
    "Picea spp. (other spruce)" = "dotdash",
    "Pinus strobus (white pine)" = "solid",
    "Pinus spp. (other pine)" = "dotted", 
    "Populus spp. (aspen)" = "dotted",
    "Quercus rubra (red oak)" = "dotted",
    "Subcanopy spp." = "dotdash",
    "Thuja occidentalis (white cedar)" = 'solid'
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2006, 2023)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 4))

net_saps

svg(paste0(new_path, "figures/", "Figure_3A_", park, "_net_saplings_by_species_cycle.svg"),
    height = 4.6, width = 8)
net_saps
dev.off()

#----- Understory composition -----
tlu_plants <- prepTaxa() %>% select(TSN, ScientificName, CommonName, Genus, Family)
tlu_plants$CommonName[tlu_plants$ScientificName == "Pinus strobus"] <- "white pine"
tlu_plants$CommonName[tlu_plants$ScientificName == "Fagus grandifolia"] <- "beech"
tlu_plants$CommonName[tlu_plants$ScientificName == "Quercus rubra"] <- "red oak"
tlu_plants$CommonName[tlu_plants$ScientificName == "Thuja occidentalis"] <- "white cedar"

plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()
head(tlu_plants)

quads <- do.call(joinQuadSpecies, args_all) %>% filter(IsGerminant == FALSE) %>%
  left_join(., tlu_plants, by = c("TSN", "ScientificName")) %>% 
  left_join(., plot_yr, by = c('Plot_Name', 'SampleYear')) %>%
  select(Plot_Name, SampleYear, Family, Genus, ScientificName, CommonName, quad_avg_cov, quad_pct_freq, 
         Tree, TreeShrub, Shrub, Vine, Herbaceous, Graminoid, FernAlly, InvasiveNETN, Exotic) 

domspp <- as.data.frame(table(quads$ScientificName)) |> arrange(-Freq)
head(quads)
quad_grps <- quads |> mutate(spp_grp = case_when(ScientificName == "Picea rubens" ~ "Picea rubens (red spruce)",
                                                 #ScientificName == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                                 #ScientificName == "Trientalis borealis" ~ "Trientalis borealis (starflower)",
                                                 ScientificName == "Gaylussacia baccata" ~ "Gaylussacia baccata (black huckleberry)",
                                                 ScientificName == "Kalmia angustifolia" ~ "Kalmia angustifolia (sheep laurel)",
                                                 ScientificName == "Abies balsamea" ~ "Abies balsamea (balsam fir)",
                                                 #ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                                                 Genus == "Vaccinium" ~ "Vaccinium spp. (blueberry species)",
                                                 #Family == "Ericaceae" ~ "Other Ericaceous species",
                                                 #Family == "Asteraceae" ~ "Asteraceae (aster family)",
                                                 FernAlly == TRUE ~ "Ferns",
                                                 Graminoid == TRUE ~ "Graminoids",
                                                 Shrub == TRUE & InvasiveNETN == FALSE ~ "Other native shrubs",
                                                 Shrub == TRUE & InvasiveNETN == TRUE ~ "Invasive shrubs",
                                                 Tree == TRUE ~ "Other tree species",
                                                 Herbaceous == TRUE & Exotic == FALSE ~ "Other herbaceous species",
                                                 TRUE ~ "Other species")) |> 
#                                                 TRUE ~ paste0(ScientificName, " (", CommonName, ")"))) |> 
  filter(! ScientificName %in% c("Unknown Herb - 01", "Unknown Herb - 02"))

#View(as.data.frame(table(quad_grps$ScientificName, quad_grps$spp_grp)))
as.data.frame(table(quad_grps$spp_grp)) |> arrange(desc(Freq))

quadspp <- as.data.frame(table(quad_grps$spp_grp, quad_grps$ScientificName))

quad_sum <- quad_grps |> group_by(Plot_Name, SampleYear, spp_grp) |> 
  summarize(avg_cov = sum(quad_avg_cov, na.rm = T), .groups = 'drop')

# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                            SampleYear = unique(plot_yr$SampleYear),
                            spp_grp = unique(quad_sum$spp_grp)) |> 
  filter(spp_grp != "None present (NA)") |> 
  mutate(species = word(spp_grp, 1),
         genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
         sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |> 
  select(Plot_Name, SampleYear, spp_grp, sppcode)

plot_spp_yr1$sppcode[plot_spp_yr1$sppcode == "AST(AS"] <- "ASTER"
plot_spp_yr <- left_join(plot_yr, plot_spp_yr1, by = c("Plot_Name", "SampleYear"))

dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))

quad_smooth <- left_join(plot_spp_yr, quad_sum |> select(Plot_Name, SampleYear, spp_grp, avg_cov), 
                          by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 

quad_smooth[,c("avg_cov")][is.na(quad_smooth[,c("avg_cov")])] <- 0
table(quad_smooth$sppcode)

spp_list <- sort(unique(quad_smooth$sppcode))

span = 4/5

quad_smooth <- purrr::map_dfr(spp_list, 
                               function(spp){
                                 df <- quad_smooth |> filter(sppcode %in% spp)
                                 case_boot_loess(df, x = "SampleYear", y = "avg_cov", ID = "Plot_Name",
                                                 group = "sppcode", 
                                                 span = span, num_reps = 1) |> #1000) |>
                                   mutate(sppcode = spp)
                               }
)

# Determine if significant based on whether first and last year CIs overlap
quad_smooth2 <- 
  left_join(quad_smooth, 
            quad_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
quad_smooth3 <- left_join(quad_smooth2,
                          plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                          by = c('sppcode'), 
                          relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |> 
  arrange(spp_grp)

quad_smooth3$estimate[quad_smooth3$estimate < 0] <- 0
table(quad_smooth3$spp_grp)

cover_plot <- 
  ggplot(quad_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Quadrat % Cover") +
  theme_FHM()+
  scale_color_manual(values = c(
    "Abies balsamea (balsam fir)" = '#005ce6',
    #"Acer rubrum (red maple)" = "#28C225",
    #"Asteraceae (aster family)" = "#A8F312",
    "Ferns" = "#008A13",
    "Gaylussacia baccata (black huckleberry)" = "#FFAA00",
    "Graminoids" = "#F3F012",
    "Kalmia angustifolia (sheep laurel)" = "#FC59FF",
    "Invasive shrubs" = "#FF0000",
    #"Maianthemum canadense (Canada mayflower)" = "#2BCFB1",
    "Other native shrubs" = "#704489",
    #"Other species" = "#CACACA",
    "Other Ericaceous species" = "#A38410",
    "Other herbaceous species" = "#2BCFB1",
    "Other tree species" = "#28C225",
    "Picea rubens (red spruce)" = "#A80000",
    #"Trientalis borealis (starflower)" = "#A76CCB",
    "Vaccinium spp. (blueberry species)" = "#428EFC" 
  ),
  name = NULL) +
  scale_linetype_manual(values = c(
    "Abies balsamea (balsam fir)" = 'dotdash',
    #"Acer rubrum (red maple)" = "solid",
    #"Asteraceae (aster family)" = "dotdash",
    "Ferns" = "solid",
    "Gaylussacia baccata (black huckleberry)" = "solid",
    "Graminoids" = "dotdash",
    "Kalmia angustifolia (sheep laurel)" = "solid",
    "Invasive shrubs" = "solid",
    #"Maianthemum canadense (Canada mayflower)" = "dotted",
    "Other native shrubs" = "solid",
    #"Other species" = "solid",
    "Other Ericaceous species" = "dotted", # Gaultheria spp., Pyrola spp.,
    "Other herbaceous species" = "dotted",
    "Other tree species" = "dotdash",
    "Picea rubens (red spruce)" = "solid",
    #"Trientalis borealis (starflower)" = "dotted",
    "Vaccinium spp. (blueberry species)" = "solid" 
  ),
  name = NULL) +
  scale_x_continuous(breaks = c(seq(2006, 2023, by = 2), 2023), 
                     limits = c(2005.9, 2023.1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm')) + 
  guides(color = guide_legend(nrow = 3))

cover_plot

svg(paste0(new_path, "figures/", "Figure_6_", park, "_smoothed_quad_cover_by_species.svg"),
    height = 7.5, width = 8.5)
cover_plot
dev.off()


save.image(paste0("./rdata/", park, "_loess_data.RData"))
