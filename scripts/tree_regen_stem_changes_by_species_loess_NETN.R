#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time: NETN
# ++++++++ MUST RUN source_script.R FIRST ++++++++
#-------------------------------------------------------------------
#library(ggpubr)
span <- 4/5 #roughly linear between timesteps

if(!exists('trspp_grps')){stop("Must run source_script_NETN.R before this script will work.")}
head(trspp_grps)
#---- Tree trends by species ----
trees1 <- do.call(joinTreeData, args = c(args_vs, status = 'live'))

plot_evs <- do.call(joinLocEvent, args = args_vs) |> select(Plot_Name, SampleYear, cycle, IsStuntedWoodland, PlotCode, PanelCode) |> 
                filter(IsStuntedWoodland == FALSE) #|> group_by(SampleYear, cycle) 

trees <- left_join(trees1, plot_evs, by = c("Plot_Name", "cycle", "SampleYear")) |> 
          filter(IsStuntedWoodland == FALSE) |>        
          mutate(BA_m2ha = BA_cm2/ifelse(ParkUnit == "ACAD", 225, 400),
                stems = 1) |> 
         select(Plot_Name, SampleYear, cycle, ScientificName, 
                stems, BA_m2ha) |> 
  arrange(Plot_Name, SampleYear)  
  
tree_grps <- left_join(trees, trspp_grps |> select(Species, spp_grp, sppcode), 
                       by = c("ScientificName" = "Species")) |> 
  filter(!ScientificName %in% "None present")

if(nrow(tree_grps[which(is.na(tree_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in tree_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group

###Park specific changes to tree species groups###
#Must match listed changes in forest_summary_code_NETN.R
if(park == "MORR"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Ilex opaca" ~ "SUBCAN",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Ilex opaca" ~ "Subcanopy",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
} 
if(park == "ROVA"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               ScientificName == "Pinus resinosa" ~ "PINRES",
                               ScientificName == "Robinia pseudoacacia" ~ "OTHEXO",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               ScientificName == "Pinus resinosa" ~ "Pinus resinosa (red pine)",
                               ScientificName == "Robinia pseudoacacia" ~ "Other Exotic",
                               TRUE ~ spp_grp))
} 
if(park == "WEFA"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
} 
if(park == "SARA"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides") ~ "Populus spp. (aspen)",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides") ~ "Populus spp. (aspen)",
                               TRUE ~ spp_grp))
}
if(park == "MABI"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName %in% c("Larix decidua", "Picea abies", 
                                                     "Pinus sylvestris") ~ "EXOPLA",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName  %in% c("Larix decidua", "Picea abies", 
                                                      "Pinus sylvestris") ~ "Exotic plantation spp.",
                               TRUE ~ spp_grp))
} 
if(park == "SAGA"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               TRUE ~ spp_grp))
} 
if(park == "MIMA"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Juniperus virginiana" ~ "SUBCAN",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               ScientificName == "Pinus rigida" ~ "OTHNAT",
                               ScientificName %in% c("Cladrastis kentukea", "Robinia pseudoacacia") ~ "OTHEXO",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Juniperus virginiana" ~ "Other native subcanopy spp.",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               ScientificName == "Pinus rigida" ~ "Other native canopy spp.",
                               ScientificName %in% c("Cladrastis kentukea", "Robinia pseudoacacia") ~ "Other exotic spp.",
                               TRUE ~ spp_grp))
}
if(park == "ACAD"){
  tree_grps <- tree_grps |> 
    mutate(sppcode = case_when(ScientificName == "Abies balsamea" ~ "ABIBAL",
                               ScientificName %in% c("Picea glauca", "Picea mariana",
                                                     "Picea rubens", "Picea") ~ "PICSPP",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides") ~ "POPSPP",
                               ScientificName %in% c("Larix laricina", "Thuja occidentalis") ~ "OTHCON",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Abies balsamea" ~ "Abies balsamea (balsam fir)",
                               ScientificName %in% c("Picea glauca", "Picea mariana",
                                                     "Picea rubens", "Picea") ~ "Picea spp. (spruce)",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides")  ~ "Populus spp. (aspen)",
                               ScientificName  %in% c("Larix laricina", "Thuja occidentalis") ~ "Other conifer",
                               TRUE ~ spp_grp))
}

tree_grps <- tree_grps |>  mutate(spp_grp = case_when(spp_grp == "Other Native" ~ "Other native canopy spp.",
                                                       spp_grp == "Subcanopy" ~ "Other native subcanopy spp.",
                                                       spp_grp == "Other Exotic" ~ "Other exotic spp.",
                                                       TRUE ~ spp_grp))
table(tree_grps$spp_grp)
table(tree_grps$ScientificName, tree_grps$spp_grp)

plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()
length(unique(plot_yr$Plot_Name))

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                            SampleYear = unique(plot_yr$SampleYear),
                            spp_grp = unique(tree_grps$spp_grp)) |> 
  select(Plot_Name, SampleYear, spp_grp)

#plot_spp_yr1$sppcode[plot_spp_yr1$ScientificName == "Acer saccharum"] <- "ACESAC3"
head(tree_grps)
plot_spp_yr2 <- left_join(plot_spp_yr1, 
                          tree_grps |> select(spp_grp) |> unique(), 
                          by = "spp_grp", relationship = 'many-to-many')

plot_spp_yr3 <- left_join(plot_yr, plot_spp_yr2, by = c("Plot_Name", "SampleYear"))

dup_spp_check <- as.data.frame(table(plot_spp_yr3$spp_grp))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all tree species have the same frequency in expand grid. Check for duplicate species codes."))

# Join group code back in
head(plot_spp_yr3)
#head(trspp_grps) #switched to tree_grps df because losing the edits that were made to species groups 
head(tree_grps)
plot_spp_yr <- left_join(plot_spp_yr3, tree_grps |> select(sppcode, spp_grp) |> unique(), 
                         by = "spp_grp")

tree_spp_sum1 <- left_join(plot_spp_yr, 
                           tree_grps |> select(Plot_Name, SampleYear, spp_grp, sppcode, stems, BA_m2ha), 
                           by = c("Plot_Name", "SampleYear", "spp_grp", 'sppcode'),
                           relationship = 'many-to-many') #|> 

tree_spp_sum1[,c("stems", "BA_m2ha")][is.na(tree_spp_sum1[,c("stems", "BA_m2ha")])] <- 0

conv_to_ha <- ifelse(park == "ACAD", 10000/225, 10000/400)

tree_spp_sum <- tree_spp_sum1 |> group_by(Plot_Name, SampleYear, spp_grp, sppcode) |> 
  summarize(stems_ha = (sum(stems)) * conv_to_ha,
            BA_m2ha = sum(BA_m2ha), .groups = 'drop')

head(tree_spp_sum)
spp_list <- sort(unique(tree_spp_sum$sppcode))
spp_list

#span = 8/length(unique(tree_spp_sum$SampleYear)) #9 # MIDN span is 4/5?
length(unique(tree_spp_sum$spp_grp))
table(tree_spp_sum$spp_grp)

#---- Tree Stems
tree_stem_smooth <- purrr::map_dfr(spp_list, 
                                   function(spp){
                                     df <- tree_spp_sum |> filter(sppcode %in% spp)
                                     case_boot_loess(df, x = "SampleYear", y = "stems_ha", ID = "Plot_Name",
                                                     group = "sppcode", 
                                                     span = span, num_reps = 1) |>
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

#--- Tree BA
tree_BA_smooth <- purrr::map_dfr(spp_list, 
                                 function(spp){
                                   df <- tree_spp_sum |> filter(sppcode %in% spp)
                                   case_boot_loess(df, x = "SampleYear", y = "BA_m2ha", ID = "Plot_Name",
                                                   group = "sppcode", 
                                                   span = span, num_reps = 1) |>
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


net_ba_plot <- tree_spp_sum  |> 
  summarize(net_ba = sum(BA_m2ha), .by = c(Plot_Name, SampleYear)) |> 
  mutate(cycle = case_when(SampleYear %in% c(2006:2009) ~ 1,
                           SampleYear %in% c(2010:2013) ~ 2,
                           SampleYear %in% c(2014:2017) ~ 3,
                           SampleYear %in% c(2018:2022) ~ 4,
                           SampleYear %in% c(2023:2026) ~ 5))

net_ba_year <- net_ba_plot |> summarize(mean_ba = mean(net_ba),
                                        se_ba = sd(net_ba)/sqrt(n()),
                                        .by = c(SampleYear))

net_ba_cycle <- net_ba_plot |> summarize(mean_ba = mean(net_ba),
                                         se_ba = sd(net_ba)/sqrt(n()),
                                         .by = c(cycle))

ggplot(net_ba_plot, aes(x = SampleYear, y = net_ba, group = Plot_Name)) + 
  geom_point(color = 'dimgrey', size = 2) + 
  geom_line(color = "#B5B5B5") + 
  forestNETN::theme_FHM() + 
  scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023, 2025)) 

ggplot(net_ba_plot, aes(x = cycle, y = net_ba, group = Plot_Name)) + 
  geom_point(color = 'dimgrey', size = 2) + 
  geom_line(color = "#B5B5B5") + 
  forestNETN::theme_FHM()  

ggplot(net_ba_year, aes(x = SampleYear, y = mean_ba)) + 
  geom_point(color = 'dimgrey', size = 2) + 
  geom_errorbar(aes(ymin = mean_ba - se_ba, ymax = mean_ba + se_ba)) +
  #geom_smooth(color = "#B5B5B5", span = 0.7, se = F) + 
  forestNETN::theme_FHM() 

ggplot(net_ba_cycle, aes(x = cycle, y = mean_ba)) + 
  geom_point(color = 'dimgrey', size = 2) + 
  geom_smooth(color = "#B5B5B5", span = 0.9, se = F) + 
  forestNETN::theme_FHM() 


# Colors to start with. Can change them per park if needed
cols = c(
  "Acer rubrum (red maple)" = "#38A800",
  "Acer platanoides (Norway maple)" = "#8b0000",
  "Acer spp. (maple)" = "#00FF00",
  "Acer saccharum (sugar maple)" = "#009999",
  "Ailanthus altissima (tree-of-heaven)" = "#cd4a8f",
  "Asimina triloba (pawpaw)" = "#FF00C5",
  "Betula lenta (black birch)" = "#ffd8b1", # darkened color for 2024 NETN figs; does not match maps or MIDN
  "Betula spp. (birch)" = "#ffd8b1", 
  "Carya spp. (hickory)" = "#911eb4",
  "Fagus grandifolia (American beech)" = "#FFAA00",
  "Fraxinus spp. (ash)" = "#A87000",
  "Ilex opaca (American holly)" = "#42d4f4",
  "Juniperus virginiana (eastern redcedar)" = "#9371B9",
  "Liquidambar styraciflua (sweetgum)" = "#FFFF00",
  "Liriodendron tulipifera (tulip poplar)" = "#4363d8",
  "Nyssa sylvatica (black gum)" = "#000075",
  "Other exotic spp." = "#ca0020",
  "Other native canopy spp." = "#d9d9d9",
  "Pinus spp. (pine)" = "#5A462B",
  "Pinus strobus (white pine)" = "#5A1111",
  "Pinus taeda (loblolly pine)" = "#5A1111", #assumes no overlap in PINSTR and PINTAE
  "Pinus virginiana (Virginia pine)" = "#E5740D",
  "Pinus resinosa (red pine)" = "#E5740D",
  "Prunus spp. (native cherry)" ="#00E6A9", 
  "Pyrus calleryana (Bradford pear)" = "#cd4a8f",
  "Quercus spp. (oak)" = "#0E5D2C",
  "Robinia pseudoacacia (black locust)" = "#cccc00",
  "Other native subcanopy spp." = "#ffa8b4",
  "Tsuga canadensis (eastern hemlock)" = "#9bd2ef",
  "Ulmus spp. (native elm)" = "#808000", 
  "Unknown spp." = "#CACACA",
  "Diospyros virginiana (persimmon)" = "#006666", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "#ffd8b1", #for ASIS only
  "Sassafras albidum (sassafrass)" = "#59538A", #ASIS only
  "Abies balsamea (balsam fir)" = '#911eb4',#ACAD only
  "Other conifer" = "#42d4f4",#ACAD only
  "Picea spp. (spruce)" = "#000075",#ACAD only
  "Populus spp. (aspen)" = "#FFFF00")#ACAD only


lines = c(
  "Acer rubrum (red maple)" = "solid",
  "Acer platanoides (Norway maple)" = "solid",
  "Acer saccharum (sugar maple)" = "dotdash",
  "Acer spp. (maple)" = "solid",
  "Ailanthus altissima (tree-of-heaven)" = "solid",
  "Asimina triloba (pawpaw)" = "dashed",
  "Betula lenta (black birch)" = "dashed",
  "Betula spp. (birch)" = "dashed", # Either use BETLEN or BETSPP
  "Carya spp. (hickory)" = "solid",
  "Fagus grandifolia (American beech)" = "solid",
  "Fraxinus spp. (ash)" = "solid",
  "Ilex opaca (American holly)" = "solid",
  "Juniperus virginiana (eastern redcedar)" = "dotdash",
  "Liquidambar styraciflua (sweetgum)" = "solid",
  "Liriodendron tulipifera (tulip poplar)" = "solid",
  "Nyssa sylvatica (black gum)" = "dashed",
  "Other exotic spp." = "dashed",
  "Other native canopy spp." = "solid",
  "Pinus spp. (pine)" = "dotdash",
  "Pinus strobus (white pine)" = "dotdash",
  "Pinus taeda (loblolly pine)" = "dotdash",
  "Pinus virginiana (Virginia pine)" = "dotdash",
  "Pinus resinosa (red pine)" = "dotdash",
  "Prunus spp. (native cherry)" = "dotdash", 
  "Pyrus calleryana (Bradford pear)" = "dotted",
  "Quercus spp. (oak)" = "solid",
  "Robinia pseudoacacia (black locust)" = "dashed",
  "Other native subcanopy spp." = "solid",
  "Tsuga canadensis (eastern hemlock)" = "dashed",
  "Ulmus spp. (native elm)" = "dashed", 
  "Unknown spp." = "dotted",
  "Diospyros virginiana (American persimmon)" = "dashed", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "dashed", #for ASIS only
  "Sassafras albidum (sassafrass)" = "dashed", #ASIS only
  "Abies balsamea (balsam fir)" = 'dashed',#ACAD only
  "Other conifer" = "dotdash",#ACAD only
  "Picea spp. (spruce)" = "solid",#ACAD only
  "Populus spp. (aspen)" = "dotted")#ACAD only

#---- Net stem/BA plots by species
net_stems <- 
  ggplot(tree_stem_smooth3, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Tree Density (stems/ha)") +
  # geom_line(data = mor_rec_tot |> filter(unit_type == "stem"),
  #           aes(x = cycle, y = park_total), color = 'black') +
  theme_FHM() +
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines,  name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', # b/c shares page with 4B 
        legend.key.width = unit(1.75, 'cm'),
        plot.margin = margin(0.1, 0.4, 1.5, 0.4, 'cm')) #+ 
#guides(color = guide_legend(nrow = spp_rows))

net_stems
ggsave(paste0(new_path, "figures/", "Figure_5A_", park, "_smoothed_tree_stems_by_species_cycle.svg"),
       height = 6.15, width = 8, units = 'in')

ggsave(paste0(new_path, "figures/", "Figure_5A_", park, "_smoothed_tree_stems_by_species_cycle.png"),
       height = 6.15, width = 8, units = 'in', dpi = 600)


net_ba <- 
  ggplot(tree_BA_smooth3, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Tree Basal Area (sq.m/ha)") +
  # geom_line(data = mor_rec_tot |> filter(unit_type == "stem"),
  #           aes(x = cycle, y = park_total), color = 'black') +
  theme_FHM()+
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.75, 'cm'),
        plot.margin = margin(0.5, 0.4, 0.1, 0.4, 'cm')) #+ 
# guides(color = guide_legend(nrow = spp_rows))

net_ba
ggsave(paste0(new_path, "figures/", "Figure_5B_", park, "_smoothed_BA_by_species_cycle.svg"),
       height = 6.15, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_5B_", park, "_smoothed_BA_by_species_cycle.png"),
       height = 6.15, width = 8, dpi = 600)

ggarrange(net_stems, net_ba, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B.")) 

ggsave(paste0(new_path, "figures/Figure_5_", park, "_smoothed_tree_dens_BA_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_5_", park, "_smoothed_tree_dens_BA_by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)


#----- Similar figures for seedlings and saplings -----
reg1 <- do.call(joinRegenData, c(args_all, units = 'sq.m')) |> 
  filter(!ScientificName %in% "None present") # b/c treat as shrub until tree-size

reg <- left_join(reg1, plot_evs, by = c("Plot_Name", "cycle", "SampleYear")) |> 
  filter(IsStuntedWoodland == FALSE)

length(unique(reg$Plot_Name))

reg_grps <- left_join(reg, trspp_grps |> select(Species, spp_grp, sppcode), 
                      by = c("ScientificName" = "Species"))

#Park specific exceptions to default groupings
if(park == "MORR"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Ilex opaca" ~ "SUBCAN",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Ilex opaca" ~ "Subcanopy",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
  
} 
if(park == "ROVA"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               ScientificName == "Robinia pseudoacacia" ~ "OTHEXO",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               ScientificName == "Robinia pseudoacacia" ~ "Other Exotic",
                               TRUE ~ spp_grp))
} 
if(park == "WEFA"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
}
if(park == "SARA"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides") ~ "POPSPP",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides") ~ "Populus spp. (aspen)",
                               TRUE ~ spp_grp))
}
if(park == "MABI"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Picea abies" ~ "EXOPLA",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Picea abies" ~ "Exotic plantation spp.",
                               TRUE ~ spp_grp))
} 
if(park == "SAGA"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               TRUE ~ spp_grp))
} 
if(park == "MIMA"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Juniperus virginiana" ~ "SUBCAN",
                               ScientificName %in% c("Pinus" ,"Pinus rigida") ~ "OTHNAT",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               ScientificName %in% c("Cladrastis kentukea", "Robinia pseudoacacia") ~ "OTHEXO",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName %in% c("Juniperus virginiana") ~ "Subcanopy",
                               ScientificName %in% c("Pinus", "Pinus rigida") ~ "Other native canopy spp.",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               ScientificName %in% c("Cladrastis kentukea", "Robinia pseudoacacia") ~ "Other Exotic",
                               TRUE ~ spp_grp))
}
if(park == "ACAD"){
  reg_grps <- reg_grps |> 
    mutate(sppcode = case_when(ScientificName == "Abies balsamea" ~ "ABIBAL",
                               ScientificName == "Picea" ~ "PICSPP",
                               ScientificName %in% c("Picea glauca", "Picea mariana",
                                                     "Picea rubens", "Picea") ~ "PICSPP",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides") ~ "POPSPP",
                               ScientificName %in% c("Larix laricina", "Thuja occidentalis") ~ "OTHCON",
                               ScientificName == "Sorbus decora" ~ "SUBCAN",
                               TRUE ~ sppcode)) |> 
    mutate(spp_grp = case_when(ScientificName == "Abies balsamea" ~ "Abies balsamea (balsam fir)",
                               ScientificName %in% c("Picea glauca", "Picea mariana",
                                                     "Picea rubens", "Picea") ~ "Picea spp. (spruce)",
                               ScientificName %in% c("Populus", "Populus grandidentata", 
                                                     "Populus tremuloides")  ~ "Populus spp. (aspen)",
                               ScientificName  %in% c("Larix laricina", "Thuja occidentalis") ~ "Other conifer",
                               ScientificName == "Sorbus decora" ~ "Subcanopy",
                               TRUE ~ spp_grp))
}

reg_grps <- reg_grps |> mutate(spp_grp = case_when(spp_grp == "Other Native" ~ "Other native canopy spp.",
                                                    spp_grp == "Subcanopy" ~ "Other native subcanopy spp.",
                                                    spp_grp == "Other Exotic" ~ "Other exotic spp.",
                                                    TRUE ~ spp_grp))

if(nrow(reg_grps[which(is.na(reg_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in reg_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group

head(reg_grps)
table(reg_grps$spp_grp)
table(reg_grps$ScientificName, reg_grps$spp_grp)

# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

length(unique(plot_yr$Plot_Name))
# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_rspp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                             SampleYear = unique(plot_yr$SampleYear),
                             spp_grp = unique(reg_grps$spp_grp)) |> 
  select(Plot_Name, SampleYear, spp_grp)

plot_rspp_yr2 <- left_join(plot_rspp_yr1, 
                           reg_grps |> select(spp_grp) |> unique(), 
                           by = "spp_grp", relationship = 'many-to-many')

plot_rspp_yr3 <- left_join(plot_yr, plot_rspp_yr2, by = c("Plot_Name", "SampleYear"))

dup_rspp_check <- as.data.frame(table(plot_rspp_yr3$spp_grp))

if(length(unique(dup_rspp_check$Freq)) > 1)(stop("Not all regen species have the same frequency in expand grid. Check for duplicate species codes."))

# Join group code back in
head(plot_spp_yr3)
head(reg_grps)
plot_rspp_yr <- left_join(plot_rspp_yr3, reg_grps |> select(sppcode, spp_grp) |> unique(), 
                          by = "spp_grp")
length(unique(plot_spp_yr$Plot_Name))
reg_spp_smooth <- left_join(plot_rspp_yr, reg_grps |> select(Plot_Name, SampleYear, spp_grp, seed_den, sap_den), 
                            by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
                # filter(SampleYear > 2006) #dropped first year b/c only 1 microplot

reg_spp_smooth[,c("seed_den", "sap_den")][is.na(reg_spp_smooth[,c("seed_den", "sap_den")])] <- 0

spp_list <- sort(unique(reg_spp_smooth$sppcode))
spp_list

length(spp_list) # may be longer than Map 3 b/c includes all cycles

#span = 8/length(unique(reg_spp_smooth$SampleYear)) # MIDN is span = 4/5
table(reg_spp_smooth$SampleYear, reg_spp_smooth$Plot_Name)
length(unique(reg_spp_smooth$Plot_Name))

seed_smooth <- purrr::map_dfr(spp_list, 
                              function(spp){
                              df <- reg_spp_smooth |> filter(sppcode %in% spp)
                              case_boot_loess(df, x = "SampleYear", y = "seed_den", ID = "Plot_Name",
                                              group = "sppcode", 
                                              span = span, num_reps = 1) |>
                               mutate(sppcode = spp)
                              }
                              )

sap_smooth <- purrr::map_dfr(spp_list, 
                              function(spp){
                                df <- reg_spp_smooth |> filter(sppcode %in% spp)
                                case_boot_loess(df, x = "SampleYear", y = "sap_den", ID = "Plot_Name",
                                                group = "sppcode", 
                                                span = span, num_reps = 1) |>
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
                          plot_rspp_yr |> select(spp_grp, sppcode) |> unique(),
                          by = c('sppcode'), 
                          relationship = 'many-to-many') |> 
                mutate(spp_grp = as.character(spp_grp)) |> 
                arrange(spp_grp)

spp_list <- sort(unique(seed_smooth3$spp_grp))
spp_list

length(spp_list) # may be longer than Map 3 b/c includes all cycles


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
                         plot_rspp_yr |> select(spp_grp, sppcode) |> unique(),
                         by = c('sppcode'), 
                         relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |>
  group_by(sppcode) |> mutate(drop = ifelse(sum(estimate) == 0, "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  arrange(spp_grp)

spp_list <- sort(unique(sap_smooth3$spp_grp))
spp_list

length(spp_list)

# sap_trends <- 
#   ggplot(sap_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
#                            color = sign, fill = sign)) +
#   geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2) +
#   geom_line(linewidth = 0.5) +
#   scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
#                                    "signinc" = 'solid', "signdec" = 'solid'), drop = FALSE) +
#   scale_fill_manual(values = c("notmod" = "white", "nonsign" =  "#696969",
#                                "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE)+
#   scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
#                                 "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE) +
#   facet_wrap(~spp_grp, scales = 'free_y') + 
#   labs(y = "Saplings (stems/sq.m)", x = "Year") +
#   scale_x_continuous(breaks = c(seq(from, to, by = 3), to), 
#                      limits = c(2005, to)) +
#   theme_FHM() + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.position = 'bottom')
# 
# sap_trends
# 
# svg(paste0(new_path, "figures/", "Figure_XA_", park, "_smoothed_saplings_by_species_cycle.svg"),
#     height = 8, width = 7)
# sap_trends
# dev.off()
  
net_seeds <- 
  ggplot(seed_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Seedling Density (stems/sq.m)") +
  theme_FHM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.75, 'cm'),
        plot.margin = margin(0.1, 0.4, 1.5, 0.4, 'cm'))# + 
#guides(color = guide_legend(nrow = 5))

net_seeds

ggsave(paste0(new_path, "figures/", "Figure_4A_", park, "_net_seedlings_by_species_cycle.svg"),
       height = 6.15, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_4A_", park, "_net_seedlings_by_species_cycle.png"),
       height = 6.15, width = 8, dpi = 600)

net_saps <- 
  ggplot(sap_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Sapling Density (stems/sq.m)") +
  theme_FHM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.75, 'cm'),
        plot.margin = margin(0.5, 0.4, 0.1, 0.4, 'cm')) #+
#guides(color = guide_legend(nrow = spp_rows))

net_saps

ggsave(paste0(new_path, "figures/", "Figure_4B_", park, "_net_saplings_by_species_cycle.svg"),
       height = 6.15, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_4B_", park, "_net_saplings_by_species_cycle.png"),
       height = 6.15, width = 8, dpi = 600)

ggarrange(net_seeds, net_saps, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B."))

ggsave(paste0(new_path, "figures/Figure_4_", park, "_smoothed_regen_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_4_", park, "_smoothed_regen_by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)

#----- Trends in invasive guilds over time -----
#stunted woodlands included
guilds1 <- do.call(sumQuadGuilds, c(args_vs, speciesType = 'invasive', splitHerb = F))
guild_list <- sort(unique(guilds1$Group))

# Incorporating plots with no invasive cover
plotev_grid <- data.frame(Plot_Name = rep(plot_evs$Plot_Name, 4), 
                          SampleYear = rep(plot_evs$SampleYear, 4),
                          cycle = rep(plot_evs$cycle, 4),
                          Group = rep(guild_list, each = nrow(plot_evs)))

guilds <- left_join(plotev_grid,
                    guilds1, 
                    by = c("Plot_Name", "SampleYear", "cycle", "Group"))
guilds$quad_pct_cover[is.na(guilds$quad_pct_cover)] <- 0
guilds$quad_pct_freq[is.na(guilds$quad_pct_freq)] <- 0

ggplot(guilds |> filter(Group == "Shrub"), 
       aes(x = cycle, y = quad_pct_cover)) +
  geom_point(color = 'dimgrey') + 
  geom_line(color = "#b5b5b5") + 
  forestNETN::theme_FHM() +
  # scale_x_continuous(limits = c(1,5.2), breaks = c(1:5))+
  # geom_text(aes(label = ifelse(cycle == 5, paste0("  ", as.numeric(substr(Plot_Name, 7, 8))), "")), 
  #           hjust = 0)
  facet_wrap(~Plot_Name) + 
  labs(x = "cycle", y = "Quad. % Cover")

guild_smooth <- purrr::map_dfr(guild_list,
                               function(g){
                                 df <- guilds |> filter(Group %in% g)
                                 case_boot_loess(df, x = "SampleYear", y = "quad_pct_cover", 
                                                 ID = "Plot_Name", span = span,
                                                 group = "Group",
                                                 num_reps = 1) |> 
                                   mutate(guild = g)
                               })
# relating shrub cover and regen
shrub_guild <- guilds |> filter(Group == "Shrub")

reg <- do.call(joinRegenData, 
               args = c(args_all, speciesType = 'native', 
                        canopyForm = 'canopy', units = 'sq.m'))

reg_cycle_table1 <- reg |>  summarize(seed_den = round(sum(seed_den, na.rm = TRUE), 2),
                                      sap_den = round(sum(sap_den, na.rm = TRUE), 2),
                                      stock = round(sum(stock, na.rm = TRUE), 2),
                                      .by = c(Plot_Name, cycle)) 

reg_cycle_table <- left_join(plot_evs |> select(Plot_Name, cycle, Plot = PlotCode, Panel = PanelCode),
                             reg_cycle_table1, 
                             by = c("Plot_Name", "cycle"))

reg_cols <- c("seed_den", "sap_den", "stock")
reg_cycle_table[, reg_cols][is.na(reg_cycle_table[, reg_cols])] <- 0 

shrub_reg <- left_join(reg_cycle_table, shrub_guild, by = c("Plot_Name", "cycle")) |> 
  mutate(year_fac = as.factor(SampleYear), 
         year_cen = SampleYear - 2006)


if(park == "MORR"){
ggplot(shrub_reg, aes(quad_pct_cover, stock)) +
  geom_point(colour = "grey50", alpha = 0.6) +
  geom_smooth(se = F, linewidth = 0.75) +
  theme_FHM() + 
  labs(x = "Invasive Shrub % Cover", y = "Stocking Index") +
  geom_vline(xintercept = 10, color = 'red', linetype = 'dashed') + 
  geom_hline(yintercept = 100, color = 'forestgreen', linetype = 'dashed') +
  annotate("text", x = 8, y = 750, label = "10% cover", angle = 90, color = 'red') +
  annotate('text', x = 110, y = 125, label = "Sufficiently Stocked", color = "forestgreen")

ggsave(paste0(new_path, "figures/", "Figure_7_", park, "_invasive_cover_vs_stocking_index.svg"),
       height = 4.6, width = 8)
}

# mod <- lm(stock~ quad_pct_cover, data = shrub_reg)
# mod_seg <- segmented(mod, seg.Z = ~quad_pct_cover, psi = 15)
# slope(mod_seg)
# mod_seg$psi[, "Est."] #7% 5-10% cover class

# 
# davies.test(mod_seg, seg.Z = ~quad_pct_cover) # 12.2 %
# 
# # broken.line returns fitted values + SE on a fine grid
# br <- broken.line(mod_seg, se.fit = TRUE)
# plot_df <- data.frame(
#   quad_pct_cover   = shrub_reg$quad_pct_cover,
#   fit = br$fit,
#   lo  = br$fit - 1.96 * br$se.fit,
#   hi  = br$fit + 1.96 * br$se.fit
# )
# plot_df <- plot_df[order(plot_df$quad_pct_cover), ]
# 
# ggplot(shrub_reg, aes(quad_pct_cover, seed_den)) +
#   geom_point(colour = "grey50", alpha = 0.6) +
#   geom_ribbon(data = plot_df, aes(y = fit, ymin = lo, ymax = hi),
#               fill = "steelblue", alpha = 0.3) +
#   geom_line(data = plot_df, aes(y = fit), colour = "steelblue", linewidth = 1) +
#   geom_vline(xintercept = mod_seg$psi[, "Est."],
#              linetype = "dashed", colour = "firebrick") +
#   theme_minimal()
# 
# summary(mod_seg)
# mod_seg$lme.fit.noG
# mod_seg$lme.fit
# 
# attr(mod_seg$psi.i, "is.break")
# 
# mod_seg$psi.i
# mod_seg$fixed.eta.delta
# mod_seg$fixed.eta.psi
# mod_seg$fixed.psi
# mod_seg$psi.i
# 
# plot.segmented.lme(mod_seg)
# 
# plot(mod_seg)
# slope(mod_seg)

#guild_smooth2 <- 
gcols <- c("Tree" = "#4A68BF",
           "Shrub" = "#CD5C5C",
           "Herbaceous" = "#228b22",
           "Graminoid" = "#ffd700")

guild_plot <- 
  ggplot(guild_smooth, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = guild, group = guild), linewidth = 1.5) +
  labs(x = NULL, y = "% Invasive Cover") +
  theme_FHM()+
  scale_color_manual(values = gcols,  name = "Invasive Guild") +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right',  
        legend.key.width = unit(1.75, 'cm'), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10), 
        plot.margin = margin(0.4, 0.4, 0.1, 0.4, "cm"))

guild_plot

ggsave(paste0(new_path, "figures/", "Figure_6_", park, "_smoothed_invasive_cover_by_guild_cycle.svg"),
       height = 4.6, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_6_", park, "_smoothed_invasive_cover_by_guild_cycle.png"),
       height = 4.6, width = 8, dpi = 600)


# #---- Shrub cover -----
# # 
# shrubs <- do.call(joinMicroShrubData, args_vs) |> filter(SampleYear >= 2010)
# table(shrubs$ScientificName, shrubs$SampleYear)
# 
# other_native <- c("Amphicarpaea bracteata", "Aronia melanocarpa", "Gaylussacia baccata",
#                   "Hamamelis virginiana", # off for SAGA
#                   "Ilex verticillata", "Lyonia ligustrina", "Rosa",
#                   "Spiraea alba", "Vaccinium angustifolium", "Vaccinium pallidum", 
#                   "Vaccinium stamineum", "Zanthoxylum americanum", 'Rhus typhina', 
#                   'Vaccinium corymbosum') # adjust per park
# corylus = c("Corylus americana", "Corylus cornuta")
# other_exotic <- c("Crataegus", "Viburnum lantana", "Rhodotypos scandens", 
#                   'Viburnum dilatatum', 'Viburnum sieboldii',
#                   'Euonymus alatus')# adjust per park
# cornus <- c("Cornus", "Cornus amomum", "Cornus racemosa")
# rubus <- c("Rubus", "Rubus allegheniensis", "Rubus idaeus", "Rubus occidentalis", "Rubus odoratus")
# viburnum <- c("Viburnum dentatum", "Viburnum lentago", 'Viburnum lantanoides')
# vitis <- c("Vitis", "Vitis aestivalis", "Vitis riparia")
# lonicera <- c("Lonicera", "Lonicera - Exotic", "Lonicera morrowii")
# parth <- c("Parthenocissus", "Parthenocissus quinquefolia")
# natvines <- c(parth, vitis, "Toxicodendron radicans")
# ligustrum <- c("Ligustrum", "Ligustrum vulgare")
# 
# table(shrubs$ScientificName)
# 
# shrubs <- left_join(shrubs, prepTaxa() |> select(ScientificName, CommonName), by = "ScientificName")
# 
# shrubs <- shrubs |> 
#   mutate(spp_grp = case_when(ScientificName %in% c(other_native, viburnum, parth, natvines) ~ "Other Native spp.",
#                              ScientificName %in% other_exotic ~ "Other Exotic spp.", 
#                              ScientificName %in% cornus ~ "Cornus spp. (dogwood)",
#                              ScientificName %in% corylus ~ "Corylus spp. (hazelnut)",
#                              ScientificName %in% rubus ~ "Rubus spp. (brambles)",
#                              #ScientificName %in% viburnum ~ "Viburnum spp. (arrowwood)", # included in other_native
#                              #ScientificName %in% vitis ~ "Vitis spp. (grape)",#  included in other_native
#                              ScientificName %in% lonicera ~ "Lonicera spp. (exotic honeysuckle)",
#                             # ScientificName %in% natvines ~ "Native vine spp.", # included in other_native
#                              ScientificName %in% ligustrum ~ "Ligustrum spp. (privet)",
#                              TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
# 
# shrub_sum <- shrubs |> group_by(Plot_Name, SampleYear, spp_grp) |> 
#   summarize(avg_cov = sum(shrub_avg_cov, na.rm = T), .groups = 'drop')
# 
# table(shrub_sum$spp_grp)
# head(shrubs)
# 
# # Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
# plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> filter(SampleYear >= 2010) |> unique()
# 
# # This will create all combination of plot, year, spp, but adds years not sampled by plots.
# # Need to then left join to drop unsampled years.
# plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
#                            SampleYear = unique(plot_yr$SampleYear),
#                            spp_grp = unique(shrub_sum$spp_grp)) |> 
#   filter(spp_grp != "None present (NA)") |> 
#   mutate(species = word(spp_grp, 1),
#          genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
#          sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |> 
#   select(Plot_Name, SampleYear, spp_grp, sppcode)
# 
# plot_spp_yr <- left_join(plot_yr, plot_spp_yr1, by = c("Plot_Name", "SampleYear"))
# 
# dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))
# 
# if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))
# 
# shrub_smooth <- left_join(plot_spp_yr, shrub_sum |> select(Plot_Name, SampleYear, spp_grp, avg_cov), 
#                             by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
# 
# shrub_smooth[,c("avg_cov")][is.na(shrub_smooth[,c("avg_cov")])] <- 0
# table(shrub_smooth$sppcode)
# 
# spp_list <- sort(unique(shrub_smooth$sppcode))
# 
# span = 8/length(unique(shrub_smooth$SampleYear))
# 
# head(shrub_smooth)
# shrub_smooth <- purrr::map_dfr(spp_list, 
#                               function(spp){
#                                 df <- shrub_smooth |> filter(sppcode %in% spp)
#                                 case_boot_loess(df, x = "SampleYear", y = "avg_cov", ID = "Plot_Name",
#                                                 group = "sppcode", 
#                                                 span = span, num_reps = 1000) |>
#                                   mutate(sppcode = spp)
#                               }
# )
# 
# # Determine if significant based on whether first and last year CIs overlap
# shrub_smooth2 <- 
#   left_join(shrub_smooth, 
#             shrub_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
#               summarize(up_first = first(upper95), up_last = last(upper95),
#                         lo_first = first(lower95), lo_last = last(lower95),
#                         sign = case_when(up_first < lo_last ~ "signinc",
#                                          lo_first > up_last ~ "signdec",
#                                          is.na(up_first) ~ "notmod",
#                                          TRUE ~ "nonsign")) |> 
#               select(sppcode, sign),
#             by = "sppcode")
# 
# # Join full group names back into dataframe
# shrub_smooth3 <- left_join(shrub_smooth2,
#                           plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
#                           by = c('sppcode'), 
#                           relationship = 'many-to-many') |> 
#   mutate(spp_grp = as.character(spp_grp)) |> 
#   arrange(spp_grp)
# 
# # Plotting trends by species group facet
# shrub_trends <- 
#   ggplot(shrub_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
#                            color = sign, fill = sign)) +
#   geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2) +
#   geom_line(linewidth = 0.5) +
#   scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
#                                    "signinc" = 'solid', "signdec" = 'solid'), drop = FALSE) +
#   scale_fill_manual(values = c("notmod" = "white", "nonsign" =  "#696969",
#                                "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE)+
#   scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
#                                 "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE) +
#   facet_wrap(~spp_grp, scales = 'free_y') + 
#   labs(y = "Shrub % Cover", x = "Year") +
#   scale_x_continuous(breaks = c(seq(2010, to, by = 3), to), 
#                      limits = c(2009, to)) +
#   theme_FHM() + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
#         legend.position = 'bottom')
# 
# shrub_trends
# 
# svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
#     height = 8, width = 7)
# shrub_trends
# dev.off()
# 
# table(shrub_smooth3$spp_grp)
# 
# net_shrubs <- 
#   ggplot(shrub_smooth3, 
#          aes(x = SampleYear, y = estimate)) +
#   geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
#   labs(x = NULL, y = "Shrub % Cover") +
#   theme_FHM()+
#   scale_color_manual(values = c(
#   "Berberis thunbergii (Japanese barberry)" = "#4CE600",
#    "Celastrus orbiculatus (oriental bittersweet)" = "#FFAA00",
#     "Cornus spp. (dogwood)" = "#95DE34",
#     "Cornus alternifolia (alternate-leaf dogwood)" = "#95de34",
#     "Corylus spp. (hazelnut)" = "#57A588",
#     "Hamamelis virginiana (American witchhazel)" = "#FFF000",
#     "Lonicera spp. (exotic honeysuckle)" = "#0070FF",
#     "Native vine spp." = "#308E33",
#     "Other Exotic spp." = "#F9CF36",
#     "Other Native spp." = "#828282",
#     "Rhamnus cathartica (common buckthorn)" = "#FF7854",
#     "Rosa multiflora (multiflora rose)" = "#BB3636",
#     "Rubus spp. (brambles)" = "#DC91F6",
#     "Toxicodendron radicans (eastern poison ivy)" = "#937648",
#     "Viburnum spp. (arrowwood)" = "#57A588",
#     "Vitis spp. (grape)" = "#8C62B4",
#     "Zanthoxylum americanum (Common pricky-ash)" = "#B9C63A",
#     "Viburnum lantanoides (hobblebush)" = "#DC91F6",
#     "Ligustrum spp. (privet)" = "#B9C63A",
#     "Lindera benzoin (northern spicebush)" = "#95de34",
#     "Lonicera japonica (Japanese honeysuckle)" = "#0070FF",
#     "Rubus phoenicolasius (wine raspberry)" = "#937648"
#   ), name = NULL) +
#   scale_linetype_manual(values = c(
#    "Berberis thunbergii (Japanese barberry)" = "solid",
#    "Celastrus orbiculatus (oriental bittersweet)" = "solid",
#     "Cornus spp. (dogwood)" = "dotdash",
#     "Cornus alternifolia (alternate-leaf dogwood)" = "solid",
#     "Corylus spp. (hazelnut)" = "dotdash",
#     "Hamamelis virginiana (American witchhazel)" = 'dotdash',
#     "Lonicera spp. (exotic honeysuckle)" = "solid",
#     "Native vine spp." = "dashed",
#     "Other Exotic spp." = "solid",
#     "Other Native spp." = "dotdash",
#     "Parthenocissus spp. (Virginia creeper)" = "dotted",
#     "Rhamnus cathartica (common buckthorn)" = "solid",
#     "Rosa multiflora (multiflora rose)" = "solid",
#     "Rubus spp. (brambles)" = "dotdash",
#     "Toxicodendron radicans (eastern poison ivy)" = "dotted",
#     "Viburnum spp. (arrowwood)" = "dotdash",
#     "Vitis spp. (grape)" = "dotdash",
#      "Viburnum lantanoides (hobblebush)" = "solid",
#    "Zanthoxylum americanum (Common pricky-ash)" = "dotdash",
#    "Ligustrum spp. (privet)" = "solid",
#    "Lindera benzoin (northern spicebush)" = "dotted",
#    "Lonicera japonica (Japanese honeysuckle)" = "dotted",
#    "Rubus phoenicolasius (wine raspberry)" = "dotdash"
#   ), name = NULL) +
#   scale_x_continuous(breaks = c(seq(2010, to, by = 2), to), 
#                      limits = c(2009.9, to)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.position = 'bottom', 
#         legend.key.width = unit(1, 'cm')) + 
#   guides(color = guide_legend(nrow = 3))
# 
# net_shrubs
# 
# svg(paste0(new_path, "figures/", "Figure_5_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
#     height = 6, width = 8)
# net_shrubs
# dev.off()
