#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time
# ++++++++ MUST RUN source_script.R FIRST ++++++++
#-------------------------------------------------------------------

if(!exists('trspp_grps')){stop("Must run source_script_MIDN.R before this script will work.")}
head(trspp_grps)
trspp_grps <- trspp_grps |> rename(sppcode = Group_Code, spp_grp = Group_MIDN) |> select(-Group)

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

tree_grps <- left_join(trees, trspp_grps |> select(Species, spp_grp, sppcode), 
                       by = c("ScientificName" = "Species"))

if(nrow(tree_grps[which(is.na(tree_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in tree_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group

plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

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
head(trspp_grps)
plot_spp_yr <- left_join(plot_spp_yr3, trspp_grps |> select(sppcode, spp_grp) |> unique(), 
                         by = "spp_grp")

tree_spp_sum1 <- left_join(plot_spp_yr, 
                           tree_grps |> select(Plot_Name, SampleYear, spp_grp, sppcode, stems, BA_m2ha), 
                           by = c("Plot_Name", "SampleYear", "spp_grp", 'sppcode'),
                           relationship = 'many-to-many') #|> 

tree_spp_sum1[,c("stems", "BA_m2ha")][is.na(tree_spp_sum1[,c("stems", "BA_m2ha")])] <- 0

conv_to_ha <- 10000/400

tree_spp_sum <- tree_spp_sum1 |> group_by(Plot_Name, SampleYear, spp_grp, sppcode) |> 
  summarize(stems_ha = (sum(stems)) * conv_to_ha,
            BA_m2ha = sum(BA_m2ha), .groups = 'drop')

head(tree_spp_sum)
spp_list <- sort(unique(tree_spp_sum$sppcode))

span = 4/5
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
                     limits = c(2005, 2024)) +
  theme_FVM() + 
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
                     limits = c(2005, 2024)) +
  theme_FVM() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        legend.position = 'bottom')

tree_BA_trends

svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_Tree_BA_by_species_cycle.svg"),
    height = 8, width = 7)
tree_BA_trends
dev.off()


table(tree_stem_smooth3$spp_grp)
# Colors to start with. Can change them per park if needed
cols = c(
  "Acer rubrum (red maple)" = "#00c990",
  "Acer spp. (maple)" = "#00FF00",
  "Ailanthus altissima (tree-of-heaven)" = "#FF00C5",
  "Betula lenta (black birch)" = "#38A800",
  "Betula spp. (black birch)" = "#05e689", # Either use BETLEN or BETSPP
  "Carya spp. (hickory)" = "#911eb4",
  "Fagus grandifolia (American beech)" = "#FFAA00",
  "Fraxinus spp. (ash)" = "#A87000",
  "Ilex opaca (American holly)" = "#42d4f4",
  "Juniperus virginiana (eastern redcedar)" = "#9371B9",
  "Liquidambar styraciflua (sweetgum)" = "#FFFF00",
  "Liriodendron tulipifera (tulip poplar)" = "#4363d8",
  "Nyssa sylvatica (black gum)" = "#000075",
  "Other Exotic" = "#ca0020",
  "Other Native" = "#828282",
  "Pinus spp. (pine)" = "#5A462B",
  # "Pinus strobus (eastern white pine)" = "#5A1111",
  # "Pinus taeda (loblolly pine)" = "#5A1111", #assumes no overlap in PINSTR and PINTAE
  # "Pinus virginiana (Virginia pine)" = "#E5740D",
  "Prunus spp. (native cherry)" ="#00E6A9", 
  "Pyrus calleryana (Bradford pear)" = "#cd4a8f",
  "Quercus spp. (oak)" = "#0E5D2C",
  "Robinia pseudoacacia (black locust)" = "#efdf00",
  "Subcanopy" = "#ffa8b4",
  "Tsuga canadensis (eastern hemlock)" = "#9bd2ef",
  "Ulmus spp. (native elm)" = "#59538A", 
  "Unknown spp." = "#CACACA")

lines = c(
  "Acer rubrum (red maple)" = "solid",
  "Acer spp. (maple)" = "dashed",
  "Ailanthus altissima (tree-of-heaven)" = "solid",
  "Betula lenta (black birch)" = "solid",
  "Betula spp. (black birch)" = "dashed", # Either use BETLEN or BETSPP
  "Carya spp. (hickory)" = "solid",
  "Fagus grandifolia (American beech)" = "solid",
  "Fraxinus spp. (ash)" = "solid",
  "Ilex opaca (American holly)" = "solid",
  "Juniperus virginiana (eastern redcedar)" = "dotdash",
  "Liquidambar styraciflua (sweetgum)" = "solid",
  "Liriodendron tulipifera (tulip poplar)" = "solid",
  "Nyssa sylvatica (black gum)" = "dotdash",
  "Other Exotic" = "dashed",
  "Other Native" = "solid",
  "Pinus spp. (pine)" = "solid",
  "Prunus spp. (native cherry)" = "dashed", 
  "Pyrus calleryana (Bradford pear)" = "dotted",
  "Quercus spp. (oak)" = "solid",
  "Robinia pseudoacacia (black locust)" = "dashed",
  "Subcanopy" = "solid",
  "Tsuga canadensis (eastern hemlock)" = "dashed",
  "Ulmus spp. (native elm)" = "dotted", 
  "Unknown spp." = "dotted")

#---- Net stem/BA plots by species
net_stems <- 
  ggplot(tree_stem_smooth3, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Tree Density (stems/ha)") +
  # geom_line(data = mor_rec_tot %>% filter(unit_type == "stem"),
  #           aes(x = cycle, y = park_total), color = 'black') +
  theme_FVM()+
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines,  name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'none', # b/c shares page with 4B 
        legend.key.width = unit(1, 'cm'))

net_stems
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
  theme_FVM()+
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
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
reg1 <- do.call(joinRegenData, c(args_all, units = 'sq.m')) |> 
  filter(!ScientificName %in% "None present") # b/c treat as shrub until tree-size

reg <- left_join(reg1, plot_evs, by = c("Plot_Name", "cycle", "SampleYear")) |> 
  mutate(stock = stock * (4 * pi)) #100pt scale

reg_grps <- left_join(reg, trspp_grps |> select(Species, spp_grp, sppcode), 
                      by = c("ScientificName" = "Species"))

if(nrow(reg_grps[which(is.na(reg_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in reg_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group

# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

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
head(trspp_grps)
plot_rspp_yr <- left_join(plot_rspp_yr3, trspp_grps |> select(sppcode, spp_grp) |> unique(), 
                         by = "spp_grp")

reg_spp_smooth <- left_join(plot_rspp_yr, reg_grps |> select(Plot_Name, SampleYear, spp_grp, seed_den, sap_den), 
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
                     limits = c(2005, 2024)) +
  theme_FVM() + 
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
                     limits = c(2005, 2024)) +
  theme_FVM() + 
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
  theme_FVM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
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
  theme_FVM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
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

#----- Trends in invasive guilds over time -----
guilds <- do.call(sumQuadGuilds, c(args_vs, speciesType = 'invasive', splitHerb = F))
guild_list <- sort(unique(guilds$Group))

guild_smooth <- purrr::map_dfr(guild_list,
                               function(g){
                                 df <- guilds |> filter(Group %in% g)
                                 case_boot_loess(df, x = "SampleYear", y = "quad_pct_cover", 
                                                 ID = "Plot_Name", span = span,
                                                 group = "Group",
                                                 num_reps = 1) |> 
                                   mutate(guild = g)
                               })

#guild_smooth2 <- 
gcols <- c("Tree" = "#4A68BF",
           "Shrub" = "#CD5C5C",
           "Herbaceous" = "#228b22",
           "Graminoid" = "#ffd700")

guild_plot <- 
  ggplot(guild_smooth, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = guild, group = guild), linewidth = 1.5) +
  labs(x = NULL, y = "% Invasive Cover") +
  theme_FVM()+
  scale_color_manual(values = gcols,  name = "Invasive Guild") +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom',  
        legend.key.width = unit(1, 'cm'))

svg(paste0(new_path, "figures/", "Figure_5A_", park, "_smoothed_invasive_cover_by_guild_cycle.svg"),
    height = 4.6, width = 8)
guild_plot
dev.off()

#----- Number of Ash tree stems over time ------
Fraxinus_spp <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', 
                  'Fraxinus nigra', 'Fraxinus profunda')

frax <- do.call(joinTreeData, c(args_vs, status = 'live')) |> filter(ScientificName %in% Fraxinus_spp)
head(frax)

frax_sum <- frax |> group_by(Plot_Name, cycle, ScientificName) |> 
  summarize(num_stems = sum(num_stems), .groups = 'drop') |> 
  mutate(sppcode = toupper(paste0(substr(word(ScientificName, 1), 1, 3), 
                                  substr(word(ScientificName, 2), 1, 3)))) |> 
  pivot_wider(names_from = sppcode, values_from = num_stems, values_fill = 0) #|> 
  #mutate(FRAXSPP = FRAPEN + FRAAME + FRANIG) 
head(frax_sum)

frax_spp <- names(frax_sum[,grepl("FRA", names(frax_sum))])

frax_sum$FRAXSPP = rowSums(frax_sum[,frax_spp])

fraxspp_wide <- frax_sum |> select(Plot_Name, cycle, FRAXSPP) |> 
  pivot_wider(names_from = cycle, values_from = FRAXSPP, names_glue = "{'FRAX'}_{cycle}")

plots <- do.call(joinLocEvent, args = args_vs) |> 
  select(Plot_Name, X = xCoordinate, Y = yCoordinate) |> unique()

fraxspp <- left_join(plots, fraxspp_wide, by = "Plot_Name") |> unique()
fraxspp[,4:ncol(fraxspp)][is.na(fraxspp[,4:ncol(fraxspp)])] <- 0
head(fraxspp)


write_to_shp <- function(data, x = "X", y = "Y", shp_name){
  st_write(st_as_sf(data, coords = c(x, y), crs = park_crs),
           shp_name, delete_layer = TRUE)#FALSE)
}

write_to_shp(fraxspp, 
             shp_name = paste0(new_path, "shapefiles/", park, "_ash_trees_by_cycle_", to, ".shp" ))
