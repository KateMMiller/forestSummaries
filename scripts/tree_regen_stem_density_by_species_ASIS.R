#-------------------------------------------------------------------
# Densities of Tree, sapling and seedlings in first cycle
# ++++++++ MUST RUN source_script.R FIRST ++++++++
#-------------------------------------------------------------------

library(cowplot)

if(!exists('trspp_grps')){stop("Must run source_script_MIDN.R before this script will work.")}
head(trspp_grps)

# Colors for ggplot legend
cols = c(
  "Acer rubrum (red maple)" = "#1EA57F",# "#00c990",
  "Acer spp. (maple)" = "#00FF00",
  "Ailanthus altissima (tree-of-heaven)" = "#FF00C5",
  "Betula lenta (black birch)" = "#38A800",
  "Betula spp. (black birch)" = "#05e689", # Either use BETLEN or BETSPP
  "Carya spp. (hickory)" = "#911eb4",
  "Diospyros virginiana (persimmon)" = "#DC970D",
  "Fagus grandifolia (American beech)" = "#FFAA00",
  "Fraxinus spp. (ash)" = "#A87000",
  "Ilex opaca (American holly)" = "#42d4f4",
  "Juniperus virginiana (eastern redcedar)" = "#9371B9",
  "Liquidambar styraciflua (sweetgum)" = "#FFFF00",
  "Liriodendron tulipifera (tulip poplar)" = "#4363d8",
  "Morella cerifera (wax myrtle)" = "#B051A1",
  "Nyssa sylvatica (black gum)" = "#000075",
  "Other Exotic" = "#ca0020",
  "Other Native" = "#C9CFCD",
  "Pinus taeda (loblolly pine)" = "#5A462B",
  # "Pinus strobus (eastern white pine)" = "#5A1111",
  # "Pinus taeda (loblolly pine)" = "#5A1111", #assumes no overlap in PINSTR and PINTAE
  # "Pinus virginiana (Virginia pine)" = "#E5740D",
  "Prunus spp. (native cherry)" ="#00E6A9", 
  "Pyrus calleryana (Bradford pear)" = "#cd4a8f",
  "Quercus spp. (oak)" = "#23984F",
  "Robinia pseudoacacia (black locust)" = "#efdf00",
  "Subcanopy" = "#ffa8b4",
  "Tsuga canadensis (eastern hemlock)" = "#9bd2ef",
  "Ulmus spp. (native elm)" = "#59538A", 
  "Unknown spp." = "#CACACA")

wd_spp <- sumSpeciesList(park = "ASIS", from = from, to = to) |> 
  filter(ScientificName %in% trspp_grps$Species)

table(wd_spp$ScientificName)

wd_spp2 <- left_join(wd_spp, trspp_grps |> select(Species, spp_grp), 
                     by = c("ScientificName" = "Species"))

table(wd_spp2$ScientificName)

# Revising species groups because low diversity and unique from other MIDN/NCBN parks
wd_spp2$spp_grp[wd_spp2$spp_grp == "Pinus spp. (pine)"] <- "Pinus taeda (loblolly pine)"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Morella cerifera"] <- "Morella cerifera (wax myrtle)"
wd_spp2$spp_grp[wd_spp2$ScientificName == "Diospyros virginiana"] <- "Diospyros virginiana (persimmon)"

table(wd_spp2$spp_grp)

# Tree Plots
tr_sum <- wd_spp2 |> group_by(spp_grp) |> summarize(avg_dens = sum(tree_stems)/num_plots,
                                                    avg_dens_ha = (avg_dens * 10000)/400,
                                                    avg_BA = sum(BA_cm2)/num_plots,
                                                    avg_BA_m2ha = avg_BA/400) |> 
                                          select(-avg_dens, -avg_BA) 
#colSums(tr_sum[,2:3])

tr_dens_plot <- 
  ggplot(tr_sum, aes(y = avg_dens_ha, x = "Tree Density", fill = fct_reorder(spp_grp, avg_dens_ha))) + 
    geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
    labs(y = "stems/ha") +
    scale_fill_manual(values = cols,  name = NULL) +
    theme_FVM() + theme(axis.title.x = element_blank(),
                        legend.title = element_blank(), 
                        legend.position = 'none')
tr_dens_plot

tr_BA_plot <- 
  ggplot(tr_sum, aes(y = avg_BA_m2ha, x = "Tree BA", fill = fct_reorder(spp_grp, avg_BA_m2ha))) + 
  geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
  labs(y = "Basal Area (sq.m/ha)") +
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'none')
tr_BA_plot

# Regen Plots
names(wd_spp2)
regen_sum <- wd_spp2 |> group_by(spp_grp) |> summarize(avg_seed_dens = sum(seed_den)/num_plots,
                                                       avg_sap_dens = sum(sap_den)/num_plots)
head(regen_sum)

regen_sum2 <- wd_spp2 |> group_by(ScientificName) |> summarize(avg_seed_dens = sum(seed_den)/num_plots,
                                                       avg_sap_dens = sum(sap_den)/num_plots)
#colSums(tr_sum[,2:3])

seed_dens_plot <- 
  ggplot(regen_sum, aes(y = avg_seed_dens, x = "Seedling Density", fill = fct_reorder(spp_grp, avg_seed_dens))) + 
  geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
  labs(y = "stems/sq.m") +
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'none')
seed_dens_plot

sap_dens_plot <- 
  ggplot(regen_sum, aes(y = avg_sap_dens, x = "Sapling Density", fill = fct_reorder(spp_grp, avg_sap_dens))) + 
  geom_bar(position = 'stack', stat = 'identity', color = 'dimgrey') + 
  labs(y = "stems/sq.m") +
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'none')
sap_dens_plot

# create fake plot with all species in dataframe, so can use 1 legend.
fake_plot <- ggplot(wd_spp2, aes(x = ParkUnit, y = SampleYear, fill = spp_grp)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = cols,  name = NULL) +
  theme_FVM() + theme(axis.title.x = element_blank(),
                      legend.title = element_blank(), 
                      legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 4))
fake_plot

spp_leg <- get_legend(fake_plot)

full_plot <- 
  plot_grid(
  plot_grid(tr_BA_plot, tr_dens_plot, sap_dens_plot, seed_dens_plot, ncol = 4, nrow = 1),
  spp_leg, ncol = 1, nrow = 2)

full_plot

ggsave(paste0(new_path, "figures/", "Figure_4_", park, "_tree_sapling_regen_by_species.svg"),
       height = 5.5, width = 7.5, units = 'in')

ggsave(paste0(new_path, "figures/", "Figure_4_", park, "_tree_sapling_regen_by_species.png"),
       height = 5.5, width = 7.5, units = 'in', dpi = 600)

