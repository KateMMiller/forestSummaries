library(tidyverse)
library(forestNETN)
library(vegan)

importData()
VIEWS_NETN$Taxa_NETN$IsCanopyExclusion[VIEWS_NETN$Taxa_NETN$Genus == "Fraxinus"] <- TRUE

#---- Params -----
park = "SARA"
from = 2022
to = 2023
num_plots = 32
plot_size = 400
from_prev = 2016
to_prev = 2019

# Plot list
plotevs <- joinLocEvent(park = park, from = from, to = to) |> select(Plot_Name, SampleYear)

# Deer Browse Index
dbi <- joinStandData(park = park, from = from, to = to) |> select(Plot_Name, dbi = Deer_Browse_Index)

mean_dbi <- mean(dbi$dbi)
mean_dbi # 4.22

dbiprev <- joinStandData(park = park, from = from, to = to) |> select(Plot_Name, dbi = Deer_Browse_Index)
dbi_prev <- mean(dbiprev$dbi)
dbi_prev # 4.125

# Regen densities
reg <- joinRegenData(park = park, from = from, to = to, units = 'sq.m') 

reg$CanopyExclusion[reg$ScientificName %in% c("Fraxinus americana", "Fraxinus nigra", 
                                                "Fraxinus pennsylvanica", "Fraxinus")] <- TRUE
reg$NatCan <- ifelse(reg$CanopyExclusion == FALSE & reg$Exotic == FALSE, 1, 0)

DBI_threshold <- ifelse(mean_dbi <= 3, 50, 100)

reg_natcan <- reg |> filter(NatCan == 1) |> 
  group_by(Plot_Name) |> 
  summarize(sapden = sum(sap_den, na.rm = T),
            seedden = sum(seed_den, na.rm = T),
            stock = sum(stock, na.rm = T), .groups = 'drop') |> 
  mutate(stocked = ifelse(stock > DBI_threshold, 1, 0)) # DBI > 3, so stocking must be 100

regsum_natcan <- reg_natcan |> 
  summarize(sapden = sum(sapden)/num_plots,
            seedden = sum(seedden)/num_plots,
            stock = sum(stock)/num_plots,
            pct_stocked = sum(stocked)/num_plots*100)

reg_tot <- reg |> group_by(Plot_Name) |> 
  summarize(
    sap_tot = sum(sap_den, na.rm = T),
    seed_tot = sum(seed_den, na.rm = T)
  )

length(unique(reg_tot$Plot_Name))

reg_natcan <- reg |> filter(NatCan == 1) |> 
  group_by(Plot_Name) |> 
  summarize(sap_natcan = sum(sap_den, na.rm = T),
            seed_natcan = sum(seed_den, na.rm = T))

reg_comb <- left_join(reg_tot, reg_natcan, by = "Plot_Name") |> 
  mutate(sap_dens_pct = sap_natcan/sap_tot,
         seed_dens_pct = seed_natcan/seed_tot)

if(length(unique(reg_comb$Plot_Name)) < num_plots){warning("Need to left_join with plotevs")} 

reg_comb[,2:7][is.na(reg_comb[,2:7])] <- 0

reg_pct <- reg_comb |> 
  summarize(sap_pct = mean(sap_dens_pct) * 100,
            seed_pct = mean(seed_dens_pct) * 100)

reg_pct
regsum_natcan
sort(unique(reg$ScientificName))

# Sorenson similarity
reg_seed <- reg |> 
  mutate(genus = word(ScientificName, 1),
         species = ifelse(is.na(word(ScientificName, 2)), "SPP", word(ScientificName, 2)),
         sppcode1 = toupper(paste0(substr(genus, 1, 3), substr(species, 1, 3))),
         sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3", 
                             ScientificName == "Quercus (Red group)" ~ "QUESPP", 
                             TRUE ~ sppcode1)) |> 
  select(Plot_Name, sppcode, seed_den) |> 
  group_by(Plot_Name, sppcode) |> 
  summarize(seed_den = sum(seed_den, na.rm = T), .groups = 'drop') |> 
  pivot_wider(names_from = sppcode, values_from = seed_den, values_fill = 0)

reg_sap <- reg |> 
  mutate(genus = word(ScientificName, 1),
         species = ifelse(is.na(word(ScientificName, 2)), "SPP", word(ScientificName, 2)),
         sppcode1 = toupper(paste0(substr(genus, 1, 3), substr(species, 1, 3))),
         sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3", 
                             ScientificName == "Quercus (Red group)" ~ "QUESPP", 
                             TRUE ~ sppcode1)) |> 
  select(Plot_Name, sppcode, sap_den) |> 
  group_by(Plot_Name, sppcode) |> 
  summarize(sap_den = sum(sap_den, na.rm = T), .groups = 'drop') |> 
  pivot_wider(names_from = sppcode, values_from = sap_den, values_fill = 0)

trees <- joinTreeData(park = park, from = from, to = to, status = 'live') |> 
  mutate(genus = word(ScientificName, 1),
         species = ifelse(is.na(word(ScientificName, 2)), "SPP", word(ScientificName, 2)),
         sppcode1 = toupper(paste0(substr(genus, 1, 3), substr(species, 1, 3))),
         sppcode = ifelse(ScientificName == "Acer saccharum", "ACESAC3", sppcode1)) |> 
  select(Plot_Name, sppcode, BA_cm2) |> 
  group_by(Plot_Name, sppcode) |> 
  summarize(treeBA = sum(BA_cm2, na.rm = T)/plot_size, .groups = 'drop') |> 
  pivot_wider(names_from = sppcode, values_from = treeBA, values_fill = 0)

all_spp <- c("Plot_Name", sort(unique(c(names(trees[,-1]), names(reg_seed[,-1]), names(reg_sap[,-1])))))

seed_miss <- setdiff(all_spp, names(reg_seed))
reg_seed[seed_miss] <- 0
reg_seed <- reg_seed[all_spp]
reg_seed$strata <- "seedling"

sap_miss <- setdiff(all_spp, names(reg_sap))
reg_sap[sap_miss] <- 0
reg_sap <- reg_sap[all_spp]
reg_sap$strata = "sapling"

tree_miss <- setdiff(all_spp, names(trees))
trees[tree_miss] <- 0
trees <- trees[all_spp]
trees$strata = "tree"

comb <- rbind(reg_seed, reg_sap, trees)

sor_fun <- function(df){
  df2 <- df %>% select_if(~is.numeric(.) && sum(.) != 0) # remove species that sum to 0
  sor <-  betadiver(df2, method = 'sor')
  return(sor)
}

sor_sap <- comb |> filter(strata %in% c("tree", "sapling")) |> 
  group_by(Plot_Name) |> nest() |> 
  mutate(sor_sap = purrr::map(data, sor_fun)) |> 
  unnest(cols = c("sor_sap")) |> select(-data)

sor_sap_mean <- mean(sor_sap$sor_sap, na.rm = T)

sor_seed <- comb |> filter(strata %in% c("tree", "seedling")) |> 
  group_by(Plot_Name) |> nest() |> 
  mutate(sor_seed = purrr::map(data, sor_fun)) |> 
  unnest(cols = c("sor_seed")) |> select(-data)

sor_seed_mean <- mean(sor_seed$sor_seed, na.rm = T)

# Tree DBH distribution
tree_dist <- sumTreeDBHDist(park = park, from = from, to = to, status = 'live')

tree_dist2 <- tree_dist |> 
  summarize(dbh_10cm = sum(dens_10_19.9)/num_plots,
            dbh_20cm = sum(dens_20_29.9)/num_plots,
            dbh_30cm = sum(dens_30_39.9)/num_plots,
            dbh_40cm = sum(dens_40_49.9)/num_plots,
            dbh_50cm = sum(dens_50_59.9)/num_plots, 
            dbh_60cm = sum(dens_60_69.9)/num_plots,                                      
            dbh_70cm = sum(dens_70_79.9)/num_plots,                                 
            dbh_80cm = sum(dens_80_89.9)/num_plots, 
            dbh_90cm = sum(dens_90_99.9)/num_plots,
            dbh_100cm = sum(dens_100p)/num_plots) |> 
  mutate(park = park) |> 
  pivot_longer(cols = -park, names_to = "size_class", values_to = "density") 
  
tree_dist2$class <- as.numeric(gsub("\\D", "", tree_dist2$size_class))

lin_mod <- lm(density ~ class, data = tree_dist2)
exp_mod <- lm(log(density + 1) ~ class, data = tree_dist2)

aic_check <- data.frame(park = park, 
                        linear = AIC(lin_mod),
                        exp = AIC(exp_mod) + sum(2*log(tree_dist2$density + 1)))
aic_check 
flat_dist <- ifelse(aic_check$linear < aic_check$exp, 1, 0)

#----- Regen Debt Table -----
debt <- data.frame(Metric = c("Sapling Density", "Seedling Density", "Pct Stocked Plots",
                              "Stocking Index", "Deer Browse Impacts", "Flat Tree Diam. Dist.",
                              "Sapling Composition", "Seedling Composition", 
                              "Sorensen Sapling", "Sorensen Seedling"),
                   Value = c(regsum_natcan$sapden, regsum_natcan$seedden, regsum_natcan$pct_stocked,
                             regsum_natcan$stock, mean_dbi, flat_dist, 
                             reg_pct$sap_pct, reg_pct$seed_pct, 
                             sor_sap_mean, sor_seed_mean),
                   Units = c("(stems/sq.m)", "(stems/sq.m)", "%", 
                             "(per 2m radius microplot)", "(range: 1 - 5)", "", "%", "%", 
                             "(range: 0 - 1)", "(range: 0 - 1)"))
debt <- debt |> mutate(
  status = 
    case_when(Metric == "Sapling Density" & Value < 0.1 ~ "Critical",
              Metric == "Sapling Density" & Value >= 0.1 & Value < 0.16 ~ "Caution",
              Metric == "Sapling Density" & Value >= 0.16 ~ "Acceptable",
              
              Metric == "Seedling Density" & Value < 0.25 ~ "Critical",
              Metric == "Seedling Density" & Value >= 0.25 & Value < 2 ~ "Caution",
              Metric == "Seedling Density" & Value >= 2.0 ~ "Acceptable",
              
              Metric == "Pct Stocked Plots" & Value < 33 ~ "Critical",
              Metric == "Pct Stocked Plots" & Value >= 33 & Value < 67 ~ "Caution",
              Metric == "Pct Stocked Plots" & Value >= 67 ~ "Acceptable",
              
              Metric == "Stocking Index" & Value < 25 ~ "Critical",
              Metric == "Stocking Index" & Value >= 25 & Value < 100 ~ "Caution",
              Metric == "Stocking Index" & Value >= 100 ~ "Acceptable",
              
              Metric == "Deer Browse Impacts" & Value >= 4 ~ "Critical",
              Metric == "Deer Browse Impacts" & Value > 3 & Value < 4 ~ "Caution",
              Metric == "Deer Browse Impacts" & Value <= 3 ~ "Acceptable",
              
              Metric == "Flat Tree Diam. Dist." & Value == 1 ~ "Critical",
              Metric == "Flat Tree Diam. Dist." & Value == 0 ~ "Acceptable",
              
              Metric == "Sapling Composition" & Value < 50 ~ "Critical",
              Metric == "Sapling Composition" & Value >= 50 & Value <= 70 ~ "Caution",
              Metric == "Sapling Composition" & Value > 70 ~ "Acceptable",
              
              Metric == "Seedling Composition" & Value < 50 ~ "Critical",
              Metric == "Seedling Composition" & Value >= 50 & Value <= 70 ~ "Caution",
              Metric == "Seedling Composition" & Value > 70 ~ "Acceptable",
              
              Metric == "Sorensen Sapling" & Value < 0.2 ~ "Critical",
              Metric == "Sorensen Sapling" & Value >= 0.2 ~ "Acceptable",

              Metric == "Sorensen Seedling" & Value < 0.2 ~ "Critical",
              Metric == "Sorensen Seedling" & Value >= 0.2 ~ "Acceptable",
              
              TRUE ~ "UNKNOWN"
              )
  )

debt_final <- 
  rbind(debt,
        data.frame(Metric = 'Regen. Debt Status', 
                   Value = sum(debt$status == "Critical"), 
                   Units = NA,
                   status = case_when(sum(debt$status == "Critical") >= 6 ~ "Imminent Failure",
                                      between(sum(debt$status == "Critical"), 4, 5) ~ "Probable Failure",
                                      between(sum(debt$status == "Critical"), 2, 3) ~ "Insecure",
                                      sum(debt$status == "Critical") <= 1 ~ "Secure",
                                      TRUE ~ "UNK"))
       )

debt_final$rank <- as.numeric(row.names(debt_final))
debt_final$label_order <- reorder(debt_final$Metric, debt_final$rank)
debt_final$status_fac <- factor(debt_final$status, levels = c("Acceptable", "Caution", "Critical"))

debt_final$text_label <- paste0(as.character(paste0(round(debt_final$Value, 2))), " ", debt_final$Units)
debt_final$text_label <- ifelse(debt_final$Metric == "Flat Tree Diam. Dist." & debt_final$Value == 1, 
                                paste0("TRUE"), 
                            ifelse(debt_final$Metric == "Flat Tree Diam. Dist." & debt_final$Value == 0,
                                   paste0("FALSE"), paste0(debt_final$text_label)))
debt_final$text_label <- ifelse(debt_final$Metric == "Regen. Debt Status", 
                                paste0(debt_final$status), 
                                paste0(debt_final$text_label))

debt_final

results_plot <- 
  ggplot(debt_final, 
         aes(x = 1, y = ordered(label_order, levels = rev(label_order)),
             fill = label_order)) +
  geom_tile(aes(fill = status_fac), color = 'black', size = 0.5)+
  geom_text(aes(label = text_label,
                fontface = ifelse(Metric == "Regen. Debt Status", 2, 1))) +
  scale_fill_manual(values = c('Acceptable' = '#BDEBA7',
                               'Caution' = "#FFFF79",
                               'Critical' = "#FF5B5B"),
                    labels = c("Acceptable",
                               "Caution",
                               "Critical"),
                    name = NULL, 
                    drop = FALSE)+
  labs(x = NULL, y = NULL) + 
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10),
        #text = element_text(size = 9.5),
        legend.position = 'bottom',
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10)
) 

results_plot

path <- paste0("C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/", 
               park, "/", to, '/figures/')
ggsave(paste0(path, "Figure_X_Regen_Debt_table.svg"), height = 6, width = 4.5, units = 'in')

#--- SARA Values ---
# sapden = 0.0257; Critical
# seedden = 0.216; Critical
# stock = 11.5; Critical
# pct_stocked = 3.12; Critical
# DBI = 4.22; Critical
# Sapling comp = 0.219; Critical
# Seedling comp = 0.333 # Critical
# Sor Sapling = 0.15 # Critical
# Sor Seedling = 0.337 # Acceptable
# Flat Diam Dist = FALSE # Acceptable

# Thresholds are in Table 2 of Miller et al. 2023




