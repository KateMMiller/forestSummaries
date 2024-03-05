#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time
# ++++++++ MUST RUN source_script.R FIRST ++++++++
#-------------------------------------------------------------------
library(vegan)

#---- Params -----

# Plot list
plotevs <- joinLocEvent(park = park, from = from_4yr, to = to) |> #filter(IsStuntedWoodland == FALSE) |> 
  select(Plot_Name, SampleYear) |> filter(!Plot_Name %in% "COLO-380") # remove if plot sampled again

# Deer Browse Index
dbi <- joinStandData(park = park, from = from_4yr, to = to) |> #filter(IsStuntedWoodland == FALSE) |> 
  select(Plot_Name, dbi = Deer_Browse_Index) |> filter(!Plot_Name %in% "COLO-380") 

mean_dbi <- mean(dbi$dbi)
mean_dbi # SARA = 4.22; SAGA 3.47; 2.6 ACAD

dbiprev <- joinStandData(park = park, from = from_prev, to = to_prev) |> select(Plot_Name, dbi = Deer_Browse_Index) |> filter(!Plot_Name %in% "COLO-380") 
dbi_prev <- mean(dbiprev$dbi)
dbi_prev # SARA = 4.125; SAGA = 3.38; 2.6 ACAD 

# DBI distribution plot
dbi_all <- joinStandData(park = park, from = from, to = to) |> 
  select(Plot_Name, cycle, dbi = Deer_Browse_Index) |> filter(!Plot_Name %in% "COLO-380") 

dbi_sum <- dbi_all |> group_by(cycle, dbi) |> 
  summarize(num_plots = sum(!is.na(dbi)), .groups = 'drop') |>
  filter(!(is.na(dbi)))

 # pivot_wider(names_from = dbi, values_from = num_plots, names_glue = "DBI_{.name}", values_fill = 0)

# Update to include all DBI values (1 and 2 lumped)
dbi_grid <- expand.grid(cycle = unique(dbi_sum$cycle), dbi = 2:5)
dbi_grid$dbi_fac <- factor(dbi_grid$dbi, levels = c(2, 3, 4, 5), labels = c("Low", "Medium", "High", "Very High"))

dbi_sum2 <- left_join(dbi_grid, dbi_sum, by = c("cycle", "dbi"))
dbi_sum2$num_plots[is.na(dbi_sum2$num_plots)] <- 0

dbi_plot <- 
ggplot(dbi_sum2, aes(x = cycle, y = num_plots, fill = dbi_fac, color = dbi_fac)) +
  geom_bar(position = 'fill', stat = 'identity', na.rm = T, color = '#696969') +
  theme_FVM() +
  # scale_color_manual(values = c("Low" = "#05e689", "Medium" = "#efdf00", 
  #                               "High" = "#f94b24", "Very High" = "#a60808"),
  #                    labels = c("Low", "Medium", "High", "Very High"), 
  #                    name = "Deer Browse Impact") +
  scale_fill_manual(values = c("Low" = "#05e689", "Medium" = "#efdf00", 
                               "High" = "#f94b24", "Very High" = "#a60808"),
                    labels = c("Low", "Medium", "High", "Very High"), 
                    name = "Deer Browse Impact") +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00), labels = c(0, 25, 50, 75, 100)) +
  labs(y = "Proportion of Plots")

# svg(paste0(new_path, "figures/", "Figure_6_", park, "_DBI_by_cycle.svg"),
#     height = 6.15, width = 8)
# dbi_plot
# dev.off()

figpath <- paste0(path, park, "/", to, '/figures/')
ggsave(paste0(figpath, "Figure_3_", park, "_DBI_by_cycle.svg"), height = 6.15, width = 8, units = 'in')


# Regen densities
# reg is all spp. reg$NatCan is used for metrics that only include native canopy forming spp.
reg <- joinRegenData(park = park, from = from_4yr, to = to, units = 'sq.m') |> filter(!Plot_Name %in% "COLO-380") 

reg$CanopyExclusion[reg$ScientificName %in% c("Fraxinus americana", "Fraxinus nigra", 
                                                "Fraxinus pennsylvanica", "Fraxinus")] <- TRUE
reg$NatCan <- ifelse(reg$CanopyExclusion == FALSE & reg$Exotic == FALSE, 1, 0)

table(reg$ScientificName, reg$NatCan) # only native canopy forming included

DBI_threshold <- ifelse(mean_dbi <= 3, 50, 100)

reg_natcan <- reg |> filter(NatCan == 1) |> 
  group_by(Plot_Name) |> 
  summarize(sapden = sum(sap_den, na.rm = T),
            seedden = sum(seed_den, na.rm = T),
            stock = (sum(stock, na.rm = T)) * (4 * pi), .groups = 'drop') |> # for 100pt scale
  mutate(stocked = ifelse(stock > DBI_threshold, 1, 0)) # DBI > 3, so stocking must be 100

regsum_natcan <- reg_natcan |> 
  summarize(sapden = sum(sapden)/num_plots,
            seedden = sum(seedden)/num_plots,
            stock = (sum(stock)/num_plots), 
            pct_stocked = sum(stocked)/num_plots*100)

reg_tot <- reg |> group_by(Plot_Name) |> 
  summarize(
    sap_tot = sum(sap_den, na.rm = T),
    seed_tot = sum(seed_den, na.rm = T)
  )

if(length(unique(reg_tot$Plot_Name)) < num_plots & !park %in% "COLO"){
  warning(paste0("Regen debt metrics don't include the total number of plots for ", park, 
                 ". Compare total number of plots = ", num_plots, " regen debt plot tally = ", 
                 length(unique(reg_tot$Plot_Name))))}

reg_natcan <- reg |> filter(NatCan == 1) |> 
  group_by(Plot_Name) |> 
  summarize(sap_natcan = sum(sap_den, na.rm = T),
            seed_natcan = sum(seed_den, na.rm = T))

reg_comb <- left_join(reg_tot, reg_natcan, by = "Plot_Name") |> 
  mutate(sap_dens_pct = sap_natcan/sap_tot,
         seed_dens_pct = seed_natcan/seed_tot)
#intersect(names(plotevs), names(reg_comb))
reg_comb <- left_join(plotevs, reg_comb, by = "Plot_Name")
#if(length(unique(reg_comb$Plot_Name)) < num_plots){warning("Need to left_join with plotevs")} 
head(reg_comb)

reg_comb[,2:7][is.na(reg_comb[,2:7])] <- 0
reg_comb$sap_dens_pct[is.nan(reg_comb$sap_dens_pct)] <- 0
reg_comb$seed_dens_pct[is.nan(reg_comb$seed_dens_pct)] <- 0

reg_pct <- reg_comb |> 
  summarize(sap_pct = mean(sap_dens_pct, na.rm = T) * 100,
            seed_pct = mean(seed_dens_pct, na.rm = T) * 100)

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
                             ScientificName == "Quercus (White group)" ~ "QUESPP",
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
                             ScientificName == "Quercus (White group)" ~ "QUESPP",
                             TRUE ~ sppcode1)) |> 
  select(Plot_Name, sppcode, sap_den) |> 
  group_by(Plot_Name, sppcode) |> 
  summarize(sap_den = sum(sap_den, na.rm = T), .groups = 'drop') |> 
  pivot_wider(names_from = sppcode, values_from = sap_den, values_fill = 0)

trees <- joinTreeData(park = park, from = from_4yr, to = to, status = 'live') |> 
  filter(!Plot_Name %in% "COLO-380") |> 
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

sor_sap <- comb %>% filter(strata %in% c("tree", "sapling")) %>% 
  group_by(Plot_Name) %>% nest() %>% 
  mutate(sap_sor = purrr::map(data, sor_fun)) %>%
  unnest(cols = c(sap_sor)) %>% select(Plot_Name, sap_sor) %>% data.frame() 

sor_sap_mean <- mean(sor_sap$sap_sor, na.rm = T)

sor_seed <- comb %>% filter(strata %in% c("tree", "seedling")) %>% 
  group_by(Plot_Name) %>% nest() %>% 
  mutate(seed_sor = purrr::map(data, sor_fun)) %>% 
  unnest(cols = c(seed_sor)) %>% select(Plot_Name, seed_sor) %>% data.frame()

sor_seed_mean <- mean(sor_seed$seed_sor, na.rm = T)

# Tree DBH distribution
tree_dist <- sumTreeDBHDist(park = park, from = from_4yr, to = to, status = 'live') |> filter(!Plot_Name %in% "COLO-380") 

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
debt <- data.frame(Metric = c("Sapling Density", "Seedling Density", "% Stocked Plots",
                              "Stocking Index", "Deer Browse Impacts", "Flat Tree Diam. Dist.",
                              "Sapling Composition", "Seedling Comp.", 
                              "Sorensen Sapling", "Sorensen Seedling"),
                   Value = c(regsum_natcan$sapden, regsum_natcan$seedden, regsum_natcan$pct_stocked,
                             regsum_natcan$stock, mean_dbi, flat_dist, 
                             reg_pct$sap_pct, reg_pct$seed_pct, 
                             sor_sap_mean, sor_seed_mean),
                   Units = c("(stems/m^2)", "(stems/m^2)", "%", 
                             "(2m radius scale)", "(range: 1 - 5)", "", "%", "%", 
                             "(range: 0 - 1)", "(range: 0 - 1)"))
debt <- debt |> mutate(
  status = 
    case_when(Metric == "Sapling Density" & Value < 0.1 ~ "Critical",
              Metric == "Sapling Density" & Value >= 0.1 & Value < 0.16 ~ "Caution",
              Metric == "Sapling Density" & Value >= 0.16 ~ "Acceptable",
              
              Metric == "Seedling Density" & Value < 0.25 ~ "Critical",
              Metric == "Seedling Density" & Value >= 0.25 & Value < 2 ~ "Caution",
              Metric == "Seedling Density" & Value >= 2.0 ~ "Acceptable",
              
              Metric == "% Stocked Plots" & Value < 33 ~ "Critical",
              Metric == "% Stocked Plots" & Value >= 33 & Value < 67 ~ "Caution",
              Metric == "% Stocked Plots" & Value >= 67 ~ "Acceptable",
              
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
              
              Metric == "Seedling Comp." & Value < 50 ~ "Critical",
              Metric == "Seedling Comp." & Value >= 50 & Value <= 70 ~ "Caution",
              Metric == "Seedling Comp." & Value > 70 ~ "Acceptable",
              
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
  geom_tile(aes(fill = status_fac), color = 'black', linewidth = 0.5)+
  geom_text(aes(label = text_label,
                fontface = ifelse(Metric == "Regen. Debt Status", 2, 1))) +
  scale_fill_manual(values = c('Acceptable' = "#BDEBA7", 
                               'Caution' = "#FFFF79",
                               'Critical' = "#FF5B5B"), 
                    na.value = "white",
                    labels = c("Acceptable",
                               "Caution",
                               "Critical"),
                    breaks = c("Acceptable", "Caution", "Critical"),
                    name = NULL, 
                    drop = FALSE)+ #, na.translate = FALSE)+
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

figpath2 <- paste0(path, park, "/", to, '/figures/') # not hard coded

ggsave(paste0(figpath2, "Figure_2_Regen_Debt_table.svg"), height = 6, width = 4.5, units = 'in')

debt_final <- debt_final |> mutate(park = park)

write.csv(debt_final, "Regen_Debt_table.csv", row.names= F)
# debt_append <- read.csv("Regen_Debt_table.csv")
# debt_append2 <- rbind(debt_append, debt_final) 
# write.csv(debt_append2, "Regen_Debt_table.csv", row.names = F)

# Now open svg and make the Regen Debt Status fill white, "#FFFFFF", and font size 13 instead of 11.
