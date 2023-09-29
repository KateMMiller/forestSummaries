#-------------------------------------
# Forest Data Summaries: Regeneration
#-------------------------------------

new_path = paste0(path, park, "/", as.character(to), "/")

#---- Functions ----
# Write dataframe to shapefile using common settings
write_to_shp <- function(data, x = "X", y = "Y", shp_name){
  st_write(st_as_sf(data, coords = c(x, y), crs = park_crs),
           shp_name, delete_layer = TRUE)#FALSE)
}

#---- Plot event lists ----
plotevs <- do.call(joinLocEvent, args_all)
plotevs_vs <- do.call(joinLocEvent, args_vs)
plotevs_4yr <- plotevs %>% filter(between(SampleYear, from_4yr, to))

#---- Table 1. Regen densities by plot and year ----
reg <- do.call(joinRegenData, 
               args = c(args_all, speciesType = 'native', 
                        canopyForm = 'canopy', units = 'sq.m'))


reg_cycle_table <- reg %>% group_by(Plot_Name, cycle) %>% 
                           summarize(seed_den = round(sum(seed_den, na.rm = TRUE), 2),
                                     sap_den = round(sum(sap_den, na.rm = TRUE), 2),
                                     stock = round(sum(stock, na.rm = TRUE), 2) * (4 * pi), 
                                     .groups = 'drop') %>% 
                           left_join(plotevs %>% 
                                       select(Plot_Name, cycle, Plot = PlotCode, Panel = PanelCode),
                                     ., by = c("Plot_Name", "cycle"))

reg_cols <- c("seed_den", "sap_den", "stock")
reg_cycle_table[, reg_cols][is.na(reg_cycle_table[, reg_cols])] <- 0 

reg_cycle_wide <- reg_cycle_table %>% 
  pivot_wider(names_from = cycle, values_from = c(seed_den, sap_den, stock))

head(reg_cycle_wide)

write.csv(reg_cycle_wide, 
          paste0(new_path, "tables/", "Table_1_", park, "_regen_by_cycle.csv"), row.names = FALSE)

#---- Map 1 regen by cycle ----
reg_cycle <- reg %>% group_by(Plot_Name, cycle) %>% 
                     summarize(regen_den = sum(regen_den, na.rm = TRUE), .groups = 'drop') %>% 
                     left_join(plotevs %>% select(Plot_Name, xCoordinate, yCoordinate, cycle), ., 
                               by = c("Plot_Name", "cycle")) %>% 
                     arrange(Plot_Name, cycle) %>% 
                     pivot_wider(names_from = cycle, values_from = regen_den, 
                                 names_prefix = "cycle_", values_fill = 0) %>% 
                     rename(X = xCoordinate, Y = yCoordinate) #abbr for shapefile
names(reg_cycle)

max(reg_cycle[,4:ncol(reg_cycle)])

write_to_shp(reg_cycle, 
             shp_name = paste0(new_path, "shapefiles/", park, "_regen_by_cycle_", to, ".shp" ))

#---- Map 2 regen by size class ----
reg_sz_cols <- c("seed_15_30cm", "seed_30_100cm", "seed_100_150cm", "seed_p150cm", "sap_den") 

reg_size <- reg %>% group_by(Plot_Name, SampleYear) |> 
  summarize_at(vars(all_of(reg_sz_cols)), sum, na.rm = T)

reg_size_4yr <- reg_size %>% filter(between(SampleYear, from_4yr, to)) %>% 
                             left_join(plotevs_4yr %>% select(Plot_Name, xCoordinate, yCoordinate),
                                       ., by = 'Plot_Name')

reg_size_4yr[, reg_sz_cols][reg_size_4yr[is.na(reg_size_4yr[,reg_sz_cols])]] <- 0
head(reg_size_4yr)

colnames(reg_size_4yr) <- c("Plot_Name", "X", "Y", "SampleYear", 
                             "s15_30", "s30_100", "s100_150", "s150p", "sap") #abbr. for shapefile

reg_size_4yr$total <- rowSums(reg_size_4yr[,5:ncol(reg_size_4yr)])

write_to_shp(reg_size_4yr, 
             shp_name = paste0(new_path, "shapefiles/", park, 
                               "_regen_by_size_class_cycle_", cycle_latest, ".shp"))

#---- Figure 1A Regen trends by size class ----
# Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
reg_vs <- do.call(joinRegenData, 
                  args = c(args_vs, speciesType = 'native', 
                           canopyForm = 'canopy', units = 'sq.m'))

reg_size_cy <- reg_vs %>% group_by(Plot_Name, cycle) %>% 
                          summarize_at(vars(all_of(reg_sz_cols)), sum, na.rm = TRUE) %>% 
                          left_join(plotevs_vs %>% select(Plot_Name, cycle),
                                    ., by = c("Plot_Name", "cycle")) 

reg_size_cy[reg_sz_cols][reg_size_cy[is.na(reg_sz_cols)]] <- 0

# Use forestTrends to generate loess-smoothed CIs
reg_smooth1 <- map_dfr(reg_sz_cols, 
                      ~case_boot_loess(reg_size_cy, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                       span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                      mutate(size_class = .x)
)

# Smoother can result in negative values when really low. Converting those to 0 and converting CIs to NA
# A bit hand-wavy, but so is loess smoothing.
reg_smooth <- reg_smooth1
reg_smooth$estimate_orig <- reg_smooth$estimate
reg_smooth$estimate[reg_smooth$estimate_orig < 0] <- 0
reg_smooth$lower95[reg_smooth$estimate_orig < 0] <- NA
reg_smooth$upper95[reg_smooth$estimate_orig < 0] <- NA

# For plot labels
reg_colors <- c("seed_15_30cm" = "#D6D6FF", 
                "seed_30_100cm" = "#8F97E3", 
                "seed_100_150cm" = "#556CC9", 
                "seed_p150cm" = "#244EAD", 
                "sap_den" = "#05e646")

reg_smooth$size_class <- factor(reg_smooth$size_class, 
                                levels = c("seed_15_30cm", "seed_30_100cm", 
                                           "seed_100_150cm", "seed_p150cm", 
                                           "sap_den"))

# Set up cycle labels for figures
#midn1 <- c("FRSP", "PETE", "RICH")
midn1_labs <- c("1" = "Cycle 1: 2007 \u2013 2010",
                "2" = "Cycle 2: 2011 \u2013 2014",
                "3" = "Cycle 3: 2015 \u2013 2018",
                "4" = "Cycle 4: 2019 \u2013 2022",
                "5" = "Cycle 5: 2023")
#midn2 <- c("APCO", "BOWA", "GETT", "HOFU", "VAFO")
midn2_labs <- c("1" = "Cycle 1: 2007 \u2013 2010",
                "2" = "Cycle 2: 2011 \u2013 2014",
                "3" = "Cycle 3: 2015 \u2013 2018",
                "4" = "Cycle 4: 2019 \u2013 2023")
#ncbn <- c("GEWA", "THST")
ncbn_labs <- c("1" = "Cycle 1: 2008 \u2013 2011",
               "2" = "Cycle 2: 2012 \u2013 2015",
               "3" = "Cycle 3: 2016 \u2013 2019",
               "4" = "Cycle 4: 2019 \u2020 2023")
colo_labs <- c("1" = "Cycle 1: 2011 \u2013 2014",
               "2" = "Cycle 2: 2015 \u2013 2018",
               "3" = "Cycle 3: 2019 \u2013 2022",
               "4" = "Cycle 4: 2023")
sahi_labs = c("1" = "Cycle 1: 2009",
              "2" = "Cycle 2: 2013",
              "3" = "Cycle 3: 2017",
              "4" = "Cycle 4: 2023")
asis_labs = c("1" = "Cycle 1: 2019 \u2013 2023") #full cycle ends in 2024
  

cycle_labs <- switch(park,
                     "FRSP" = midn1_labs, 
                     "PETE" = midn1_labs,
                     "RICH" = midn1_labs,
                     "APCO" = midn2_labs,
                     "BOWA" = midn2_labs,
                     "GETT" = midn2_labs,
                     "HOFU" = midn2_labs,
                     "VAFO" = midn2_labs,
                     "GEWA" = ncbn_labs,
                     "THST" = ncbn_labs,
                     "COLO" = colo_labs,
                     "SAHI" = sahi_labs,
                     "ASIS" = asis_labs)

reg_labels <- c("15 \u2013 30 cm", "30 \u2013 100 cm", "100 \u2013 150 cm",
                ">150 cm & < 1 cm DBH", "Saplings: 1 \u2013 9.9 cm DBH")

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = size_class, y = estimate, color = size_class,#linetype = sign, 
                         group = size_class))+ theme_FVM()+
  geom_bar(stat = 'identity', aes(fill = size_class, color = 'DimGrey'))+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = bquote(Stems/m^2))+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs), ncol = 5)+
  scale_color_manual(values = reg_colors, name = "Size Class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "Size Class",
                    labels = reg_labels)+
  scale_x_discrete(name = "Size Class",
                   labels = reg_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

reg_trend_plot
  
ggsave(paste0(new_path, "figures/", "Figure_1A_", park, "_regen_by_size_class_by_cycle.svg"),
       height = 5.5, width = 7.5, units = 'in')

#---- Figure 1B Diam. dist. trends by size class ----
  # Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
  # Including all species and canopy forms
tree_dd <- do.call(sumTreeDBHDist, args = c(args_vs, status = 'live'))

dbh_cols <- c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9', 
              'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
              'dens_90_99.9', 'dens_100p')

tree_dbh_sm <- map_dfr(dbh_cols, 
                    ~case_boot_loess(tree_dd, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                     mutate(dbh_class = .x)
)
tree_dbh_sm

tree_dbh_sm$estimate[tree_dbh_sm$estimate < 0] <- 0
# tree_dbh_sm$lower95[tree_dbh_sm$estimate < 0] <- NA
# tree_dbh_sm$upper95[tree_dbh_sm$estimate < 0] <- NA

tree_dbh_sm$dbh_class <- factor(tree_dbh_sm$dbh_class, 
                                levels = dbh_cols)
levels(tree_dbh_sm$dbh_class)

dbh_labels <- c("10 \u2013 19.9 cm", 
                "20 \u2013 29.9 cm", 
                "30 \u2013 39.9 cm", 
                "40 \u2013 49.9 cm", 
                "50 \u2013 59.9 cm", 
                "60 \u2013 69.9 cm", 
                "70 \u2013 79.9 cm", 
                "80 \u2013 89.9 cm", 
                "90 \u2013 99.9 cm",
                ">= 100 cm")

dbh_labels <- c("10", 
                "20", 
                "30", 
                "40", 
                "50", 
                "60", 
                "70", 
                "80", 
                "90",
                "100+")

# check flat diameter distribution
head(tree_dbh_sm)

e <- min(tree_dbh_sm$estimate[tree_dbh_sm$estimate > 0])

tree_dbh_sm <- suppressWarnings(# warning is for 'c', which we're not using
  tree_dbh_sm %>% 
  mutate(dbh_class2 = gsub("p", "", dbh_class)) %>% 
  separate(dbh_class2, into = c("a", "b", "c"), sep = "_", remove = FALSE) %>% 
  mutate(class = as.numeric(b)) %>% select(-a, -b, -c, -dbh_class2))

AIC_test <- map_dfr(seq_along(cycle_labs), function(x){
  df <- tree_dbh_sm %>% filter(cycle == x)
  lin_mod <- lm(estimate ~ class, data = df)
  exp_mod <- lm(log(estimate + e) ~ class, data = df)
  aic_check <- data.frame(cycle = x,
                          linear = AIC(lin_mod),
                          exp = AIC(exp_mod) + sum(2*log(df$estimate + e)))
})

AIC_test$best_mod <- ifelse(AIC_test$linear < AIC_test$exp + 4, "linear", "log-linear")
# added 4 to exp because linear is the simpler model. The log-linear model needs
# to be < 4 points lower than linear AIC to count.

cycle_labs_tr = c("1" = ifelse(AIC_test$best_mod[AIC_test$cycle == 1] == 'linear',
                               paste0(cycle_labs[1], "*"), cycle_labs[1]),
                  "2" = ifelse(AIC_test$best_mod[AIC_test$cycle == 2] == 'linear',
                               paste0(cycle_labs[2], "*"), cycle_labs[2]),
                  "3" = ifelse(AIC_test$best_mod[AIC_test$cycle == 3] == 'linear',
                               paste0(cycle_labs[3], "*"), cycle_labs[3]),
                  "4" = ifelse(AIC_test$best_mod[AIC_test$cycle == 4] == 'linear',
                               paste0(cycle_labs[4], "*"), cycle_labs[4]),
                  "5" = ifelse(AIC_test$best_mod[AIC_test$cycle == 5] == 'linear',
                               paste0(cycle_labs[5], "*"), cycle_labs[5]))

dbh_trend_plot <- 
  ggplot(tree_dbh_sm, aes(x = dbh_class, y = estimate))+ theme_FVM()+
  geom_bar(stat = 'identity', fill = "#81B082" , color = 'DimGrey')+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = "Stems/ha")+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs_tr), ncol = 5)+
  scale_color_manual(values = reg_colors, name = "DBH Size Class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "DBH Size Class",
                    labels = reg_labels)+
  scale_x_discrete(name = "DBH Size Class",
                   labels = dbh_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

dbh_trend_plot

ggsave(paste0(new_path, "figures/", "Figure_1B_", park, "_tree_dbh_dist_by_cycle.svg"),
       height = 5, width = 7.5, units = 'in')

#---- Map 3 Regen by composition ----
reg_all <- do.call(joinRegenData, args = c(args_4yr, units = 'sq.m')) |> filter(!ScientificName %in% 'None present')

head(trspp_grps) # loaded in source_script_MIDN.R. Use as a first cut for grouping.

reg_grps <- left_join(reg_all, trspp_grps, by = c("ScientificName" = "Species"))

if(nrow(reg_grps[which(is.na(reg_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in reg_spp_grps$spp_grp, meaning at least one species is missing a group.")} #check if any spp. is missing a group
head(reg_grps)

reg_wide <- reg_grps %>% group_by(Plot_Name, sppcode) %>% 
  summarize(regen_den = sum(regen_den, na.rm = TRUE), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = "Plot_Name") %>% arrange(sppcode) %>% 
  pivot_wider(names_from = sppcode, values_from = regen_den, values_fill = 0) %>% 
  arrange(Plot_Name)

reg_wide <- if("NONPRE" %in% names(reg_wide)){reg_wide %>% select(-NONPRE)}else{reg_wide} 

reg_wide$total <- rowSums(reg_wide[,4:ncol(reg_wide)])
reg_wide$logtot <- log(reg_wide$total + 1)

names(sort(desc(colSums(reg_wide[,c(4:(ncol(reg_wide)-2))]))))


write_to_shp(reg_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_regen_by_spp_cycle", cycle_latest, ".shp"))

#---- Map 4 Tree canopy composition ----
tree_4yr <- do.call(joinTreeData, args = c(args_4yr, status = 'live'))

tree_grps <- left_join(tree_4yr, trspp_grps, by = c("ScientificName" = "Species"))

if(nrow(tree_grps[which(is.na(tree_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in reg_spp_grps$spp_grp, meaning at least one species is missing a group.")} #check if any spp. is missing a group
head(tree_grps)

tree_wide <- tree_grps %>% group_by(Plot_Name, sppcode) %>% 
  summarize(BAm2ha = sum(BA_cm2, na.rm = TRUE)/400, .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = "Plot_Name") %>% arrange(sppcode) %>% 
  pivot_wider(names_from = sppcode, values_from = BAm2ha, values_fill = 0) 

tree_wide$total <- rowSums(tree_wide[,4:ncol(tree_wide)])
tree_wide$logtot <- log(tree_wide$total + 1)

names(tree_wide)
write_to_shp(tree_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_tree_by_spp_cycle", cycle_latest, ".shp"))

#---- Map 5 Regen stocking index ----
reg_4yr <- do.call(joinRegenData,
                   args = c(args_4yr, speciesType = 'native',
                            canopyForm = 'canopy', units = 'sq.m'))

reg_4yr_stock <- reg_4yr %>% group_by(Plot_Name) %>% 
                             summarize(stock = (sum(stock, na.rm = T)) * (4 * pi)) %>% 
                 left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate) %>% unique(),
                           ., by = "Plot_Name")

write_to_shp(reg_4yr_stock, shp_name = 
         paste0(new_path, "shapefiles/", park, "_stocking_index_cycle_",
                cycle_latest, ".shp"))

#---- Map 6 Deer Browse Index ----
stand_4yr <- do.call(joinStandData, args_4yr) 

dbi <- left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
                 stand_4yr, by = "Plot_Name") %>% 
       select(Plot_Name, X, Y, DBI = Deer_Browse_Index)

mean(dbi$DBI)  #VAFO = 4.39

write_to_shp(dbi, shp_name = 
         paste0(new_path, "shapefiles/", park, "_dbi_cycle_", cycle_latest, ".shp"))


#---- Map 7 Invasive % Cover by Cycle ----
invcov <- do.call(joinQuadSpecies, args = c(args_all, speciesType = 'invasive')) %>% 
  select(Plot_Name, cycle, ScientificName, quad_avg_cov) %>% 
  group_by(Plot_Name, cycle) %>% summarize(quad_cov = sum(quad_avg_cov), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate, cycle),
            ., by = c("Plot_Name", "cycle")) %>% 
  mutate(quad_cov = replace_na(quad_cov, 0))

invcov_wide <- invcov %>% pivot_wider(names_from = cycle, values_from = quad_cov)

write_to_shp(invcov_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_inv_cover_by_cycle.shp"))

#---- Map 8 Invasive % Cover by Species ----
# Lump some species in the same genus
invspp_4yr <- joinQuadSpecies(from = from_4yr, to = to, speciesType = 'invasive') %>% 
  select(ScientificName) %>% unique() %>% arrange(ScientificName)

table(invspp_4yr$ScientificName)

Ligustrum = c("Ligustrum", "Ligustrum obtusifolium", "Ligustrum vulgare")
Lonicera = c("Lonicera - Exotic", "Lonicera morrowii", "Lonicera X bella")
Vincetoxicum = c("Vincetoxicum", "Vincetoxicum hirundinaria", "Vincetoxicum nigrum",
                 "Vincetoxicum rossicum")

#sort(unique(invspp_4yr$ScientificName))

invspp1 <- do.call(joinQuadSpecies, 
                   #args = list(from = 2017, to = 2022, speciesType = 'invasive')) %>% 
                   args = c(args_4yr, speciesType = 'invasive')) %>% 
  select(Plot_Name, ScientificName, quad_avg_cov) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name), ., by = "Plot_Name") %>% 
  mutate(quad_avg_cov = replace_na(quad_avg_cov, 0),
         present = ifelse(quad_avg_cov > 0, 1, 0),
         ScientificName = case_when(
           ScientificName %in% Ligustrum ~ "Ligustrum",
           ScientificName %in% Lonicera ~ "Lonicera - Exotic",
           ScientificName %in% Vincetoxicum ~ "Vincetoxicum",
           TRUE ~ ScientificName)) %>% 
  arrange(Plot_Name, ScientificName)

# Determine 12 most common invasives in a park by cover
plotspp_df <- data.frame(expand.grid(unique(invspp1$Plot_Name), 
                                     unique(invspp1$ScientificName), 
                                     stringsAsFactors = FALSE)) %>% 
  rename(Plot_Name = Var1, ScientificName = Var2) %>% 
  filter(!is.na(ScientificName))

topspp <- invspp1 %>% left_join(plotspp_df, ., by = c('Plot_Name', 'ScientificName')) %>% 
  arrange(Plot_Name, ScientificName) %>% 
  mutate(quad_avg_cov = replace_na(quad_avg_cov, 0),
         present = replace_na(present, 0)) %>% 
  group_by(ScientificName) %>% 
  summarize(avg_cov = mean(quad_avg_cov, na.rm = T),
            num_plots = sum(present), .groups = 'drop') %>% 
  arrange(desc(num_plots)) %>% slice(1:10) %>% select(ScientificName) #MIMA only
  #arrange(desc(avg_cov)) %>% slice(1:12) %>% select(ScientificName)

# Prep for shapefile
invspp <- invspp1 %>% 
  mutate(ScientificName = case_when(
    ScientificName %in% topspp$ScientificName ~ ScientificName, 
    ScientificName == "None present" ~ "None present", 
    TRUE ~ "Other invasive")) %>% 
  group_by(Plot_Name, ScientificName) %>% 
  summarize(quad_cov = sum(quad_avg_cov), .groups = 'drop') %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  #left_join(., prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName") %>% 
  select(Plot_Name, X, Y, ScientificName, quad_cov) %>% 
  mutate(quad_cov = replace_na(quad_cov, 0),
         sppcode = 
           case_when(ScientificName == "Lonicera - Exotic" ~ "LONEXO",
                     is.na(word(ScientificName, 2)) ~ 
                       toupper(paste0(substr(word(ScientificName, 1), 1, 3), "SPP")),
                     TRUE ~ toupper(paste0(substr(word(ScientificName, 1), 1, 3), 
                       substr(word(ScientificName, 2), 1, 3))))) %>% 
  arrange(sppcode, Plot_Name) %>% 
  select(Plot_Name, X, Y, sppcode, quad_cov) %>% 
  pivot_wider(names_from = sppcode, values_from = quad_cov, values_fill = 0) #%>% 
  #select(-NONPRE)

if("NONPRE" %in% names(invspp)){invspp <- invspp |> select(-NONPRE)}

invspp$totcov = rowSums(invspp[,4:ncol(invspp)])

write_to_shp(invspp, shp_name = 
               paste0(new_path, "shapefiles/", park, "_inv_cover_by_species.shp"))

#---- Map 9 Tree Pests/Diseases ----
# First compile plot-level disturbances that may include priority pests/pathogens
disturb <- do.call(joinStandDisturbance, args = args_4yr) %>% 
  filter(DisturbanceLabel != "None") %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", DisturbanceNote) ~ "BLD",
                          grepl("Emerald|emerald|EAB", DisturbanceNote) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", DisturbanceNote) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                DisturbanceNote) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", DisturbanceNote) ~ "BBD",
                          grepl("GM|spongy|gypsy", DisturbanceNote) ~ "GM",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()
#mutate(detect = 'plot_dist')

# Next compile pest/pathogens from Tree Conditions
treecond_4yr <- do.call(joinTreeConditions, args = c(args_4yr, status = 'live'))

pests <- c("ALB", "BBD", "BLD", "BC", "BWA", "DOG", "EAB", "EHS", "GM", "HWA", "RPS", 
           "SB", "SLF", "SOD", "SPB", "SW")

treepests <- treecond_4yr %>% select(Plot_Name, all_of(pests)) %>% 
  #group_by(Plot_Name) %>% summarize(across(all_of(pests), ~ifelse(sum(.x) > 0, 1, 0))) %>% 
  group_by(Plot_Name) %>% summarize_at(vars(all_of(pests)), ~ifelse(sum(.x) > 0, 1, 0)) %>% 
  pivot_longer(-Plot_Name, names_to = "pest", values_to = 'tree_cond') %>% 
  filter(tree_cond > 0) %>% 
  arrange(Plot_Name) %>% unique() %>% select(Plot_Name, pest)


# Compile notes from visit that could contain mentions of pests

vnotes <- do.call(joinVisitNotes, args = args_4yr) %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", Notes) ~ "BLD",
                          grepl("Emerald|emerald|EAB", Notes) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", Notes) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                Notes) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", Notes) ~ "BBD",
                          grepl("GM|spongy|gypsy", Notes) ~ "GM",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()

# Combine detections to 1 shapefile
pest_detects <- rbind(treepests, disturb, vnotes) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, SampleYear, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  select(Plot_Name, SampleYear, X, Y, everything()) %>% unique() %>%  
  mutate(pest = replace_na(pest, "None"),
         detect = ifelse(pest == "None", 0, 1))

pests_wide <- pest_detects %>% 
  pivot_wider(names_from = pest, values_from = detect, values_fill = 0) %>% 
  select(-None)

# if(park == "MABI"){
# pests_wide$EAB[pests_wide$Plot_Name == "MABI-013" & pests_wide$SampleYear == 2022] <- 0
# # Tree note said "No EAB", but was picked up in query for positive EAB detections
# }
if(ncol(pests_wide) > 4){
pests_wide$none <- rowSums(pests_wide[,5:ncol(pests_wide)])
} else {pests_wide$none <- 0}
write_to_shp(pests_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_pest_detections_", cycle_latest, ".shp"))

#---- Map 10 Canopy Cover ----
cancov <- do.call(joinStandData, args = args_all) %>% 
  select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate, CrownClos = Pct_Crown_Closure)

cancov_wide <- cancov %>% pivot_wider(names_from = cycle, values_from = CrownClos)

write_to_shp(cancov_wide, shp_name = paste0(new_path, "shapefiles/", park, "_canopy_cover.shp"))

#---- Table 2 Average Invasive cover by plot and cycle ----
inv_plots <- do.call(sumSpeciesList, args = c(args_all, speciesType = "invasive")) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% 
  group_by(Plot_Name, PlotCode, cycle, PanelCode) %>% 
  summarize(inv_cov = sum(quad_avg_cov, na.rm = T),
            numspp = sum(present), .groups = 'drop')

inv_plots_wide <- inv_plots %>% pivot_wider(names_from = cycle, 
                                            values_from = c(inv_cov, numspp)) %>% 
  select(-Plot_Name) %>% mutate(PanelCode = ifelse(PanelCode %in% c(1:2), 1, 2))

write.csv(inv_plots_wide, paste0(new_path, "tables/", "Table_2_", park, 
                                 "_invasives_by_plot_cycle.csv"), row.names = FALSE)

#---- Table 3 Invasive species by number of plots cycle
inv_spp1 <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% arrange(cycle) %>% 
  group_by(ScientificName, cycle) %>% summarize(num_plots = sum(present), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_")

centaurea <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Centaurea", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(centaurea[,-1])

lonicera <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Lonicera", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(lonicera[,-1])

euonymus <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Euonymus", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(euonymus[,-1])

ligustrum <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Ligustrum", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(ligustrum[,-1])

vincetoxicum <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Vincetoxicum", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(vincetoxicum[,-1])


inv_spp <- left_join(inv_spp1, prepTaxa() %>% select(ScientificName, CommonName),
                     by = "ScientificName") %>% select(ScientificName, CommonName, everything())

write.csv(inv_spp, paste0(new_path, "tables/", "Table_3_", park,
                          "_num_invspp_by_cycle.csv"), row.names = FALSE)

#---- Early Detections -----
taxa <- prepTaxa()
spp_all <- do.call(sumSpeciesList, args = c(args_4yr))

# Need to import ParkTaxonProtectedStatus table from local database until it's added to the taxon view
connect <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=MIDN_Forest;trusted_connection=TRUE;ReadOnly=True"

con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1)
xref_taxon <- RODBC::sqlQuery(con, paste0("SELECT * FROM [MIDN_Forest].[xrefCOMN].[ParkTaxonProtectedStatus]"))
tlu_park <- RODBC::sqlQuery(con, paste0("SELECT * FROM [MIDN_Forest].[tluCOMN].[Park]"))
RODBC::odbcClose(con)

tlu_park2 <- tlu_park %>% select(ID, Unit) %>% 
  unique() %>% 
  rename(ParkID = ID) %>% 
  filter(Unit %in% park)

ised_taxon1 <- xref_taxon %>% select(ParkID, TaxonID, IsEarlyDetection) %>% 
  filter(IsEarlyDetection == 1) %>% 
  unique()

ised_taxon2 <- inner_join(ised_taxon1, tlu_park2, by = "ParkID") %>% select(-ParkID) %>% unique()

ised_taxon <- left_join(ised_taxon2, taxa %>% select(TaxonID, TSN, ScientificName), 
                        by = "TaxonID")

ised_join <- left_join(spp_all, ised_taxon, by = c("TSN", "ScientificName", "ParkUnit" = "Unit")) %>% 
  filter(IsEarlyDetection == 1) %>% 
  select(-SampleYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
         -quad_pct_freq, -IsEarlyDetection) %>% 
  arrange(Plot_Name, ScientificName) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% filter(!is.na(ScientificName)) %>% 
  select(Plot_Name, X, Y, ScientificName, quad_avg_cov)

write.csv(ised_join, paste0(new_path, "tables/", park, "_early_detection_plant_species.csv"),
          row.names = FALSE)

#---- ED Pests ----
priority_pests <- c("ALB", "BLD", "EAB", "EHS", "HWA", "RPS", "SLF", "SOD", "SPB", "SW")

pest_eds <- pests_wide %>% select(Plot_Name, X, Y, any_of(priority_pests)) %>% 
  mutate(pres = ifelse(ncol(.) > 3, rowSums(.[4:ncol(.)]), 0)) %>% filter(pres > 0)

if(nrow(pest_eds) > 0){
write.csv(pest_eds, paste0(new_path, 'tables/', park, "_pest_detections.csv"), row.names = F)
}

#---- CWD by cycle
cwd1 <- do.call(joinCWDData, args = args_vs) %>% select(Plot_Name, cycle, CWD_Vol)

cwd <- cwd1 %>% group_by(Plot_Name, cycle) %>% 
  summarize(cwd_vol = sum(CWD_Vol, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name", "cycle"))
  
cwd$cwd_vol[is.na(cwd$cwd_vol)] <- 0

cwd_wide <- cwd %>% pivot_wider(names_from = cycle, 
                                values_from = cwd_vol, 
                                names_prefix = "cycle_",
                                values_fill = 0) 

apply(cwd_wide[,4:ncol(cwd_wide)], 2, mean)

max_cwd <- max(cwd_wide[,c(4:ncol(cwd_wide))])

