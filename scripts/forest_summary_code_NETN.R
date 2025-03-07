#-------------------------------------
# Forest Data Summaries: Regeneration
#-------------------------------------

#---- Functions ----
# Write dataframe to shapefile using common settings
write_to_shp <- function(data, x = "X", y = "Y", shp_name){
  st_write(st_as_sf(data, coords = c(x, y), crs = park_crs),
           shp_name, delete_layer = TRUE)#FALSE)
}

#---- Plot event lists ----
plotevs <- do.call(joinLocEvent, args_all)
plotevs_vs <- do.call(joinLocEvent, args_vs)
plotevs_4yr1 <- plotevs %>% filter(between(SampleYear, from_4yr, to))

# Take the highest sample year when multiple panels were sampled within a year
# and only one of those panels is the most recent visit to that plot. 
plotevs_4yr <- plotevs_4yr1 |> group_by(ParkUnit, PanelCode, Plot_Name, IsQAQC) |> 
  slice_max(SampleYear) |> ungroup()

# vector of the EventIDs that represent the most recent visit to each plot
evs_4yr <- plotevs_4yr$EventID

#---- Table 1. Regen densities by plot and year ----
reg <- do.call(joinRegenData, 
               args = c(args_all, speciesType = 'native', 
                        canopyForm = 'canopy', units = 'sq.m'))


reg_cycle_table <- reg %>% group_by(Plot_Name, cycle) %>% 
                           summarize(seed_den = round(sum(seed_den, na.rm = TRUE), 2),
                                     sap_den = round(sum(sap_den, na.rm = TRUE), 2),
                                     stock = round(sum(stock, na.rm = TRUE), 2), 
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
reg_cycle3 <- reg %>% group_by(Plot_Name, PlotCode, cycle) %>% 
  summarize(regen_den = sum(regen_den, na.rm = TRUE), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, PlotCode, xCoordinate, yCoordinate, cycle), ., 
            by = c("Plot_Name", "PlotCode", "cycle")) %>% 
  arrange(Plot_Name, PlotCode, cycle) %>% 
  pivot_wider(names_from = cycle, values_from = regen_den, 
              names_prefix = "cycle_", values_fill = NA) %>% 
  rename(X = xCoordinate, Y = yCoordinate) #abbr for shapefile

reg_cycle2 <- reg_cycle3 %>% mutate(MAX = max(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

reg_cycle <- reg_cycle2 %>% mutate(MIN = min(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

#max(reg_cycle[,4:ncol(reg_cycle)])

reg_no <- reg_cycle|> rowwise() |> mutate(tot_reg = sum(c_across((starts_with("cycle"))), na.rm = T)) |> 
  filter(tot_reg == 0)

no_reg <- reg_no$Plot_Name

reg_cycle <- reg_cycle|> filter(!(Plot_Name %in% no_reg)) #check total # of plots in all dfs is right

reg_cycle_incom <- reg_cycle %>% filter_at(vars(last_col()), all_vars(is.na(.))) %>% select(-tail(names(.), 1))
reg_cycle_com <- reg_cycle %>% filter_at(vars(last_col()), all_vars(!is.na(.)))

if(nrow(reg_no) >0){
  write_to_shp(reg_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_regen_by_cycle_no_reg", ".shp" ))
}

if(nrow(reg_cycle_incom) >0){
  write_to_shp(reg_cycle_incom, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_regen_by_cycle_incomplete", ".shp" ))
}

write_to_shp(reg_cycle_com, 
             shp_name = paste0(new_path, "shapefiles/", park, "_regen_by_cycle", ".shp" ))


#---- Map 2 regen by size class ----
reg_sz_cols <- c("seed_15_30cm", "seed_30_100cm", "seed_100_150cm", "seed_p150cm", "sap_den") 

reg_size <- reg  |> filter(EventID %in% evs_4yr) |> 
  group_by(Plot_Name, PlotCode, SampleYear, EventID) |> 
  summarize_at(vars(all_of(reg_sz_cols)), sum, na.rm = T) |> ungroup()

reg_size_4yr <- reg_size %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, PlotCode, xCoordinate, yCoordinate),
            ., by = c('Plot_Name', 'PlotCode')) |> 
  select(-EventID)

reg_size_4yr$seed_15_30cm[is.na(reg_size_4yr$seed_15_30cm)] <- 0
reg_size_4yr$seed_30_100cm[is.na(reg_size_4yr$seed_30_100cm)] <- 0
reg_size_4yr$seed_100_150cm[is.na(reg_size_4yr$seed_100_150cm)] <- 0
reg_size_4yr$seed_p150cm[is.na(reg_size_4yr$seed_p150cm)] <- 0
reg_size_4yr$sap_den[is.na(reg_size_4yr$sap_den)] <- 0

head(reg_size_4yr)

colnames(reg_size_4yr) <- c("Plot_Name", "PlotCode", "X", "Y", "SampleYear", 
                             "s15_30", "s30_100", "s100_150", "s150p", "sap") #abbr. for shapefile

reg_size_4yr$total <- rowSums(reg_size_4yr[,6:ncol(reg_size_4yr)])

size_no <- reg_size_4yr |> filter(total == 0)

no_size <- size_no$Plot_Name

reg_size_4yr <- reg_size_4yr|> filter(!(Plot_Name %in% no_size)) #check total # of plots in all dfs is right

if(nrow(size_no) >0){
  write_to_shp(size_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_regen_by_size_class_cycle", cycle_latest, "_no_reg", ".shp" ))
}


write_to_shp(reg_size_4yr, 
             shp_name = paste0(new_path, "shapefiles/", park, 
                               "_regen_by_size_class_cycle_", cycle_latest, ".shp"))

#---- Figure 3A Regen trends by size class ----
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
reg_smooth <- map_dfr(reg_sz_cols, 
                      ~case_boot_loess(reg_size_cy, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                      mutate(size_class = .x)
)

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
# netn1+2: SARA, MABI, SAGA, MIMA
netn1_labs = c("1" = "Cycle 1: 2006 & 2008",
               "2" = "Cycle 2: 2010 & 2012", 
               "3" = "Cycle 3: 2014 & 2016", 
               "4" = "Cycle 4: 2018 & 2022",
               "5" = "Cycle 5: 2023")
# netn2: MORR, ROVA, WEFA
netn2_labs = c("1" = "Cycle 1: 2007 \u2013 2009",
               "2" = "Cycle 2: 2011 \u2013 2013", 
               "3" = "Cycle 3: 2015 \u2013 2017", 
               "4" = "Cycle 4: 2019 \u2013 2022",
               "5" = "Cycle 5: 2024") 

ACAD_labs = c("1" = "Cycle 1: 2006 \u2013 2009",
               "2" = "Cycle 2: 2010 \u2013 2013", 
               "3" = "Cycle 3: 2014 \u2013 2017", 
               "4" = "Cycle 4: 2018 \u2013 2021",
               "5" = "Cycle 5: 2022 \u2013 2024")

cycle_labs <- switch(park,
                     "SARA" = netn1_labs, 
                     "MABI" = netn1_labs,
                     "SAGA" = netn1_labs,
                     "MIMA" = netn1_labs,
                     "MORR" = netn2_labs,
                     "ROVA" = netn2_labs,
                     "WEFA" = netn2_labs,
                     "ACAD" = ACAD_labs)

reg_labels <- c("15 \u2013 30 cm", "30 \u2013 100 cm", "100 \u2013 150 cm",
                ">150 cm & < 1 cm DBH", "Saplings: 1 \u2013 9.9 cm DBH")

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = size_class, y = estimate, color = size_class,#linetype = sign, 
                         group = size_class))+ theme_FHM()+
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
  
ggsave(paste0(new_path, "figures/", "Figure_3A_", park, "_regen_by_size_class_by_cycle.svg"),
       height = 5, width = 7.5, units = 'in')

#---- Figure 3B Diam. dist. trends by size class ----
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

tree_dbh_sm$estimate[tree_dbh_sm$estimate < 0] <- 0

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
  ggplot(tree_dbh_sm, aes(x = dbh_class, y = estimate))+ theme_FHM()+
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

ggsave(paste0(new_path, "figures/", "Figure_3B_", park, "_tree_dbh_dist_by_cycle.svg"),
       height = 4.6, width = 7.8, units = 'in')

#---- Map 3 Regen by composition ----
reg_all <- do.call(joinRegenData, args = c(args_4yr, units = 'sq.m'))|> 
                  filter(!ScientificName %in% c('None present', "Not Sampled")) # just selected park

### Check dominant NETN species to pull out of standard groups ###
#Run for each park every year and update list of exceptions below if necessary
# !!! Must update tree_regen_stem_changes_by_species_loess.R as well!!!
reg_park <- do.call(joinRegenData, args = list(park, from_4yr, to = to, units = 'sq.m'))|>
                      filter(!ScientificName %in% c('None present', "Not Sampled")) # just selected park

dom_regspp <- reg_park %>% group_by(Plot_Name, ScientificName) %>% summarize(reg = sum(regen_den, na.rm = T)) %>%
                           group_by(ScientificName) %>% summarize(reg = sum(reg, na.rm = T)) %>% arrange(desc(reg))
view(dom_regspp)
###

head(trspp_grps) # loaded in source_script_NETN.R. Use as default grouping.

reg_grps <- left_join(reg_all, trspp_grps, by = c("ScientificName" = "Species")) |> select(-New)

if(nrow(reg_grps[which(is.na(reg_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in reg_spp_grps$spp_grp, meaning at least one species is missing a group.")} #check if any spp. is missing a group
head(reg_grps)

#Park specific exceptions to default groupings
if(park == "MORR"){
  reg_grps <- reg_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Ilex opaca" ~ "SUBCAN",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Ilex opaca" ~ "Subcanopy",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
  
} 
  if(park == "ROVA"){
    reg_grps <- reg_grps %>% 
      mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                                 ScientificName == "Acer platanoides" ~ "ACEPLA",
                                 ScientificName == "Betula lenta" ~ "BETSPP",
                                 ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                                 ScientificName == "Robinia pseudoacacia" ~ "OTHEXO",
                                 TRUE ~ sppcode)) %>% 
      mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                                 ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                                 ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                                 ScientificName == "Nyssa sylvatica" ~ "Other Native",
                                 ScientificName == "Robinia pseudoacacia" ~ "Other Exotic",
                                 TRUE ~ spp_grp))
  } 
if(park == "WEFA"){
  reg_grps <- reg_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
}
if(park == "SARA"){
  reg_grps <- reg_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Populus" ~ "POPSPP",
                               ScientificName == "Populus grandidentata" ~ "POPSPP",
                               ScientificName == "Populus tremuloides" ~ "POPSPP",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Populus" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus grandidentata" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus tremuloides" ~ "Populus spp. (aspen)",
                               TRUE ~ spp_grp))
}
if(park == "MABI"){
  reg_grps <- reg_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Picea abies" ~ "EXOPLA",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Picea abies" ~ "Exotic plantation spp.",
                               TRUE ~ spp_grp))
} 
if(park == "SAGA"){
  reg_grps <- reg_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               TRUE ~ spp_grp))
} 
if(park == "MIMA"){
  reg_grps <- reg_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Cladrastis kentukea" ~ "OTHEXO",
                               ScientificName == "Juniperus virginiana" ~ "OTHNAT",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               ScientificName == "Robinia pseudoacacia" ~ "OTHEXO",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Cladrastis kentukea" ~ "Other Exotic",
                               ScientificName == "Juniperus virginiana" ~ "Other Native",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               ScientificName == "Robinia pseudoacacia" ~ "Other Exotic",
                               TRUE ~ spp_grp))
}
if(park == "ACAD"){
  reg_grps <- reg_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Abies balsamea" ~ "ABIBAL",
                               ScientificName == "Larix laricina" ~ "OTHCON",
                               ScientificName == "Picea" ~ "PICSPP",
                               ScientificName == "Picea glauca" ~ "PICSPP",
                               ScientificName == "Picea mariana" ~ "PICSPP",
                               ScientificName == "Picea rubens" ~ "PICSPP",
                               ScientificName == "Populus" ~ "POPSPP",
                               ScientificName == "Populus grandidentata" ~ "POPSPP",
                               ScientificName == "Populus tremuloides" ~ "POPSPP",
                               ScientificName == "Thuja occidentalis" ~ "OTHCON",
                               ScientificName == "Sorbus decora" ~ "SUBCAN",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Abies balsamea" ~ "Abies balsamea (balsam fir)",
                               ScientificName == "Larix laricina" ~ "Other conifer",
                               ScientificName == "Picea" ~ "Picea spp. (spruce)",
                               ScientificName == "Picea rubens" ~ "Picea spp. (spruce)",
                               ScientificName == "Picea mariana" ~ "Picea spp. (spruce)",
                               ScientificName == "Picea glauca" ~ "Picea spp. (spruce)",
                               ScientificName == "Populus" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus grandidentata" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus tremuloides" ~ "Populus spp. (aspen)",
                               ScientificName == "Thuja occidentalis" ~ "Other conifer",
                               ScientificName == "Sorbus decora" ~ "Subcanopy",
                               TRUE ~ spp_grp))
}

reg_wide <- reg_grps %>% group_by(Plot_Name, PlotCode, sppcode) %>% 
  summarize(regen_den = sum(regen_den, na.rm = TRUE), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, PlotCode, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = c("Plot_Name", "PlotCode")) %>% arrange(sppcode) %>% 
  pivot_wider(names_from = sppcode, values_from = regen_den, values_fill = 0) %>% 
  arrange(Plot_Name)

reg_wide <- if("NA" %in% names(reg_wide)){reg_wide %>% select(-"NA")}else{reg_wide} 

reg_wide$total <- rowSums(reg_wide[,5:ncol(reg_wide)])
reg_wide$logtot <- log(reg_wide$total + 1)

names(sort(desc(colSums(reg_wide[,c(5:(ncol(reg_wide)-2))]))))

regcomp_no <- reg_wide |> filter(total == 0)

no_regcomp <- regcomp_no$Plot_Name

reg_wide <- reg_wide|> filter(!(Plot_Name %in% no_regcomp)) #check total # of plots in all dfs is right

if(nrow(regcomp_no) >0){
  write_to_shp(regcomp_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_regen_by_spp_cycle", cycle_latest, "_no_reg", ".shp" ))
}

write_to_shp(reg_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_regen_by_spp_cycle", cycle_latest, ".shp"))

#Table of regen species and groups used in Map 3 (Table 5)
reg_spp <- reg_grps %>% select(ScientificName, spp_grp, sppcode) %>% unique() %>%
                        mutate(spp_grp = case_when(spp_grp == "Other Native" ~ "Other native canopy spp.",
                                                   spp_grp == "Subcanopy" ~ "Other native subcanopy spp.",
                                                   spp_grp == "Other Exotic" ~ "Other exotic spp.",
                                                    TRUE ~ spp_grp)) %>% 
                        rename('Group: Map 3. Regeneration' = spp_grp) %>% select(-sppcode) %>% 
                        arrange(ScientificName)


#---- Map 4 Tree canopy composition ----
tree_4yr <- do.call(joinTreeData, args = c(args_4yr, status = 'live'))

### Check dominant NETN species to pull out of standard groups ###
#Run for each park every year and update list of exceptions below if necessary 
#!!! must update tree_regen_stem_changes_by_species_loess.R as well!!!
trees_park <- do.call(joinTreeData, args = list(park, from_4yr, to = to, status = 'live')) # just selected park

dom_trspp <- trees_park %>% group_by(Plot_Name, ScientificName) %>% summarize(ba = sum(BA_cm2, na.rm = T)) %>%
  group_by(ScientificName) %>% summarize(ba = sum(ba, na.rm = T)) %>% arrange(desc(ba))

view(dom_trspp)
###

tree_grps <- left_join(tree_4yr, trspp_grps, by = c("ScientificName" = "Species")) |> 
  filter(!ScientificName %in% c("None present", "Not Sampled"))

if(nrow(tree_grps[which(is.na(tree_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in tree_grps$spp_grp, meaning at least one species is missing a group.")} #check if any spp. is missing a group
head(tree_grps)
#Park specific exceptions to default groupings
if(park == "MORR"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Ilex opaca" ~ "SUBCAN",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Ilex opaca" ~ "Subcanopy",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
} 
if(park == "ROVA"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Fraxinus americana" ~ "SUBCAN",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               ScientificName == "Pinus resinosa" ~ "PINRES",
                               ScientificName == "Robinia pseudoacacia" ~ "OTHEXO",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Fraxinus americana" ~ "Subcanopy",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               ScientificName == "Pinus resinosa" ~ "Pinus resinosa (red pine)",
                               ScientificName == "Robinia pseudoacacia" ~ "Other Exotic",
                               TRUE ~ spp_grp))
} 
if(park == "WEFA"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Nyssa sylvatica" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Nyssa sylvatica" ~ "Other Native",
                               TRUE ~ spp_grp))
} 

if(park == "SARA"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               ScientificName == "Populus" ~ "POPSPP",
                               ScientificName == "Populus grandidentata" ~ "POPSPP",
                               ScientificName == "Populus tremuloides" ~ "POPSPP",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               ScientificName == "Populus" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus grandidentata" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus tremuloides" ~ "Populus spp. (aspen)",
                               TRUE ~ spp_grp))
}
if(park == "MABI"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3",
                               ScientificName == "Betula lenta" ~ "BETSPP",
                               ScientificName == "Larix decidua" ~ "EXOPLA",
                               ScientificName == "Picea abies" ~ "EXOPLA",
                               ScientificName == "Pinus sylvestris" ~ "EXOPLA",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                               ScientificName == "Betula lenta" ~ "Betula spp. (birch)",
                               ScientificName == "Larix decidua" ~ "Exotic plantation spp.",
                               ScientificName == "Picea abies" ~ "Exotic plantation spp.",
                               ScientificName == "Pinus sylvestris" ~ "Exotic plantation spp.",
                               TRUE ~ spp_grp))
} 
if(park == "SAGA"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               TRUE ~ spp_grp))
} 
if(park == "MIMA"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Acer platanoides" ~ "ACEPLA",
                               ScientificName == "Acer rubrum" ~ "ACESPP",
                               ScientificName == "Cladrastis kentukea" ~ "OTHEXO",
                               ScientificName == "Juniperus virginiana" ~ "OTHNAT",
                               ScientificName == "Pinus strobus" ~ "PINSTR",
                               ScientificName == "Robinia pseudoacacia" ~ "OTHEXO",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Acer rubrum" ~ "Acer spp. (maple)",
                               ScientificName == "Acer platanoides" ~ "Acer platanoides (Norway maple)",
                               ScientificName == "Cladrastis kentukea" ~ "Other Exotic",
                               ScientificName == "Juniperus virginiana" ~ "Other Native",
                               ScientificName == "Pinus strobus" ~ "Pinus strobus (white pine)",
                               ScientificName == "Robinia pseudoacacia" ~ "Other Exotic",
                               TRUE ~ spp_grp))
}
if(park == "ACAD"){
  tree_grps <- tree_grps %>% 
    mutate(sppcode = case_when(ScientificName == "Abies balsamea" ~ "ABIBAL",
                               ScientificName == "Larix laricina" ~ "OTHCON",
                               ScientificName == "Picea" ~ "PICSPP",
                               ScientificName == "Picea glauca" ~ "PICSPP",
                               ScientificName == "Picea mariana" ~ "PICSPP",
                               ScientificName == "Picea rubens" ~ "PICSPP",
                               ScientificName == "Picea rubens" ~ "PICSPP",
                               ScientificName == "Populus" ~ "POPSPP",
                               ScientificName == "Populus grandidentata" ~ "POPSPP",
                               ScientificName == "Populus tremuloides" ~ "POPSPP",
                               ScientificName == "Thuja occidentalis" ~ "OTHCON",
                               TRUE ~ sppcode)) %>% 
    mutate(spp_grp = case_when(ScientificName == "Abies balsamea" ~ "Abies balsamea (balsam fir)",
                               ScientificName == "Larix laricina" ~ "Other conifer",
                               ScientificName == "Picea" ~ "Picea spp. (spruce)",
                               ScientificName == "Picea rubens" ~ "Picea spp. (spruce)",
                               ScientificName == "Picea mariana" ~ "Picea spp. (spruce)",
                               ScientificName == "Picea glauca" ~ "Picea spp. (spruce)",
                               ScientificName == "Populus" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus grandidentata" ~ "Populus spp. (aspen)",
                               ScientificName == "Populus tremuloides" ~ "Populus spp. (aspen)",
                               ScientificName == "Thuja occidentalis" ~ "Other conifer",
                               TRUE ~ spp_grp))
}

tree_wide <- tree_grps %>% group_by(Plot_Name, PlotCode, sppcode) %>% 
  summarize(BAm2ha = sum(BA_cm2, na.rm = TRUE)/400, .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, PlotCode, X = xCoordinate, Y = yCoordinate) %>% unique(),
            ., by = c("Plot_Name", "PlotCode")) %>% arrange(sppcode) %>% 
  pivot_wider(names_from = sppcode, values_from = BAm2ha, values_fill = 0) 

tree_wide <- if("NA" %in% names(tree_wide)){tree_wide %>% select(-"NA")}else{tree_wide} 

tree_wide$total <- rowSums(tree_wide[,5:ncol(tree_wide)])
tree_wide$logtot <- log(tree_wide$total + 1)

names(tree_wide)

treecomp_no <- tree_wide |> filter(total == 0)

no_treecomp <- treecomp_no$Plot_Name

tree_wide <- tree_wide|> filter(!(Plot_Name %in% no_treecomp)) #check total # of plots in all dfs is right

if(nrow(treecomp_no) >0){
  write_to_shp(treecomp_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_tree_by_spp_cycle", cycle_latest, "_no_trees", ".shp" ))
}

write_to_shp(tree_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_tree_by_spp_cycle", cycle_latest, ".shp"))

#Table of tree species and groups used in Map 4 (Table 5)
tree_spp <- tree_grps %>% select(ScientificName, spp_grp, sppcode) %>% unique() %>%
  mutate(spp_grp = case_when(spp_grp == "Other Native" ~ "Other native canopy spp.",
                             spp_grp == "Subcanopy" ~ "Other native subcanopy spp.",
                             spp_grp == "Other Exotic" ~ "Other exotic spp.",
                             TRUE ~ spp_grp)) %>% 
  rename('Group: Map 4. Tree Canopy' = spp_grp) %>% select(-sppcode) %>% 
  arrange(ScientificName)

#---- Map 5 Regen stocking index ----
reg_4yr <- do.call(joinRegenData,
                   args = c(args_4yr, speciesType = 'native',
                            canopyForm = 'canopy', units = 'sq.m'))

reg_4yr_stock <- reg_4yr %>% group_by(Plot_Name, PlotCode) %>% 
                             summarize(stock = sum(stock, na.rm = T)) %>% 
                 left_join(plotevs %>% select(Plot_Name, PlotCode, X = xCoordinate, Y = yCoordinate) %>% unique(),
                           ., by = c("Plot_Name", "PlotCode"))

write_to_shp(reg_4yr_stock, shp_name = 
         paste0(new_path, "shapefiles/", park, "_stocking_index_cycle_",
                cycle_latest, ".shp"))

#---- Map 6 Deer Browse Index ----
stand_4yr <- do.call(joinStandData, args_4yr) 

dbi <- left_join(plotevs_4yr %>% select(Plot_Name, PlotCode, X = xCoordinate, Y = yCoordinate),
                 stand_4yr, by = c("Plot_Name", "PlotCode")) %>% 
       select(Plot_Name, PlotCode,X, Y, DBI = Deer_Browse_Index)

mean(dbi$DBI) #SAGA = 3.67 

write_to_shp(dbi, shp_name = 
         paste0(new_path, "shapefiles/", park, "_dbi_cycle_", cycle_latest, ".shp"))

#---- Map 7 Canopy Cover ----
cancov <- do.call(joinStandData, args = args_all)  %>% 
  select(Plot_Name, PlotCode, cycle, X = xCoordinate, Y = yCoordinate, CrownClos = Pct_Crown_Closure) |> 
  arrange(cycle, Plot_Name, PlotCode) %>% 
  filter(!Plot_Name %in% "COLO-380") #Canopy cover not collected in MIDN in 2007

cancov_wide3 <- cancov %>% pivot_wider(names_from = cycle, 
                                       values_from = CrownClos, 
                                       names_prefix = "cycle_",
                                       values_fill = NA)

cancov_wide2 <- cancov_wide3 %>% mutate(MAX = max(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

cancov_wide <- cancov_wide2 %>% mutate(MIN = min(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

cancov_no_C1 <- cancov_wide |> filter(is.na(cycle_1))

no_cancov_C1 <- cancov_no_C1$Plot_Name

cancov_wide <- cancov_wide |> filter(!(Plot_Name %in% no_cancov_C1)) #check total # of plots in all dfs is right

cancov_cycle_incom <- cancov_wide %>% filter_at(vars(last_col()), all_vars(is.na(.))) %>% select(-tail(names(.), 1))
cancov_cycle_com <- cancov_wide %>% filter_at(vars(last_col()), all_vars(!is.na(.)))

if(nrow(cancov_no_C1) >0){
  write_to_shp(cancov_no_C1, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_canopy_cover_by_no_cancov_C1", ".shp" ))
}

if(nrow(cancov_cycle_incom) >0){
  write_to_shp(cancov_cycle_incom, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_canopy_cover_incomplete", ".shp" ))
}

write_to_shp(cancov_cycle_com, shp_name = paste0(new_path, "shapefiles/", park, "_canopy_cover.shp"))


#---- Map 9 Invasive % Cover by Cycle ----
invcov <- do.call(joinQuadSpecies, args = c(args_all, speciesType = 'invasive')) %>% 
  select(Plot_Name, PlotCode, cycle, ScientificName, quad_avg_cov) %>% 
  group_by(Plot_Name, PlotCode, cycle) %>% summarize(quad_cov = sum(quad_avg_cov), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, PlotCode, X = xCoordinate, Y = yCoordinate, cycle),
            ., by = c("Plot_Name", "PlotCode", "cycle")) %>% 
  mutate(quad_cov = replace_na(quad_cov, 0))

invcov_wide3 <- invcov %>% pivot_wider(names_from = cycle, 
                                       values_from = quad_cov, 
                                       names_prefix = "cycle_",
                                       values_fill = NA)

invcov_wide2 <- invcov_wide3 %>% mutate(MAX = max(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

invcov_wide <- invcov_wide2 %>% mutate(MIN = min(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

invcov_no <- invcov_wide |> rowwise() |> mutate(tot_invcov = sum(c_across((starts_with("cycle"))), na.rm = T)) |> 
  filter(tot_invcov == 0)

no_invcov <- invcov_no$Plot_Name

invcov_wide <- invcov_wide |> filter(!(Plot_Name %in% no_invcov)) #check total # of plots in all dfs is right

invcov_cycle_incom <- invcov_wide %>% filter_at(vars(last_col()), all_vars(is.na(.))) %>% select(-tail(names(.), 1))
invcov_cycle_com <- invcov_wide %>% filter_at(vars(last_col()), all_vars(!is.na(.)))

if(nrow(invcov_no) >0){
  write_to_shp(invcov_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_inv_cover_by_cycle_no_invcov", ".shp" ))
}

if(nrow(invcov_cycle_incom) >0){
  write_to_shp(invcov_cycle_incom, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_inv_cover_by_cycle_incomplete_", ".shp" ))
}

write_to_shp(invcov_cycle_com, shp_name = 
               paste0(new_path, "shapefiles/", park, "_inv_cover_by_cycle.shp"))

#Average invasive vs native cover for last census (useful for report)
allcov <- do.call(joinQuadSpecies, args = c(args_4yr, speciesType = 'all')) %>% 
  select(Plot_Name, cycle, ScientificName, quad_avg_cov, Exotic)

invspp <- prepTaxa() %>% select(ScientificName, InvasiveNETN)

covsum <- left_join(allcov, invspp, by = "ScientificName") %>% 
  filter(!(InvasiveNETN == FALSE & Exotic == TRUE)) %>% # native vs. invasive; Exotic, but not invasive species are dropped
  group_by(Plot_Name, InvasiveNETN) %>% 
  summarize(avgcov = sum(quad_avg_cov, na.rm = TRUE), .groups = 'drop') %>% 
  group_by(InvasiveNETN) %>% summarize(avg_cov = sum(avgcov)/length(evs_4yr))


#---- Map 10 Invasive % Cover by Species ----
# Lump some species in the same genus
invspp_4yr <- joinQuadSpecies(from = from_4yr, to = to, speciesType = 'invasive') %>% 
  select(ScientificName) %>% unique() %>% arrange(ScientificName)

Ligustrum = c("Ligustrum", "Ligustrum obtusifolium", "Ligustrum vulgare")
Lonicera = c("Lonicera - Exotic", "Lonicera morrowii", "Lonicera X bella", "Lonicera", 
             "Lonicera maackii")
Vincetoxicum = c("Vincetoxicum", "Vincetoxicum hirundinaria", "Vincetoxicum nigrum",
                 "Vincetoxicum rossicum")
Elaeagnus = c("Elaeagnus", "Elaeagnus angustifolia", "Elaeagnus umbellata")
Euonymus = c("Euonymus", "Euonymus alatus", "Euonymus atropurpureus", "Euonymus fortunei")
Centaurea = c("Centaurea", "Centaurea jacea", "Centaurea stoebe")

sort(unique(invspp_4yr$ScientificName))

invspp1 <- do.call(joinQuadSpecies, 
                   #args = list(from = 2017, to = 2022, speciesType = 'invasive')) %>% 
                   args = c(args_4yr, speciesType = 'invasive')) %>% 
  select(Plot_Name, PlotCode, ScientificName, quad_avg_cov) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, PlotCode), ., by = c("Plot_Name", "PlotCode")) %>% 
  mutate(quad_avg_cov = replace_na(quad_avg_cov, 0),
         present = ifelse(quad_avg_cov > 0, 1, 0),
         ScientificName = case_when(
           ScientificName %in% Ligustrum ~ "Ligustrum",
           ScientificName %in% Lonicera ~ "Lonicera - Exotic",
           ScientificName %in% Vincetoxicum ~ "Vincetoxicum",
           ScientificName %in% Elaeagnus ~ "Elaeagnus",
           ScientificName %in% Euonymus ~"Euonymus",
           ScientificName %in% Centaurea ~"Centaurea",
           TRUE ~ ScientificName)) %>% 
  arrange(Plot_Name, PlotCode, ScientificName)

# Determine 10 most common invasives in a park by average cover
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
  #arrange(desc(num_plots)) %>% slice(1:10) %>% select(ScientificName) 
  arrange(desc(avg_cov)) %>% slice(1:10) %>% select(ScientificName) # going with top 10 by average cover for 2024

# Prep for shapefile
invspp <- invspp1 %>% 
  mutate(ScientificName = case_when(
    ScientificName %in% topspp$ScientificName ~ ScientificName, 
    is.na(ScientificName) ~ "None present", #None present is coming through as NA now
    TRUE ~ "Other invasive")) %>% 
  group_by(Plot_Name, PlotCode, ScientificName) %>% 
  summarize(quad_cov = sum(quad_avg_cov), .groups = 'drop') %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, PlotCode, X = xCoordinate, Y = yCoordinate),
            ., by = c("Plot_Name", "PlotCode")) %>% 
  #left_join(., prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName") %>% 
  select(Plot_Name, PlotCode, X, Y, ScientificName, quad_cov) %>% 
  mutate(quad_cov = replace_na(quad_cov, 0),
         sppcode = 
           case_when(ScientificName == "Lonicera - Exotic" ~ "LONEXO",
                     is.na(word(ScientificName, 2)) ~ 
                       toupper(paste0(substr(word(ScientificName, 1), 1, 3), "SPP")),
                     TRUE ~ toupper(paste0(substr(word(ScientificName, 1), 1, 3), 
                       substr(word(ScientificName, 2), 1, 3))))) %>% 
  arrange(sppcode, Plot_Name) %>% 
  select(Plot_Name, PlotCode, X, Y, sppcode, quad_cov) %>% 
  pivot_wider(names_from = sppcode, values_from = quad_cov, values_fill = 0) %>% 
  select(-any_of("NONPRE"))

invspp$totcov = rowSums(invspp[,5:ncol(invspp)])

invspp_no <- invspp |> filter(totcov == 0)

no_invs <- invspp_no$Plot_Name

invspp <- invspp|> filter(!(Plot_Name %in% no_invs)) #check total # of plots in all dfs is right

if(nrow(invspp_no) >0){
  write_to_shp(invspp_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_inv_cover_by_species_no_invasives", ".shp" ))
}

write_to_shp(invspp, shp_name = 
               paste0(new_path, "shapefiles/", park, "_inv_cover_by_species.shp"))

#---- Map 8 Tree Pests/Diseases ----
# First compile plot-level disturbances that may include priority pests/pathogens
disturb <- do.call(joinStandDisturbance, args = args_4yr) %>% 
  filter(DisturbanceSummary != "None") %>% 
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
  pivot_longer(-c(Plot_Name), names_to = "pest", values_to = 'tree_cond') %>% 
  filter(tree_cond > 0) %>% 
  arrange(Plot_Name) %>% unique() %>% select(Plot_Name, pest)


# Compile notes from visit that could contain mentions of pests
#Review notes for false positives first, remove from final table below
vnotesReview <- do.call(joinVisitNotes, args = args_4yr) %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", Notes) ~ Notes,
                          grepl("Emerald|emerald|EAB", Notes) ~ Notes,
                          grepl("Red pine scale|RPS|red pine scale", Notes) ~ Notes,
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                Notes) ~ Notes,
                          grepl("BBD|beech bark disease|Beech bark disease", Notes) ~ Notes,
                          grepl("GM|spongy|gypsy", Notes) ~ Notes,
                          grepl("jumping|crazy|snake|snakeworm|worm", Notes) ~ Notes,
                          TRUE ~ NA_character_)) %>% filter(!is.na(pest)) 

vnotes <- do.call(joinVisitNotes, args = args_4yr) %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", Notes) ~ "BLD",
                          grepl("Emerald|emerald|EAB", Notes) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", Notes) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                Notes) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", Notes) ~ "BBD",
                          grepl("GM|spongy|gypsy", Notes) ~ "GM",
                          grepl("Jumping|jumping|crazy|snake|snakeworm|worm|worms", Notes) ~ "CSW",
                          TRUE ~ NA_character_)) %>% 
  filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()

# Combine detections to 1 shapefile
pest_detects <- rbind(treepests, disturb, vnotes) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, SampleYear, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  select(Plot_Name, SampleYear, X, Y, everything()) %>% unique() %>%  
  mutate(pest = replace_na(pest, "None"),
         detect = ifelse(pest == "None", 0, 1))

pests_wide <- pest_detects %>% 
  pivot_wider(names_from = pest, values_from = detect, values_fill = 0) 

if(park == "MABI"){
pests_wide$EAB[pests_wide$Plot_Name == "MABI-013" & pests_wide$SampleYear == 2022] <- 0
# Tree note said "No EAB", but was picked up in query for positive EAB detections
}

if(park == "MABI"){
  pests_wide$EAB[pests_wide$Plot_Name == "MABI-005" & pests_wide$SampleYear == 2023] <- 0
  # Tree note said "No sign of EAB", but was picked up in query for positive EAB detections
}

if(park == "SAGA" & to == 2022){
  pests_wide$EAB[pests_wide$Plot_Name == "SAGA-017" & pests_wide$SampleYear == 2022] <- 0
  # Tree note said "No EAB", but was picked up in query for positive EAB detections
}


if(park %in% c("MABI", "SAGA")){
  worms <- do.call(joinStandData, args = args_vs) %>% select(Plot_Name, cycle, Earthworms) %>% 
    mutate(Earthworms = ifelse(Earthworms == 1, 1, 0)) %>% 
    group_by(Plot_Name) %>% 
    summarize(worms = ifelse(sum(Earthworms, na.rm = T) > 0, 1, 0))
  
  pests_wide <- left_join(pests_wide, worms, by = "Plot_Name")
  
  }


pests_wide <- left_join( plotevs_4yr |> select(Plot_Name, PlotCode), pests_wide, by = "Plot_Name") 

pests_wide$totpests = rowSums(pests_wide[,6:ncol(pests_wide)], na.rm = T)

pests_no <- pests_wide |> filter(totpests == 0)

no_pests <- pests_no$Plot_Name

pests_wide <- pests_wide|> filter(!(Plot_Name %in% no_pests)) |> select(-totpests, -None) #check total # of plots in all dfs is right

if(nrow(pests_no) >0){
  write_to_shp(pests_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_no_pests", ".shp" ))
}
if(ncol(pests_wide) >5){
  pest.1 <- pests_wide %>% filter(.[[6]] > 0)
  if(nrow(pest.1) >0){
    write_to_shp(pest.1, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[6]), ".shp" ))
  }}

if(ncol(pests_wide) >6){
  pest.2 <- pests_wide %>% filter(.[[7]] > 0)
  if(nrow(pest.2) >0){
    write_to_shp(pest.2, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[7]), ".shp" ))
  }}

if(ncol(pests_wide) >7){
  pest.3 <- pests_wide %>% filter(.[[8]] > 0)
  if(nrow(pest.3) >0){
    write_to_shp(pest.3, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[8]), ".shp" ))
  }}

if(ncol(pests_wide) >8){
  pest.4 <- pests_wide %>% filter(.[[9]] > 0)
  if(nrow(pest.4) >0){
    write_to_shp(pest.4, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[9]), ".shp" ))
  }}

if(ncol(pests_wide) >9){
  pest.5 <- pests_wide %>% filter(.[[10]] > 0)
  if(nrow(pest.5) >0){
    write_to_shp(pest.5, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[10]), ".shp" ))
  }}

if(ncol(pests_wide) >10){
  pest.6 <- pests_wide %>% filter(.[[11]] > 0)
  if(nrow(pest.6) >0){
    write_to_shp(pest.6, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[11]), ".shp" ))
  }}

if(ncol(pests_wide) >11){
  pest.7 <- pests_wide %>% filter(.[[12]] > 0)
  if(nrow(pest.7) >0){
    write_to_shp(pest.7, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[12]), ".shp" ))
  }}

if(ncol(pests_wide) >12){
  pest.8 <- pests_wide %>% filter(.[[13]] > 0)
  if(nrow(pest.8) >0){
    write_to_shp(pest.8, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[13]), ".shp" ))
  }}

if(ncol(pests_wide) >13){
  pest.9 <- pests_wide %>% filter(.[[14]] > 0)
  if(nrow(pest.9) >0){
    write_to_shp(pest.9, 
                 shp_name = paste0(new_path,  "shapefiles/", park, "_pest_detections", "_pest_", names(pests_wide[14]), ".shp" ))
  }}

if(nrow(pests_wide) >0){
  write_to_shp(pests_wide, shp_name = 
                 paste0(new_path, "shapefiles/", park, "_pest_detections", "_all", ".shp"))
}

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

# Table 3 Exotic species by number of plots cycle -------------------------
#for 2024 including all exotic species found in plots instead of only invasives
inv_spp1 <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% 
  group_by(ScientificName, cycle) %>% summarize(num_plots = sum(present), .groups = 'drop') %>% 
  arrange(cycle) %>% # moved arrange() b/c cycle_1 was coming out last instead of first
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

inv_spp2 <- left_join(inv_spp1, prepTaxa() %>% select(ScientificName, CommonName, InvasiveNETN),
                     by = "ScientificName") %>% select(ScientificName, CommonName, everything())

inv_spp <- inv_spp2 %>%  filter(ScientificName != 'None present') %>% 
                         mutate(InvasiveNETN = case_when(InvasiveNETN == 'TRUE' ~ 'Yes',
                                                         InvasiveNETN =='FALSE' ~ 'No')) %>% 
                         relocate(InvasiveNETN, .after = CommonName) |> arrange(ScientificName)

write.csv(inv_spp, paste0(new_path, "tables/", "Table_3_", park,
                          "_num_invspp_by_cycle.csv"), row.names = FALSE)

#---- Table 4: Early Detections -----
taxa <- prepTaxa()
spp_all <- do.call(sumSpeciesList, args = c(args_4yr))

# Need to import ParkTaxonProtectedStatus table from local database until it's added to the taxon view
connect <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=NETN_Forest;trusted_connection=TRUE;ReadOnly=True"

con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1)
xref_taxon <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[xrefCOMN].[ParkTaxonProtectedStatus]"))
tlu_park <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[tluCOMN].[Park]"))
RODBC::odbcClose(con)

tlu_park2 <- tlu_park %>% select(ID, Unit) %>% 
  unique() %>% 
  rename(ParkID = ID) %>% 
  filter(Unit %in% park)

ised_taxon1 <- xref_taxon %>% select(ParkID, TaxonID, IsEarlyDetection) %>% 
  filter(IsEarlyDetection == 1) %>% 
  unique()

if(park == "MIMA"){
ised_taxon1 <- rbind(ised_taxon1, 
                     data.frame(ParkID = c(5, 6), # MIMA 2 units 
                                TaxonID = c(517, 986), # AMPBRE, PHAARU 
                                IsEarlyDetection = c(1, 1)))
}

ised_taxon2 <- inner_join(ised_taxon1, tlu_park2, by = "ParkID") %>% select(-ParkID) %>% unique()

ised_taxon <- left_join(ised_taxon2, taxa %>% select(TaxonID, TSN, ScientificName), 
                        by = "TaxonID")

ised_join <- left_join(spp_all, ised_taxon, by = c("TSN", "ScientificName", "ParkUnit" = "Unit")) %>% 
  filter(IsEarlyDetection == 1) %>% 
  select(-cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
         -quad_pct_freq, -IsEarlyDetection) %>% 
  arrange(Plot_Name, ScientificName) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% filter(!is.na(ScientificName)) %>% 
  select(Plot_Name, SampleYear, X, Y, ScientificName, quad_avg_cov)

write.csv(ised_join, paste0(new_path, "tables/", park, "_early_detection_plant_species.csv"),
          row.names = FALSE)

#---- ED Pests ----
priority_pests <- c("ALB", "BLD", "EAB", "EHS", "HWA", "RPS", "SLF", "SOD", "SPB", "SW")

pest_eds <- pests_wide %>% select(Plot_Name, SampleYear, X, Y, any_of(priority_pests)) # pulling in pests even if they = 0, creates false positive in .shp

#Remove Priority Pest columns w/ 0 detections
if(ncol(pest_eds) >= 5){
  pest_eds1 <- pest_eds %>% select(5:ncol(pest_eds)) %>% select(where(~any(. != 0)))
  pest_eds <- cbind(pest_eds |> select(Plot_Name, SampleYear, X, Y,),
                    pest_eds1) 
} 
#Confirm final plot list matches pest_eds

if(ncol(pest_eds) >= 5){
  pest_eds$num_pres <- rowSums(pest_eds[5:ncol(pest_eds)])
  pest_eds <- pest_eds |> filter(num_pres >= 1)
  write.csv(pest_eds, paste0(new_path, 'tables/', park, "_pest_detections.csv"), row.names = F)
}

#---- ED all species ----
ised_spp <- left_join(ised_join, 
                      prepTaxa() |> select(ScientificName, CommonName, Tree, Shrub, Vine, Herbaceous, Graminoid),
                      by = "ScientificName") |> 
  mutate(type = case_when(Tree == 1 ~ 'tree', 
                          Shrub == 1 ~ 'shrub',
                          Vine == 1 ~ 'shrub',
                          Graminoid == 1 ~ 'graminoid',
                          Herbaceous == 1 ~ 'herbaceous',
                          TRUE ~ "UNK")) |> 
  select(-Tree, -Shrub, -Vine, -Herbaceous, -Graminoid)|> arrange(type, ScientificName)

pest_names <- read.csv("tree_conditions_table.csv")

ed_all <-
  if(ncol(pest_eds) >= 5){
    pest_pres <- names(pest_eds[names(pest_eds) %in% priority_pests])
    pest_eds_long <- pest_eds |> select(-num_pres) |> 
      pivot_longer(cols = all_of(pest_pres), 
                   names_to = "pest", values_to = "pres") #|> 
    #select(-pres) #including this line includes every pest found in the park for every plot with  1+ pest, whether or not it's present
    
    pest_eds2 <- left_join(pest_eds_long, pest_names, by = c("pest" = "Code")) |> 
      select(-pest) |> mutate(quad_avg_cov = NA_real_, type = 'pest')|> arrange(ScientificName)|>
      filter(pres == 1) |> select(-pres)
    
    ed_all <- rbind(ised_spp, pest_eds2)
  } else {ised_spp}

ed_all_final <- ed_all |> select(Plot_Name, SampleYear, X, Y, ScientificName, CommonName, type) |> 
  distinct() # was getting duplicates if pest was recorded as a condition and in a note

write.csv(ed_all_final, paste0(new_path, 'tables/', "Table_4_", park, "_early_detections.csv"), row.names = F)


# Table 5: Tree species included in Map 3+4 -------------------------------

grp_spp <- full_join(reg_spp,tree_spp, by = "ScientificName") %>% arrange(ScientificName)
write.csv(grp_spp, paste0(new_path, "tables/", "Table_5_", park, "_tree_species_in_Map3_4.csv"),
          row.names = FALSE)

#---- Invasive Detections for MABI/SAGA/ACAD ---- 
taxa <- prepTaxa()

spp_invD <- do.call(sumSpeciesList, args = c(args_4yr, speciesType = 'invasive')) |> 
  select(Plot_Name, SampleYear, quad_avg_cov, ScientificName) |> 
  filter(!ScientificName %in% "None present")

spp_invD2 <- left_join(spp_invD, plotevs_4yr |> select(Plot_Name, X = xCoordinate, Y = yCoordinate),
                      by = "Plot_Name")
#---- Rubus cover for MABI -----
if(park == "MABI"){
rubus <- do.call(joinQuadSpecies, args = args_vs) %>% filter(grepl("Rubus", ScientificName))

rubus2 <- rubus %>% group_by(Plot_Name, cycle) %>% 
  summarize(avg_cov = sum(quad_avg_cov, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name", "cycle")) 

rubus2$avg_cov[is.na(rubus2$avg_cov)] <- 0

rubus_wide <- rubus2 %>% pivot_wider(names_from = cycle, values_from = avg_cov, values_fill = 0)

write_to_shp(rubus_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_Rubus_cover_", ".shp"))

}


# Map 11: CWD by cycle ------------------------------------
cwd1 <- do.call(joinCWDData, args = args_vs) %>% select(Plot_Name, cycle, CWD_Vol) |> 
  filter(!Plot_Name %in% "COLO-380") 

cwd <- cwd1 %>% group_by(Plot_Name, cycle) %>% 
  summarize(cwd_vol = sum(CWD_Vol, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, PlotCode, cycle, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name", "cycle"))
  
cwd$cwd_vol[is.na(cwd$cwd_vol)] <- 0

cwd_wide3 <- cwd %>% pivot_wider(names_from = cycle, 
                                 values_from = cwd_vol, 
                                 names_prefix = "cycle_",
                                 values_fill = NA) # Better for mapping if unsampled plots are NA not 0

apply(cwd_wide3[,5:ncol(cwd_wide3)], 2, mean)

max_cwd <- max(cwd_wide3[,c(5:ncol(cwd_wide3))], na.rm = T)

cwd_wide2 <- cwd_wide3 %>% mutate(MAX = max(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

cwd_wide <- cwd_wide2 %>% mutate(MIN = min(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

cwd_no <- cwd_wide |> rowwise() |> mutate(tot_cwd = sum(c_across((starts_with("cycle"))), na.rm = T)) |> 
  filter(tot_cwd == 0)

no_cwd <- cwd_no$Plot_Name

cwd_wide <- cwd_wide |> filter(!(Plot_Name %in% no_cwd)) # No. of plots in no_ash + frax_wide should = tot plots for park

cwd_cycle_incom <- cwd_wide %>% filter_at(vars(last_col()), all_vars(is.na(.))) %>% select(-tail(names(.), 1))
cwd_cycle_com <- cwd_wide %>% filter_at(vars(last_col()), all_vars(!is.na(.)))

if(nrow(cwd_no) >0){
  write_to_shp(cwd_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_cwd_vol_by_cycle_no_cwd", ".shp" ))
}

if(nrow(cwd_cycle_incom) >0){
  write_to_shp(cwd_cycle_incom, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_CWD_vol_by_cycle_incomplete", ".shp" ))
}

write_to_shp(cwd_cycle_com, shp_name = 
               paste0(new_path, "shapefiles/", park, "_CWD_vol_by_cycle",  ".shp"))

# Map 12: CWD Rating ------------------------------------------------------
cwd_4yr1 <- joinCWDData(from = from_4yr, to = to) %>% select(Plot_Name, CWD_Vol) |> 
  filter(!Plot_Name %in% "COLO-380") 

cwd_4yr <- cwd_4yr1 %>% group_by(Plot_Name) %>% 
  summarize(cwd_vol = sum(CWD_Vol, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, PlotCode, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name"))

write_to_shp(cwd_4yr, shp_name = 
               paste0(new_path, "shapefiles/", park, "_CWD_vol_for_rating", ".shp"))


#----- Map 13: Number of Ash tree stems over time ------
Fraxinus_spp <- c('Fraxinus', 'Fraxinus americana', 'Fraxinus pennsylvanica', 
                  'Fraxinus nigra', 'Fraxinus profunda')

frax1 <- do.call(joinTreeData, c(args_vs, status = 'live')) |> filter(ScientificName %in% Fraxinus_spp)

frax2 <- frax1 %>% group_by(Plot_Name, PlotCode, cycle) %>% 
  summarize(num_stems = sum(num_stems),
            sppcode = "FRAXSPP",
            .groups = 'drop') 

frax_plotevs <- plotevs %>% select(Plot_Name, PlotCode, cycle, X = xCoordinate, Y = yCoordinate) 

frax <- left_join(frax_plotevs, frax2 |> select(-sppcode), by = c("Plot_Name", "PlotCode", "cycle")) %>%
  arrange(cycle, Plot_Name, PlotCode) 

frax$num_stems[is.na(frax$num_stems)] <- 0

frax_wide3 <- frax %>% pivot_wider(names_from = cycle, 
                                   values_from = num_stems, 
                                   names_prefix = "cycle_",
                                   values_fill = NA) # Better for mapping if unsampled plots are NA not 0

frax_wide2 <- frax_wide3 %>% mutate(MAX = max(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

frax_wide <- frax_wide2 %>% mutate(MIN = min(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")


frax_no <- frax_wide |> rowwise() |> mutate(tot_stems = sum(c_across((starts_with("cycle"))), na.rm = T)) |> 
  filter(tot_stems == 0)
no_ash <- frax_no$Plot_Name

frax_wide <- frax_wide |> filter(!(Plot_Name %in% no_ash)) # No. of plots in no_ash + frax_wide should = tot plots for park

frax_cycle_incom <- frax_wide %>%  filter_at(vars(last_col()), all_vars(is.na(.))) %>% select(-tail(names(.), 1))
frax_cycle_com <- frax_wide |>  filter_at(vars(last_col()), all_vars(!is.na(.)))

if(nrow(frax_no) >0){
  write_to_shp(frax_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_ash_trees_by_cycle_no_ash", ".shp" ))
}

if(nrow(frax_cycle_incom) >0){
  write_to_shp(frax_cycle_incom, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_ash_trees_by_cycle_incomplete", ".shp" ))
}


if(nrow(frax_cycle_com) >0){
  write_to_shp(frax_cycle_com, 
               shp_name = paste0(new_path, "shapefiles/", park, "_ash_trees_by_cycle", ".shp" ))
}

#----- Map 14?: Number of Beech tree stems over time ------
fagus1 <- do.call(joinTreeData, c(args_vs, status = 'live')) |> filter(ScientificName == 'Fagus grandifolia')

fagus2 <- fagus1 %>% group_by(Plot_Name, PlotCode, cycle) %>% 
  summarize(num_stems = sum(num_stems),
            sppcode = "FAGGRA",
            .groups = 'drop') 

fagus_plotevs <- plotevs %>% select(Plot_Name, PlotCode, cycle, X = xCoordinate, Y = yCoordinate) 

fagus <- left_join(fagus_plotevs, fagus2 |> select(-sppcode), by = c("Plot_Name", "PlotCode", "cycle")) %>%
  arrange(cycle, Plot_Name, PlotCode) 

fagus$num_stems[is.na(fagus$num_stems)] <- 0

fagus_wide3 <- fagus %>% pivot_wider(names_from = cycle, 
                                     values_from = num_stems, 
                                     names_prefix = "cycle_",
                                     values_fill = NA) # Better for mapping if unsampled plots are NA not 0

fagus_wide2 <- fagus_wide3 %>% mutate(MAX = max(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

fagus_wide <- fagus_wide2 %>% mutate(MIN = min(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")


fagus_no <- fagus_wide |> rowwise() |> mutate(tot_stems = sum(c_across((starts_with("cycle"))), na.rm = T)) |> 
  filter(tot_stems == 0)
no_beech <- fagus_no$Plot_Name

fagus_wide <- fagus_wide |> filter(!(Plot_Name %in% no_beech)) # No. of plots in no_beech + fagus_wide should = tot plots for park

fagus_cycle_incom <- fagus_wide %>%  filter_at(vars(last_col()), all_vars(is.na(.))) %>% select(-tail(names(.), 1))
fagus_cycle_com <- fagus_wide |>  filter_at(vars(last_col()), all_vars(!is.na(.)))

if(nrow(fagus_no) >0){
  write_to_shp(fagus_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_beech_trees_by_cycle_no_beech", ".shp" ))
}

if(nrow(fagus_cycle_incom) >0){
  write_to_shp(fagus_cycle_incom, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_beech_trees_by_cycle_incomplete", ".shp" ))
}


if(nrow(fagus_cycle_com) >0){
  write_to_shp(fagus_cycle_com, 
               shp_name = paste0(new_path, "shapefiles/", park, "_beech_trees_by_cycle", ".shp" ))
}

#----- Map 14?: Proportion of beech (number of stems) ------
fagus1 <- do.call(joinTreeData, c(args_vs, status = 'live')) |> filter(ScientificName == 'Fagus grandifolia')

fagus2 <- fagus1 %>% group_by(Plot_Name, PlotCode, cycle) %>% 
  summarize(num_stems = sum(num_stems),
            sppcode = "FAGGRA",
            .groups = 'drop') 

fagus_plotevs <- plotevs %>% select(Plot_Name, PlotCode, cycle, X = xCoordinate, Y = yCoordinate) 

fagus <- left_join(fagus_plotevs, fagus2 |> select(-sppcode), by = c("Plot_Name", "PlotCode", "cycle")) %>%
  arrange(cycle, Plot_Name, PlotCode) 

fagus$num_stems[is.na(fagus$num_stems)] <- 0

fagus_wide3 <- fagus %>% pivot_wider(names_from = cycle, 
                                     values_from = num_stems, 
                                     names_prefix = "cycle_",
                                     values_fill = NA) # Better for mapping if unsampled plots are NA not 0

fagus_wide2 <- fagus_wide3 %>% mutate(MAX = max(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")

fagus_wide <- fagus_wide2 %>% mutate(MIN = min(across(starts_with("cycle")), na.rm = T), .before = "cycle_1")


fagus_no <- fagus_wide |> rowwise() |> mutate(tot_stems = sum(c_across((starts_with("cycle"))), na.rm = T)) |> 
  filter(tot_stems == 0)
no_beech <- fagus_no$Plot_Name

fagus_wide <- fagus_wide |> filter(!(Plot_Name %in% no_beech)) # No. of plots in no_beech + fagus_wide should = tot plots for park

fagus_cycle_incom <- fagus_wide %>%  filter_at(vars(last_col()), all_vars(is.na(.))) %>% select(-tail(names(.), 1))
fagus_cycle_com <- fagus_wide |>  filter_at(vars(last_col()), all_vars(!is.na(.)))

if(nrow(fagus_no) >0){
  write_to_shp(fagus_no, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_beech_trees_by_cycle_no_beech", ".shp" ))
}

if(nrow(fagus_cycle_incom) >0){
  write_to_shp(fagus_cycle_incom, 
               shp_name = paste0(new_path,  "shapefiles/", park, "_beech_trees_by_cycle_incomplete", ".shp" ))
}


if(nrow(fagus_cycle_com) >0){
  write_to_shp(fagus_cycle_com, 
               shp_name = paste0(new_path, "shapefiles/", park, "_beech_trees_by_cycle", ".shp" ))
}
#---- MABI Only: plot harvest history -----
if(park == "MABI"){
cut_trees <- do.call(joinTreeData, args_all) |> filter(TreeStatusCode == "DC")
cut_df <- as.data.frame(table(cut_trees$Plot_Name, cut_trees$SampleYear))
cut_wide <- cut_df |> pivot_wider(names_from = Var2, values_from = Freq)
write.csv(cut_wide, paste0(new_path, "tables/", "Table_5_", park, "_harvesting_history.csv"), row.names = F)
}

if(park == "MABI"){
  ferns <- sumSpeciesList(park = "MABI", from = from_4yr, to = to) |> 
    filter(ScientificName %in% c("Dennstaedtia punctilobula", "Thelypteris noveboracensis")) |> 
    select(Plot_Name, ScientificName, quad_avg_cov)

  ferns_sum <- ferns |> group_by(Plot_Name) |> summarize(avg_cov = sum(quad_avg_cov)) |> 
    filter(avg_cov >= 15)
  ferns_sum
  
}

