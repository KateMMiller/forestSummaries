#---- Map 9 Tree Pests/Diseases ----
# First compile plot-level disturbances that may include priority pests/pathogens
disturb <- joinStandDisturbance(park = 'all', from = 2018, to = 2023) %>%  filter(!Plot_Name %in% "COLO-380") %>%
  filter(DisturbanceLabel != "None") %>%
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", DisturbanceNote) ~ "BLD",
                          grepl("Emerald|emerald|EAB", DisturbanceNote) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", DisturbanceNote) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                DisturbanceNote) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", DisturbanceNote) ~ "BBD",
                          grepl("GM|spongy|gypsy", DisturbanceNote) ~ "GM",
                          grepl("SLF|spotted lanternfly", DisturbanceNote) ~ "SLF",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, SampleYear, pest) %>% unique()
#mutate(detect = 'plot_dist')

# Next compile pest/pathogens from Tree Conditions
treecond_4yr <- joinTreeConditions(park = 'all', from = 2018, to = 2023, status = 'live') |> 
  filter(!Plot_Name %in% "COLO-380") 

pests <- c("ALB", "BBD", "BLD", "BC", "BWA", "DOG", "EAB", "EHS", "GM", "HWA", "RPS", 
           "SB", "SLF", "SOD", "SPB", "SW")

treepests <- treecond_4yr %>% select(Plot_Name, SampleYear, all_of(pests)) %>% 
  #group_by(Plot_Name) %>% summarize(across(all_of(pests), ~ifelse(sum(.x) > 0, 1, 0))) %>% 
  group_by(Plot_Name, SampleYear) %>% 
  summarize_at(vars(all_of(pests)), ~ifelse(sum(.x) > 0, 1, 0)) %>% 
  pivot_longer(-c(Plot_Name, SampleYear), names_to = "pest", values_to = 'tree_cond') %>% 
  filter(tree_cond > 0) %>% 
  arrange(Plot_Name) %>% unique() %>% select(Plot_Name, SampleYear, pest)

# Compile notes from visit that could contain mentions of pests
vnotes <- joinVisitNotes(park = 'all', from = 2018, to = 2023) %>%
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", Notes) ~ "BLD",
                          grepl("Emerald|emerald|EAB", Notes) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", Notes) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly",
                                Notes) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", Notes) ~ "BBD",
                          grepl("GM|spongy|gypsy", Notes) ~ "GM",
                          grepl("SLF|spotted lanternfly", Notes) ~ "SLF",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>%
  select(Plot_Name, pest) %>% unique()

# Combine detections to 1 shapefile
plotevs <- joinLocEvent(park = "all", from = 2018, to = 2023) |> 
  select(Plot_Name, SampleYear, X = xCoordinate, Y = yCoordinate) 

pest_detects <- rbind(treepests, disturb) %>% #, vnotes) %>% 
  left_join(plotevs,
            ., by = c("Plot_Name", "SampleYear")) %>% 
  select(Plot_Name, SampleYear, X, Y, everything()) %>% unique() %>%  
  mutate(pest = replace_na(pest, "None"),
         detect = ifelse(pest == "None", 0, 1))

pests_wide <- pest_detects %>% 
  pivot_wider(names_from = pest, values_from = detect, values_fill = 0) %>% 
  select(-None) |> arrange(Plot_Name, SampleYear)

table(pests_wide$SampleYear)
pests_4yr <- pests_wide |> group_by(Plot_Name) |> slice(n())
head(pests_4yr)

# if(park == "MABI"){
# pests_wide$EAB[pests_wide$Plot_Name == "MABI-013" & pests_wide$SampleYear == 2022] <- 0
# # Tree note said "No EAB", but was picked up in query for positive EAB detections
# }

#---- ED Pests ----
#priority_pests <- c("ALB", "BLD", "EAB", "EHS", "HWA", "RPS", "SLF", "SOD", "SPB", "SW")

pest_eds <- pests_wide %>% select(Plot_Name, SampleYear, X, Y, any_of(pests)) 
pest_eds$num_pres <- rowSums(pest_eds[5:ncol(pest_eds)])
pest_eds <- pest_eds |> filter(num_pres >= 1)
write.csv(pest_eds, './output/pest_early_detections.csv', row.names = F)

