#---------------------------------------
# Code to extract detections of MABI RTE species list in NETN forest plots
#  List downloaded from NatureServe Explorer in 2019 and from VT NHP on 20220825
#---------------------------------------

library(forestNETN)
library(tidyverse)

importData()

mabi_spp <- sumSpeciesList(park = "MABI", from = 2022, to = 2023) %>% 
  select(Plot_Name, SampleYear, ScientificName, tree_stems:addspp_present)

rte_list <- read.csv("MABI_RTE_list.csv") # From NatureServe
colnames(rte_list) <- c("ScientificName", "NS_GlobalRank", "VT_Rank")

rte_list2 <- read.csv("VT_NHP_RTE_list_20230711.csv")[,1:4]
colnames(rte_list2) <- c("ScientificName", "VT_Rank", "Federal_Rank", "NatServe")

names(rte_list1)

mabi_rte1 <- inner_join(rte_list %>% select(ScientificName), mabi_spp, by = "ScientificName")
mabi_rte2 <- inner_join(rte_list2 %>% select(ScientificName), mabi_spp, by = "ScientificName")

mabi_rte <- rbind(mabi_rte1, mabi_rte2)

if(nrow(mabi_rte) > 0){write.csv(mabi_rte, 
                                 paste0(new_path, "tables/", park, "RTE_detections.csv"), 
                                 row.names = FALSE)}

# No detections in 2022 and 2023.