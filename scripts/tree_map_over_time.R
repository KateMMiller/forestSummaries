library(tidyverse)
library(forestNETN)

start = 2006
end = 2026


prepTreeMap <- function(df){
  get_coords <- function(df){
    az <- ifelse(df$Azimuth - df$Orientation < 0, 360 - df$Orientation + df$Azimuth,
                 df$Azimuth - df$Orientation)
    df <- df |> mutate(x = Distance*sin(az*(pi/180)),
                        y = Distance*cos(az*(pi/180)))}
  df <- get_coords(df) # add x,y coordinates to data

  df$Orientation[df$Orientation == 360] <- 0
  return(df)} 

plotName = "ACAD-001"
plot_events <- joinLocEvent(park = "ACAD", from = start, to = end) |> filter(Plot_Name %in% plotName) |> 
  select(Plot_Name, EventID, Orientation, cycle) %>% unique()

tree_events <- joinTreeData(park = "ACAD", from = start, to = end) |> filter(Plot_Name %in% plotName) |>
  select(Plot_Name, ParkUnit, PlotID, EventID, SampleYear, IsQAQC, TagCode, TreeStatusCode,
         Distance, Azimuth, DBHcm, BA_cm2)

# Combine plot visit and tree data for plotting
tree_evs_rec1 <- left_join(plot_events, tree_events, by = c("Plot_Name", "EventID")) |>
  arrange(Plot_Name, -SampleYear) 

tree_evs_rec1$DBHcm[is.na(tree_evs_rec1$DBHcm)] <- 10 # for excluded status trees
tree_evs_rec1 <- prepTreeMap(tree_evs_rec1) 

live <- c("1", "AB", "AF", "AL", "AM", "AS")
dead <- c("2", "DB", "DF", "DL", "DM", "DS")
recruit <- c("RB", "RF", "RL", "RS")
exclude <- c('DC', '0', 'ES','EX','NA', 'XP') # Removed DC from excluded list

# Drop trees that were dead in first visit
tree_evs_rec2 <- tree_evs_rec1 |> #filter(!(SampleYear == start & StatusCode == 'dead')) |> 
  filter(!TreeStatusCode %in% exclude) |> 
  mutate(tag = sprintf("%02d", TagCode),
         tree_id = paste0(Plot_Name, "-", tag),
         status = ifelse(TreeStatusCode %in% c(live, recruit), "live",
                    ifelse(TreeStatusCode %in% dead, "dead",
                         "unk"))) |>
  select(Plot_Name, tree_id, status, TreeStatusCode, tag,
         SampleYear, cycle, DBHcm, Distance, Azimuth, Orientation) |>
  arrange(Plot_Name, tree_id, SampleYear)  |>
  mutate(dbh_fill = DBHcm) |>
  tidyr::fill(dbh_fill, .direction = 'down', .by = c(Plot_Name, tree_id)) |> # fills for dead trees missing DBH
  mutate(dbh_prev = dplyr::lag(dbh_fill, 1),
         year_prev = dplyr::lag(SampleYear, 1),
         year_length1 = SampleYear - year_prev,
         year_length = ifelse(is.na(year_length1), 4, year_length1), # including a filler
         dbh_growth = (dbh_fill - dbh_prev)/year_length,
         .by = c(Plot_Name, tree_id))
table(tree_evs_rec2$status)

# make trees
tree_evs_rec3 <- tree_evs_rec2 |>
  mutate(prev_stat = lag(status, 1),
         .by = c(Plot_Name, tree_id)) |>
  mutate(first_stat = first(status), .by = c(Plot_Name, tree_id)) |>
  #filter(!prev_stat %in% 'dead') |> 
  #filter(!first_stat %in% "dead") |> 
  select(Plot_Name:dbh_fill, first_stat, prev_stat)

tree_evs_rec <- prepTreeMap(tree_evs_rec3)

table(tree_evs_rec$status)
str(tree_evs_rec)
str(tree_evs_am)
# Set up table of all trees over the full monitoring period and set their location and size for dead trees
tree_evs_am <- data.frame(Plot_Name = rep("ACAD-001", 3),
                          tree_id = rep("ACAD-001-34", 3),
                          status = rep("live", 3),
                          TreeStatusCode = rep("AS", 3),
                          tag = rep("34", 3),
                          SampleYear = c(2006, 2010, 2014),
                          cycle = c(1, 2, 3),
                          DBHcm = c(26.6, 27.0, 27.4), # filling in as avg. growth between measured visits
                          Distance = rep(7, 3),
                          Azimuth = rep(310, 3),
                          Orientation = rep(232, 3),
                          dbh_fill = c(26.6, 27.0, 27.4),
                          prev_stat = rep("live", 3),
                          first_stat = rep("live", 3),
                          x = rep(6.847033, 3),
                          y = rep(1.455382, 3),
                          StatusCode = rep("live", 3)
                          )

tree_ev_fill <- rbind(tree_evs_rec |> filter(!TreeStatusCode %in% "AM"),
                      tree_evs_am) |> 
                arrange(tree_id, SampleYear) |> 
  mutate(stat_change = ifelse(prev_stat == "live" & StatusCode == "dead", "died", "no change")) |> 
  filter(x != 0 )

# Set up plot aesthetics
status_cols <- c("#11A300", "grey")
names(status_cols) <- as.character(c('live', 'dead'))

# Change the shape so dead are an X and stay the same size

plotTreeMap <- function(year){
  df <- tree_ev_fill |> filter(SampleYear == year)
  title <- paste0(unique(df$Plot_Name), ": ", unique(df$SampleYear))
  parkcode <- unique(df$ParkUnit)

p <- 
  ggplot(data = df |> arrange(-DBHcm), 
            aes(x = x, y = y, group = status, fill = status,
                size = DBHcm)) +
  geom_rect(aes(xmin = -8.3, xmax = 8.3, ymin = -8.3, ymax = 8.3),
            color = 'black', fill = "lightgrey", alpha = 0.3, size = 0.1) +
  geom_segment(aes(x = -8.3, xend = 8.3, y = 0, yend = 0), lwd = 1, color = 'DimGrey') +
  geom_segment(aes(x = 0, xend = 0, y = -8.3, yend = 8.3), lwd = 1, color = 'DimGrey') +
  geom_point(shape = 21) +
  geom_point(data = df |> filter(stat_change == "died"), shape = "X") +
  xlim(-11, 11) +
  ylim(-11, 11) +
  scale_fill_manual(values = status_cols, name = "Status") +
  #theme_classic() +
  #scale_shape_manual(values = c("dead" = 4, "live" = 21)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        strip.background = element_rect(fill = 'grey90'))+
  guides(shape = T, size = 'none') +
  scale_size_continuous(range = c(2, 10)) +
  coord_cartesian(xlim = c(-10.16, 10.16), clip = 'off')+
  labs(x = NULL, y = NULL, fill = 'Status')+
  geom_text(x = 9.0, y = 9.0, size = 5, label = 'UR')+
  geom_text(x = 9.0, y = -9.0, size = 5, label = 'BR')+
  geom_text(x = -9.0, y = -9.0, size = 5, label = 'BL')+
  geom_text(x = -9.0, y = 9.0, size = 5, label = 'UL')+
  geom_text(x = 0, y = 10.5, label = title, size = 6, color = 'red', fontface = 'bold') 
  p


# leg <- cowplot::get_legend(ggplot(data = df, aes(x = x, y = y, group = StatusCode, fill = StatusCode))+
#                              geom_point(aes(fill = StatusCode), shape = 21, size = 6)+
#                              labs(fill = 'Status')+
#                              scale_fill_manual(values = status_cols)+
#                              guides(shape = T))
# 
# p_final <- cowplot::plot_grid(p, leg, rel_widths = c(1.1, 0.2))
# p_final
}

plotTreeMap(2026)

path <- "C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Forest_Health/Presentations/2026_Trees_of_Acadia/"
plotTreeMap(2006)
ggsave(paste0(path, plotName, "_2006", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2010)
ggsave(paste0(path, plotName, "_2010", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2014)
ggsave(paste0(path, plotName, "_2014", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2018)
ggsave(paste0(path, plotName, "_2018", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2022)
ggsave(paste0(path, plotName, "_2022", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2026)
ggsave(paste0(path, plotName, "_2026", ".jpeg"), width  = 6, height = 6)


# plot 173
plotName = "ACAD-173"
plot_events <- joinLocEvent(park = "ACAD", from = start, to = end) |> filter(Plot_Name %in% plotName) |> 
  select(Plot_Name, EventID, Orientation, cycle) %>% unique()

tree_events <- joinTreeData(park = "ACAD", from = start, to = end) |> filter(Plot_Name %in% plotName) |>
  select(Plot_Name, ParkUnit, PlotID, EventID, SampleYear, IsQAQC, TagCode, TreeStatusCode,
         Distance, Azimuth, DBHcm, BA_cm2)

# Combine plot visit and tree data for plotting
tree_evs_rec1 <- left_join(plot_events, tree_events, by = c("Plot_Name", "EventID")) |>
  arrange(Plot_Name, -SampleYear) 

tree_evs_rec1$DBHcm[is.na(tree_evs_rec1$DBHcm)] <- 10 # for excluded status trees
tree_evs_rec1 <- prepTreeMap(tree_evs_rec1) 

live <- c("1", "AB", "AF", "AL", "AM", "AS")
dead <- c("2", "DB", "DF", "DL", "DM", "DS")
recruit <- c("RB", "RF", "RL", "RS")
exclude <- c('DC', '0', 'ES','EX','NA', 'XP') # Removed DC from excluded list

# Drop trees that were dead in first visit
tree_evs_rec2 <- tree_evs_rec1 |> #filter(!(SampleYear == start & StatusCode == 'dead')) |> 
  filter(!TreeStatusCode %in% exclude) |> 
  mutate(tag = sprintf("%02d", TagCode),
         tree_id = paste0(Plot_Name, "-", tag),
         status = ifelse(TreeStatusCode %in% c(live, recruit), "live",
                         ifelse(TreeStatusCode %in% dead, "dead",
                                "unk"))) |>
  select(Plot_Name, tree_id, status, TreeStatusCode, tag,
         SampleYear, cycle, DBHcm, Distance, Azimuth, Orientation) |>
  arrange(Plot_Name, tree_id, SampleYear)  |>
  mutate(dbh_fill = DBHcm) |>
  tidyr::fill(dbh_fill, .direction = 'down', .by = c(Plot_Name, tree_id)) |> # fills for dead trees missing DBH
  mutate(dbh_prev = dplyr::lag(dbh_fill, 1),
         year_prev = dplyr::lag(SampleYear, 1),
         year_length1 = SampleYear - year_prev,
         year_length = ifelse(is.na(year_length1), 4, year_length1), # including a filler
         dbh_growth = (dbh_fill - dbh_prev)/year_length,
         .by = c(Plot_Name, tree_id)) |> 
  filter(!TreeStatusCode %in% "AM") # added in 2023
table(tree_evs_rec2$status)
table(tree_evs_rec2$TreeStatusCode)

# make trees
tree_evs_rec3 <- tree_evs_rec2 |>
  mutate(prev_stat = lag(status, 1),
         .by = c(Plot_Name, tree_id)) |>
  mutate(first_stat = first(status), .by = c(Plot_Name, tree_id)) |>
  #filter(!prev_stat %in% 'dead') |> 
  #filter(!first_stat %in% "dead") |> 
  select(Plot_Name:dbh_fill, first_stat, prev_stat)

head(tree_evs_rec)

tree_evs_rec <- prepTreeMap(tree_evs_rec3)

table(tree_evs_rec$status)
str(tree_evs_rec)
str(tree_evs_am)

tree_ev_fill <- tree_evs_rec |> 
  arrange(tree_id, SampleYear) |> 
  mutate(stat_change = ifelse(prev_stat == "live" & StatusCode == "dead", "died", "no change")) |> 
  filter(x != 0 )

table(tree_ev_fill$SampleYear)

# Set up plot aesthetics
status_cols <- c("#11A300", "grey")
names(status_cols) <- as.character(c('live', 'dead'))

path <- "C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Forest_Health/Presentations/2026_Trees_of_Acadia/"
plotTreeMap(2010)
ggsave(paste0(path, plotName, "_2010", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2014)
ggsave(paste0(path, plotName, "_2014", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2018)
ggsave(paste0(path, plotName, "_2018", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2022)
ggsave(paste0(path, plotName, "_2022", ".jpeg"), width  = 6, height = 6)
plotTreeMap(2026)
ggsave(paste0(path, plotName, "_2026", ".jpeg"), width  = 6, height = 6)

