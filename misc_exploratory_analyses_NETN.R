#---- Broken stick analysis ----

mod <- lm(stock~ quad_pct_cover, data = shrub_reg)
mod_seg <- segmented(mod, seg.Z = ~quad_pct_cover, psi = 15)
slope(mod_seg)
mod_seg$psi[, "Est."] #7% 5-10% cover class


davies.test(mod_seg, seg.Z = ~quad_pct_cover) # 12.2 %

# broken.line returns fitted values + SE on a fine grid
br <- broken.line(mod_seg, se.fit = TRUE)
plot_df <- data.frame(
  quad_pct_cover   = shrub_reg$quad_pct_cover,
  fit = br$fit,
  lo  = br$fit - 1.96 * br$se.fit,
  hi  = br$fit + 1.96 * br$se.fit
)
plot_df <- plot_df[order(plot_df$quad_pct_cover), ]

ggplot(shrub_reg, aes(quad_pct_cover, seed_den)) +
  geom_point(colour = "grey50", alpha = 0.6) +
  geom_ribbon(data = plot_df, aes(y = fit, ymin = lo, ymax = hi),
              fill = "steelblue", alpha = 0.3) +
  geom_line(data = plot_df, aes(y = fit), colour = "steelblue", linewidth = 1) +
  geom_vline(xintercept = mod_seg$psi[, "Est."],
             linetype = "dashed", colour = "firebrick") +
  theme_minimal()

summary(mod_seg)
mod_seg$lme.fit.noG
mod_seg$lme.fit

attr(mod_seg$psi.i, "is.break")

mod_seg$psi.i
mod_seg$fixed.eta.delta
mod_seg$fixed.eta.psi
mod_seg$fixed.psi
mod_seg$psi.i

plot.segmented.lme(mod_seg)

plot(mod_seg)
slope(mod_seg)


# #---- Shrub cover -----
#
shrubs <- do.call(joinMicroShrubData, args_vs) |> filter(SampleYear >= 2010)
table(shrubs$ScientificName, shrubs$SampleYear)

other_native <- c("Amphicarpaea bracteata", "Aronia melanocarpa", "Gaylussacia baccata",
                  "Hamamelis virginiana", # off for SAGA
                  "Ilex verticillata", "Lyonia ligustrina", "Rosa",
                  "Spiraea alba", "Vaccinium angustifolium", "Vaccinium pallidum",
                  "Vaccinium stamineum", "Zanthoxylum americanum", 'Rhus typhina',
                  'Vaccinium corymbosum') # adjust per park
corylus = c("Corylus americana", "Corylus cornuta")
other_exotic <- c("Crataegus", "Viburnum lantana", "Rhodotypos scandens",
                  'Viburnum dilatatum', 'Viburnum sieboldii',
                  'Euonymus alatus')# adjust per park
cornus <- c("Cornus", "Cornus amomum", "Cornus racemosa")
rubus <- c("Rubus", "Rubus allegheniensis", "Rubus idaeus", "Rubus occidentalis", "Rubus odoratus")
viburnum <- c("Viburnum dentatum", "Viburnum lentago", 'Viburnum lantanoides')
vitis <- c("Vitis", "Vitis aestivalis", "Vitis riparia")
lonicera <- c("Lonicera", "Lonicera - Exotic", "Lonicera morrowii")
parth <- c("Parthenocissus", "Parthenocissus quinquefolia")
natvines <- c(parth, vitis, "Toxicodendron radicans")
ligustrum <- c("Ligustrum", "Ligustrum vulgare")

table(shrubs$ScientificName)

shrubs <- left_join(shrubs, prepTaxa() |> select(ScientificName, CommonName), by = "ScientificName")

shrubs <- shrubs |>
  mutate(spp_grp = case_when(ScientificName %in% c(other_native, viburnum, parth, natvines) ~ "Other Native spp.",
                             ScientificName %in% other_exotic ~ "Other Exotic spp.",
                             ScientificName %in% cornus ~ "Cornus spp. (dogwood)",
                             ScientificName %in% corylus ~ "Corylus spp. (hazelnut)",
                             ScientificName %in% rubus ~ "Rubus spp. (brambles)",
                             #ScientificName %in% viburnum ~ "Viburnum spp. (arrowwood)", # included in other_native
                             #ScientificName %in% vitis ~ "Vitis spp. (grape)",#  included in other_native
                             ScientificName %in% lonicera ~ "Lonicera spp. (exotic honeysuckle)",
                            # ScientificName %in% natvines ~ "Native vine spp.", # included in other_native
                             ScientificName %in% ligustrum ~ "Ligustrum spp. (privet)",
                             TRUE ~ paste0(ScientificName, " (", CommonName, ")")))

shrub_sum <- shrubs |> group_by(Plot_Name, SampleYear, spp_grp) |>
  summarize(avg_cov = sum(shrub_avg_cov, na.rm = T), .groups = 'drop')

table(shrub_sum$spp_grp)
head(shrubs)

# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> filter(SampleYear >= 2010) |> unique()

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name),
                           SampleYear = unique(plot_yr$SampleYear),
                           spp_grp = unique(shrub_sum$spp_grp)) |>
  filter(spp_grp != "None present (NA)") |>
  mutate(species = word(spp_grp, 1),
         genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
         sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |>
  select(Plot_Name, SampleYear, spp_grp, sppcode)

plot_spp_yr <- left_join(plot_yr, plot_spp_yr1, by = c("Plot_Name", "SampleYear"))

dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))

shrub_smooth <- left_join(plot_spp_yr, shrub_sum |> select(Plot_Name, SampleYear, spp_grp, avg_cov),
                            by = c("Plot_Name", "SampleYear", "spp_grp")) #|>

shrub_smooth[,c("avg_cov")][is.na(shrub_smooth[,c("avg_cov")])] <- 0
table(shrub_smooth$sppcode)

spp_list <- sort(unique(shrub_smooth$sppcode))

span = 8/length(unique(shrub_smooth$SampleYear))

head(shrub_smooth)
shrub_smooth <- purrr::map_dfr(spp_list,
                              function(spp){
                                df <- shrub_smooth |> filter(sppcode %in% spp)
                                case_boot_loess(df, x = "SampleYear", y = "avg_cov", ID = "Plot_Name",
                                                group = "sppcode",
                                                span = span, num_reps = 1000) |>
                                  mutate(sppcode = spp)
                              }
)

# Determine if significant based on whether first and last year CIs overlap
shrub_smooth2 <-
  left_join(shrub_smooth,
            shrub_smooth |> arrange(SampleYear) |> group_by(sppcode) |>
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |>
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
shrub_smooth3 <- left_join(shrub_smooth2,
                          plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                          by = c('sppcode'),
                          relationship = 'many-to-many') |>
  mutate(spp_grp = as.character(spp_grp)) |>
  arrange(spp_grp)

# Plotting trends by species group facet
shrub_trends <-
  ggplot(shrub_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
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
  labs(y = "Shrub % Cover", x = "Year") +
  scale_x_continuous(breaks = c(seq(2010, to, by = 3), to),
                     limits = c(2009, to)) +
  theme_FHM() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom')

shrub_trends

svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
    height = 8, width = 7)
shrub_trends
dev.off()

table(shrub_smooth3$spp_grp)

net_shrubs <-
  ggplot(shrub_smooth3,
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Shrub % Cover") +
  theme_FHM()+
  scale_color_manual(values = c(
  "Berberis thunbergii (Japanese barberry)" = "#4CE600",
   "Celastrus orbiculatus (oriental bittersweet)" = "#FFAA00",
    "Cornus spp. (dogwood)" = "#95DE34",
    "Cornus alternifolia (alternate-leaf dogwood)" = "#95de34",
    "Corylus spp. (hazelnut)" = "#57A588",
    "Hamamelis virginiana (American witchhazel)" = "#FFF000",
    "Lonicera spp. (exotic honeysuckle)" = "#0070FF",
    "Native vine spp." = "#308E33",
    "Other Exotic spp." = "#F9CF36",
    "Other Native spp." = "#828282",
    "Rhamnus cathartica (common buckthorn)" = "#FF7854",
    "Rosa multiflora (multiflora rose)" = "#BB3636",
    "Rubus spp. (brambles)" = "#DC91F6",
    "Toxicodendron radicans (eastern poison ivy)" = "#937648",
    "Viburnum spp. (arrowwood)" = "#57A588",
    "Vitis spp. (grape)" = "#8C62B4",
    "Zanthoxylum americanum (Common pricky-ash)" = "#B9C63A",
    "Viburnum lantanoides (hobblebush)" = "#DC91F6",
    "Ligustrum spp. (privet)" = "#B9C63A",
    "Lindera benzoin (northern spicebush)" = "#95de34",
    "Lonicera japonica (Japanese honeysuckle)" = "#0070FF",
    "Rubus phoenicolasius (wine raspberry)" = "#937648"
  ), name = NULL) +
  scale_linetype_manual(values = c(
   "Berberis thunbergii (Japanese barberry)" = "solid",
   "Celastrus orbiculatus (oriental bittersweet)" = "solid",
    "Cornus spp. (dogwood)" = "dotdash",
    "Cornus alternifolia (alternate-leaf dogwood)" = "solid",
    "Corylus spp. (hazelnut)" = "dotdash",
    "Hamamelis virginiana (American witchhazel)" = 'dotdash',
    "Lonicera spp. (exotic honeysuckle)" = "solid",
    "Native vine spp." = "dashed",
    "Other Exotic spp." = "solid",
    "Other Native spp." = "dotdash",
    "Parthenocissus spp. (Virginia creeper)" = "dotted",
    "Rhamnus cathartica (common buckthorn)" = "solid",
    "Rosa multiflora (multiflora rose)" = "solid",
    "Rubus spp. (brambles)" = "dotdash",
    "Toxicodendron radicans (eastern poison ivy)" = "dotted",
    "Viburnum spp. (arrowwood)" = "dotdash",
    "Vitis spp. (grape)" = "dotdash",
     "Viburnum lantanoides (hobblebush)" = "solid",
   "Zanthoxylum americanum (Common pricky-ash)" = "dotdash",
   "Ligustrum spp. (privet)" = "solid",
   "Lindera benzoin (northern spicebush)" = "dotted",
   "Lonicera japonica (Japanese honeysuckle)" = "dotted",
   "Rubus phoenicolasius (wine raspberry)" = "dotdash"
  ), name = NULL) +
  scale_x_continuous(breaks = c(seq(2010, to, by = 2), to),
                     limits = c(2009.9, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'bottom',
        legend.key.width = unit(1, 'cm')) +
  guides(color = guide_legend(nrow = 3))

net_shrubs

svg(paste0(new_path, "figures/", "Figure_5_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
    height = 6, width = 8)
net_shrubs
dev.off()
