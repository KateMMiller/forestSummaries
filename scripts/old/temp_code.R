reg_colors <- c("seed_15_30cm" = "#D6D6FF", 
                "seed_30_100cm" = "#8F97E3", 
                "seed_100_150cm" = "#556CC9", 
                "seed_p150cm" = "#244EAD", 
                "sap_den" = "#05e646")

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = cycle, y = estimate, color = size_class,#linetype = sign, 
                         group = size_class))+ theme_FHM()+
  
  geom_line(size = 0.5)+
  geom_point(size = 2, shape = 21, alpha = 0.8)+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, size = 0.5)+
  # geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = size_class, color = size_class),
  #             lty = 1, alpha = 0.2, na.rm = TRUE)+
  labs(x  = "Cycle", y = bquote(Stems/m^2))+
  scale_color_manual(values = reg_colors, name = "Size Class", 
                     labels = c("15 \u2013 30 cm",
                                "30 \u2013 100 cm",
                                "100 \u2013 150 cm",
                                ">150 cm & < 1 cm DBH",
                                "Saplings: 1 \u2013 9.9cm DBH"
                     ))+
  scale_fill_manual(values = reg_colors, name = "Size Class",
                    labels = c("15 \u2013 30 cm",
                               "30 \u2013 100 cm",
                               "100 \u2013 150 cm",
                               ">150 cm & < 1 cm DBH",
                               "Saplings: 1 \u2013 9.9cm DBH"
                    ))