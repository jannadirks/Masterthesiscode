#Plot intercept median split 

merged_data_intercept$cesd_category_median <- as.factor(merged_data_intercept$cesd_category_median)

ggplot(merged_data_intercept, aes(x = actual_intercept_2_scaled, y = actual_intercept_1_scaled, color = cesd_category_median)) +
  geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.1, height = 0.1)) +  # Transparente Punkte mit Jitter
  geom_smooth(aes(group = cesd_category_median), method = "lm", se = TRUE, size = 1, alpha = 0.05) +  # Modellvorhersagen mit Konfidenzintervallen
  labs(
    title = "Moderationseffekt: CESD Kategorie",
    subtitle = "Wie CESD die Beziehung zwischen Actigraphy und BISQ beeinflusst",
    x = "Intercepts Actigraphy (z-scores)",
    y = "Intercepts BISQ (z-scores)",
    color = "CESD Kategorie"
  ) +
  scale_color_manual(
    values = c("darkred", "steelblue"),
    labels = c("Low CESD (0)", "High CESD (1)")
  ) +
  coord_cartesian(xlim = c(-3, 2), ylim = c(-2, 2)) +  # Einschränkung der Achsenbereiche
  coord_fixed(ratio = 1) +  # Sicherstellen, dass ein Schritt auf beiden Achsen gleich groß ist
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 
  #annotate("text", x = 0.5, y = 2, label = "Positive Steigung (CESD = 0)", color = "darkred", size = 4, hjust = 0) +
  #annotate("text", x = -2, y = -1.5, label = "Negative Steigung (CESD = 1)", color = "steelblue", size = 4, hjust = 0)

ggsave("moderation_plot_ohneIntervall_mediansplit.png", 
       width = 6, height = 6, units = "in", dpi = 300)


#Plot slope data 
merged_data_slope$cesd_category_median <- as.factor(merged_data_slope$cesd_category_median)

ggplot(merged_data_slope, aes(x = actual_slope_2_scaled, y = actual_slope_1_scaled, color = cesd_category_median)) +
  geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.1, height = 0.1)) +  # Transparente Punkte mit Jitter
  geom_smooth(aes(group = cesd_category_median), method = "lm", se = TRUE, size = 1, alpha = 0.05) +  # Modellvorhersagen mit Konfidenzintervallen
  labs(
    title = "Moderationseffekt: CESD Kategorie ",
    subtitle = "Wie CESD die Beziehung zwischen Actigraphy und BISQ beeinflusst",
    x = "Slope Actigraphy (z-scores)",
    y = "Slope BISQ (z-scores)",
    color = "CESD Kategorie"
  ) +
  scale_color_manual(
    values = c("darkred", "steelblue"),
    labels = c("Low CESD (0)", "High CESD (1)")
  ) +
  coord_cartesian(xlim = c(-3, 2), ylim = c(-2, 2)) +  # Einschränkung der Achsenbereiche
  coord_fixed(ratio = 1) +  # Sicherstellen, dass ein Schritt auf beiden Achsen gleich groß ist
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 
#annotate("text", x = 0.5, y = 2, label = "Positive Steigung (CESD = 0)", color = "darkred", size = 4, hjust = 0) +
#annotate("text", x = -2, y = -1.5, label = "Negative Steigung (CESD = 1)", color = "steelblue", size = 4, hjust = 0)

ggsave("moderation_plot_slope_mediansplit.png", 
       width = 10, height = 6, units = "in", dpi = 300)
