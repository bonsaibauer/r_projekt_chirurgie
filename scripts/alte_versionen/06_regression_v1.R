# ---------------------------------------------
# 06_regression.R
# Lineare Regressionsanalyse: Komplikationsrisiko ~ Blutverlust
# ---------------------------------------------

library(ggplot2)
library(broom)  # für tidy() bei Modellausgabe

# Lade den Datensatz
load("data/chirurgische_komplikationen.RData")

# Exportverzeichnis
output_dir <- "export/06_regression"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Einheitliches Theme
theme_white_report <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black"),
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold")
    )
}

# Regressionsmodell
model <- lm(Komplikationsrisiko ~ Blutverlust, data = daten)

# Zusammenfassung in Konsole
summary(model)

# Regressionsplot erstellen
reg_plot <- ggplot(daten, aes(x = Blutverlust, y = Komplikationsrisiko)) +
  geom_point(alpha = 0.5, color = "gray40") +
  geom_smooth(method = "lm", color = "blue", linewidth = 1.2, se = TRUE) +
  labs(title = "Lineare Regression: Komplikationsrisiko ~ Blutverlust",
       x = "Blutverlust (ml)",
       y = "Komplikationsrisiko (Skalenwert)") +
  theme_white_report()

# Plot speichern
ggsave(file.path(output_dir, "regression_blut_komp.png"), plot = reg_plot, width = 8, height = 6)

# Modellergebnisse in Tabelle umwandeln
model_table <- broom::tidy(model)
write.csv(model_table, file.path(output_dir, "regression_koeffizienten.csv"), row.names = FALSE)

# R² separat speichern
r_squared <- summary(model)$r.squared
writeLines(paste("R² =", round(r_squared, 4)),
           file.path(output_dir, "r_squared.txt"))
