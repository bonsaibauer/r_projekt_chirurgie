# ---------------------------------------------
# 05_korrelation.R
# Korrelationsanalyse mit Visualisierung
# ---------------------------------------------

library(ggplot2)

# Datensatz laden
load("data/chirurgische_komplikationen.RData")

# Zielordner
output_dir <- "export/05_korrelation"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Theme
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

# Hilfsfunktion für Korrelationsplot mit Trendlinie
plot_correlation <- function(xvar, yvar, xlabel, ylabel, daten, title) {
  ggplot(daten, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.5, color = "gray40") +
    geom_smooth(method = "lm", color = "blue", linewidth = 1, se = FALSE) +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_white_report()
}

# Berechnung der Korrelationskoeffizienten
cor_ops_blut <- cor(daten$Operationsdauer, daten$Blutverlust)
cor_ops_komp <- cor(daten$Operationsdauer, daten$Komplikationsrisiko)
cor_blut_komp <- cor(daten$Blutverlust, daten$Komplikationsrisiko)

# Tabelle erzeugen
korrelationsmatrix <- data.frame(
  Kombination = c("Operationsdauer & Blutverlust",
                  "Operationsdauer & Komplikationsrisiko",
                  "Blutverlust & Komplikationsrisiko"),
  Korrelationskoeffizient = round(c(cor_ops_blut, cor_ops_komp, cor_blut_komp), 3)
)

# Tabelle als CSV speichern
write.csv(korrelationsmatrix, file.path(output_dir, "korrelationen.csv"), row.names = FALSE)

# Plots speichern
ggsave(file.path(output_dir, "plot_ops_blut.png"),
       plot = plot_correlation("Operationsdauer", "Blutverlust", "Operationsdauer (Min)", "Blutverlust (ml)", daten, "Korrelation: OP-Dauer & Blutverlust"),
       width = 7, height = 5)

ggsave(file.path(output_dir, "plot_ops_komp.png"),
       plot = plot_correlation("Operationsdauer", "Komplikationsrisiko", "Operationsdauer (Min)", "Komplikationsrisiko", daten, "Korrelation: OP-Dauer & Komplikationsrisiko"),
       width = 7, height = 5)

ggsave(file.path(output_dir, "plot_blut_komp.png"),
       plot = plot_correlation("Blutverlust", "Komplikationsrisiko", "Blutverlust (ml)", "Komplikationsrisiko", daten, "Korrelation: Blutverlust & Komplikationsrisiko"),
       width = 7, height = 5)

# Textausgabe für Konsole
cat("Korrelation (OP-Dauer & Blutverlust):", cor_ops_blut, "\n")
cat("Korrelation (OP-Dauer & Komplikationsrisiko):", cor_ops_komp, "\n")
cat("Korrelation (Blutverlust & Komplikationsrisiko):", cor_blut_komp, "\n")
