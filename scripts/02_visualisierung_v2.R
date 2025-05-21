# ---------------------------------------------
# 02_visualisierung.R
# Grafische Darstellung der Daten mit ggplot2
# inkl. Histogramme & Beeswarmplots
# ---------------------------------------------

# Lade Pakete
library(ggplot2)
library(ggbeeswarm)

# Lade den Datensatz
load("data/chirurgische_komplikationen.RData")

# Zielordner für die Plots
output_dir <- "export/02_visualisierung"

# Erstelle Ordner (rekursiv), falls nicht vorhanden
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

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

# ---------------------------------------------
# Histogramme

# Histogramm: Operationsdauer
ggplot(daten, aes(x = Operationsdauer)) +
  geom_histogram(binwidth = 20, fill = "lightblue", color = "black") +
  labs(title = "Histogramm: Operationsdauer", x = "Minuten", y = "Frequenz") +
  xlim(30, 300) +
  theme_white_report()
ggsave(file.path(output_dir, "hist_operationsdauer.png"), width = 8, height = 6)

# Histogramm: Blutverlust
ggplot(daten, aes(x = Blutverlust)) +
  geom_histogram(binwidth = 20, fill = "lightgreen", color = "black") +
  labs(title = "Histogramm: Blutverlust", x = "Milliliter", y = "Frequenz") +
  xlim(30, 300) +
  theme_white_report()
ggsave(file.path(output_dir, "hist_blutverlust.png"), width = 8, height = 6)

# Histogramm: Komplikationsrisiko
ggplot(daten, aes(x = Komplikationsrisiko)) +
  geom_histogram(binwidth = 20, fill = "lightcoral", color = "black") +
  labs(title = "Histogramm: Komplikationsrisiko", x = "Skalenwert", y = "Frequenz") +
  xlim(30, 300) +
  theme_white_report()
ggsave(file.path(output_dir, "hist_komplikationsrisiko.png"), width = 8, height = 6)

# ---------------------------------------------
# Beeswarmplots mit ggbeeswarm

# Funktion für Beeswarmplot
plot_beeswarm <- function(var, var_label, farbe, dateiname) {
  # Neues Datenframe mit explizitem Gruppierungslabel auf X
  df <- data.frame(Wert = daten[[var]], Variable = var_label)
  
  p <- ggplot(df, aes(x = Variable, y = Wert)) +
    geom_beeswarm(color = farbe, size = 1.5, alpha = 0.6, priority = "density") +
    labs(
      title = paste("Beeswarmplot:", var_label),
      x = "Merkmal",
      y = var_label
    ) +
    ylim(30, 300) +
    theme_white_report()
  
  ggsave(file.path(output_dir, dateiname), plot = p, width = 6, height = 6)
}


# Beeswarmplots erzeugen
plot_beeswarm("Operationsdauer", "Operationsdauer (Minuten)", "blue", "beeswarm_operationsdauer.png")
plot_beeswarm("Blutverlust", "Blutverlust (ml)", "darkgreen", "beeswarm_blutverlust.png")
plot_beeswarm("Komplikationsrisiko", "Komplikationsrisiko (Skalenwert)", "firebrick", "beeswarm_komplikationsrisiko.png")

