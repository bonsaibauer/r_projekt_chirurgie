# ---------------------------------------------
# 02_visualisierung.R
# Grafische Darstellung der Daten mit ggplot2
# inkl. Histogramme und kombinierter Beeswarmplot
# ---------------------------------------------

# Lade Pakete
library(ggplot2)
library(ggbeeswarm)
library(reshape2)

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
# Gemeinsamer Beeswarmplot für alle Merkmale

# Daten in Long-Format bringen
daten_long <- melt(daten,
                   measure.vars = c("Operationsdauer", "Blutverlust", "Komplikationsrisiko"),
                   variable.name = "Merkmal",
                   value.name = "Wert")

# Plot erstellen
p_beeswarm_all <- ggplot(daten_long, aes(x = Merkmal, y = Wert)) +
  geom_beeswarm(priority = "density", color = "darkblue", size = 1.5, alpha = 0.6) +
  labs(
    title = "Beeswarmplot: Vergleich der Merkmale",
    x = "Merkmal",
    y = "Wert (30–300)"
  ) +
  ylim(30, 300) +
  theme_white_report()

# Speichern
ggsave(file.path(output_dir, "beeswarm_alle_merkmale.png"),
       plot = p_beeswarm_all, width = 9, height = 6)
