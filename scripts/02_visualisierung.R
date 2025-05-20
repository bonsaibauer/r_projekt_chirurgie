# ---------------------------------------------
# 02_visualisierung.R
# Grafische Darstellung der Daten mit ggplot2
# inkl. weißem Hintergrund und schwarzer Schrift
# ---------------------------------------------

# Lade Paket
library(ggplot2)

# Lade den Datensatz
load("data/chirurgische_komplikationen.RData")

# Zielordner für die Plots
output_dir <- "export/02_visualisierung"

# Erstelle Ordner (rekursiv), falls nicht vorhanden
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Benutzerdefiniertes Theme mit weißem Hintergrund und schwarzer Schrift
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

# Boxplot: Operationsdauer
ggplot(daten, aes(y = Operationsdauer)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot: Operationsdauer", y = "Minuten") +
  ylim(30, 300) +
  theme_white_report()
ggsave(file.path(output_dir, "boxplot_operationsdauer.png"), width = 6, height = 6)

# Boxplot: Blutverlust
ggplot(daten, aes(y = Blutverlust)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot: Blutverlust", y = "Milliliter") +
  ylim(30, 300) +
  theme_white_report()
ggsave(file.path(output_dir, "boxplot_blutverlust.png"), width = 6, height = 6)

# Boxplot: Komplikationsrisiko
ggplot(daten, aes(y = Komplikationsrisiko)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Boxplot: Komplikationsrisiko", y = "Skalenwert") +
  ylim(30, 300) +
  theme_white_report()
ggsave(file.path(output_dir, "boxplot_komplikationsrisiko.png"), width = 6, height = 6)

