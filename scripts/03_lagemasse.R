# ---------------------------------------------
# 03_lagemasse.R
# Berechnung und grafische Darstellung von Lagemaßen
# ---------------------------------------------

# Lade Paket
library(ggplot2)

# Lade den Datensatz
load("data/chirurgische_komplikationen.RData")

# Zielordner für Grafiken
output_dir <- "export/03_lagemasse"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Einheitliches Design mit weißem Hintergrund und schwarzer Schrift
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

# Hilfsfunktion für Lagemaß-Plot
plot_lagemasse <- function(var_name, var_label, daten) {
  ggplot(daten, aes(x = .data[[var_name]])) +
    geom_histogram(aes(y = ..density..), binwidth = 20, fill = "gray80", color = "black") +
    geom_density(color = "darkgray", linewidth = 1) +
    geom_vline(xintercept = mean(daten[[var_name]]), color = "blue", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = median(daten[[var_name]]), color = "red", linetype = "solid", linewidth = 1) +
    labs(title = paste("Lagemasse:", var_label),
         x = var_label,
         y = "Dichte") +
    theme_white_report()
}

# Berechne Mittelwert & Median + erstelle Plots

# Operationsdauer
mean_ops <- mean(daten$Operationsdauer)
median_ops <- median(daten$Operationsdauer)
p1 <- plot_lagemasse("Operationsdauer", "Operationsdauer (Minuten)", daten)
ggsave(file.path(output_dir, "lagemasse_operationsdauer.png"), plot = p1, width = 8, height = 6)

# Blutverlust
mean_blut <- mean(daten$Blutverlust)
median_blut <- median(daten$Blutverlust)
p2 <- plot_lagemasse("Blutverlust", "Blutverlust (ml)", daten)
ggsave(file.path(output_dir, "lagemasse_blutverlust.png"), plot = p2, width = 8, height = 6)

# Komplikationsrisiko
mean_komp <- mean(daten$Komplikationsrisiko)
median_komp <- median(daten$Komplikationsrisiko)
p3 <- plot_lagemasse("Komplikationsrisiko", "Komplikationsrisiko (Skalenwert)", daten)
ggsave(file.path(output_dir, "lagemasse_komplikationsrisiko.png"), plot = p3, width = 8, height = 6)

# Konsolenausgabe (z. B. beim Sourcen im Script)
cat("Mittelwerte:\n",
    "Operationsdauer:", mean_ops, "\n",
    "Blutverlust:", mean_blut, "\n",
    "Komplikationsrisiko:", mean_komp, "\n\n")

cat("Mediane:\n",
    "Operationsdauer:", median_ops, "\n",
    "Blutverlust:", median_blut, "\n",
    "Komplikationsrisiko:", median_komp, "\n")
