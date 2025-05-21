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

# Modus-Funktion definieren
modus <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Hilfsfunktion für Lagemaß-Plot (inkl. Modus)
plot_lagemasse <- function(var_name, var_label, daten) {
  m <- mean(daten[[var_name]])
  med <- median(daten[[var_name]])
  mod <- modus(daten[[var_name]])
  
  ggplot(daten, aes(x = .data[[var_name]])) +
    geom_histogram(aes(y = ..density..), binwidth = 20, fill = "gray80", color = "black") +
    geom_density(color = "darkgray", linewidth = 1) +
    geom_vline(xintercept = m, color = "blue", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = med, color = "red", linetype = "solid", linewidth = 1) +
    geom_vline(xintercept = mod, color = "green", linetype = "dotted", linewidth = 1) +
    labs(title = paste("Lagemasse:", var_label),
         subtitle = "Blau = Mittelwert, Rot = Median, Grün = Modus",
         x = var_label,
         y = "Dichte") +
    theme_white_report()
}

# Berechnungen + Plots

# Operationsdauer
mean_ops <- mean(daten$Operationsdauer)
median_ops <- median(daten$Operationsdauer)
modus_ops <- modus(daten$Operationsdauer)
p1 <- plot_lagemasse("Operationsdauer", "Operationsdauer (Minuten)", daten)
ggsave(file.path(output_dir, "lagemasse_operationsdauer.png"), plot = p1, width = 8, height = 6)

# Blutverlust
mean_blut <- mean(daten$Blutverlust)
median_blut <- median(daten$Blutverlust)
modus_blut <- modus(daten$Blutverlust)
p2 <- plot_lagemasse("Blutverlust", "Blutverlust (ml)", daten)
ggsave(file.path(output_dir, "lagemasse_blutverlust.png"), plot = p2, width = 8, height = 6)

# Komplikationsrisiko
mean_komp <- mean(daten$Komplikationsrisiko)
median_komp <- median(daten$Komplikationsrisiko)
modus_komp <- modus(daten$Komplikationsrisiko)
p3 <- plot_lagemasse("Komplikationsrisiko", "Komplikationsrisiko (Skalenwert)", daten)
ggsave(file.path(output_dir, "lagemasse_komplikationsrisiko.png"), plot = p3, width = 8, height = 6)

# Konsolenausgabe
cat("Mittelwerte:\n",
    "Operationsdauer:", mean_ops, "\n",
    "Blutverlust:", mean_blut, "\n",
    "Komplikationsrisiko:", mean_komp, "\n\n")

cat("Mediane:\n",
    "Operationsdauer:", median_ops, "\n",
    "Blutverlust:", median_blut, "\n",
    "Komplikationsrisiko:", median_komp, "\n\n")

cat("Modus:\n",
    "Operationsdauer:", modus_ops, "\n",
    "Blutverlust:", modus_blut, "\n",
    "Komplikationsrisiko:", modus_komp, "\n\n")

