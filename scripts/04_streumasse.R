# ---------------------------------------------
# 04_streumasse.R
# Berechnung und grafische Darstellung von Streumaßen
# ---------------------------------------------

# Lade Paket
library(ggplot2)

# Lade den Datensatz
load("data/chirurgische_komplikationen.RData")

# Zielordner für Export
output_dir <- "export/04_streumasse"
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

# Hilfsfunktion für Boxplot mit IQR-Anzeige
plot_streumasse <- function(var_name, var_label, daten) {
  ggplot(daten, aes(y = .data[[var_name]])) +
    geom_boxplot(fill = "lightgray", color = "black") +
    labs(title = paste("Streumaße:", var_label), y = var_label) +
    ylim(30, 300) +
    theme_white_report()
}

# Berechne Streumaße und speichere Boxplots

# Operationsdauer
range_ops <- range(daten$Operationsdauer)
var_ops <- var(daten$Operationsdauer)
sd_ops <- sd(daten$Operationsdauer)
p1 <- plot_streumasse("Operationsdauer", "Operationsdauer (Minuten)", daten)
ggsave(file.path(output_dir, "streumasse_operationsdauer.png"), plot = p1, width = 6, height = 6)

# Blutverlust
range_blut <- range(daten$Blutverlust)
var_blut <- var(daten$Blutverlust)
sd_blut <- sd(daten$Blutverlust)
p2 <- plot_streumasse("Blutverlust", "Blutverlust (ml)", daten)
ggsave(file.path(output_dir, "streumasse_blutverlust.png"), plot = p2, width = 6, height = 6)

# Komplikationsrisiko
range_komp <- range(daten$Komplikationsrisiko)
var_komp <- var(daten$Komplikationsrisiko)
sd_komp <- sd(daten$Komplikationsrisiko)
p3 <- plot_streumasse("Komplikationsrisiko", "Komplikationsrisiko (Skalenwert)", daten)
ggsave(file.path(output_dir, "streumasse_komplikationsrisiko.png"), plot = p3, width = 6, height = 6)

# Ausgabe in der Konsole
# Ausgabe in der Konsole
cat("Spannweiten:\n",
    "Operationsdauer:", paste(range_ops, collapse = " - "), "\n",
    "Blutverlust:", paste(range_blut, collapse = " - "), "\n",
    "Komplikationsrisiko:", paste(range_komp, collapse = " - "), "\n\n")

cat("Standardabweichungen:\n",
    "Operationsdauer:", round(sd_ops, 2), "\n",
    "Blutverlust:", round(sd_blut, 2), "\n",
    "Komplikationsrisiko:", round(sd_komp, 2), "\n\n")

cat("Varianzen:\n",
    "Operationsdauer:", round(var_ops, 2), "\n",
    "Blutverlust:", round(var_blut, 2), "\n",
    "Komplikationsrisiko:", round(var_komp, 2), "\n")

