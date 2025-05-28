# ---------------------------------------------
# 04_streumasse.R
# Berechnung und grafische Darstellung von Streumaßen
# ---------------------------------------------

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

# ---------------------------------------------
# Funktion: Boxplot mit mittiger Beschriftung
plot_streumasse <- function(var_name, var_label, daten) {
  var_data <- daten[[var_name]]
  stats <- fivenum(var_data)
  iqr <- stats[4] - stats[2]
  
  ggplot(daten, aes(y = .data[[var_name]])) +
    geom_boxplot(fill = "lightgray", color = "black", width = 0.2) +
    annotate("text", x = 1, y = stats[1], label = paste("Min:", stats[1]), hjust = 0.5, size = 3) +
    annotate("text", x = 1, y = stats[2], label = paste("Q1:", stats[2]), hjust = 0.5, size = 3) +
    annotate("text", x = 1, y = stats[3], label = paste("Median:", stats[3]), hjust = 0.5, size = 3, fontface = "bold") +
    annotate("text", x = 1, y = stats[4], label = paste("Q3:", stats[4]), hjust = 0.5, size = 3) +
    annotate("text", x = 1, y = stats[5], label = paste("Max:", stats[5]), hjust = 0.5, size = 3) +
    annotate("text", x = 1, y = stats[3] + 25, label = paste("IQR:", round(iqr, 1)), hjust = 0.5, fontface = "italic", size = 3) +
    labs(title = paste("Boxplot:", var_label), y = var_label) +
    ylim(30, 300) +
    theme_white_report()
}

# ---------------------------------------------
# Funktion: Balkendiagramm für IQR & MAD
plot_iqr_mad <- function(iqr_val, mad_val, var_label, var_name) {
  df <- data.frame(
    Maß = c("IQR", "MAD"),
    Wert = c(iqr_val, mad_val)
  )
  p <- ggplot(df, aes(x = Maß, y = Wert, fill = Maß)) +
    geom_bar(stat = "identity", width = 0.5) +
    labs(title = paste("IQR & MAD:", var_label), y = "Wert") +
    theme_white_report() +
    theme(legend.position = "none")
  ggsave(file.path(output_dir, paste0("iqr_mad_", tolower(var_name), ".png")),
         plot = p, width = 5, height = 5)
}

# ---------------------------------------------
# Berechnung & Visualisierung je Variable

# Operationsdauer
range_ops <- range(daten$Operationsdauer)
var_ops <- var(daten$Operationsdauer)
sd_ops <- sd(daten$Operationsdauer)
iqr_ops <- IQR(daten$Operationsdauer)
mad_ops <- mad(daten$Operationsdauer)

p1 <- plot_streumasse("Operationsdauer", "Operationsdauer (Minuten)", daten)
ggsave(file.path(output_dir, "streumasse_operationsdauer.png"), plot = p1, width = 6, height = 6)
plot_iqr_mad(iqr_ops, mad_ops, "Operationsdauer (Minuten)", "operationsdauer")

# Blutverlust
range_blut <- range(daten$Blutverlust)
var_blut <- var(daten$Blutverlust)
sd_blut <- sd(daten$Blutverlust)
iqr_blut <- IQR(daten$Blutverlust)
mad_blut <- mad(daten$Blutverlust)

p2 <- plot_streumasse("Blutverlust", "Blutverlust (ml)", daten)
ggsave(file.path(output_dir, "streumasse_blutverlust.png"), plot = p2, width = 6, height = 6)
plot_iqr_mad(iqr_blut, mad_blut, "Blutverlust (ml)", "blutverlust")

# Komplikationsrisiko
range_komp <- range(daten$Komplikationsrisiko)
var_komp <- var(daten$Komplikationsrisiko)
sd_komp <- sd(daten$Komplikationsrisiko)
iqr_komp <- IQR(daten$Komplikationsrisiko)
mad_komp <- mad(daten$Komplikationsrisiko)

p3 <- plot_streumasse("Komplikationsrisiko", "Komplikationsrisiko (Skalenwert)", daten)
ggsave(file.path(output_dir, "streumasse_komplikationsrisiko.png"), plot = p3, width = 6, height = 6)
plot_iqr_mad(iqr_komp, mad_komp, "Komplikationsrisiko", "komplikationsrisiko")

# ---------------------------------------------
# Konsolenausgabe

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
    "Komplikationsrisiko:", round(var_komp, 2), "\n\n")

cat("Interquartilsabstände (IQR):\n",
    "Operationsdauer:", iqr_ops, "\n",
    "Blutverlust:", iqr_blut, "\n",
    "Komplikationsrisiko:", iqr_komp, "\n\n")

cat("Mittlere absolute Abweichungen (MAD):\n",
    "Operationsdauer:", round(mad_ops, 2), "\n",
    "Blutverlust:", round(mad_blut, 2), "\n",
    "Komplikationsrisiko:", round(mad_komp, 2), "\n")
