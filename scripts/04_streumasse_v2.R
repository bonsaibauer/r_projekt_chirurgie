# ---------------------------------------------
# 04_streumasse.R
# Berechnung und grafische Darstellung von Streumaßen
# ---------------------------------------------

library(ggplot2)
library(reshape2)  # für melt()

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
# Gemeinsamer Boxplot mit Annotationen
plot_streumasse_gesamt <- function(daten) {
  daten_long <- melt(daten, measure.vars = c("Operationsdauer", "Blutverlust", "Komplikationsrisiko"),
                     variable.name = "Merkmal", value.name = "Wert")
  
  # Fünfpunktzusammenfassung & IQR pro Merkmal
  stats_df <- aggregate(Wert ~ Merkmal, daten_long, fivenum)
  stats_df <- do.call(data.frame, stats_df)
  names(stats_df)[2:6] <- c("Min", "Q1", "Median", "Q3", "Max")
  stats_df$IQR <- stats_df$Q3 - stats_df$Q1
  
  p <- ggplot(daten_long, aes(x = Merkmal, y = Wert)) +
    geom_boxplot(fill = "lightgray", color = "black", width = 0.5) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Min, label = paste("Min:", round(Min))), size = 3, hjust = 0.5, vjust = 1.5) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Q1, label = paste("Q1:", round(Q1))), size = 3, hjust = 0.5, vjust = 1.5) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Median, label = paste("Median:", round(Median))), size = 3, hjust = 0.5, vjust = -0.7, fontface = "bold") +
    geom_text(data = stats_df, aes(x = Merkmal, y = Q3, label = paste("Q3:", round(Q3))), size = 3, hjust = 0.5, vjust = -0.7) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Max, label = paste("Max:", round(Max))), size = 3, hjust = 0.5, vjust = -0.7) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Median + 25, label = paste("IQR:", round(IQR, 1))), size = 3, fontface = "italic") +
    labs(title = "Boxplots der Merkmale mit Kennwerten", x = "", y = "Wert") +
    ylim(30, 300) +
    theme_white_report()
  
  ggsave(file.path(output_dir, "streumasse_boxplots_gesamt.png"), plot = p, width = 10, height = 6)
}

# ---------------------------------------------
# Balkendiagramm für IQR & MAD
plot_iqr_mad <- function(iqr_val, mad_val, var_label, var_name) {
  df <- data.frame(
    Maß = c("IQR", "MAD"),
    Wert = c(iqr_val, mad_val)
  )
  
  p <- ggplot(df, aes(x = Maß, y = Wert, fill = Maß)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = round(Wert, 2)), vjust = -0.5, size = 4) +
    labs(title = paste("IQR & MAD:", var_label), y = "Wert") +
    ylim(0, max(df$Wert) * 1.2) +
    theme_white_report() +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, paste0("iqr_mad_", tolower(var_name), ".png")),
         plot = p, width = 5, height = 5)
}

# ---------------------------------------------
# Berechnungen und Plots für Streumaße

# Operationsdauer
range_ops <- range(daten$Operationsdauer)
var_ops <- var(daten$Operationsdauer)
sd_ops <- sd(daten$Operationsdauer)
iqr_ops <- IQR(daten$Operationsdauer)
mad_ops <- mad(daten$Operationsdauer)
plot_iqr_mad(iqr_ops, mad_ops, "Operationsdauer (Minuten)", "operationsdauer")

# Blutverlust
range_blut <- range(daten$Blutverlust)
var_blut <- var(daten$Blutverlust)
sd_blut <- sd(daten$Blutverlust)
iqr_blut <- IQR(daten$Blutverlust)
mad_blut <- mad(daten$Blutverlust)
plot_iqr_mad(iqr_blut, mad_blut, "Blutverlust (ml)", "blutverlust")

# Komplikationsrisiko
range_komp <- range(daten$Komplikationsrisiko)
var_komp <- var(daten$Komplikationsrisiko)
sd_komp <- sd(daten$Komplikationsrisiko)
iqr_komp <- IQR(daten$Komplikationsrisiko)
mad_komp <- mad(daten$Komplikationsrisiko)
plot_iqr_mad(iqr_komp, mad_komp, "Komplikationsrisiko", "komplikationsrisiko")

# Gemeinsamer Boxplot
plot_streumasse_gesamt(daten)

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
