# ---------------------------------------------
# 04_streumasse.R
# Berechnung, Visualisierung & APA-Tabelle für Streumaße
cat("----------------------------------------- \n")
cat("04_streumasse.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# Pakete installieren und laden
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", type = "binary")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2", type = "binary")
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer", type = "binary")
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable", type = "binary")

library(ggplot2)
library(reshape2)
library(officer)
library(flextable)

# Datensatz laden
load("data/chirurgische_komplikationen.RData")

# Exportordner
output_dir <- "export/04_streumasse"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Plot-Theme
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

# Boxplot-Gesamtfunktion
plot_streumasse_gesamt <- function(daten) {
  daten_long <- melt(daten, measure.vars = c("Operationsdauer", "Blutverlust", "Komplikationsrisiko"),
                     variable.name = "Merkmal", value.name = "Wert")
  stats_df <- aggregate(Wert ~ Merkmal, daten_long, fivenum)
  stats_df <- do.call(data.frame, stats_df)
  names(stats_df)[2:6] <- c("Min", "Q1", "Median", "Q3", "Max")
  stats_df$IQR <- stats_df$Q3 - stats_df$Q1
  
  p <- ggplot(daten_long, aes(x = Merkmal, y = Wert)) +
    geom_boxplot(fill = "lightgray", color = "black", width = 0.5) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Min, label = paste("Min:", round(Min))), size = 3, vjust = 1.5) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Q1, label = paste("Q1:", round(Q1))), size = 3, vjust = 1.5) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Median, label = paste("Median:", round(Median))), size = 3, vjust = -0.7, fontface = "bold") +
    geom_text(data = stats_df, aes(x = Merkmal, y = Q3, label = paste("Q3:", round(Q3))), size = 3, vjust = -0.7) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Max, label = paste("Max:", round(Max))), size = 3, vjust = -0.7) +
    geom_text(data = stats_df, aes(x = Merkmal, y = Median + 25, label = paste("IQR:", round(IQR, 1))), size = 3, fontface = "italic") +
    labs(title = "Boxplots der Merkmale mit Kennwerten", x = "", y = "Wert") +
    ylim(30, 300) +
    theme_white_report()
  
  ggsave(file.path(output_dir, "streumasse_boxplots_gesamt.png"), plot = p, width = 10, height = 6)
}

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
  
  ggsave(file.path(output_dir, paste0("iqr_mad_", tolower(var_name), ".png")), plot = p, width = 5, height = 5)
}

# Berechnungen für jede Variable
range_ops <- range(daten$Operationsdauer)
sd_ops <- sd(daten$Operationsdauer)
var_ops <- var(daten$Operationsdauer)
iqr_ops <- IQR(daten$Operationsdauer)
mad_ops <- mad(daten$Operationsdauer)
plot_iqr_mad(iqr_ops, mad_ops, "Operationsdauer (Minuten)", "operationsdauer")

range_blut <- range(daten$Blutverlust)
sd_blut <- sd(daten$Blutverlust)
var_blut <- var(daten$Blutverlust)
iqr_blut <- IQR(daten$Blutverlust)
mad_blut <- mad(daten$Blutverlust)
plot_iqr_mad(iqr_blut, mad_blut, "Blutverlust (ml)", "blutverlust")

range_komp <- range(daten$Komplikationsrisiko)
sd_komp <- sd(daten$Komplikationsrisiko)
var_komp <- var(daten$Komplikationsrisiko)
iqr_komp <- IQR(daten$Komplikationsrisiko)
mad_komp <- mad(daten$Komplikationsrisiko)
plot_iqr_mad(iqr_komp, mad_komp, "Komplikationsrisiko", "komplikationsrisiko")

# Gemeinsamer Boxplot speichern
plot_streumasse_gesamt(daten)

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

# APA-Tabelle als Word-Datei
streumasse_df <- data.frame(
  Variable = c("Operationsdauer", "Blutverlust", "Komplikationsrisiko"),
  Range = c(paste(range_ops, collapse = "–"),
            paste(range_blut, collapse = "–"),
            paste(range_komp, collapse = "–")),
  SD = c(round(sd_ops, 2), round(sd_blut, 2), round(sd_komp, 2)),
  Variance = c(round(var_ops, 2), round(var_blut, 2), round(var_komp, 2)),
  IQR = c(round(iqr_ops, 2), round(iqr_blut, 2), round(iqr_komp, 2)),
  MAD = c(round(mad_ops, 2), round(mad_blut, 2), round(mad_komp, 2))
)

ft_streu <- flextable(streumasse_df) |>
  set_caption("Table 2\nMeasures of Dispersion for the Examined Variables") |>
  autofit() |>
  align(align = "left", part = "all") |>
  align(j = 2:6, align = "right", part = "all") |>
  bold(part = "header") |>
  fontsize(size = 11, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  padding(padding = 4, part = "all")

doc_streu <- read_docx()
doc_streu <- body_add_par(doc_streu, "Table 2", style = "heading 1")
doc_streu <- body_add_par(doc_streu, "Measures of dispersion (range, SD, variance, IQR, MAD) for each of the three study variables.", style = "Normal")
doc_streu <- body_add_flextable(doc_streu, ft_streu)

docx_path_streu <- file.path(output_dir, "dispersion_measures_APA.docx")
print(doc_streu, target = docx_path_streu)
cat("\n✅ APA-Tabelle gespeichert unter:", docx_path_streu, "\n")
