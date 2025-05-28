# ---------------------------------------------
# 04_streumasse.R
# Berechnung, Visualisierung & stargazer-Tabelle für Streumaße
cat("----------------------------------------- \n")
cat("04_streumasse.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# 1) Pakete installieren und laden, wenn nötig
cat("🔄 Überprüfe, ob die Pakete ggplot2, reshape2, stargazer und dplyr installiert sind...\n")

# Warnungen vorübergehend deaktivieren
options(warn = -1)

# 1.1) ggplot2 Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", type = "binary")
library(ggplot2)

# 1.2) reshape2 Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2", type = "binary")
library(reshape2)

# 1.3) stargazer Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer", type = "binary")
library(stargazer)

# 1.4) dplyr Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", type = "binary")
library(dplyr)

cat("✅ Alle Pakete wurden erfolgreich geladen.\n")

# 2) Ordnerstruktur erstellen, wenn noch nicht vorhanden
cat("🔄 Überprüfe, ob der Ordner 'export/04_streumasse' existiert...\n")
output_dir <- "export/04_streumasse"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("✅ Ordner wurde erstellt.\n")
} else {
  cat("✅ Ordner existiert bereits.\n")
}

# 3) Einheitliches Theme für Plots
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

# 4) Funktionen berechnen, je nach Aufgabenstellung
cat("🔄 Berechnung der Streumaße für jede Variable (Min, Max, Range, SD, Variance, CV)...\n")

# Berechnungsfunktion für die Streumaße (Min, Max, Range, SD, Variance, Coefficient of Variation)
calculate_dispersion_measures <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  range_val <- max_val - min_val
  sd_val <- sd(x)
  var_val <- var(x)
  mean_val <- mean(x)
  cv_val <- sd_val / mean_val * 100  # Variationskoeffizient in Prozent
  list(min = min_val, max = max_val, range = range_val, sd = sd_val, var = var_val, cv = cv_val)
}

# Berechnungen für jede Variable
dispersion_ops <- calculate_dispersion_measures(daten$Operationsdauer)
dispersion_blut <- calculate_dispersion_measures(daten$Blutverlust)
dispersion_komp <- calculate_dispersion_measures(daten$Komplikationsrisiko)

# Kombinieren der Ergebnisse in einem DataFrame
dispersion_df <- data.frame(
  Variable = c("Operationsdauer", "Blutverlust", "Komplikationsrisiko"),
  Min = c(dispersion_ops$min, dispersion_blut$min, dispersion_komp$min),
  Max = c(dispersion_ops$max, dispersion_blut$max, dispersion_komp$max),
  Range = c(dispersion_ops$range, dispersion_blut$range, dispersion_komp$range),
  SD = c(dispersion_ops$sd, dispersion_blut$sd, dispersion_komp$sd),
  Variance = c(dispersion_ops$var, dispersion_blut$var, dispersion_komp$var),
  CV = c(dispersion_ops$cv, dispersion_blut$cv, dispersion_komp$cv)
)

# 5) Plots in R-Studio unter Reiter Plots ausgeben
cat("📊 Alle Plots werden in R-Studio unter dem Reiter 'Plots' angezeigt.\n")

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
  
  print(p)  # Plot im Reiter "Plots" anzeigen
  ggsave(file.path(output_dir, "streumasse_boxplots_gesamt.png"), plot = p, width = 10, height = 6)
  cat("✅ Boxplot als PNG gespeichert.\n")
}

# Gemeinsamer Boxplot speichern
plot_streumasse_gesamt(daten)

# 6) Plots exportieren
cat("🔄 Exportiere die Plots als PNG...\n")
ggsave(file.path(output_dir, "streumasse_boxplots_gesamt.png"))
cat("✅ Plot wurde erfolgreich exportiert.\n")

# 7) Stargazer-Tabelle für die Streumaße speichern
cat("🔄 Erstelle stargazer-Tabelle als .txt...\n")
stargazer(dispersion_df, type = "text", summary = FALSE, rownames = FALSE, 
          out = file.path(output_dir, "streumasse_tabelle.txt"))
cat("✅ stargazer-Tabelle wurde als .txt-Datei erstellt und gespeichert.\n")

# Warnungen wieder aktivieren
options(warn = 0)
