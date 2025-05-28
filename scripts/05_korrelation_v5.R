# ---------------------------------------------
# 05_korrelation.R
# Berechnung, Visualisierung & Korrelationsanalyse
cat("----------------------------------------- \n")
cat("05_korrelation.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# 1) Pakete installieren und laden, wenn nötig
cat("🔄 Überprüfe, ob die Pakete ggplot2, stargazer und gridExtra installiert sind...\n")

# Warnungen vorübergehend deaktivieren
options(warn = -1)

# 1.1) ggplot2 Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", type = "binary")
library(ggplot2)

# 1.2) stargazer Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer", type = "binary")
library(stargazer)

# 1.3) gridExtra Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra", type = "binary")
library(gridExtra)

cat("✅ Alle Pakete wurden erfolgreich geladen.\n")

# 2) Ordnerstruktur erstellen, wenn noch nicht vorhanden
cat("🔄 Überprüfe, ob der Ordner 'export/05_korrelation' existiert...\n")
output_dir <- "export/05_korrelation"
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
cat("🔄 Berechnung der Korrelationsmatrix...\n")

# Subset für Korrelationsanalyse erstellen
subset_cor <- subset(daten, select = c(Operationsdauer, Blutverlust, Komplikationsrisiko))

# Korrelationsmatrix berechnen
cor_matrix <- cor(subset_cor)

# Korrelationswerte ausgeben
cat("----------------------------------------- \n")
cat("Korrelationskoeffizienten: \n")
cat("----------------------------------------- \n")
cat("Operationsdauer & Blutverlust:", round(cor(subset_cor$Operationsdauer, subset_cor$Blutverlust), 3), "\n")
cat("Operationsdauer & Komplikationsrisiko:", round(cor(subset_cor$Operationsdauer, subset_cor$Komplikationsrisiko), 3), "\n")
cat("Blutverlust & Komplikationsrisiko:", round(cor(subset_cor$Blutverlust, subset_cor$Komplikationsrisiko), 3), "\n")
cat("----------------------------------------- \n")
cat("----------------------------------------- \n")
# 5) Plots in R-Studio unter Reiter Plots ausgeben
cat("📊 Alle Plots werden in R-Studio unter dem Reiter 'Plots' angezeigt.\n")

# Hilfsfunktion für Korrelationsplot mit Trendlinie
plot_correlation <- function(xvar, yvar, xlabel, ylabel, daten, title) {
  ggplot(daten, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.5, color = "gray40") +
    geom_smooth(method = "lm", color = "blue", linewidth = 1, se = FALSE) +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_white_report()
}

# Korrelationsplots erstellen und anzeigen
plot_ops_blut <- plot_correlation("Operationsdauer", "Blutverlust", "Operationsdauer (Min)", "Blutverlust (ml)", daten, "Korrelation: OP-Dauer & Blutverlust")
print(plot_ops_blut)

plot_ops_komp <- plot_correlation("Operationsdauer", "Komplikationsrisiko", "Operationsdauer (Min)", "Komplikationsrisiko", daten, "Korrelation: OP-Dauer & Komplikationsrisiko")
print(plot_ops_komp)

plot_blut_komp <- plot_correlation("Blutverlust", "Komplikationsrisiko", "Blutverlust (ml)", "Komplikationsrisiko", daten, "Korrelation: Blutverlust & Komplikationsrisiko")
print(plot_blut_komp)

# 6) Plots exportieren
cat("🔄 Exportiere die Korrelationsplots als PNG...\n")

ggsave(file.path(output_dir, "plot_ops_blut.png"), plot = plot_ops_blut, width = 7, height = 5)
ggsave(file.path(output_dir, "plot_ops_komp.png"), plot = plot_ops_komp, width = 7, height = 5)
ggsave(file.path(output_dir, "plot_blut_komp.png"), plot = plot_blut_komp, width = 7, height = 5)

cat("✅ Alle Plots wurden erfolgreich exportiert.\n")

# 7) Stargazer-Tabelle für die Korrelationsmatrix speichern und im RStudio Viewer anzeigen
cat("🔄 Erstelle stargazer-Tabelle für die Korrelationsmatrix als .html...\n")

# HTML-Datei für die Korrelationsmatrix erzeugen
html_output <- file.path(output_dir, "korrelationsmatrix.html")
capture.output(stargazer(cor_matrix, type = "html", out = html_output), file = NULL)

# HTML-Datei im RStudio Viewer anzeigen
if (interactive()) {
  viewer <- getOption("viewer")
  viewer(html_output)
}

cat("✅ stargazer-Tabelle wurde als .html-Datei erstellt und im RStudio Viewer angezeigt.\n")

# Warnungen wieder aktivieren
options(warn = 0)
