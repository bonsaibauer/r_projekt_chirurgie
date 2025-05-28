# ---------------------------------------------
# 07_Regression.R
cat("----------------------------------------- \n")
cat("06_Regression.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# 1) Pakete installieren und laden, wenn nötig
cat("🔄 Überprüfe, ob die Pakete ggplot2, stargazer installiert sind...\n")

# Warnungen vorübergehend deaktivieren
options(warn = -1)

# 1.1) ggplot2 Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", type = "binary")
library(ggplot2)

# 1.2) stargazer Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer", type = "binary")
library(stargazer)

cat("✅ Alle Pakete wurden erfolgreich geladen.\n")

# 2) Ordnerstruktur erstellen, wenn noch nicht vorhanden
cat("🔄 Überprüfe, ob der Ordner 'export/07_regression' existiert...\n")
output_dir <- "export/06_regression"
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
cat("🔄 Berechnung der Regressionsmodelle...\n")

# Regressionsmodelle
reg1 <- lm(Komplikationsrisiko ~ Blutverlust, data = daten)  # Komplikationsrisiko ~ Blutverlust
reg2 <- lm(Komplikationsrisiko ~ Operationsdauer, data = daten)  # Komplikationsrisiko ~ Operationsdauer
reg3 <- lm(Blutverlust ~ Operationsdauer, data = daten)  # Operationsdauer ~ Blutverlust

# 5) Plots in R-Studio unter Reiter Plots ausgeben
cat("📊 Alle Plots werden in R-Studio unter dem Reiter 'Plots' angezeigt.\n")

# Regressionsplots speichern
plot_regression <- function(xvar, yvar, daten, title, xlabel, ylabel, filename) {
  p <- ggplot(daten, aes(x = !!sym(xvar), y = !!sym(yvar))) +
    geom_point(alpha = 0.5, color = "gray40") +
    geom_smooth(method = "lm", color = "blue", se = TRUE) +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_white_report()
  print(p)  # Plot im Reiter "Plots" anzeigen
  ggsave(file.path(output_dir, filename), plot = p, width = 8, height = 6)
}

# Plots für jede Regression erstellen
plot_regression("Blutverlust", "Komplikationsrisiko", daten,
                "Regression: Komplikationsrisiko ~ Blutverlust",
                "Blutverlust (ml)", "Komplikationsrisiko", "reg_blut.png")

plot_regression("Operationsdauer", "Komplikationsrisiko", daten,
                "Regression: Komplikationsrisiko ~ Operationsdauer",
                "Operationsdauer (Minuten)", "Komplikationsrisiko", "reg_ops.png")

# 5) Neuer Plot: Regression zwischen Operationsdauer und Blutverlust
plot_regression("Operationsdauer", "Blutverlust", daten,
                "Regression: Blutverlust ~ Operationsdauer",
                "Operationsdauer (Minuten)", "Blutverlust (ml)", "reg_ops_blut.png")

# 6) Plots exportieren
cat("🔄 Exportiere die Regressionsplots als PNG...\n")

ggsave(file.path(output_dir, "reg_blut.png"))
ggsave(file.path(output_dir, "reg_ops.png"))
ggsave(file.path(output_dir, "reg_ops_blut.png"))

cat("✅ Alle Plots wurden erfolgreich exportiert.\n")

# 7) Regressions-Ergebnisse mit stargazer erzeugen und als .html speichern
cat("🔄 Erstelle stargazer-Tabelle für die Regressionsmodelle als .html...\n")
stargazer(reg1, reg2, reg3, type = "html", 
          title = "Regressionstabelle: Komplikationsrisiko ~ Blutverlust / Operationsdauer",
          out = file.path(output_dir, "regressionstabelle.html"))

cat("✅ stargazer-Tabelle wurde als .html-Datei erstellt und gespeichert.\n")

# **Warum wurde der Pearson-Korrelationskoeffizient gewählt?**
cat("\nDer Pearson-Korrelationskoeffizient wurde gewählt, weil die Variablen 'Blutverlust' und 'Operationsdauer' metrische Daten sind und eine lineare Beziehung zwischen ihnen erwartet wird. \nDer Pearson-Koeffizient ist der am häufigsten verwendete Korrelationskoeffizient für metrische Daten und zeigt die Stärke und Richtung der linearen Beziehung zwischen zwei Variablen.\n")

# 8) HTML im RStudio Viewer anzeigen
cat("🔄 Zeige die Regressionstabelle im RStudio Viewer an...\n")
if (interactive()) {
  viewer <- getOption("viewer")
  viewer(file.path(output_dir, "regressionstabelle.html"))
}

# Ausgabe der Regressionskoeffizienten in der Konsole
cat("----------------------------------------- \n")
cat("----------------------------------------- \n")
cat("Regression: Komplikationsrisiko ~ Blutverlust\n")
print(summary(reg1))
cat("----------------------------------------- \n")
cat("----------------------------------------- \n")
cat("\nRegression: Komplikationsrisiko ~ Operationsdauer\n")
print(summary(reg2))
cat("----------------------------------------- \n")
cat("----------------------------------------- \n")
cat("\nRegression: Operationsdauer ~ Blutverlust\n")
print(summary(reg3))

# Warnungen wieder aktivieren
options(warn = 0)
