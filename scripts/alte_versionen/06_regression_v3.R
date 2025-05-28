# ---------------------------------------------
# 06_regression.R
cat("----------------------------------------- \n")
cat("06_regression.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# Lade Pakete
library(ggplot2)
library(stargazer)  # Ersetzt apaTables durch stargazer

# Lade den Datensatz
load("data/chirurgische_komplikationen.RData")

# Exportverzeichnis
output_dir <- "export/06_regression"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Theme definieren
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

# Regressionsplots speichern
plot_regression <- function(xvar, yvar, daten, title, xlabel, ylabel, filename) {
  p <- ggplot(daten, aes(x = !!sym(xvar), y = !!sym(yvar))) +
    geom_point(alpha = 0.5, color = "gray40") +
    geom_smooth(method = "lm", color = "blue", se = TRUE) +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_white_report()
  ggsave(file.path(output_dir, filename), plot = p, width = 8, height = 6)
}

# Einzelne Regressionen
reg1 <- lm(Komplikationsrisiko ~ Blutverlust, data = daten)
reg2 <- lm(Komplikationsrisiko ~ Operationsdauer, data = daten)
reg3 <- lm(Komplikationsrisiko ~ Blutverlust + Operationsdauer, data = daten)

# Plots erstellen
plot_regression("Blutverlust", "Komplikationsrisiko", daten,
                "Regression: Komplikationsrisiko ~ Blutverlust",
                "Blutverlust (ml)", "Komplikationsrisiko", "reg_blut.png")

plot_regression("Operationsdauer", "Komplikationsrisiko", daten,
                "Regression: Komplikationsrisiko ~ Operationsdauer",
                "Operationsdauer (Minuten)", "Komplikationsrisiko", "reg_ops.png")

# Regressions-Ergebnisse mit stargazer erzeugen und als .html speichern
stargazer(reg1, reg2, reg3, type = "html", 
          title = "Regressionstabelle: Komplikationsrisiko ~ Blutverlust / Operationsdauer",
          out = file.path(output_dir, "regressionstabelle.html"))

# Ausgabe der Regressionskoeffizienten in der Konsole
cat("Regression: Komplikationsrisiko ~ Blutverlust\n")
print(summary(reg1))
cat("\nRegression: Komplikationsrisiko ~ Operationsdauer\n")
print(summary(reg2))
cat("\nRegression: Komplikationsrisiko ~ Blutverlust + Operationsdauer\n")
print(summary(reg3))
