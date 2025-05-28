# ---------------------------------------------
# 03_lagemasse.R
# Berechnung und grafische Darstellung der Lagemaße
# inkl. Histogramme und stargazer-Tabelle
cat("----------------------------------------- \n")
cat("03_lagemasse.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# 1) Packages laden und installieren, wenn nötig
cat("🔄 Überprüfe, ob die Pakete ggplot2, stargazer und dplyr installiert sind...\n")

# Warnungen vorübergehend deaktivieren


# 1.1) ggplot2 Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
} 
library(ggplot2)

# 1.2) stargazer Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("stargazer", quietly = TRUE)) {
  install.packages("stargazer")
} 
library(stargazer)

# 1.3) dplyr Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
} 
library(dplyr)

cat("✅ Alle Pakete wurden erfolgreich geladen.\n")

# 2) Ordnerstruktur erstellen, wenn noch nicht vorhanden
cat("🔄 Überprüfe, ob der Ordner 'export/03_lagemasse' existiert...\n")
export_dir <- "export/03_lagemasse"

if (!dir.exists(export_dir)) {
  dir.create(export_dir, recursive = TRUE)
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
cat("🔄 Lade den Datensatz 'chirurgische_komplikationen.RData'...\n")
load("data/chirurgische_komplikationen.RData")

# ---------------------------------------------
# Lagemaße berechnen

modus <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

berechne_lagemasse <- function(x, name) {
  data.frame(
    Variable = name,
    Mean = round(mean(x), 2),
    Median = round(median(x), 2),
    Mode = modus(x)
  )
}

# Lagemaße berechnen
lagemaße_df <- rbind(
  berechne_lagemasse(daten$Operationsdauer, "Operationsdauer"),
  berechne_lagemasse(daten$Blutverlust, "Blutverlust"),
  berechne_lagemasse(daten$Komplikationsrisiko, "Komplikationsrisiko")
)

cat("✅ Lagemaße wurden berechnet.\n")

# Ausgabe der berechneten Lagemaße in der Konsole (untereinander und ohne HTML-Tags)
cat("\nBerechnete Lagemaße:\n")
cat("\nOperationsdauer:\n")
cat("Mean: ", lagemaße_df$Mean[1], "\n")
cat("Median: ", lagemaße_df$Median[1], "\n")
cat("Mode: ", lagemaße_df$Mode[1], "\n")

cat("\nBlutverlust:\n")
cat("Mean: ", lagemaße_df$Mean[2], "\n")
cat("Median: ", lagemaße_df$Median[2], "\n")
cat("Mode: ", lagemaße_df$Mode[2], "\n")

cat("\nKomplikationsrisiko:\n")
cat("Mean: ", lagemaße_df$Mean[3], "\n")
cat("Median: ", lagemaße_df$Median[3], "\n")
cat("Mode: ", lagemaße_df$Mode[3], "\n")

# ---------------------------------------------
# Plots für Lagemaße erstellen

plot_lagemasse <- function(var_name, var_label, y_label, daten, sprache = "en") {
  x <- daten[[var_name]]
  m <- mean(x)
  md <- median(x)
  mo <- modus(x)
  
  if (sprache == "de") {
    titel <- paste("Lagemaße:", var_label)
    untertitel <- "Blau = Mittelwert, Rot = Median, Grün = Modus"
  } else {
    titel <- paste("Measures of Central Tendency:", var_label)
    untertitel <- "Blue = Mean, Red = Median, Green = Mode"
  }
  
  ggplot(daten, aes(x = x)) +
    geom_histogram(aes(y = ..density..), binwidth = 20, fill = "gray80", color = "black") +
    geom_density(color = "darkgray", linewidth = 1) +
    geom_vline(xintercept = m, color = "blue", linetype = "dashed") +
    geom_vline(xintercept = md, color = "red", linetype = "solid") +
    geom_vline(xintercept = mo, color = "green", linetype = "dotted") +
    labs(
      title = titel,
      subtitle = untertitel,
      x = var_label,
      y = y_label
    ) +
    theme_white_report()
}

# Plots erstellen und speichern
cat("🔄 Erstelle Plots für alle Lagemaße...\n")

# Duration of Surgery
p_duration <- plot_lagemasse("Operationsdauer", "Operationsdauer (Minuten)", "Dichte", daten, "de")
ggsave(file.path(export_dir, "lagemasse_duration_de.png"), plot = p_duration, width = 8, height = 6)
print(p_duration)  # Plot im Reiter "Plots" anzeigen

# Blood Loss
p_bloodloss <- plot_lagemasse("Blutverlust", "Blutverlust (ml)", "Dichte", daten, "de")
ggsave(file.path(export_dir, "lagemasse_bloodloss_de.png"), plot = p_bloodloss, width = 8, height = 6)
print(p_bloodloss)  # Plot im Reiter "Plots" anzeigen

# Complication Risk
p_risk <- plot_lagemasse("Komplikationsrisiko", "Komplikationsrisiko (Skalenwert)", "Dichte", daten, "de")
ggsave(file.path(export_dir, "lagemasse_risk_de.png"), plot = p_risk, width = 8, height = 6)
print(p_risk)  # Plot im Reiter "Plots" anzeigen

cat("✅ Alle Plots wurden erstellt und gespeichert.\n")

# ---------------------------------------------
# Tabelle mit stargazer ausgeben

cat("🔄 Erstelle stargazer-Tabelle für die Lagemaße...\n")

# Stargazer für die Lagemaße in einem schönen Format
stargazer(lagemaße_df, type = "html", out = file.path(export_dir, "lagemasse_tabelle.html"))

cat("✅ stargazer-Tabelle wurde erstellt und gespeichert.\n")

# 5) Plots in R-Studio unter Reiter Plots ausgeben
cat("📊 Alle Plots werden in R-Studio unter dem Reiter 'Plots' angezeigt.\n")

# Nach dem Plot die Warnungen wieder aktivieren

