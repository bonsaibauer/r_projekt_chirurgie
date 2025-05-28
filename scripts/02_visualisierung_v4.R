# ---------------------------------------------
# 02_visualisierung.R
# Grafische Darstellung der Daten mit ggplot2
# inkl. Histogramme und kombinierter Beeswarmplot
cat("----------------------------------------- \n")
cat("02_visualisierung.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# 1) Packages laden und installieren, wenn nötig
cat("🔄 Überprüfe, ob die Pakete ggplot2, ggbeeswarm und reshape2 installiert sind...\n")

# Warnungen vorübergehend deaktivieren
options(warn = -1)

# 1.1) ggplot2 Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
} 
library(ggplot2)

# 1.2) ggbeeswarm Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
  install.packages("ggbeeswarm")
} 
library(ggbeeswarm)

# 1.3) reshape2 Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
} 
library(reshape2)

cat("✅ Alle Pakete wurden erfolgreich geladen.\n")

# 2) Ordnerstruktur erstellen, wenn noch nicht vorhanden
cat("🔄 Überprüfe, ob der Ordner 'export/02_visualisierung' existiert...\n")
output_dir <- "export/02_visualisierung"

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
cat("🔄 Lade den Datensatz 'chirurgische_komplikationen.RData'...\n")
load("data/chirurgische_komplikationen.RData")

# ---------------------------------------------
# Histogramme

# Histogramm: Operationsdauer
p_hist_operationsdauer <- ggplot(daten, aes(x = Operationsdauer)) +
  geom_histogram(binwidth = 20, fill = "lightblue", color = "black") +
  labs(title = "Histogramm: Operationsdauer", x = "Minuten", y = "Frequenz") +
  xlim(30, 300) +
  theme_white_report()

# Histogramm: Blutverlust
p_hist_blutverlust <- ggplot(daten, aes(x = Blutverlust)) +
  geom_histogram(binwidth = 20, fill = "lightgreen", color = "black") +
  labs(title = "Histogramm: Blutverlust", x = "Milliliter", y = "Frequenz") +
  xlim(30, 300) +
  theme_white_report()

# Histogramm: Komplikationsrisiko
p_hist_komplikationsrisiko <- ggplot(daten, aes(x = Komplikationsrisiko)) +
  geom_histogram(binwidth = 20, fill = "lightcoral", color = "black") +
  labs(title = "Histogramm: Komplikationsrisiko", x = "Skalenwert", y = "Frequenz") +
  xlim(30, 300) +
  theme_white_report()

# Plots anzeigen und speichern
plots <- list(p_hist_operationsdauer, p_hist_blutverlust, p_hist_komplikationsrisiko)
names(plots) <- c("hist_operationsdauer.png", "hist_blutverlust.png", "hist_komplikationsrisiko.png")

for (plot_name in names(plots)) {
  print(plots[[plot_name]])  # Plot im Reiter "Plots" anzeigen
  ggsave(file.path(output_dir, plot_name), plot = plots[[plot_name]], width = 8, height = 6)
}

cat("✅ Alle Histogramme wurden erstellt und gespeichert.\n")

# ---------------------------------------------
# Gemeinsamer Beeswarmplot für alle Merkmale

daten_long <- melt(daten,
                   measure.vars = c("Operationsdauer", "Blutverlust", "Komplikationsrisiko"),
                   variable.name = "Merkmal",
                   value.name = "Wert")

p_beeswarm_all <- ggplot(daten_long, aes(x = Merkmal, y = Wert)) +
  geom_beeswarm(priority = "density", color = "darkblue", size = 1.5, alpha = 0.6) +
  labs(
    title = "Beeswarmplot: Vergleich der Merkmale",
    x = "Merkmal",
    y = "Wert (30–300)"
  ) +
  ylim(30, 300) +
  theme_white_report()

# Plot anzeigen und speichern
print(p_beeswarm_all)  # Plot im Reiter "Plots" anzeigen
ggsave(file.path(output_dir, "beeswarm_alle_merkmale.png"),
       plot = p_beeswarm_all, width = 9, height = 6)

cat("✅ Beeswarmplot für alle Merkmale wurde erstellt und gespeichert.\n")

# 5) Plots in R-Studio unter Reiter Plots ausgeben
cat("📊 Alle Plots werden in R-Studio unter dem Reiter 'Plots' angezeigt.\n")

# Nach dem Plot die Warnungen wieder aktivieren
options(warn = 0)
