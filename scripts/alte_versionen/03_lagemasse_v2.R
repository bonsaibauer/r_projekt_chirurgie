# ---------------------------------------------
# 03_lagemasse.R
# Lagemaße berechnen, grafisch darstellen und APA-Tabelle erzeugen (ohne Rtools)
# ---------------------------------------------

# Nur Binärpakete installieren (kein Rtools erforderlich!)
installiere_binär <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, type = "binary")
  }
  library(pkg, character.only = TRUE)
}

# Pakete laden
installiere_binär("ggplot2")
installiere_binär("officer")
installiere_binär("flextable")

# Datensatz laden
load("data/chirurgische_komplikationen.RData")

# Exportverzeichnis
export_dir <- "export/03_lagemasse"
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)

# Weißes ggplot2-Theme
theme_white_report <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white", color = "black"),
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold")
    )
}

# Modus-Funktion
modus <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Berechnung Lagemaße
berechne_lagemasse <- function(x, name) {
  data.frame(
    Variable = name,
    Mittelwert = round(mean(x), 2),
    Median = round(median(x), 2),
    Modus = modus(x)
  )
}

# Plot-Funktion
plot_lagemasse <- function(var_name, var_label, daten) {
  x <- daten[[var_name]]
  m <- mean(x)
  md <- median(x)
  mo <- modus(x)
  
  ggplot(daten, aes(x = x)) +
    geom_histogram(aes(y = ..density..), binwidth = 20, fill = "gray80", color = "black") +
    geom_density(color = "darkgray", linewidth = 1) +
    geom_vline(xintercept = m, color = "blue", linetype = "dashed") +
    geom_vline(xintercept = md, color = "red", linetype = "solid") +
    geom_vline(xintercept = mo, color = "green", linetype = "dotted") +
    labs(
      title = paste("Lagemasse:", var_label),
      subtitle = "Blau = Mittelwert, Rot = Median, Grün = Modus",
      x = var_label,
      y = "Dichte"
    ) +
    theme_white_report()
}

# Lagemaße berechnen
lagemasse_df <- rbind(
  berechne_lagemasse(daten$Operationsdauer, "Operationsdauer"),
  berechne_lagemasse(daten$Blutverlust, "Blutverlust"),
  berechne_lagemasse(daten$Komplikationsrisiko, "Komplikationsrisiko")
)

# Ausgabe
print(lagemasse_df)

# Plots speichern
ggsave(file.path(export_dir, "lagemasse_operationsdauer.png"),
       plot_lagemasse("Operationsdauer", "Operationsdauer (Minuten)", daten), width = 8, height = 6)

ggsave(file.path(export_dir, "lagemasse_blutverlust.png"),
       plot_lagemasse("Blutverlust", "Blutverlust (ml)", daten), width = 8, height = 6)

ggsave(file.path(export_dir, "lagemasse_komplikationsrisiko.png"),
       plot_lagemasse("Komplikationsrisiko", "Komplikationsrisiko (Skalenwert)", daten), width = 8, height = 6)

# Word-Tabelle erstellen
ft <- flextable(lagemasse_df)
ft <- set_caption(ft, "Tabelle 1\nLagemaße (Mittelwert, Median, Modus) der untersuchten Variablen")
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(doc, "Lagemaße der untersuchten Variablen", style = "heading 1")
doc <- body_add_par(doc, "Die folgende Tabelle zeigt die berechneten Lagemaße für jede Variable:", style = "Normal")
doc <- body_add_flextable(doc, ft)

# Speichern
output_docx <- file.path(export_dir, "lagemasse_tabelle_apa.docx")
print(doc, target = output_docx)
cat("\n✅ Word-Datei gespeichert unter:", output_docx, "\n")
