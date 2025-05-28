# ---------------------------------------------
# 03_lagemasse.R – Lagemaße, Plots (de/en) & APA-Tabelle
cat("----------------------------------------- \n")
cat("03_lagemasse.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# Nur Binärpakete installieren
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
installiere_binär("dplyr")

# Datensatz laden
load("data/chirurgische_komplikationen.RData")

# Exportverzeichnis
export_dir <- "export/03_lagemasse"
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)

# Variablennamen ins Englische umbenennen
daten <- daten %>%
  rename(
    Duration_of_Surgery = Operationsdauer,
    Blood_Loss = Blutverlust,
    Complication_Risk = Komplikationsrisiko
  )

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

# Lagemaße berechnen
berechne_lagemasse <- function(x, name) {
  data.frame(
    Variable = name,
    Mean = round(mean(x), 2),
    Median = round(median(x), 2),
    Mode = modus(x)
  )
}

# Plot-Funktion mit Sprachumschaltung
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

# Lagemaße berechnen
lagemasse_df <- rbind(
  berechne_lagemasse(daten$Duration_of_Surgery, "Operationsdauer"),
  berechne_lagemasse(daten$Blood_Loss, "Blutverlust"),
  berechne_lagemasse(daten$Complication_Risk, "Komplikationsrisiko")
)

colnames(lagemasse_df) <- c("Variable", "Mean", "Median", "Mode")

# Ausgabe in Konsole
print(lagemasse_df)

# ===== Plots speichern (zweisprachig) =====

# Duration of Surgery
ggsave(file.path(export_dir, "lagemasse_duration_en.png"),
       plot = plot_lagemasse("Duration_of_Surgery", "Duration of Surgery (minutes)", "Density", daten, "en"),
       width = 8, height = 6)

ggsave(file.path(export_dir, "lagemasse_duration_de.png"),
       plot = plot_lagemasse("Duration_of_Surgery", "Operationsdauer (Minuten)", "Dichte", daten, "de"),
       width = 8, height = 6)

# Blood Loss
ggsave(file.path(export_dir, "lagemasse_bloodloss_en.png"),
       plot = plot_lagemasse("Blood_Loss", "Blood Loss (ml)", "Density", daten, "en"),
       width = 8, height = 6)

ggsave(file.path(export_dir, "lagemasse_bloodloss_de.png"),
       plot = plot_lagemasse("Blood_Loss", "Blutverlust (ml)", "Dichte", daten, "de"),
       width = 8, height = 6)

# Complication Risk
ggsave(file.path(export_dir, "lagemasse_risk_en.png"),
       plot = plot_lagemasse("Complication_Risk", "Complication Risk (scale)", "Density", daten, "en"),
       width = 8, height = 6)

ggsave(file.path(export_dir, "lagemasse_risk_de.png"),
       plot = plot_lagemasse("Complication_Risk", "Komplikationsrisiko (Skalenwert)", "Dichte", daten, "de"),
       width = 8, height = 6)

# ===== APA-Tabelle erstellen =====
ft <- flextable(lagemasse_df)
ft <- set_caption(ft, "Table 1\nMeasures of Central Tendency (Mean, Median, Mode) for the Examined Variables")
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(doc, "Measures of Central Tendency", style = "heading 1")
doc <- body_add_par(doc, "The table below presents the mean, median, and mode for each variable in the dataset.", style = "Normal")
doc <- body_add_flextable(doc, ft)

# Speichern
output_docx <- file.path(export_dir, "central_tendency_table_apa.docx")
print(doc, target = output_docx)
cat("\n✅ Word-Datei gespeichert unter:", output_docx, "\n")
