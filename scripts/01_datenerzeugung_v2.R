# ---------------------------------------------
# 01_datenerzeugung.R
cat("----------------------------------------- \n")
cat("01_datenerzeugung.R \n")
cat("----------------------------------------- \n")
# ---------------------------------------------

# 1) Packages laden und installieren, wenn nötig

# 1.1) here-Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
  cat("✅ Paket 'here' wurde installiert und geladen.\n")
} else {
  library(here)
  cat("✅ Paket 'here' wurde geladen.\n")
}

# 2) Ordnerstruktur erstellen, wenn noch nicht vorhanden

# 2.1) Zielordner "data" im Projekt-Root
ordner <- here::here("data")  # Diese Zeile muss NACH dem Laden von 'here' kommen!

# 2.2) Ordner "data" erstellen, wenn er noch nicht existiert
if (!dir.exists(ordner)) {
  dir.create(ordner, recursive = TRUE)
  cat("✅ Ordner 'data' wurde erfolgreich erstellt.\n")
} else {
  cat("✅ Ordner 'data' existiert bereits.\n")
}

# 3) Einheitliches Theme für Plots

# 4) Funktionen berechnen und Datenerzeugung

# 4.1) Zufallszahlengenerator für Reproduzierbarkeit setzen
set.seed(123)

# 4.2) Dateipfade
dateiname_rdata <- file.path(ordner, "chirurgische_komplikationen.RData")
dateiname_csv <- file.path(ordner, "chirurgische_komplikationen.csv")

# 4.3) Prüfen, ob beide Dateien bereits existieren
if (!file.exists(dateiname_rdata) && !file.exists(dateiname_csv)) {
  # 4.4) Anzahl der Beobachtungen
  n <- 500
  
  # 4.5) Erzeugung der Zufallsvariablen im Bereich 30–300
  operationsdauer <- sample(30:300, n, replace = TRUE)
  blutverlust <- sample(30:300, n, replace = TRUE)
  komplikationsrisiko <- sample(30:300, n, replace = TRUE)
  cat("📌 Zufallsdaten für Operationsdauer, Blutverlust und Komplikationsrisiko wurden erfolgreich erzeugt.\n")
  
  # 4.6) Zusammenfassung als Data Frame
  daten <- data.frame(
    Operationsdauer = operationsdauer,
    Blutverlust = blutverlust,
    Komplikationsrisiko = komplikationsrisiko
  )
  cat("📌 Datenframe mit den Zufallsdaten wurde erstellt.\n")
  
  # 4.7) Speichern der Daten
  save(daten, file = dateiname_rdata)
  write.csv(daten, file = dateiname_csv, row.names = FALSE)
  cat("✅ Datendateien wurden erfolgreich gespeichert (RData und CSV).\n")
  
  # 4.8) Ausgabe der ersten Zeilen
  cat("📌 Erste 6 Zeilen des generierten Datensatzes:\n")
  cat(paste(capture.output(head(daten)), collapse = "\n"), "\n")
} else {
  # **Neuer Abschnitt: Wenn die Datendateien existieren, wird nur das Laden durchgeführt.**
  cat("❗ Datendateien existieren bereits. Datenerzeugung wird übersprungen.\n")
  # 4.9) Daten aus der bestehenden Datei laden, wenn sie bereits existieren
  load(dateiname_rdata)
  
  # **Ausgabe der ersten 6 Zeilen des geladenen Datensatzes mit cat**
  cat("📌 Erste 6 Zeilen des geladenen Datensatzes:\n")
  cat(paste(capture.output(head(daten)), collapse = "\n"), "\n")
}


# **5) Plots in R-Studio unter Reiter Plots ausgeben**

# **6) Plots exportieren**

# 7) Alle berechneten Werte roh ausgeben
