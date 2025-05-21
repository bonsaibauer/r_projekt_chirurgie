# ---------------------------------------------
# 01_datenerzeugung.R
# ---------------------------------------------

# Zufallszahlengenerator für Reproduzierbarkeit setzen
set.seed(123)

# here-Paket laden (automatisch installieren, wenn nötig)
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# Zielordner "data" im Projekt-Root
ordner <- here::here("data")  # Diese Zeile muss NACH dem Laden von 'here' kommen!

# Dateipfade
dateiname_rdata <- file.path(ordner, "chirurgische_komplikationen.RData")
dateiname_csv <- file.path(ordner, "chirurgische_komplikationen.csv")

# Prüfen, ob beide Dateien bereits existieren
if (!file.exists(dateiname_rdata) && !file.exists(dateiname_csv)) {
  # Anzahl der Beobachtungen
  n <- 500
  
  # Erzeugung der Zufallsvariablen im Bereich 30–300
  operationsdauer <- sample(30:300, n, replace = TRUE)
  blutverlust <- sample(30:300, n, replace = TRUE)
  komplikationsrisiko <- sample(30:300, n, replace = TRUE)
  
  # Zusammenfassung als Data Frame
  daten <- data.frame(
    Operationsdauer = operationsdauer,
    Blutverlust = blutverlust,
    Komplikationsrisiko = komplikationsrisiko
  )
  
  # Ordner "data" erstellen, wenn er noch nicht existiert
  if (!dir.exists(ordner)) {
    dir.create(ordner, recursive = TRUE)
  }
  
  # Speichern der Daten
  save(daten, file = dateiname_rdata)
  write.csv(daten, file = dateiname_csv, row.names = FALSE)
  
  # Ausgabe der ersten Zeilen
  print(head(daten))
} else {
  message("Datendateien existieren bereits. Datenerzeugung wurde übersprungen.")
}

