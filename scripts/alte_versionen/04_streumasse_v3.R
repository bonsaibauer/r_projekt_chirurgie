# Pakete installieren und laden
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer", type = "binary")
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable", type = "binary")
library(officer)
library(flextable)

# Streumaße berechnen
streumasse_df <- data.frame(
  Variable = c("Operationsdauer", "Blutverlust", "Komplikationsrisiko"),
  Range = c(paste(range_ops, collapse = "–"),
            paste(range_blut, collapse = "–"),
            paste(range_komp, collapse = "–")),
  SD = c(round(sd_ops, 2), round(sd_blut, 2), round(sd_komp, 2)),
  Variance = c(round(var_ops, 2), round(var_blut, 2), round(var_komp, 2)),
  IQR = c(round(iqr_ops, 2), round(iqr_blut, 2), round(iqr_komp, 2)),
  MAD = c(round(mad_ops, 2), round(mad_blut, 2), round(mad_komp, 2))
)

# Flextable erstellen
ft_streu <- flextable(streumasse_df)
ft_streu <- set_caption(ft_streu, "Tabelle 2\nStreumaße der untersuchten Variablen")
ft_streu <- autofit(ft_streu)

# Word-Dokument erstellen und speichern
doc_streu <- read_docx()
doc_streu <- body_add_par(doc_streu, "Streumaße der untersuchten Variablen", style = "heading 1")
doc_streu <- body_add_par(doc_streu, "Die folgende Tabelle zeigt die berechneten Streumaße für jede Variable:", style = "Normal")
doc_streu <- body_add_flextable(doc_streu, ft_streu)

# Dokument speichern
docx_path_streu <- file.path(output_dir, "streumasse_tabelle_apa.docx")
print(doc_streu, target = docx_path_streu)
cat("\n✅ APA-Tabelle gespeichert unter:", docx_path_streu, "\n")
