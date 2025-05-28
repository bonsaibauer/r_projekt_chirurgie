# 📊 Thesis: Influencing Factors on Surgical Complication Risk

This thesis investigates the relationship between **operation duration**, **intraoperative blood loss**, and **complication risk** in surgical procedures based on simulated data. The analysis is performed entirely with the **R** statistical software and is designed for reproducibility.

## 🔍 Objective of the Analysis

- Creation of a simulated dataset with 3 continuous variables
- Calculation of measures of central tendency and dispersion
- Analysis of correlations and linear relationships
- Visualization of results
- Interpretation in a medical context

## 🗂️ Project Structure

```
r_projekt_chirurgie/
├── hausarbeit.Rmd                      # Main document for the thesis
├── data/                            # Simulated dataset
│   └── surgical_complications.RData
├── export/                          # Automatically generated plots & tables
├── scripts/                         # Evaluation scripts (organized by tasks)
├── README.md                        # This file
└── renv/                            # Reproducible R environment
```

## 💡 Contents of the R Scripts

| File                              | Content                                        |
|-----------------------------------|------------------------------------------------|
| `01_data_generation.R`            | Creation of the dataset with random numbers    |
| `02_visualization.R`              | Histograms and boxplots                       |
| `03_central_tendency.R`           | Mean, median, mode                           |
| `04_dispersion_measures.R`        | Variance, standard deviation, range          |
| `05_correlation.R`                | Pearson correlation, visualization           |
| `06_regression.R`                 | Regression models and R² values               |

## ⚙️ Setup & Execution

Clone the repository:
   ```bash
   git clone https://github.com/bonsaibauer/r_projekt_chirurgie.git
   ```
   
- Then open R-Studio and open r_projekt_chirurgie.Rproj.
- Second step, open hausarbeit.Rmd and install packages and renv::restore()

```bash
   renv::restore()
   ```

## 🔁 Reproducibility

- The project uses `renv` for managing R packages
- Analyses are modular and can be executed individually
- Output plots and tables are automatically saved in `export/`

## 📚 License and Use

This project is part of a study assignment at DHBW Mannheim (Program TMED23, Statistics Module). Reuse for academic purposes is allowed with proper citation.

For the assignment details, visit the [original thesis task](https://github.com/bonsaibauer/r_projekt_chirurgie/blob/main/hausarbeit-aufgabenstellung.pdf).
