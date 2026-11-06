# Student Lifestyle and Academic Performance

A group statistical-computing project using R to explore relationships between study habits, school context, and academic performance.

The project brings together three student datasets and presents the analysis through ggplot2 charts, interactive Plotly views, and an R Markdown slide presentation. Its purpose is to practice data cleaning and communicate patterns clearly, rather than predict an individual student's results.

## Start here

- [Final Presentation.Rmd](Final%20Presentation.Rmd): the presentation source and selected visualizations.
- [Final Submission Code1.R](Final%20Submission%20Code1.R): the full exploratory analysis.
- [RStudio project](Statistical%20Computing%20Project.Rproj): opens the repository as an R project.

## Example output

![Grouped bar chart comparing private- and public-school record counts across high, low, and medium family-income categories](Family%20Income%20and%20School%20Type.png)

*Saved chart from the exploratory analysis of `StudentPerformanceFactors.csv`. Public-school records outnumber private-school records in each income group in this dataset. The bars show counts, not population percentages or evidence that income causes school choice.*

## What the project explores

- Hours studied, attendance, sleep, motivation, and exam scores.
- Family income, school type, and access to school resources.
- Reading, writing, and math score distributions.
- Test preparation and parental involvement.
- Descriptive comparisons across groups represented in the datasets.

The code includes score-range checks, ordered categories, long-format reshaping, scatter plots, box plots, faceted charts, and interactive tooltips.

## Data included

The input files are stored in ZIP archives:

| Archive | CSV inside | Rows |
| --- | --- | ---: |
| `performance_fac.zip` | `StudentPerformanceFactors.csv` | 6,607 |
| `exam_scores.zip` | `student_exam_scores.csv` | 200 |
| `student_performace.zip` | `StudentsPerformance.csv` | 1,000 |

The analysis expects the third file to be named `AcademicPerformance.csv`. The setup below creates that copy after extraction.

The original code credits Kaggle sources by Lai Ng and Muhammad Shoaib for the first two datasets. Exact source links, versions, and licensing details are not recorded for all three inputs.

## Run locally

Use R 4.1 or later, which supports the `|>` pipe used in the code. RStudio is helpful for opening the project and rendering the presentation.

1. Clone or download this repository.
2. Open `Statistical Computing Project.Rproj` in RStudio.
3. Run the following from the repository root in the R console.

```r
install.packages(c(
  "tidyverse", "ggthemes", "scales", "ggrepel",
  "patchwork", "plotly", "rmarkdown", "knitr"
))

unzip("performance_fac.zip", exdir = ".")
unzip("exam_scores.zip", exdir = ".")
unzip("student_performace.zip", exdir = ".")

if (!file.exists("AcademicPerformance.csv")) {
  file.copy("StudentsPerformance.csv", "AcademicPerformance.csv")
}
```

Open `Final Submission Code1.R` and run the analysis in sections to explore the plots.

To render the presentation, use RStudio's **Knit** button or:

```r
rmarkdown::render("Final Presentation.Rmd")
```

Rendering requires Pandoc, which is normally included with RStudio. The output format is an HTML Slidy presentation. Package versions are not pinned, and the full R workflow has not been re-run as part of this documentation update.

## Technical and design choices

- **Explicit score checks:** Values outside the chosen score ranges are replaced with missing values in the cleaned data frames.
- **Ordered categories:** Low, medium, and high levels are placed in a meaningful order for comparison.
- **Long-format data:** Reading, writing, and math scores are reshaped so the same chart design can be reused across subjects.
- **Facets, jitter, and transparency:** Help separate groups and reduce overlapping points.
- **Color and shape:** Some charts use both to distinguish motivation levels, with a colorblind-friendly palette.
- **Plotly views:** Add hover details to selected ggplot2 charts.
- **R Markdown:** Keeps the presentation close to the code that creates its figures.

## Interpretation and limitations

- These are exploratory associations, not evidence that one factor causes another.
- The three datasets have different variables and score scales. They should not be treated as one directly comparable student population.
- Group averages do not describe every person in that group and should not guide decisions about individual students.
- Some figures use original data while others use cleaned copies. Check the data frame used in each section before interpreting a result.
- There is no formal causal analysis, validated prediction model, automated test suite, or pinned package environment.

## Team

The presentation credits Elsa Mathew-Lewis, Emily Sun, Alec Anghel, and Aoun Hakim. This repository contains the shared analysis and presentation; it does not assign every chart to an individual contributor.
