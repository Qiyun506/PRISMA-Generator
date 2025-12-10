Here is a comprehensive **README.md** for your application. It covers installation, usage, features, and the technical logic behind the layout.

-----

# Advanced Dynamic PRISMA Flow Generator

A robust R Shiny application designed to generate publication-quality **PRISMA flow diagrams** for systematic reviews and meta-analyses.

This tool allows researchers to dynamically define study processing stages, handle exclusions with detailed reasons, and incorporate manual additions (e.g., from other sources), all while maintaining strict visual alignment and formatting suitable for A4 PDF export.

## 🌟 Key Features

  * **Dynamic Flow Creation:** Add or remove processing stages (e.g., "Pre-Screening", "Eligibility Assessment") on the fly.
  * **Detailed Exclusions:** Log specific exclusion reasons and counts for every stage.
  * **Manual Additions:** Add side-entries for studies identified via other methods (with support for multi-line details like ClinicalTrials.gov IDs).
  * **Smart Layout Engine:**
      * **"Spine" Alignment:** Keeps the main flow (Title → Identified → Screened → Included) perfectly centered.
      * **Side Label Column:** Aligns phase labels (Identification, Screening, Included) in a strict left-hand column.
      * **Text Wrapping:** Custom left-alignment logic ensures multi-line exclusion reasons look professional.
  * **Project Management:** Save your work as a `.json` file and reload it later to continue editing.
  * **High-Res Export:** Download charts as **PNG** or **PDF** (A4 Portrait optimized).

## 📦 Prerequisites

Ensure you have R installed. You will need the following R packages:

```r
install.packages(c(
  "shiny",
  "DiagrammeR",
  "DiagrammeRsvg",
  "rsvg",
  "jsonlite",
  "glue",
  "stringr"
))
```

## 🚀 How to Run

1.  Save the application code as `app.R`.
2.  Open RStudio or your R terminal.
3.  Run the following command:

<!-- end list -->

```r
library(shiny)
runApp("path/to/app.R")
```

## 📖 Usage Guide

### 1\. Global Settings

  * **Chart Title:** Sets the main header. The layout engine anchors this directly above the first node.
  * **Footnote:** Add explanatory notes at the bottom. Supports multiple lines.
  * **Total Records:** The starting `n` number for the flow.
  * **Side Labels:** Toggle the blue "Identification", "Screening", "Included" labels on the left.

### 2\. Processing Stages

Define the linear flow of your review:

  * Click **"Add New Stage"** to create a step (e.g., "Screening").
  * **Exclusions:** Inside a stage, enter a "Reason" and "Count", then click the **`+`** button.
  * The chart automatically calculates the remaining records based on these exclusions.

### 3\. Manual Additions

Use this for records added outside the main database search (e.g., "Registers"):

  * **Category:** The title of the addition (e.g., "Additional Trials").
  * **Details:** Optional text field for IDs or notes (e.g., `NCT04194359`).
  * **Logic:** These are added *after* the exclusions of the previous stage and flow into the final "Included" pool.

### 4\. Saving & Exporting

  * **Save Project:** Downloads a `.json` file containing all your inputs, stages, and structure.
  * **Load Project:** Upload a previously saved `.json` file to restore your session.
  * **Download PDF:** Generates a vector-based PDF, ideal for submission in manuscripts.

## 🔧 Technical Implementation Details

For developers modifying the code, here is how the graph layout logic works:

### The "Spine" Strategy

To prevent the chart from shifting left/right when exclusions are added, the code uses Graphviz `group` attributes:

  * **Group `main`:** Assigned to the Title, all Main Flow Nodes, and the Final Node.
  * **Group `side`:** Assigned to the blue Side Labels.
  * **High Weight Edges:** Connections between `main` nodes have `weight=1000`. This forces the layout engine to draw a rigid vertical line (the spine), ensuring the Title is always centered relative to the final "Included" box.

### Text Alignment Fix

Standard Graphviz wrapping (`\n`) centers text. To force **Left Alignment** for exclusion lists:

  * The app uses a custom `wrap_txt` function.
  * It utilizes `str_wrap` for length calculation but replaces all newlines with `\l` (Graphviz code for "Left-aligned newline").

### Data Safety

  * `stringsAsFactors = FALSE` is strictly enforced in all data frames to prevent the "Details" text from converting to integer factors, which previously caused data loss during row deletion.
