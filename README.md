# DIAnn Post-Processing Tool

A Shiny-based interactive tool for post-processing DIAnn search results, facilitating cosine similarity calculations, data visualization, and chimeric library generation for Data-Independent Acquisition (DIA) proteomics workflows.

---

## Table of Contents
1. [Overview](#overview)
2. [Features](#features)
3. [Installation](#installation)
4. [Usage](#usage)
5. [Examples](#examples)
6. [Contributing](#contributing)
7. [License](#license)
8. [Citation](#citation)

---
## Overview
The DIAnn Post-Processing Tool is designed to enhance the accuracy and reliability of peptide and protein identifications in DIA proteomics workflows. It integrates cosine similarity calculations, interactive data visualization, and chimeric library generation into a single, user-friendly platform. The tool addresses key challenges in post-processing DIAnn search results, enabling researchers to validate peptide identifications, refine spectral libraries, and improve downstream analyses.

---
## Features
- Cosine Similarity Calculation: Compute cosine similarity between DIAnn identifications and spectral libraries to assess peptide identification confidence.
- Interactive Data Visualization: Explore retention time distributions, fragment ion similarity plots, and other key metrics.
- Chimeric Library Generation: Combine fragment ion data from multiple libraries to create optimized spectral libraries for DIA analysis.
- User-Friendly Interface: Intuitive design with dynamic dropdowns, sliders, and export options.
- Compatibility: Supports DDA and DIA spectral libraries, as well as DIAnn report files.

---

## Installation
### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended for local installation)
- Required R packages (automatically installed during setup)

### Steps
1. Clone this repository:
   ```bash
   git clone https://github.com/your-username/DIAnn-PostProcessing-Tool.git
2. Open the project in RStudio.

3. Install the required R packages by running the following command in the R console:
install.packages(c("shiny", "dplyr", "ggplot2", "data.table", "shinyjs", "DT", "ggrepel"))
4. Launch the Shiny app by running:
shiny::runApp("path/to/DIAnn-PostProcessing-Tool")
Alternatively, you can use the Docker image for a containerized deployment:

docker pull your-dockerhub-username/diann-postprocessing-tool
docker run -p 3838:3838 your-dockerhub-username/diann-postprocessing-tool

Usage
Upload Files:
Upload a DIAnn report file and a spectral library (DDA or DIA).
Specify parameters such as fragment ion matching tolerance and peak exclusion threshold.

Calculate Cosine Similarity:
Click the "Calculate" button to compute cosine similarity between the DIAnn report and the spectral library.
Filter results based on cosine similarity thresholds.

Visualize Data:
Explore retention time distributions, fragment ion similarity plots, and other visualizations.
Use dynamic dropdowns to select samples, proteins, and peptides.

Generate Chimeric Library:
Upload two cosine similarity files and their corresponding libraries.
Compare cosine similarity distributions and generate an optimized chimeric library.

Export Results:
Export cosine similarity results, visualizations, and chimeric libraries for further analysis.

Examples
Example 1: Cosine Similarity Calculation
Upload a DIAnn report and a DDA spectral library.
Set the fragment ion matching tolerance to 0.05 and the peak exclusion threshold to 0.
Click "Calculate" to compute cosine similarity.
Filter results using the slider and export the data.

Example 2: Chimeric Library Generation
Upload two cosine similarity files and their corresponding libraries.
Compare cosine similarity distributions and compute median absolute deviation (MAD).
Generate and export the chimeric library.

Contributing
We welcome contributions from the community! If you would like to contribute to the development of this tool, please follow these steps:

1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Submit a pull request with a detailed description of your changes.

For bug reports or feature requests, please open an issue on the GitHub repository.

License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

Citation
If you use this tool in your research, please cite the following publication:

[Your Manuscript Title]. [Your Name et al.]. [Journal Name]. [Year].
DOI: [Insert DOI]

GitHub Repository: https://github.com/amrelguoshy-BBC/DIA-NN_postprocessing_tool
Contact
For questions or feedback, please contact:
Amr Elguoshy
elguoshyamr@gmail.com
Niigata University

