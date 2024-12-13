This project analyzes the impact of the Affordable Care Act (ACA) on healthcare access across Minnesota’s counties. The analysis focuses on demographic categories such as age, sex, and income, visualizing the data using an interactive Shiny app. The app allows users to select specific years, demographic groups, and insurance status variables to explore how healthcare coverage has changed in Minnesota.

The repository is organized as follows:

 - Data/: Contains the raw SAHIE data files (e.g., sahie_2010.csv, sahie_2011.csv, etc.) used in the analysis and any other data we used.
 - R/: Contains R scripts for data cleaning, analysis, and the creation of the Shiny app.
app.R: Main file for the Shiny app that includes the user interface and server logic.
data_cleaning.R: Script used to clean and preprocess the data for analysis.

Shiny_App/: Folder containing the files necessary to run the Shiny app, including www/ for any web resources.
Reproducing the Work
To reproduce this work, follow these steps:

Clone the repository to your local machine:
 - Copy code into terminal
 - git clone https://github.com/yourusername/your-repo-name.git

Install necessary R packages (if not already installed):

 - Copy code into markdown or qmd
 - install.packages(c("shiny", "dplyr", "readr", "ggplot2", "sf", "tigris", "ggpubr", "plotly", "stringr"))

Load the data and run the Shiny app by opening app.R:

 - shiny::runApp("Shiny_App")
 - Ensure the SAHIE data files are located in the Data/ folder for correct app functionality.

To reproduce specific vizualizations, please refer to the ___ .qmd

The project was completed in Macalester College's Projects in Data Science Capstone class by Evan Burns, Max Clifford, and Jacob Posner. 