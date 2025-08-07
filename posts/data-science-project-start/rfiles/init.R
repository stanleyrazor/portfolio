# Function to create the project structure
create_project_structure <- function(root_dir) {
  # Create directories
  dirs <- list(
    # "data/intermediate", 
    "data/input", 
    "data/output", 
    #"data/interim", 
    "scripts/R", 
    "scripts/Globals", 
    "scripts/notebooks", 
    "output/figures", 
    "output/reports", 
    "output/models", 
    # "config", 
    "logs",
    "docs"
  )
  
  # Create directories if they don't exist
  for (dir in dirs) {
    dir_path <- file.path(root_dir, dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message(paste("Created folder:", dir_path))
    }
  }
  
  # Create README.md file
  readme_path <- file.path(root_dir, "README.md")
  readme_content <- "
# Project Overview

This project contains the following folder structure:

- **data/**: Stores datasets.
  - **raw/**: Unprocessed, raw data files.
  - **processed/**: Cleaned or processed data files.
  - **external/**: Data sourced externally.
  - **interim/**: Temporarily stored intermediate data.

- **scripts/**: R scripts for data processing and analysis.
  - **R/**: Core R scripts for analysis and modeling.
  - **functions/**: Custom R functions.
  - **notebooks/**: RMarkdown or Jupyter notebook files for exploration and reports.

- **output/**: Stores the outputs of the project.
  - **figures/**: Visualizations and figures.
  - **reports/**: Reports generated from scripts.
  - **models/**: Trained model objects.

- **config/**: Configuration files for parameters, environment settings, etc.

- **logs/**: Log files generated during processing or analysis.

- **docs/**: Documentation for the project, such as data dictionaries and codebooks.

## How to Use This Project

1. Place your raw data in the `data/raw/` folder.
2. Write your analysis scripts in the `scripts/R/` folder.
3. Store figures, models, and reports in the `output/` folder.

Feel free to modify the structure as needed to suit your project requirements.
"
  writeLines(readme_content, con = readme_path)
  message(paste("Created README file at:", readme_path))
}

# Run the function
# Replace "/path/to/project" with the path to your project root folder
# create_project_structure("/path/to/project")


# create_project_structure("temp")
# setwd("~/Desktop/Blog/Stanley Sayianka/posts")