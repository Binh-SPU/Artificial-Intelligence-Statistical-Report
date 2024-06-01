# Artificial Intelligence Data Visualization App

This Shiny web application provides interactive visualizations of data related to Artificial Intelligence (AI), including scholarly publications, patent applications, and performance metrics on knowledge tests using Rstudio.

[Here the is my little statistical app!](https://phamb5.shinyapps.io/AIFinalProject/)

## Features

### Publications/Patents Tab

- **Statistic to Plot**: Select between the number of scholarly publications or patent applications related to AI.
- **Timelapse Slider**: Adjust the year to view data from 2010 to 2021.
- **Interactive Map**: A world map displaying the selected statistic, with tooltips for detailed information on each country.

### AI Performance Tab

- **Scatter Plot**: Visualizes the performance of AI systems on knowledge tests vs. the amount of training computation. The size of the points represents the training dataset size, and the color represents different organizations.
- **Interactive Elements**: Tooltips provide detailed information about each data point.

## Data Sources

- **Scholarly Publications and Patents**: Data sourced from [Our World in Data](https://ourworldindata.org/artificial-intelligence).
- **AI Performance**: Data sourced from [Cat.eto.tech](https://cat.eto.tech/).

## Installation
To run this Shiny app locally, you need to have R and the following packages installed:

```r
install.packages(c("shiny", "tidyverse", "openintro", "plotly", "rnaturalearth", "rnaturalearthdata", "readxl", "dplyr"))
```
### Prerequisites

Before you can run this Shiny app, ensure you have R and RStudio installed on your computer. You can download them from the following links:

- [R](https://cran.r-project.org/)
- [RStudio](https://www.rstudio.com/products/rstudio/download/)
