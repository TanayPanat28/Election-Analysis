Title: Analysis of Election in Worlds Largest Democracy


This project provides in depth analysis of General Elections in India, focusing on various aspects such as state wise analysis, party-wise analysis, alliance-wise analysis and more.

The goal of the project is to provide a comprahensive analysis of India's recent political scenario.

The project was created using RStudio, with the Shiny package for interactive web application and analysis. 
The data wrangling part was performed in the Rmarkdown file using R packages like tiduverse, dplyer, rgdal, maptools etc. The visualisation was created using ggplot2, plotly, highcharter and ggparliament.


##########################################################################################################################################

Although I have created a function to install all the required libraries into your local system, following are the libraries which I used in this project:

    -> Data Preprocessing:
	- tidyverse
	- dplyr
	- plotly
	- here
	- highcharter
	- crosstalk
	- ggiraph
	- ggplot2
	- sf
	- rvest
	- viridis
	- ggrepel
	- ggthemes
	- rgdal
	- maptools
	- scales
	- rgeos
	- shiny
	- shinythemes
	- shinyWidgets
	- DT
	- shinycssloaders

##########################################################################################################################################

Instructions to run the shiny application:

    -> I have done the data preprocessing in the shinytest file provided in the zip file, please run all the code blocks so that the variable required to run the shiny application will be loaded on the local system.

    -> 'Election_Analysis' folder is the default working directory of the shiny app environment, it contains:

	- 'app.R' which is our shiny application

	- Contains all the datasets required for this project.

	- 'parliamentary-constituencies' folder contains the shape file require to plot the map of India

	- 'www' folder containing images used by the shiny app

    -> The app requires you to load the data and the local variables, please run the shinytest.rmd file.

    -> To run the shiny application, open 'app.R' file in your system and press 'run'.

##########################################################################################################################################

All the materials are included in this zip folder to build the application from scratch.

##########################################################################################################################################

Thank You!!
