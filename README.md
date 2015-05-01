# CountyTornado
Code and Data for Regional Tornado Studies

This file contains code and data for the paper "Regional Tornado Studies"

The main file is CountyTornado.Rmd. 
To run, download the latest version of R, http://cran.r-project.org/ 
Next download R-Studio, http://www.rstudio.com/
Start up R or R-Studio and install the following packages and dependencies:

httr,moments,dply,raster,spdep,splines,reshape2,
RColorBrewer,dplyr,wesanderson,magrittr

Install R-INLA:
source("http://www.math.ntnu.no/inla/givemeINLA.R" 

Open up CountyTornado.Rmd in R-Studio, then knit to html, this runs all of the code and generates the figures used in the document.


This should generate the file CountyTornado.html, which contains the R-output.

This generates a tmp directory which can be deleted, and a figures directory containing all of the figures generated for the paper.

The other files are required and should be in the same directory as the CountyTornado.Rmd file.