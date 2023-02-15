# SlakeItEasy: Image-Based Estimation of Soil Aggregate Stability

{SlakeItEasy} provides utility functions to estimate soil aggregate stability via analysis of the projected area of soil during slaking tests, following [Fajardo et al. (2016)](https://www.sciencedirect.com/science/article/pii/S0167198716300952). Functions are designed to analyze the output of high-throughput image aquisition schemes (e.g., multiple replicates, multiple samples).

<a href="https://raw.githubusercontent.com/Soil-Health-Institute/SlakeItEasy/master/misc/SIE_sticker.png">
<img src = "https://raw.githubusercontent.com/Soil-Health-Institute/SlakeItEasy/master/misc/SIE_sticker.png" alt = "SlakeItEasy hexsticker" title = "SlakeItEasy hexsticker" width = "45%" height = "45%" hspace="15" vspace="15" align="right"/></a>
    
## Installation

After installing R and R Studio, the development ersion of {SlakeItEasy} and its dependencies can be installed using the {remotes} package.

```r
remotes::install_github('latenooker/SlakeItEasy')
```

## Example workflow

Detailed vignettes are under development. In the meantime, an example workflow can be accessed as follows. 

```r
invisible(rstudioapi::navigateToFile(system.file("demo", "02_workflow.R", package = "SlakeItEasy")))
```





