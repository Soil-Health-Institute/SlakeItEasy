# SlakeItEasy: Image-Based Estimation of Soil Aggregate Stability

{SlakeItEasy} provides utility functions to estimate soil aggregate stability via analysis of images of air-dried soil and soil undergoing slaking in water, following [Fajardo et al. (2016)](https://www.sciencedirect.com/science/article/pii/S0167198716300952). Functions are provided to analyze individual images, multiple images per sample, and the output of high-throughput image aquisition schemes (e.g., multiple replicates, multiple samples).

<a href="https://raw.githubusercontent.com/Soil-Health-Institute/SlakeItEasy/master/misc/SIE_sticker.png">
<img src = "https://raw.githubusercontent.com/Soil-Health-Institute/SlakeItEasy/master/misc/SIE_sticker.png" alt = "SlakeItEasy hexsticker" title = "SlakeItEasy hexsticker" width = "40%" height = "40%" hspace="15" vspace="15" align="right"/></a>
    
## Installation

First, install [R](https://www.r-project.org/) and [R Studio](https://posit.co/download/rstudio-desktop/). If you are running Windows, you will also need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

Run the following lines in R Studio to install the latest version of {SlakeItEasy} from GitHub using the {remotes} package:

```r
# install {remotes} if missing
if (!require(remotes)) {
install.packages('remotes')
}
# install latest version of {SlakeItEasy} from GitHub repository
remotes::install_github('Soil-Health-Institute/SlakeItEasy')
```

Note that users on Windows may need to install [Strawberry Perl](https://strawberryperl.com/) before using image metadata extraction utilities from the {exifr} package. After installing Strawberry Perl, complete configuration in `R` as follows:

```r
exifr::configure_exiftool()
```

If {fftwtools} and {EBImage} fail to install on Linux, run the following in the terminal:

```console
sudo apt-get install libfftw3-dev
```

## File structure

For batch processing, image sequences (i.e., images of air-dried soil, soil upon submersion in water, and soil after slaking for a predetermined interval) should be organized into one directory per replicate (e.g., per petri dish). To make data wrangling easier, name directories with the sample ID and Petri dish number (e.g., SampleID_PetriDishNumber, as below).

<a href="https://raw.githubusercontent.com/Soil-Health-Institute/SlakeItEasy/master/misc/file_structure.png">
<img src = "https://raw.githubusercontent.com/Soil-Health-Institute/SlakeItEasy/master/misc/file_structure.png" alt = "Example file structure" title = "Example file structure" width = "25%" height = "25%" hspace="15" vspace="15" align="middle"/></a>

## Example workflow

Detailed vignettes are under development. In the meantime, an example workflow can be accessed within R Studio as follows (or by clicking [here](https://raw.githubusercontent.com/Soil-Health-Institute/SlakeItEasy/master/inst/demo/02_workflow.R)).

```r
invisible(rstudioapi::navigateToFile(system.file("demo", "02_workflow.R", package = "SlakeItEasy")))
```

## Citation

To cite {SlakeItEasy} in publications, run the following to generate a text reference and BibTeX entry:

```r
citation('SlakeItEasy')
```



