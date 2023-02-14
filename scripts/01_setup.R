lapply(c('dplyr',
         'BiocManager',
         'exifr',
         'magrittr',
         'readxl',
         'stringr'),
       function(x) {
         if (!require(x, quietly = TRUE))
           install.packages(x)
       })

BiocManager::install("EBImage")

install.packages('SlakeItEasy_0.1.0.9000.tar.gz', repos = NULL, type="source")
