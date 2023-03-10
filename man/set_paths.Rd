% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filesys_setup.R
\name{set_paths}
\alias{set_paths}
\title{Paths to Inputs/Outputs}
\usage{
set_paths(parent_dir_img = NULL, parent_dir_out = NULL, batch_name = NULL)
}
\arguments{
\item{parent_dir_img}{character string containing relative or absolute path to directory containing subdirectory with images to process}

\item{parent_dir_out}{character string containing relative or absolute path to directory where a new subdirectory will be created for output}

\item{batch_name}{character string containing name for batch of images to be processed and for associated output directory}
}
\value{
A three-element list containing the path to the input image directory, the batch name, and the path to the output directory, as character strings. Relative paths will be returned if relative paths were provided as arguments to \code{parent_dir_img} and \code{parent_dir_out}.
}
\description{
Interactively set paths to directories containing images and directory for
output files.
}
\details{
\code{set_paths} uses \code{\link[rstudioapi:file-dialogs]{rstudioapi::selectDirectory()}} to facilitate
interactive selection of input and output directories. If \code{parent_dir_img}
is unspecified, the location of the current active project will be used as
the initial directory for selection of directory containing images.
}
\examples{
# Start selection in directory of active project
paths <- set_paths()

# Start selection in directory other than active project
# paths2 <- set_paths(parent_dir_img = '~/Documents/MySlakesData/')
}
