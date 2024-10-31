#' Paths to Inputs/Outputs
#'
#' Interactively set paths to directories containing images and directory for
#' output files.
#'
#' @details `set_paths` uses [rstudioapi::selectDirectory()] to facilitate interactive selection of input and output directories. If `parent_dir_img` is unspecified, the location of the current active project will be used as the initial directory for selection of directory containing images.
#'
#' @param parent_dir_img character string containing relative or absolute path to directory containing subdirectory with images to process
#' @param parent_dir_out character string containing relative or absolute path to directory where a new subdirectory will be created for output
#' @param batch_name character string containing name for batch of images to be processed and for associated output directory
#'
#' @return A three-element list containing the path to the input image directory, the path to the output directory, and the batch name, as character strings. Relative paths will be returned if relative paths were provided as arguments to `parent_dir_img` and `parent_dir_out`.
#'
#' @export
#'
#' @examples
#' # Start selection in directory of active project
#' paths <- set_paths()
#'
#' # Start selection in directory other than active project
#' # paths2 <- set_paths(parent_dir_img = '~/Documents/MySlakesData/')

set_paths <- function(parent_dir_img = NULL, parent_dir_out = NULL, batch_name = NULL) {

  # if parent_dir_img is unspecified, use location of project as initial directory for selection of directory containing images

  if (is.null(parent_dir_img) | !dir.exists(format(parent_dir_img))) {

    parent_dir_img <- rstudioapi::getActiveProject()

  }

  # select directory containing images

  dir_img <- rstudioapi::selectDirectory(label = 'Look for images here', path = parent_dir_img)

  # if parent_dir_out is unspecified, interactively select location for creation of new directory containing output files

  if (is.null(parent_dir_out) | !dir.exists(format(parent_dir_out))) {

    parent_dir_out <- rstudioapi::selectDirectory(path =  dir_img,
                                                   label = 'Save output here')

  }

  # if batch_name is unspecified, use name of directory containing images

  if (is.null(batch_name)) {

    # extract name of directory containing images from absolute path
    dir_img_rel <- gsub("^.*/", "", dir_img)

    batch_name <- dir_img_rel

  }

  # path to directory for output files

  dir_out <- file.path(parent_dir_out, batch_name)

  if (is.null(dir_img)) {
    stop('No image directory specified.', call. = F)
  } else {
    if (!dir.exists(dir_img)){
      stop('Image directory does not exist.', call. = F)
    } else {
      if (length(dir_out) == 0 | is.null(dir_out)) {
       stop('No output directory specified.', call. = F)
      } else {
      cat('Image directory: ', dir_img, '\nOutput directory: ', dir_out, '\n')
      return(list(image_dir =  dir_img, output_dir = dir_out, batch_name = batch_name))
    }
  }
  }
}

#' Silently Create a Directory
#'
#' Create a directory; suppress warning if directory already exists.
#'
#' @param direc character string containing path name
#' @param ... additional arguments passed to `dir.create`
#'
#' @export
#'
dir_create_silent <- function(direc, ...) {

  suppressWarnings(dir.create(direc, ...))

  if(!dir.exists(direc)) {stop(paste0('Could not create directory ', direc))}

}

#' Create Directory for Output
#'
#' Create a directory for output of the analysis of a batch of images
#'
#' @details `dir_setup` creates a batch-level directory for processed images and metadata. Subdirectories not created by [SlakeItEasy::process_petri()] can be created with `dir_setup` by providing a vector of subdirectory names (as relative paths) to the `subdirs` argument. By default, subdirectories are created for later manual sorting of images that need reprocessing and images that are unusable for analysis.
#'
#' @param output_dir character string with absolute path to output directory
#' @param subdirs character vector with names of subdirectories to create
#' @param return_paths logical. If TRUE, a vector containing absolute paths to created subdirectories is returned.
#'
#' @return Character vector with absolute paths to created subdirectories if `return_paths` is TRUE.
#' @export
#'
#' @examples
#' # Interactively set paths
#' paths <- set_paths()
#'
#' # Create batch-level directory with default subdirectories to_reprocess and unusable.
#' # Retain absolute paths to subdirectories as a character vector.
#' dirs_for_flagged_imgs <- dir_setup(paths$output_dir)

dir_setup <- function(output_dir, subdirs = c('to_reprocess', 'unusable'), return_paths = T){

  # name subdirectories

  dir_vec <- file.path(output_dir, subdirs)

  names(dir_vec) <- subdirs

  # create subdirectories

  invisible(lapply(dir_vec, dir_create_silent, recursive = T))

  if (return_paths) {

    # return subdirectory paths
    return(dir_vec)

  }

}

#' Open File Explorer
#'
#' Open one or more directories in the system file explorer.
#'
#' @param dirs character vector with absolute paths to one or more directories to open.
#'
#' @export
#'
#' @examples
#' # Interactively set paths
#' paths <- set_paths()
#'
#' # Create batch-level directory with default subdirectories to_reprocess and unusable.
#' # Retain absolute paths to subdirectories as a character vector.
#' dirs_for_flagged_imgs <- dir_setup(paths$output_dir)
#'
#' # Open directories
#' opendirs(dirs_for_flagged_imgs)
#'
opendirs <- function(dirs){
  invisible(lapply(dirs, function(x){
    cur_dir <- getwd()
    setwd(x)
    if (.Platform['OS.type'] == "windows"){
      shell.exec(".")
    } else {
      system(paste(Sys.getenv("R_BROWSER"), "."))
    }
    on.exit(setwd(cur_dir))
  }
  ))
}

#' Extract Image Metadata
#'
#' Get EXIF metadata and temporal information for a directory of images.
#'
#' @details A simple wrapper around [exifr::read_exif()], supplementing EXIF metadata with temporal information extracted from image file names. Within the batch-level directory `dir`, it is assumed that images are organized into replicate-level subdirectories. Each subdirectory should contain an initial image of air-dry soil, an image of soil collected upon submersion in water, and a final image collected upon conclusion of the slaking measurement sequence. `get_metadata` calculates the elapsed time for each subdirectory as well as the elapsed time of slaking, assuming the second image corresponds to time zero.
#'
#' @param dir character string with relative or absolute path to directory containing images
#' @param filename_prefix character string indicating any text preceding the date and time in image file names
#' @param image_extension character string indicating the image file type
#' @param datetime_fmt character string indicating the date and time format used in image file names (see [base::strptime()])
#'
#' @return A data frame (tibble) containing EXIF metadata and temporal information extracted from image file names.
#' @export
#'
#' @examples
#' # Interactively set paths
#' paths <- set_paths(parent_dir_img = 'inst/images')
#'
#' # Get image metadata
#' (metadata <- get_metadata(paths$image_dir, filename_prefix = 'IMG_'))
#'
#'

get_metadata <- function(dir, filename_prefix = NULL, filename_suffix = NULL, image_extension = 'jpg', datetime_fmt = '%Y%m%d_%H%M%S'){

  # prepend period to image file extension, if not provided

  if (substr(image_extension, 1, 1) != '.') {
    image_extension <- paste0('.', image_extension)
  }

  if (!tolower(image_extension) %in% c('.jpg', '.jpeg', '.png', '.tif', '.tiff')) {
    stop('Invalid image type. Currently supported formats are JPEG, PNG, and TIFF.', call. = T)
  }

  metadat <- exifr::read_exif(list.files(dir, full.names = T, recursive = T, pattern = paste0(image_extension, '$')))

  if (is.null(filename_prefix)) {
    filename_prefix <- ''
  }

  if (!all(startsWith(metadat$FileName, filename_prefix))) {
    stop(paste0('At least one image filename does not begin with the supplied prefix, ', filename_prefix), call. = F)
  }

  if (is.null(filename_suffix)) {
    filename_suffix <- ''
  }

  metadat <- metadat %>%
    dplyr::mutate(datetime = gsub(filename_prefix, "", FileName),
                  datetime = gsub(filename_suffix, "", FileName),
                  datetime = gsub(image_extension, "", datetime),
                  datetime = strptime(datetime, format = datetime_fmt)) %>%
    dplyr::group_by(Directory) %>%
    dplyr::mutate(elapsed_time_m = as.numeric(datetime - min(datetime)) / 60,
                  slaking_elapsed_time_m = elapsed_time_m - min(elapsed_time_m[elapsed_time_m > 0])) # total elapsed time of measurements and elapsed time of slaking, assuming each directory contains only one image prior to submersion

  if (anyNA(metadat$datetime)) {
    stop('Datetime unsuccessfully extracted from filename for one or more images. Verify that you correctly supplied your filename prefix and/or suffix, file extension, and datetime format.', call. = F)
  }

  return(metadat)

}

#' Inspect Image Metadata
#'
#' Evaluate image metadata and identify replicates that do not meet expectations for data analysis.
#'
#' @details `check_replicates()` evaluates the suitability of replicates (i.e., subdirectories for which metadata were extracted using [SlakeItEasy::get_metadata()]) for analysis. Replicates are evaluated based on number of images per subdirectory, image size, and the timing of the final image.

#' @param metadata data frame of image metadata as returned by [SlakeItEasy::get_metadata()]
#' @param final_img_time_min time of final image capture (in minutes) after submersion
#' @param final_img_tol_sec tolerance for time of final image capture (in seconds)
#' @param n_images_min minimum number of images required per replicate
#' @param n_images_max maximum number of images allowed per replicate
#'
#' @return A list of varying length. The first element is a data frame with one row summarizing the evaluation of each replicate. The second element is a character vector of absolute paths to usable replicates. Additional character vectors may be included listing replicates that were flagged for missing images (fewer images than `n_images_min`), for having extra images (more images than `n_images_max`), for containing images of more than one size, or for lacking a final image at the expected time (`final_img_time_min` \eqn{\pm} `final_img_tol_sec`).
#' @export
#'
#' @examples
#' # Interactively set paths
#' paths <- set_paths()
#'
#' # Get image metadata
#' metadata <- get_metadata(paths$image_dir)
#'
#' # Inspect image metadata
#' check <- check_replicates(metadata)
#'
#'

check_replicates <- function(metadata, final_img_time_min = 10, final_img_tol_sec = 30, n_images_min = 3, n_images_max = 3) {

  if (anyNA(metadata$datetime)) {
    stop('Datetime in metadata data frame invalid for one or more images. Verify that you correctly supplied your filename prefix, file extension, and datetime format when you ran get_metadata().', call. = F)
  }

 m <- metadata  %>%
    dplyr::arrange(Directory, datetime) %>%
    dplyr::mutate(within_final_img_tol = abs(slaking_elapsed_time_m - final_img_time_min) <= final_img_tol_sec/60) %>%
    dplyr::group_by(Directory) %>%
   dplyr::summarise(n_images = dplyr::n(), # number of images per directory
                    final_image_within_tol = sum(within_final_img_tol) > 0) %>% # timing of final image within tolerance)
   dplyr::mutate(qaqc_pass = n_images >= n_images_min &
                   n_images <= n_images_max  &
                   final_image_within_tol > 0) %>%
   dplyr::arrange(qaqc_pass)

 # report:
 # number of replicates with fewer than the expected number of images
 missing_imgs <- sum(m$n_images < n_images_min, na.rm = T)
 # number of replicates with greater than the expected number of images
 extra_imgs <- sum(m$n_images > n_images_max, na.rm = T)
 # number of replicates with no final image (within tolerance)
 wrong_final_time <- sum(m$final_image_within_tol == 0, na.rm = T)
 # number of replicates with no issues
 n_usable <- sum(m$qaqc_pass)

 usable <- m$Directory[m$qaqc_pass]

 out <- list(m = m, usable = usable)

 if (missing_imgs > 0) {
   warning(paste(missing_imgs, ' replicates do not have all images.'))
   out[['missing_imgs']] <- m$Directory[m$n_images < n_images_min]
 }

 if (extra_imgs > 0) {
   warning(paste(extra_imgs, ' replicates have more images than expected.'))
   out[['extra_imgs']] <- m$Directory[m$n_images > n_images_max]
 }

 if (wrong_final_time > 0) {
   warning(paste(wrong_final_time, ' replicates do not have a final image at the correct time.'))
   out[['wrong_final_time']] <- m$Directory[m$final_image_within_tol == 0]
 }

 cat(n_usable, '/', nrow(m), ' replicates pass QA/QC.\n')

 return(out)

}

