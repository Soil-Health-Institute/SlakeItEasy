# set paths to directories containing images and directory for output files

#' Title
#'
#' @param parent_dir_img
#' @param parent_dir_out
#' @param batch_name
#'
#' @return
#' @export
#'
#' @examples
set_paths <- function(parent_dir_img = '~/Soil Health Institute/Data Repository - Documents/Data Delivery/AmAgLab', parent_dir_out = NULL, batch_name = NULL) {

  # if parent_dir_img is unspecified, use location of project as initial directory for selection of directory containing images
  if (is.null(parent_dir_img) | !dir.exists(format(parent_dir_img))) {
    parent_dir_img <- rstudioapi::getActiveProject()
  }

  # select directory containing images

  dir_img <- rstudioapi::selectDirectory(label = 'Look for images here', path = parent_dir_img)

  # extract name of directory containing images from absolute path
  dir_img_rel <- gsub("^.*/", "", dir_img)

  # if parent_dir_out is unspecified, interactively select location for creation of new directory containing output files
  if (is.null(parent_dir_out) | !dir.exists(format(parent_dir_out))) {
    parent_dir_out <- rstudioapi::selectDirectory(label = 'Save output here')
  }

  # if batch_name is unspecified, use name of directory containing images
  if (is.null(batch_name)) {
    batch_name <- dir_img_rel
  }

  # create directory for output files
  dir_out <- paste0(parent_dir_out, '/', batch_name)

  cat('Image directory: ', dir_img, '\nOutput directory: ', dir_out, '\n')

  return(list(image_dir =  dir_img, batch_name = batch_name, output_dir = dir_out))

}

# create non-existing directory and suppress warning if directory already exists
#' Title
#'
#' @param direc
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dir_create_silent <- function(direc, ...) {

  suppressWarnings(dir.create(direc, ...))

  if(!dir.exists(direc)) {stop(paste0('Could not create   directory ', direc))}

}

# create directories for output images and log files
#' Title
#'
#' @param outdir_root
#' @param outdir_name
#' @param subdirs
#' @param ...
#' @param return_paths
#'
#' @return
#' @export
#'
#' @examples
dir_setup <- function(outdir_root, outdir_name = 'slakes_processed', subdirs = c('manually_processed', 'auto_processed', 'to_reprocess', 'log'), ..., return_paths = T){

  # name subdirectories
  dir_vec <- paste(outdir_root, outdir_name, subdirs, sep = '/')

  # create subdirectories
  invisible(lapply(dir_vec, dir_create_silent, ...))

  if (return_paths) {
    # return subdirectory paths
    return(dir_vec)
  }

}

# open file explorer at specified directory
#' Title
#'
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
opendir <- function(dir){
  cur_dir <- getwd()
  setwd(dir)
  if (.Platform['OS.type'] == "windows"){
    shell.exec(".")
  } else {
    system(paste(Sys.getenv("R_BROWSER"), "."))
  }
  on.exit(setwd(cur_dir))
}

# open file explorer in multiple directories
#' Title
#'
#' @param dirs
#'
#' @return
#' @export
#'
#' @examples
opendirs <- function(dirs){
  invisible(lapply(dirs, opendir))
}

# extract metadata for all images in a directory
#' Title
#'
#' @param dir
#' @param filename_prefix
#' @param image_extension
#'
#' @return
#' @export
#'
#' @examples
get_metadata <- function(dir, filename_prefix = 'Develop', image_extension = 'jpg'){

  metadat <- exifr::read_exif(list.files(dir, full.names = T, recursive = T, pattern = paste0(image_extension, '$')))

  metadat <- metadat %>%
    dplyr::mutate(datetime = gsub(filename_prefix, "", FileName),
                  datetime = gsub(paste0('.', image_extension), "", datetime),
                  datetime = strptime(datetime, format = '%Y%m%d_%H%M%S')) %>%
    dplyr::group_by(Directory) %>%
    dplyr::mutate(elapsed_time_m = as.numeric(datetime - min(datetime)) / 60,
                  slaking_elapsed_time_m = elapsed_time_m - min(elapsed_time_m[elapsed_time_m > 0])) # total elapsed time of measurements and elasped time of slaking, assuming each directory contains only one image prior to submersion

  return(metadat)

}


# # starttime <- min(area_df$timestamp[-which.min(area_df$timestamp)])
# # stoptime <- max(area_df$timestamp)
# #
# # stoptime - starttime
# #
# library(ggplot2);library(dplyr);library(tidyr)
#
# test <- test %>% group_by(replicate_id) %>% mutate(starttime = min(timestamp[-which.min(timestamp)]), elapsed_time = as.numeric(timestamp - starttime)/60)

# inspect metadata for each replicate
#' Title
#'
#' @param metadata
#' @param image_extension
#' @param final_img_time_min
#' @param final_img_tol_sec
#' @param n_images_min
#' @param n_images_max
#'
#' @return
#' @export
#'
#' @examples
check_replicates <- function(metadata, image_extension = "jpg", final_img_time_min = 10, final_img_tol_sec = 30, n_images_min = 3, n_images_max = 3) {

  # number of images per directory
  # final image is available at 10 min +/- tolerance (final_img_tol_sec)
  # initial image and final image are same size

 m <- metadata  %>%
    dplyr::arrange(Directory, datetime) %>%
    dplyr::mutate(within_final_img_tol = abs(slaking_elapsed_time_m - final_img_time_min) <= final_img_tol_sec/60) %>%
    dplyr::group_by(Directory) %>%
   dplyr::summarise(n_images = dplyr::n(), n_image_sizes = length(unique(Megapixels)), final_image_within_tol = sum(within_final_img_tol) > 0, initial_and_final_image_size_equal = Megapixels[elapsed_time_m == min(elapsed_time_m)] == dplyr::first(Megapixels[within_final_img_tol]))  %>%
   dplyr::mutate(qaqc_pass = n_images >= n_images_min & n_images <= n_images_max & n_image_sizes == 1 & final_image_within_tol > 0) %>%
   dplyr::arrange(qaqc_pass)

 # report:
 # number of replicates with fewer than the expected number of images
 missing_imgs <- sum(m$n_images < n_images_min, na.rm = T)
 # number of replicates with greater than the expected number of images
 extra_imgs <- sum(m$n_images > n_images_max, na.rm = T)
 # number of replicates with multiple image sizes
 multiple_sizes <- sum(m$n_image_sizes > 1, na.rm = T)
 # number of replicates with no final image (within tolerance)
 wrong_final_time <- sum(m$final_image_within_tol == 0, na.rm = T)
 # number of image sizes across replicates
 n_sizes_batch <- length(unique(metadata$Megapixels))
 # number of replicates with no issues
 n_usable <- sum(m$qaqc_pass)

 usable <- m$Directory[m$qaqc_pass]

 out <- list(m = m, usable = usable)

 if (missing_imgs > 0) {
   cat(missing_imgs, ' replicates do not have all images.\n')
   out[['missing_imgs']] <- m$Directory[m$n_images < 3]
 }

 if (extra_imgs > 0) {
   cat(extra_imgs, ' replicates have more images than expected.\n')
   out[['extra_imgs']] <- m$Directory[m$n_images > 3]
 }

 if (multiple_sizes > 0) {
   cat(multiple_sizes, ' replicates have inconsistent image resolution.\n')
   out[['multiple_sizes']] <- m$Directory[m$n_image_sizes > 1]
 }

 if (wrong_final_time > 0) {
   cat(wrong_final_time, ' replicates do not have a final image at the correct time.\n')
   out[['wrong_final_time']] <- m$Directory[m$final_image_within_tol == 0]
 }

 if (n_sizes_batch > 1) {
   cat(n_sizes_batch, ' distinct image resolutions detected in batch.\n')
 }

 cat(n_usable, '/', nrow(m), ' replicates pass QA/QC.\n')

 return(out)

}


## American Ag Lab-specific: get sample IDs from excel file(s)

#' Title
#'
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
get_IDs <- function(dir) {

  ids <- list.files(dir, recursive = T, full.names = T, pattern = '.xls')

  ids <- lapply(ids, readxl::read_excel, .name_repair = 'universal', col_type = 'text')

  ids <- do.call(dplyr::bind_rows, ids)

  return(ids)

}


