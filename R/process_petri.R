#' Get Soil Area for an Image Sequence
#'
#' @param path_to_image_set
#' @param outdir
#' @param filename_prefix
#' @param image_extension
#' @param datetime_fmt character string indicating the date and time format used in image file names (see [base::strptime()])
#' @param final_img_time_min time of final image capture (in minutes) after submersion
#' @param final_img_tol_sec tolerance for time of final image capture (in seconds)
#' @param match_resolution
#' @param safe_sep
#' @param interactive
#' @param circular_mask
#' @param false_color
#' @param d
#' @param h_offset
#' @param v_offset
#' @param aggregates_in_initial
#' @param max_rel_dist_from_centroid
#' @param max_rel_dist_from_center
#' @param fixed_crop_fraction_initial
#' @param fixed_crop_fraction_run
#' @param max_rel_objlength
#' @param automask_buffer
#' @param erode_kern
#' @param dilate_kern
#' @param normdiff_min
#'
#' @return Value of the slaking index at `final_img_time_min` \eqn{\pm} `final_img_tol_sec`
#' @export
#'
#' @examples
process_petri <- function(path_to_image_set, outdir, filename_prefix, image_extension = 'jpg', datetime_fmt = '%Y%m%d_%H%M%S', final_img_time_min = 10, final_img_tol_sec = 30, match_resolution = T, safe_sep = '_x_', interactive = F, circular_mask = T, false_color = 'red', d = 0.7, h_offset = 0, v_offset = 0, aggregates_in_initial = 3, max_rel_dist_from_centroid = 1.3,  max_rel_dist_from_center = 0.4, fixed_crop_fraction_initial = NULL, fixed_crop_fraction_run = fixed_crop_fraction_initial, max_rel_objlength = 0.4, automask_buffer = 0.15, erode_kern = 5, dilate_kern = 31,  normdiff_min = 0) {

  # get platform-specific file separator character
  filesep <- .Platform$file.sep

  if (substr(image_extension, 1, 1) != '.') {
    image_extension <- paste0('.', image_extension)
  }

  image_paths <- list.files(path_to_image_set, full.names = T, pattern = paste0(image_extension, '$'))

  image_metadata <- exifr::read_exif(image_paths)

  if (match_resolution) {

    dims_smallest_img <- dplyr::distinct(image_metadata[image_metadata$Megapixels == min(image_metadata$Megapixels),c('ImageHeight', 'ImageWidth')])

    w_new <- min(dims_smallest_img)

    h_new <- max(dims_smallest_img)

  } else {
    w_new <- NULL
    h_new <- NULL
  }

  timestamp_orig <- gsub(paste0('.', image_extension),  "", image_metadata$FileName) %>%
    gsub(filename_prefix, "", .)

  timestamp <- strptime(timestamp_orig, format = datetime_fmt)

  # timestamp_start <- stringr::str_locate(image_metadata$FileName, year)[,1]
  #
  # timestamp_orig <- substr(image_metadata$FileName, timestamp_start, stop = nchar(image_metadata$FileName) - (nchar(image_extension) + 1))
  #
  # timestamp <- strptime(timestamp_orig, format = '%Y%m%d_%H%M%S')

  area_airdry <- area_from_image(image_paths[[1]], interactive = interactive, circular_mask = circular_mask, false_color = false_color, d = d, aggregates_in_initial = aggregates_in_initial, fixed_crop_fraction = fixed_crop_fraction_initial, max_rel_dist_from_center = max_rel_dist_from_center, max_rel_dist_from_centroid = max_rel_dist_from_centroid, max_rel_objlength = max_rel_objlength, automask_buffer = automask_buffer, erode_kern = erode_kern, dilate_kern = dilate_kern,  h_offset = h_offset, v_offset = v_offset, w_new = w_new, h_new = h_new, normdiff_min = normdiff_min)

  area_list <- lapply(2:length(timestamp), function(x) area_from_image(image_paths[[x]], interactive = interactive, circular_mask = circular_mask, false_color = false_color, d = d, fixed_crop_fraction = fixed_crop_fraction_run, max_rel_dist_from_center = max_rel_dist_from_center, max_rel_dist_from_centroid = max_rel_dist_from_centroid, max_rel_objlength = max_rel_objlength, automask_buffer = automask_buffer, erode_kern = erode_kern, dilate_kern = dilate_kern, h_offset = h_offset, v_offset = v_offset, w_new = w_new, h_new = h_new, normdiff_min = normdiff_min))

  area_df <- data.frame(image_set = path_to_image_set, FileName = image_metadata$FileName, timestamp, area_pixels = c(area_airdry$area, sapply(area_list, function(x) x$area)))

 area_final <- area_df$area_pixels[max(1 + which(abs(diff(area_df$timestamp) - final_img_time_min) <= final_img_tol_sec/60))]

 stab10 <- area_airdry$area/area_final

  out <- c(list(area_airdry), area_list)

  image_set_prefix <- gsub(filesep, safe_sep, path_to_image_set, fixed = T)

  dir_falsecol <- paste0(outdir, filesep, 'images_false_color')
  dir_binary <- paste0(outdir, filesep, 'images_binary')
  dir_attr <- paste0(outdir, filesep, 'classification_attributes')
  dir_results <- paste0(outdir, filesep, 'area_by_time')

  suppressWarnings(dir.create(dir_falsecol, recursive = T))
  suppressWarnings(dir.create(dir_binary, recursive = T))
  suppressWarnings(dir.create(dir_attr, recursive = T))
  suppressWarnings(dir.create(dir_results, recursive = T))

  filenames_falsecol <- paste0(dir_falsecol, filesep, image_set_prefix, safe_sep, timestamp_orig,  image_extension)
  filenames_binaries <- paste0(dir_binary, filesep, image_set_prefix, safe_sep, timestamp_orig,  image_extension)

  invisible(lapply(1:length(out), function(x) EBImage::writeImage(out[[x]]$false_color, filenames_falsecol[x], quality = 50)))

  invisible(lapply(1:length(out), function(x) EBImage::writeImage(out[[x]]$classified, filenames_binaries[x], quality = 100)))

  saveRDS(lapply(1:length(out), function(x) attributes(out[[x]]$classified)), paste0(dir_attr, filesep, image_set_prefix, '.rds'))

  write.csv(area_df, paste0(dir_results, filesep, image_set_prefix, '.csv'), row.names = F)

  return(stab10)

}
