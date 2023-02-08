#' Title
#'
#' @param path_to_image_set
#' @param interactive
#' @param circular_crop
#' @param d
#' @param image_extension
#' @param peds_in_initial
#' @param fixed_crop_fraction_initial
#' @param fixed_crop_fraction_run
#' @param max_rel_dist_from_center
#' @param max_rel_dist_from_centroid
#' @param max_rel_objlength
#' @param autocrop_buffer
#' @param erode_kern
#' @param dilate_kern
#' @param h_offset
#' @param v_offset
#' @param match_resolution
#' @param safe_sep
#' @param outdir
#' @param normdiff_min
#'
#' @return
#' @export
#'
#' @examples
process_petri <- function(path_to_image_set, interactive = F, circular_crop = F, d = 0.7, filename_prefix = 'Develop', image_extension = 'jpg', peds_in_initial = 3, fixed_crop_fraction_initial = c(0.28, 0.6), fixed_crop_fraction_run = fixed_crop_fraction_initial, max_rel_dist_from_center = 0.4,  max_rel_dist_from_centroid = 1.3, max_rel_objlength = 0.4, autocrop_buffer = 0.15, erode_kern = 5, dilate_kern = 31,  h_offset = 0, v_offset = 0, match_resolution = F, safe_sep = '_x_', outdir = '/Users/nlooker-shi/Documents/slakes_processed/tmp', normdiff_min) {

  # get platform-specific file separator character
  filesep <- .Platform$file.sep

  image_paths <- list.files(path_to_image_set, full.names = T, pattern = paste0('.', image_extension, '$'))

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

  timestamp <- strptime(timestamp_orig, format = '%Y%m%d_%H%M%S')

  # timestamp_start <- stringr::str_locate(image_metadata$FileName, year)[,1]
  #
  # timestamp_orig <- substr(image_metadata$FileName, timestamp_start, stop = nchar(image_metadata$FileName) - (nchar(image_extension) + 1))
  #
  # timestamp <- strptime(timestamp_orig, format = '%Y%m%d_%H%M%S')

  area_airdry <- area_from_image(image_paths[[1]], interactive = interactive, circular_crop = circular_crop, d = d, peds_in_initial = peds_in_initial, fixed_crop_fraction = fixed_crop_fraction_initial, max_rel_dist_from_center = max_rel_dist_from_center, max_rel_objlength = max_rel_objlength, autocrop_buffer = autocrop_buffer, erode_kern = erode_kern, dilate_kern = dilate_kern,  h_offset = h_offset, v_offset = v_offset, w_new = w_new, h_new = h_new, normdiff_min = normdiff_min)

  area_list <- lapply(2:length(timestamp), function(x) area_from_image(image_paths[[x]], interactive = interactive, circular_crop = circular_crop, d = d, fixed_crop_fraction = fixed_crop_fraction_run, max_rel_dist_from_center = max_rel_dist_from_center, max_rel_dist_from_centroid = max_rel_dist_from_centroid, max_rel_objlength = max_rel_objlength, autocrop_buffer = autocrop_buffer, erode_kern = erode_kern, dilate_kern = dilate_kern, h_offset = h_offset, v_offset = v_offset, w_new = w_new, h_new = h_new, normdiff_min = normdiff_min))

  area_df <- data.frame(timestamp, rbind(area_airdry$area, t(sapply(area_list, function(x) x$area))))

  names(area_df)[-1] <- c('low', 'medium', 'high')

  SI_ad <- as.numeric((area_df[nrow(area_df),2:4] /   area_df$low[1]) - 1)

  potential_crop_error <- 100 * ((SI_ad[2:3] / SI_ad[1]) - 1)

  out <- c(list(area_airdry), area_list)

  image_set_prefix <- gsub(filesep, safe_sep, path_to_image_set, fixed = T)

  dir_masked <- paste0(outdir, filesep, 'images_masked')
  dir_binary <- paste0(outdir, filesep, 'images_binary')
  dir_attr <- paste0(outdir, filesep, 'classification_attributes')
  dir_results <- paste0(outdir, filesep, 'area_by_time')

  suppressWarnings(dir.create(dir_masked, recursive = T))
  suppressWarnings(dir.create(dir_binary, recursive = T))
  suppressWarnings(dir.create(dir_attr, recursive = T))
  suppressWarnings(dir.create(dir_results, recursive = T))

  filenames_masks <- paste0(dir_masked, filesep, image_set_prefix, safe_sep, timestamp_orig, '.', image_extension)
  filenames_binaries <- paste0(dir_binary, filesep, image_set_prefix, safe_sep, timestamp_orig, '.', image_extension)

  invisible(lapply(1:length(out), function(x) EBImage::writeImage(out[[x]]$masked_image, filenames_masks[x], quality = 50)))
  invisible(lapply(1:length(out), function(x) EBImage::writeImage(out[[x]]$classified, filenames_binaries[x], quality = 100)))

  saveRDS(lapply(1:length(out), function(x) attributes(out[[x]]$classified)), paste0(dir_attr, filesep, image_set_prefix, '.rds'))

  area_df$image_set <- path_to_image_set

  area_df$FileName <- image_metadata$FileName

  write.csv(area_df, paste0(dir_results, filesep, image_set_prefix, '.csv'), row.names = F)

  return(area_df)

}
