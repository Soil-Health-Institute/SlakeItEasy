#' Get Soil Area for a Single Image
#'
#' Determine the total area (in pixels) of soil present within an image after applying a circular mask. Masking can be conducted interactively or programmatically.
#'
#' @details `area_from_image()` can be called directly to classify soil in a single image, given the path to the image file, or via [SlakeItEasy::process_petri()] to classify a set of images corresponding to a measurement sequence for a single replicate.
#'
#' Proper masking of non-soil objects is critical for reliable classification with Otsu's histogram thresholding method (see [EBImage::otsu()]). Programmatic masking (interactive = F, circular = T) is recommended for most use cases. Classification results are provided as a binary image, with each soil object labeled using [EBImage::bwlabel()], and as a false color overlay to support evaluation of potential sensitive to masking parameters.
#'
#' @param path_to_rgb character string containing relative or absolute path to color image
#' @param interactive logical. If `TRUE`, image masking will be conducted interactively.
#' @param false_color character string with color name or hex code for false color overlay
#' @param d diameter of circular mask as a proportion of image width
#' @param h_offset horizontal offset for center of circular mask
#' @param v_offset vertical offset for center of circular mask
#' @param w_new width for image resizing
#' @param h_new height for image resizing
#' @param aggregates_in_initial number of soil aggregates in initial (pre-submersion) image. If not NULL (default), this number of the largest aggregates will be retained, and all smaller objects classified as soil will be disregarded.
#'
#' @return A three-element list, containing the area of soil (in pixels), an `Image` object with labeled soil pixels (see [EBImage::bwlabel()]), and the original `Image` object with soil pixels overlain in false color.
#' @export
#'
#' @examples # path to initial (pre-submersion) image
#' f_init <- system.file("images/exp1_1", "IMG_20220701_120110.jpg", package="SlakeItEasy")
#'
#' # path to second image (time zero of slaking)
#' f_t0 <- system.file("images/exp1_1", "IMG_20220701_120230.jpg", package="SlakeItEasy")
#'
#' # path to final image (ten minutes of slaking)
#' f_t10 <- system.file("images/exp1_1", "IMG_20220701_121230.jpg", package="SlakeItEasy")
#'
#' # processing with default circular mask
#' a_init <- area_from_image(f_init, aggregates_in_initial = 3)
#' a_t0 <- area_from_image(f_t0)
#' a_t10 <- area_from_image(f_t10)
#'
#' cat('initial area: ', a_init$area, '\n',
#' 'area upon submersion: ', a_t0$area, '\n',
#' 'final area: ', a_t10$area, '\n',
#' 'stab10: ', a_init$area/a_t10$area)
#'
#' par(mfrow = c(1, 3))
#' plot(a_init$false_color)
#' plot(a_t0$false_color)
#' plot(a_t10$false_color)
#'
#'
area_from_image <- function(path_to_rgb,
                            interactive = FALSE,
                            false_color = 'red',
                            d = 0.7,
                            h_offset = 0,
                            v_offset = 0,
                            w_new = NULL,
                            h_new = NULL,
                            aggregates_in_initial = NULL) {

  # read in image
  img_orig <- read_to_portrait(path_to_rgb)

  resized <- F

  # if new dimensions (w_new or h_new) are provided, resize the image
  dims <- dim(img_orig)
  if (!is.null(w_new)) {
    if (w_new != min(dims[1:2])) {
      resized <- T
      img_orig <- EBImage::resize(img_orig, w = w_new)
    }
    } else {
      if (!is.null(h_new)) {
      if (h_new != max(dims[1:2])) {
        resized <- T
        img_orig <- EBImage::resize(img_orig, h = h_new)
      }
    }
  }

  # interactive masking

  if (interactive) {
      tmp <- mask_image_circular(img_orig,
                                 d = d,
                                 stepsize_center = 0.01,
                                 stepsize_diam = 0.01,
                                 interactive = interactive,
                                 h_offset = h_offset,
                                 v_offset = v_offset)
      # classification
      tmp_binary <- rgb_to_binary(tmp)
      tmp_binary_labeled <- drop_edge_objs(tmp_binary)

      storage.mode(tmp_binary_labeled) <- 'integer'

      attr(tmp_binary_labeled, "circular_mask_center") <- attr(tmp, "circular_mask_center")
      attr(tmp_binary_labeled, "circular_mask_diam") <- attr(tmp, "circular_mask_diam")

    # retain only largest aggregates in initial (pre-submersion) image
    if (!is.null(aggregates_in_initial)) {

      tmp_binary_labeled <- EBImage::reenumerate(tmp_binary_labeled)

      features <- data.frame(EBImage::computeFeatures.shape(tmp_binary_labeled))

      features$obj_id <- 1:nrow(features)

      features_filtered <- dplyr::slice_max(features, s.area, n = aggregates_in_initial)

      obj_to_drop <- unique(features$obj_id[!(features$obj_id %in% features_filtered$obj_id)])

      tmp_binary_labeled <- EBImage::rmObjects(tmp_binary_labeled, obj_to_drop, reenumerate = F)
      attr(tmp_binary_labeled, "aggregates_in_initial") <- aggregates_in_initial

    }

    msk <- tmp_binary_labeled
    msk <- paintObjects(msk, img_orig, col= rep(false_color, 2), opac = c(1, 0.4))

    plot(msk)

    Sys.sleep(0.5)

    tmp_binary <- as.numeric(tmp_binary_labeled)

    storage.mode(tmp_binary) <- 'logical'

    area_out <- sum(tmp_binary, na.rm = T)

    attr(tmp_binary_labeled, 'crop_type') <- 'interactive'

    return(list(area = area_out, classified_image = tmp_binary_labeled, false_color = msk))

  } else{

  dims <- dim(img_orig)

    tmp <- mask_image_circular(img_orig, d = d, stepsize_center = 0.01, stepsize_diam = 0.01, interactive = interactive, h_offset = h_offset, v_offset = v_offset)
    tmp_binary <- rgb_to_binary(tmp)
    tmp_binary_labeled <- EBImage::bwlabel(tmp_binary)
    storage.mode(tmp_binary_labeled) <- 'integer'
    attr(tmp_binary_labeled, "circular_mask_center") <- attr(tmp, "circular_mask_center")
    attr(tmp_binary_labeled, "circular_mask_diam") <- attr(tmp, "circular_mask_diam")

    tmp_binary_labeled <- EBImage::reenumerate(tmp_binary_labeled)

    if (!is.null(aggregates_in_initial)) {

      features <- data.frame(EBImage::computeFeatures.shape(tmp_binary_labeled))

      features$obj_id <- 1:nrow(features)

      features_filtered <- dplyr::slice_max(features, s.area, n = aggregates_in_initial)

      obj_to_drop <- unique(features$obj_id[!(features$obj_id %in% features_filtered$obj_id)])

      tmp_binary_labeled <- EBImage::rmObjects(tmp_binary_labeled, obj_to_drop, reenumerate = F)
      attr(tmp_binary_labeled, "aggregates_in_initial") <- aggregates_in_initial

      area_out <- sum(features_filtered$s.area)


    } else{

      area_out = sum(tmp_binary)

    }

  img_attributes <- attributes(tmp)

  img_attributes_nowrite <- names(attributes(tmp_binary_labeled))

  img_attributes_to_write <- img_attributes[!names(img_attributes) %in% img_attributes_nowrite]

  for (i in seq_along(img_attributes_to_write)) {

    attr(tmp_binary_labeled, names(img_attributes_to_write)[i]) <- img_attributes_to_write[[i]]

  }
  }

  attr(tmp_binary_labeled, 'resized') <- resized

  msk <- tmp_binary_labeled
  msk <- EBImage::paintObjects(msk, img_orig, col= rep(false_color, 2), opac = c(1, 0.4))

  return(list(area = area_out,
              classified_image = tmp_binary_labeled,
              false_color = msk))

}
