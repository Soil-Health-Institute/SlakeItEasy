#' Get Soil Area for a Single Image
#'
#' Determine the total area (in pixels) of soil present within an image after masking. Masking can be conducted interactively, programmatically with circular or rectangular areas, or automatically using the normalized difference between red and green bands.
#'
#' @details `area_from_image()` can be called directly to classify soil in a single image, given the path to the image file, or via [SlakeItEasy::process_petri()] to classify a set of images corresponding to a measurement sequence for a single replicate.
#'
#' Proper masking of non-soil objects is critical for reliable classification with Otsu's histogram thresholding method (see [EBImage::otsu()]). Programmatic circular masking (interactive = F, circular = T) is recommended for most use cases. Classification results are provided as a binary image, with each soil object labeled using [EBImage::bwlabel()], and as a false color overlay to support evaluation of potential sensitive to masking parameters.
#'
#' @param path_to_rgb character string containing relative or absolute path to color image
#' @param interactive logical. If `TRUE`, image masking will be conducted interactively.
#' @param circular_mask logical. If `TRUE`, a circular mask will be applied.
#' @param false_color character string with color name or hex code for false color overlay
#' @param d diameter of circular mask as a proportion of image width
#' @param h_offset horizontal offset for center of circular mask
#' @param v_offset vertical offset for center of circular mask
#' @param w_new width for image resizing
#' @param h_new height for image resizing
#' @param aggregates_in_initial number of soil aggregates in initial (pre-submersion) image. If not NULL (default), this number of the largest aggregates will be retained, and all smaller objects classified as soil will be disregarded.
#' @param max_rel_dist_from_centroid maximum allowed relative distance from centroid of largest three objects
#' @param max_rel_dist_from_center maximum allowed relative distance from image center
#' @param fixed_crop_fraction proportion of image width to crop. If not NULL (default), must be between zero and one (exclusive).
#' @param max_rel_objlength maximum allowed object length as a proportion of image width
#' @param automask_buffer proportional buffer for automated masking. Only used if interactive=FALSE, circular_mask=FALSE, and fixed_crop_fraction=NULL.
#' @param erode_kern erosion kernel size for automated masking. Only used if interactive=FALSE, circular_mask=FALSE, and fixed_crop_fraction=NULL. See [EBImage::erode()].
#' @param dilate_kern dilation kernel size for automated masking. Only used if interactive=FALSE, circular_mask=FALSE, and fixed_crop_fraction=NULL. See [EBImage::dilate()].
#' @param normdiff_min minimum value of normalized difference between red and green bands for automated masking. Only used if interactive=FALSE, circular_mask=FALSE, and fixed_crop_fraction=NULL. See [SlakeItEasy::norm_diff()].
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
area_from_image <- function(path_to_rgb, interactive = FALSE, circular_mask = TRUE, false_color = 'red', d = 0.7, h_offset = 0, v_offset = 0, w_new = NULL, h_new = NULL, aggregates_in_initial = NULL, max_rel_dist_from_centroid = 1.3, max_rel_dist_from_center = 0.37,  fixed_crop_fraction = NULL, max_rel_objlength = 0.3, automask_buffer = 0.15, erode_kern = 11, dilate_kern = 31, normdiff_min = 0) {

  # read in image
  img_orig <- read_to_portrait(path_to_rgb)

  resized <- F

  # if new dimensions (w_new or h_new) are provided, resize the image
  if (!is.null(w_new) & !is.null(h_new)) {

    dims <- dim(img_orig)

    if (!(w_new %in% dims)) {

      resized <- T
      img_orig <- resize(img_orig, w = w_new, h = h_new)

    }

  }

  # interactive masking

  if (interactive) {
    # rectangular mask
    if (!circular_mask) {
      plot(img_orig)
      p_out <- set_crop()
      tmp <- crop_image(img_orig, p_out, mask_only = T)
      tmp_binary <- rgb_to_binary(tmp)
      bbox <- do.call(cbind, p_out)
      tmp_binary_labeled <- drop_edge_objs(tmp_binary, bbox)
    } else{
      # circular mask
      tmp <- mask_image_circular(img_orig, d = d, stepsize_center = 0.01, stepsize_diam = 0.01, interactive = interactive, h_offset = h_offset, v_offset = v_offset)
      # classification
      tmp_binary <- rgb_to_binary(tmp)
      tmp_binary_labeled <- drop_edge_objs(tmp_binary)

      storage.mode(tmp_binary_labeled) <- 'integer'

      attr(tmp_binary_labeled, "circular_mask_center") <- attr(tmp, "circular_mask_center")
      attr(tmp_binary_labeled, "circular_mask_diam") <- attr(tmp, "circular_mask_diam")
    }
    # retain only largest aggregates in initial (pre-submersion) image
    if (!is.null(aggregates_in_initial)) {

      tmp_binary_labeled <- reenumerate(tmp_binary_labeled)

      features <- data.frame(computeFeatures.shape(tmp_binary_labeled), computeFeatures.moment(tmp_binary_labeled))

      features$obj_id <- 1:nrow(features)

      features_filtered <- dplyr::slice_max(features, s.area, n = aggregates_in_initial)

      obj_to_drop <- unique(features$obj_id[!(features$obj_id %in% features_filtered$obj_id)])

      tmp_binary_labeled <- rmObjects(tmp_binary_labeled, obj_to_drop, reenumerate = F)
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

  if (circular_mask) {
    tmp <- mask_image_circular(img_orig, d = d, stepsize_center = 0.01, stepsize_diam = 0.01, interactive = interactive, h_offset = h_offset, v_offset = v_offset)
    tmp_binary <- rgb_to_binary(tmp)
    tmp_binary_labeled <- bwlabel(tmp_binary)
    storage.mode(tmp_binary_labeled) <- 'integer'
    attr(tmp_binary_labeled, "circular_mask_center") <- attr(tmp, "circular_mask_center")
    attr(tmp_binary_labeled, "circular_mask_diam") <- attr(tmp, "circular_mask_diam")
  } else {
    if (!is.null(fixed_crop_fraction)) {

      oneside_crop <- fixed_crop_fraction/2

      if (length(oneside_crop) == 2) {

        bbox <- round(cbind(dims[1] * c(oneside_crop[1], 1 - oneside_crop[1]),  dims[2] * c(oneside_crop[2], 1 - oneside_crop[2])))

      } else{bbox <- round(t(outer(dims[1:2], c(oneside_crop, 1 - oneside_crop), FUN = '*')))}

    } else {

      img_orig_b1b2 <- norm_diff(img_orig, 1, 2, normdiff_min)

      # erode_kern <- min(c(max(c(3, round(prod(dims[1:2]) * 5.8e-6))), 11))
      # dilate_kern <- min(c(round(prod(dims[1:2]) * 1.4e-4), 501))

      img_orig_b1b2_ed <- erode_and_dilate(img_orig_b1b2, erode_kern, dilate_kern)

      img_orig_b1b2_ed <- drop_edge_objs(img_orig_b1b2_ed)

      img_orig_b1b2_ed_binary <- img_orig_b1b2_ed > 0

      bbox <- matrix_bbox(1, img_orig_b1b2_ed_binary)

      bbox[1,] <-  round(bbox[1,] * (1 - automask_buffer))
      bbox[2,] <-  round(bbox[2,] * (1 + automask_buffer))

      bbox[1,] <- sapply(1:2, function(x) max(c(bbox[1,x], 1)))
      bbox[2,] <- sapply(1:2, function(x) min(c(bbox[2,x], dims[x])))

    }


    tmp <- crop_image(img_orig, list(x = bbox[,1], y = bbox[,2]), mask_only = T)

    tmp_binary <- rgb_to_binary(tmp)

    tmp_binary_labeled <- drop_edge_objs(tmp_binary, bbox)
  }

  if((nrow(table(tmp_binary_labeled)) - 1) < 3) {
    stop("Fewer than 3 objects in image interior. Try reducing fixed_crop_fraction or expanding manual mask area.", call. = F)
  }

    tmp_binary_labeled <- reenumerate(tmp_binary_labeled)

    features <- data.frame(computeFeatures.shape(tmp_binary_labeled), computeFeatures.moment(tmp_binary_labeled))

    features$obj_id <- 1:nrow(features)

    features$m.cx <- round(features$m.cx)
    features$m.cy <- round(features$m.cy)

    features$perim_to_area <- with(features, s.perimeter / s.area)

    features$dist_to_image_center <- with(features, sqrt((m.cx - dims[1]/2)^2 + (m.cy - dims[2]/2)^2))

    maximum_distance_from_center <- min(max_rel_dist_from_center*dims[1:2])

    maximum_obj_length_for_centroid <- min(max_rel_objlength*dims[1:2])

    features_filtered <- features[features$m.majoraxis <= maximum_obj_length_for_centroid & features$dist_to_image_center <= maximum_distance_from_center,]

    if (!is.null(aggregates_in_initial)) {

      features_filtered <- dplyr::slice_max(features_filtered, s.area, n = aggregates_in_initial)

      attr(tmp_binary_labeled, "aggregates_in_initial") <- aggregates_in_initial

    }

    features_filtered$dist_to_centroid <- with(features_filtered, dist_from_n_objs(m.cx, m.cy, area_var = s.area))

    min_dist <- ceiling(max(dplyr::slice_max(features_filtered, s.area, n = 3)$dist_to_centroid))

    max_dist_from_centroid <- min_dist*max_rel_dist_from_centroid

    obj_to_drop <- unique(c(features_filtered$obj_id[features_filtered$dist_to_centroid > max_dist_from_centroid], features$obj_id[!(features$obj_id %in% features_filtered$obj_id)]))

    tmp_binary_labeled <- rmObjects(tmp_binary_labeled, obj_to_drop, reenumerate = F)

    attr(tmp_binary_labeled, 'max_rel_dist_from_centroid') <- max_rel_dist_from_centroid
    attr(tmp_binary_labeled, 'max_dist_from_centroid') <- max_dist_from_centroid
    attr(tmp_binary_labeled, 'max_rel_dist_from_center') <- max_rel_dist_from_center

    area_min <- sum(features_filtered$s.area[features_filtered$dist_to_centroid <= min_dist])
    area_at_cutoff <- sum(features_filtered$s.area[features_filtered$dist_to_centroid <= max_dist_from_centroid])

    area_max <- sum(features_filtered$s.area)

    area_out <- area_at_cutoff
    # area_out <- c(area_min, area_at_cutoff, area_max)

  img_attributes <- attributes(tmp)

  img_attributes_nowrite <- names(attributes(tmp_binary_labeled))

  img_attributes_to_write <- img_attributes[!names(img_attributes) %in% img_attributes_nowrite]

  for (i in seq_along(img_attributes_to_write)) {
    attr(tmp_binary_labeled, names(img_attributes_to_write)[i]) <- img_attributes_to_write[[i]]
  }

  attr(tmp_binary_labeled, 'fixed_crop_fraction') <- fixed_crop_fraction

  }

  attr(tmp_binary_labeled, 'resized') <- resized

  msk <- tmp_binary_labeled
  msk <- paintObjects(msk, img_orig, col= rep(false_color, 2), opac = c(1, 0.4))

  return(list(area = area_out, classified_image = tmp_binary_labeled, false_color = msk))

}
