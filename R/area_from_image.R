#' Title
#'
#' @param path_to_rgb
#' @param interactive
#' @param circular_crop
#' @param d
#' @param max_rel_dist_from_centroid
#' @param max_rel_dist_from_center
#' @param peds_in_initial
#' @param fixed_crop_fraction
#' @param max_rel_objlength
#' @param autocrop_buffer
#' @param erode_kern
#' @param dilate_kern
#' @param normdiff_min
#' @param h_offset
#' @param v_offset
#' @param w_new
#' @param h_new
#'
#' @return
#' @export
#'
#' @examples
area_from_image <- function(path_to_rgb, interactive = F, circular_crop = F,  d = 0.7, max_rel_dist_from_centroid = 1.3, max_rel_dist_from_center = 0.37, peds_in_initial = NULL, fixed_crop_fraction = NULL, max_rel_objlength = 0.3, autocrop_buffer = 0.15, erode_kern = 11, dilate_kern = 31, normdiff_min = 0, h_offset = 0, v_offset = 0, w_new = NULL, h_new = NULL) {

  img_orig <- read_to_portrait(path_to_rgb)

  resized <- F

  if (!is.null(w_new) & !is.null(h_new)) {

    dims <- dim(img_orig)

    if (!(w_new %in% dims)) {

      resized <- T
      img_orig <- resize(img_orig, w = w_new, h = h_new)

    }

  }


  if (interactive) {
    if (!circular_crop) {
      plot(img_orig)
      p_out <- set_crop()
      tmp <- crop_image(img_orig, p_out, mask_only = T)
      tmp_binary <- rgb_to_binary(tmp)
      bbox <- do.call(cbind, p_out)
      tmp_binary_labeled <- drop_edge_objs(tmp_binary, bbox)
    } else{
      tmp <- crop_image_circular(img_orig, d = d, stepsize_center = 0.01, stepsize_diam = 0.01, interactive = interactive, h_offset = h_offset, v_offset = v_offset)
      tmp_binary <- rgb_to_binary(tmp)
      tmp_binary_labeled <- bwlabel(tmp_binary)
      storage.mode(tmp_binary_labeled) <- 'integer'
      attr(tmp_binary_labeled, "circular_crop_center") <- attr(tmp, "circular_crop_center")
      attr(tmp_binary_labeled, "circular_crop_diam") <- attr(tmp, "circular_crop_diam")
    }

    if (!is.null(peds_in_initial)) {

      features <- data.frame(computeFeatures.shape(tmp_binary_labeled), computeFeatures.moment(tmp_binary_labeled))

      features$obj_id <- 1:nrow(features)

      features_filtered <- dplyr::slice_max(features, s.area, n = peds_in_initial)

      obj_to_drop <- unique(features$obj_id[!(features$obj_id %in% features_filtered$obj_id)])

      tmp_binary_labeled <- rmObjects(tmp_binary_labeled, obj_to_drop, reenumerate = F)
      attr(tmp_binary_labeled, "peds_in_initial") <- peds_in_initial

    }


    msk <- tmp_binary_labeled
    msk <- paintObjects(msk, img_orig, col=c('red', 'red'), opac = c(1, 0.4))

    plot(msk)

    Sys.sleep(0.5)

    area_out <- sum(tmp_binary)

    attr(tmp_binary_labeled, 'crop_type') <- 'interactive'

    return(list(area = rep(area_out, 3), classified_image = tmp_binary_labeled, masked_image = msk))

  } else{

  dims <- dim(img_orig)

  if (circular_crop) {
    tmp <- crop_image_circular(img_orig, d = d, stepsize_center = 0.01, stepsize_diam = 0.01, interactive = interactive, h_offset = h_offset, v_offset = v_offset)
    tmp_binary <- rgb_to_binary(tmp)
    tmp_binary_labeled <- bwlabel(tmp_binary)
    storage.mode(tmp_binary_labeled) <- 'integer'
    attr(tmp_binary_labeled, "circular_crop_center") <- attr(tmp, "circular_crop_center")
    attr(tmp_binary_labeled, "circular_crop_diam") <- attr(tmp, "circular_crop_diam")
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

      bbox[1,] <-  round(bbox[1,] * (1 - autocrop_buffer))
      bbox[2,] <-  round(bbox[2,] * (1 + autocrop_buffer))

      bbox[1,] <- sapply(1:2, function(x) max(c(bbox[1,x], 1)))
      bbox[2,] <- sapply(1:2, function(x) min(c(bbox[2,x], dims[x])))

    }


    tmp <- crop_image(img_orig, list(x = bbox[,1], y = bbox[,2]), mask_only = T)

    tmp_binary <- rgb_to_binary(tmp)

    tmp_binary_labeled <- drop_edge_objs(tmp_binary, bbox)
  }

  if((nrow(table(tmp_binary_labeled)) - 1) < 3) {
    stop("Fewer than 3 objects in image interior. Try reducing fixed_crop_fraction or expanding manual crop box.", call. = F)
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

    if (!is.null(peds_in_initial)) {

      features_filtered <- dplyr::slice_max(features_filtered, s.area, n = peds_in_initial)

      attr(tmp_binary_labeled, "peds_in_initial") <- peds_in_initial

    }

    # plot(tmp_binary_labeled)
    # with(features_filtered, points(m.cx, m.cy, col = 'red'))

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

    area_out <- c(area_min, area_at_cutoff, area_max)

  img_attributes <- attributes(tmp)

  img_attributes_nowrite <- names(attributes(tmp_binary_labeled))

  img_attributes_to_write <- img_attributes[!names(img_attributes) %in% img_attributes_nowrite]

  for (i in 1:length(img_attributes_to_write)) {
    attr(tmp_binary_labeled, names(img_attributes_to_write)[i]) <- img_attributes_to_write[[i]]
  }

  attr(tmp_binary_labeled, 'fixed_crop_fraction') <- fixed_crop_fraction

  }

  attr(tmp_binary_labeled, 'resized') <- resized

  msk <- tmp_binary_labeled
  msk <- paintObjects(msk, img_orig, col=c('red', 'red'), opac = c(1, 0.4))

  return(list(area = area_out, classified_image = tmp_binary_labeled, masked_image = msk))

}
