#' Image Input
#'
#' Read an image into the current environment and check orientation. Rotate landscape-oriented images into portrait orientation.
#'
#' @details `read_to_portrait` is a simple wrapper around [EBImage::readImage()] to ensure that images are in portrait orientation prior to further processing.
#'
#'The following metadata are recorded as attributes of the `Image` object.
#'
#'Attribute `rotated` = `no` if the original image was already in portrait orientation and `yes` if the image was rotated upon input.
#'
#' Attribute `source_path` records the argument passed to `input_path`.
#'
#' @param input_path path to the image file. Path may be provided as an absolute path or relative to the current working directory.
#'
#' @return An object of class `Image`.
#'
#' @examples # path to example image file
#' f <- system.file("images/exp1_1", "IMG_20220701_120110.jpg", package="SlakeItEasy")
#'
#' # read in image
#' img_portrait <- read_to_portrait(f)
#'
#' # verify that image is in portrait orientation
#' EBImage::display(img_portrait)
#'
#' # get image dimensions
#' dim(img_portrait)
#'
#' # compare with dimensions of original image
#' dim(EBImage::readImage(f))
#'
#' @export
#'
read_to_portrait <- function(input_path) {

  # read in raw image
  img <- EBImage::readImage(input_path)

  # get image dimensions
  dims <- dim(img)

  # rotate landscape-oriented image 90 degrees
  if (dims[1] > dims[2]) {
    img <- EBImage::rotate(img, angle = 90, filter = 'none')
    dims <- dim(img)

    attr(img, 'rotated') <- 'yes'

  } else{attr(img, 'rotated') <- 'no'}

  attr(img, 'source_path') <- input_path

  return(img)

}

#' Crop or Mask an Image
#'
#' Produce a rectangular subset of an image, either with the same dimensions as the original image but pixels outside of the subset set to black (mask) or with dimensions aligned to the subset (crop).
#'
#' @details `crop image` crops or masks an `Image` object based on a rectangular extent provided as a list.
#'
#' The `Image` object returned by `crop image` has attributes `crop_dim` and `source_dim` with the crop/mask extent and original image dimensions, respectively.
#'
#' @param img `Image` object
#' @param dim_list List containing the crop/mask extent as two vectors of length two (minimum and maximum coordinates in the horizontal and in the vertical dimensions)
#' @param mask_only Logical. If `TRUE`, the output image will retain the dimensions of the input image.
#'
#' @return An object of class `Image`.
#'
#' @examples # path to example image file
#' f <- system.file("images/exp1_1", "IMG_20220701_120110.jpg", package="SlakeItEasy")
#'
#' # read in image
#' img_in <- read_to_portrait(f)
#'
#' # define extent for crop
#' e <- list(x = c(115, 540), y = c(220, 640))
#'
#' # crop image
#' img_cropped <- crop_image(img_in, e)
#'
#' EBImage::display(img_cropped)
#'
#' # mask image
#' img_masked <- crop_image(img_in, e, mask_only = T)
#'
#' EBImage::display(img_masked)
#'
#' @export
#'
crop_image <- function(img, dim_list, mask_only = F) {

  i <- lapply(dim_list, function(v) min(v):max(v))

  if (!mask_only) {

  img_crop <- img[i$x, i$y,]

  attr(img_crop, 'crop_dim') <- dim_list
  attr(img_crop, 'source_dim') <- dim(img)

  return(img_crop)

  } else {

    dims <- dim(img)
    xvec <- 1:dims[1]
    yvec <- 1:dims[2]
    xvec_mask <- xvec[xvec < min(dim_list$x) | xvec > max(dim_list$x)]

    yvec_mask <- yvec[yvec < min(dim_list$y) | yvec > max(dim_list$y)]

    img_masked <- img

    if (length(dims) == 3) {
      img_masked[xvec_mask, , ] <- NA
      img_masked[,yvec_mask, ] <- NA
    } else{
      img_masked[xvec_mask, ] <- NA
      img_masked[,yvec_mask] <- NA
    }


    attr(img_masked, 'mask_dim') <- dim_list
    attr(img_masked, 'dim_source') <- dim(img)

    return(img_masked)

  }

}

#' Interactively Determine an Extent for Rectangular Cropping
#'
#' @param npts Number of points for extent
#'
#' @return Two-element list with first element a vector of minimum and maximum horizontal coordinates and second element a vector of minimum and maximum vertical coordinates.
#' @export
#'
#' @examples
#' # path to example image file
#' f <- system.file("images/exp1_1", "IMG_20220701_120110.jpg", package="SlakeItEasy")
#'
#' # read in image
#' img_portrait <- read_to_portrait(f)
#'
#' # plot image
#' plot(img_portrait)
#'
#' # get extent for crop
#' (ext <- set_crop())
#'
set_crop <- function(npts = 2) {
  p <- locator(npts, type = "p", pch = 3, col = "red")
  p_out <- lapply(p, function(x) round(c(min(x), c(max(x)))))
  return(p_out)
}

#' Classify Soil and Background Pixels in a Color Image
#'
#' Average pixel intensities across red, green, and blue bands, and apply Otsu's binary thresholding algorithm to the pixel intensity histogram.
#'
#' @details Pixel intensities are averaged across red, green, and blue bands. The resulting histogram is then partitioned into two classes using Otsu's method for threshold optimization (Otsu 1979). Note that bandwise classification requires 10x computation time and yields results comparable to the average-then-classify approach implemented in {SlakeItEasy}. Because Otsu's method may fail to discern soil from other dark objects within the image, `rgb_to_binary` should be applied after cropping/masking the image.
#'
#' @param img Three-band `Image` object
#'
#' @return One-band `Image object`, with pixel values of 1 indicating soil and 0 indicating background.
#'
#' @references N. Otsu. A threshold selection method from gray-level histograms. IEEE Trans. Sys., Man., Cyber. 9 (1): 62-66, 1979. doi:10.1109/TSMC.1979.4310076.
#'
#' @export
#'
#' @examples
#' # path to example image file
#' f <- system.file("images/exp1_1", "IMG_20220701_120110.jpg", package="SlakeItEasy")
#'
#' # read in image
#' img <- read_to_portrait(f)
#'
#' # apply circular mask to image
#' img_masked <- mask_image_circular(img, interactive = F)
#'
#' classified <- rgb_to_binary(img_masked)
#'
#' par(mfrow = c(1, 2))
#' plot(img); plot(classified)
#'
rgb_to_binary <- function(img) {

  # average R, G, and B bands
  img <- (img[,,1] + img[,,2] + img[,,3])/3

  # change color mode to grayscale (required for EBImage::otsu())
  EBImage::colorMode(img) <- EBImage::Grayscale

  # estimate soil/background thresholds for each band
  threshold <- EBImage::otsu(img)

  # apply threshold to image
  img_binary <- 1 - (img > threshold)

  # replace NAs (from mask/crop) with zeroes - otherwise, NAs will be labeled as objects
  img_binary[is.na(img_binary)] <- 0

  storage.mode(img_binary) <- 'integer'

  attr(img_binary, 'otsu_threshold') <- threshold

  return(img_binary)

}

#' Threshold an Image by Normalized Difference between Two Bands
#'
#' Calculate the normalized difference ((x - y) / (x + y)) between two image bands, and segment the image according to a specified value of the normalized difference.
#'
#' @details Conservative method of identifying soil within an image. This algorithm is provided to support the masking process when a simple circular mask is insufficient (e.g., non-soil objects are interspersed with soil). `norm_diff` will generally underestimate the total area of soil and is intended to be used in conjunction with [EBImage::erode_and_dilate()].
#'
#' @param img `Image` object
#' @param b1_idx index for first band
#' @param b2_idx index for second band
#' @param mindiff threshold minimum value for normalized difference between bands
#'
#' @return One-band `Image object`, with pixel values of 1 indicating normalized difference values greater than or equal to `mindiff` and values of 0 indicating normalize difference values less than `mindiff`
#' @export
#'
#' @examples
#' # Path to example image
#' f <- system.file("images/exp1_1", "IMG_20220701_120110.jpg", package="SlakeItEasy")
#'
#' # Read in image
#' img <- read_to_portrait(f)
#'
#' # Apply circular mask to image
#' masked <- mask_image_circular(img, interactive = F)
#'
#' # Threshold with normalized difference
#' nd <- norm_diff(masked, 2, 3)
#' plot(nd)
#'
norm_diff <- function(img, b1_idx, b2_idx, mindiff = 0){
  out <- (img[,,b1_idx]  - img[,,b2_idx]) / (img[,,b1_idx]  + img[,,b2_idx])
  colorMode(out) <- Grayscale
  out <- out >= mindiff
  storage.mode(out) <- 'integer'
  return(out)
}

# erode and then dilate an image with a disc-shaped kernel
erode_and_dilate <- function(img, erode_size = 5, dilate_size = erode_size * 7) {

  kern <- makeBrush(erode_size, shape='disc')

  dilate(erode(img, kern), makeBrush(dilate_size, shape='disc'))

}

# get coordinates of bounding box around an object within a matrix OR return summary statistics on bbox geometry
matrix_bbox <- function(obj_id, matr, return_summary = F) {
  indices <- which(matr == obj_id, arr.ind = T)
  indices_range <- apply(indices, 2, range)

  if (return_summary) {

    indices_range <- unname(indices_range)

    xmin <- indices_range[1, 1]
    xmax <- indices_range[2,1]
    ymin <- indices_range[1, 2]
    ymax <- indices_range[2,2]

    xdiff <- xmax - xmin + 1
    ydiff <- ymax - ymin + 1

    # bounding box area
    area_out <- xdiff*ydiff

    # ratio of longer to short dimension
    LtS_out <- max(c(xdiff, ydiff)) / min(c(xdiff, ydiff))

    return(c(area_out, LtS_out))

  } else {
    return(indices_range)
  }
}

# get bounding boxes of all labeled objects in an image
get_object_dims <- function(img, return_counts = F, ...) {
  matr <- imageData(img)
  counts <- table(matr)
  obj_ids <- as.numeric(names(counts))
  obj_dims <- lapply(obj_ids, matrix_bbox, matr, ...)

  if (return_counts) {
    return(mapply(list, obj_dims, counts, SIMPLIFY = F))
  } else{
    return(obj_dims)
  }
}

# drop objects intersecting a bounding box (following https://support.bioconductor.org/p/52148/#52271)
drop_edge_objs <- function(img, bbox = NULL) {

  objects <- bwlabel(img)

  # subset for boundary pixels
  dims <- dim(objects)

  if (!is.null(bbox)) {
    xmin <- bbox[1,1]
    xmax <- bbox[2,1]
    ymin <- bbox[1,2]
    ymax <- bbox[2,2]
    border <- c(objects[xmin:xmax, ymin], objects[xmin:xmax, ymax], objects[xmin, ymin:ymax], objects[xmax, ymin:ymax])
  } else{
    border <- c(objects[1:dims[1],1], objects[1:dims[1],dims[2]],
                objects[1,1:dims[2]], objects[dims[1],1:dims[2]])
  }

  # extract object identifiers at the boundary
  ids <- unique(border[which(border != 0)])

  # create a mask containing all objects that do not intersect the edge of the image
  inner <- as.vector(objects)
  inner[which(objects %in% ids)] <- 0
  inner <- Image(inner, dims)

  storage.mode(inner) <- 'integer'

  return(inner)

}

keep_n_objs <- function(img, n = 3) {

  storage.mode(img) <- 'integer'

  tbl <- table(img)
  obj_to_drop <- as.numeric(names(sort(tbl, decreasing = T)[(n + 2):length(tbl)]))
  img_binary <- rmObjects(img, obj_to_drop, reenumerate = F)

}

# distance from centroid of largest N objects

dist_from_n_objs <- function(xcoord, ycoord, area_var, n = 3) {

  obj_ids <- which(rank(area_var) %in% (length(area_var) - n +1):length(area_var))

  xcoord_mean <- mean(xcoord[obj_ids])
  ycoord_mean <- mean(ycoord[obj_ids])

  out <- sqrt((xcoord - xcoord_mean)^2 + (ycoord - ycoord_mean)^2)

  return(out)

}

# interactively select center for circular crop

set_center <- function(img, h_offset = 0, v_offset = 0) {

  dims <- dim(img)[1:2]

  img_center <- dims[1:2]/2

  img_center[1]  <- img_center[1] * (1 + h_offset)
  img_center[2]  <- img_center[2] * (1 - v_offset)

  plot(img)
  p <- img_center
  points(p[1], p[2], pch = 3, col = 'red')
  cat("Type 'y' and 'enter' to accept centroid, or hit 'enter' to reposition:\n")
  rstudioapi::executeCommand('activateConsole')
  accept <- readline(prompt=' ')

  while (!grepl('y', tolower(accept))) {
    plot(img)
    cat("Click to place centroid for circular crop\n")
    p <- locator(1, type = "p", pch = 3, col = "red")
    cat("Type 'y' and 'enter' to accept centroid, or hit 'enter' to reposition:\n")
    accept <- readline(prompt = ' ')
  }

  return(p)

}

# create circular mask for crop

generate_circle <- function(img, d = 0.6, circ_center = NULL) {

  # get length and width (ignore 3rd dimension for multiband images)
  dims <- dim(img)[1:2]

  # crop radius in pixels
  r <- round(min(dims)*d / 2)

  # coordinate pairs for all pixels
  dims_long <- expand.grid(x = 1:dims[1], y = 1:dims[2])

  # if center is not specified, set to image centroid
  if (is.null(circ_center)) {
    circ_center <- dims[1:2]/2
  } else {
    if (class(circ_center) == 'list') {
      circ_center <- unlist(circ_center)
    }
  }


  # distance from each pixel to centroid
  dims_long$dist <- as.numeric(sqrt((dims_long[,1] - circ_center[1])^2 + (dims_long[,2] - circ_center[2])^2))

  # set values inside circle to one
  dims_long$dist[dims_long$dist <= r] <- 1

  # set values outside circle to zero
  dims_long$dist[dims_long$dist > r]  <- 0

  # format values as wide matrix and coerce to EBImage::Image class
  out <- tidyr::pivot_wider(dims_long, names_from = y , values_from = dist) %>%
    dplyr::select(-x) %>%
    as.matrix() %>%
    EBImage::Image()

  return(out)

}

#' Apply a Circular Mask to an Image
#'
#' Mask pixels outside of a circular area of interest.
#'
#' @details For most implementations of the Soil Health Institute's multi-sample aggregate stability standard operating procedure, `mask_image_circular` will be the primary image preparation step prior to pixel classification with [SlakeItEasy::rgb_to_binary()].
#'
#' @param img `Image` object
#' @param d diameter of circular mask as a proportion of image width
#' @param stepsize_center step size to reposition center of circular corp
#' @param stepsize_diam step size to adjust diameter of circular crop
#' @param interactive logical indicating whether the mask should be set interactively
#' @param h_offset horizontal offset for center of circular crop
#' @param v_offset vertical offset for center of circular crop
#'
#' @return `Image` object
#' @export
#'
#' @examples # path to example image file
#' f <- system.file("images/exp1_1", "IMG_20220701_120110.jpg", package="SlakeItEasy")
#'
#' # read in image
#' img_in <- read_to_portrait(f)
#'
#' # mask image
#'
#' img_masked <- mask_image_circular(img_in, interactive = F)
#'
#' plot(img_masked)
#'
#'
mask_image_circular <- function(img, d = 0.7, stepsize_center = 0.01, stepsize_diam = 0.01, interactive = T, h_offset = 0, v_offset = 0) {

  if (interactive) {
    cent <- set_center(img, h_offset = h_offset, v_offset = v_offset)
  } else {
    dims <- dim(img)[1:2]
    cent <- dims[1:2]/2
    cent[1]  <- cent[1] * (1 + h_offset)
    cent[2]  <- cent[2] * (1 - v_offset)
  }

  if (class(cent) == 'list') {
    cent <- unlist(cent)
  }

  d_out <- d

  if (interactive) {

    opt <- ''

    while (!grepl('y', tolower(opt))) {

      circ <- generate_circle(img = img, d = d_out, circ_center = cent)

      msk <- paintObjects(circ, img, col=c('red', 'red'), opac = c(1, 0.2))

      plot(msk)

      cat("Type:\n'u' to move up. 'j' to move down. 'h' to move left. 'k' to move right.\n'n' to enlarge. 'm' to shrink.\n'y' to accept.\n(press enter/return after key)")

      opt <- readline(prompt = ' ')

      if (grepl('u', tolower(opt))) {
        cent[2]  <- cent[2] * (1 - stepsize_center)
      }

      if (grepl('j', tolower(opt))) {
        cent[2] <- cent[2] * (1 + stepsize_center)
      }

      if (grepl('h', tolower(opt))) {
        cent[1]  <- cent[1] * (1 - stepsize_center)
      }

      if (grepl('k', tolower(opt))) {
        cent[1] <- cent[1] * (1 + stepsize_center)
      }

      if (grepl('n', tolower(opt))) {
        d_out <- d_out * (1 + stepsize_diam)
      }

      if (grepl('m', tolower(opt))) {
        d_out <- d_out * (1 - stepsize_diam)
      }
    }
  } else {
    circ <- generate_circle(img = img, d = d_out, circ_center = cent)
}


  out <- img
  out[circ == 0] <- NA

  attr(out, "circular_crop_center") <- cent
  attr(out, "circular_crop_diam") <- d_out

  return(out)

}
