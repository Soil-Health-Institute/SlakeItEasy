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
#' f <- system.file("images/exp", "exp2.jpg", package="SlakeItEasy")
#'
#' # read in image
#' img_portrait <- read_to_portrait(f)
#'
#' # verify that image is in portrait orientation
#' display(img_portrait)
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

#' Title
#'
#' @param img
#' @param dim_list
#' @param mask_only
#'
#' @return
#' @export
#'
#' @examples
crop_image <- function(img, dim_list, mask_only = F) {

  i <- lapply(dim_list, function(v) min(v):max(v))

  if (!mask_only) {

  img_crop <- img[i$x, i$y,]

  attr(img_crop, 'crop_dim') <- dim_list
  attr(img_crop, 'dim_source') <- dim(img)

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

# set crop box by clicking on graphics device window
#' Title
#'
#' @param npts
#'
#' @return
#' @export
#'
#' @examples
set_crop <- function(npts = 2) {
  p <- locator(npts, type = "p", pch = 3, col = "red")
  p_out <- lapply(p, function(x) round(c(min(x), c(max(x)))))
  return(p_out)
}

#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
interactive_crop <- function(img) {

  dev.new()

  # open_window()

  par(mfrow = c(1,1))

  plot(img)

  text(x = 20, y = 20, label = "Click two opposite corners to crop", adj = c(0,1), col = "red", cex = 2)

  p_out <- set_crop(2)

  img_crop <- crop_image(img, p_out)

  plot(img_crop)
  Sys.sleep(1)

  dev.off()

  return(img_crop)

}

#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
rgb_to_binary <- function(img) {

  EBImage::colorMode(img) <- Grayscale

  # estimate soil/background thresholds for each band
  threshold <- EBImage::otsu(img)

  img_binary <- EBImage::combine( mapply(function(frame, th) frame <= th, EBImage::getFrames(img), threshold, SIMPLIFY=FALSE) )

  # average R, G, and B bands
  img_binary <- (img_binary[,,1] + img_binary[,,2] + img_binary[,,3])/3

  img_binary <- Image(apply(img_binary, 2, function(x) as.numeric(!is.na(x) & x > 0)))

  attr(img_binary, 'otsu_thresholds') <- threshold

  return(img_binary)

}

# normalized differences between a pair of bands, with a minimum cutoff (mindiff = 0 recommended for dark objects, may not work for light-colored soils)

#' Title
#'
#' @param img
#' @param b1_idx
#' @param b2_idx
#' @param mindiff
#'
#' @return
#' @export
#'
#' @examples
norm_diff <- function(img, b1_idx, b2_idx, mindiff = 0){
  out <- (img[,,b1_idx]  - img[,,b2_idx]) / (img[,,b1_idx]  + img[,,b2_idx])
  colorMode(out) <- Grayscale
  out >= mindiff
}

# erode and then dilate an image with a disc-shaped kernel

#' Title
#'
#' @param img
#' @param erode_size
#' @param dilate_size
#'
#' @return
#' @export
#'
#' @examples
erode_and_dilate <- function(img, erode_size = 5, dilate_size = erode_size * 7) {

  kern <- makeBrush(erode_size, shape='disc')

  dilate(erode(img, kern), makeBrush(dilate_size, shape='disc'))

}

# get coordinates of bounding box around an object within a matrix OR return summary statistics on bbox geometry

#' Title
#'
#' @param obj_id
#' @param matr
#' @param return_summary
#'
#' @return
#' @export
#'
#' @examples
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
#' Title
#'
#' @param img
#' @param return_counts
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param img
#' @param bbox
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param img
#' @param n
#'
#' @return
#' @export
#'
#' @examples
keep_n_objs <- function(img, n = 3) {

  storage.mode(img) <- 'integer'

  tbl <- table(img)
  obj_to_drop <- as.numeric(names(sort(tbl, decreasing = T)[(n + 2):length(tbl)]))
  img_binary <- rmObjects(img, obj_to_drop, reenumerate = F)

}

# distance from centroid of largest N objects

#' Title
#'
#' @param xcoord
#' @param ycoord
#' @param area_var
#' @param n
#'
#' @return
#' @export
#'
#' @examples
dist_from_n_objs <- function(xcoord, ycoord, area_var, n = 3) {

  obj_ids <- which(rank(area_var) %in% (length(area_var) - n +1):length(area_var))

  xcoord_mean <- mean(xcoord[obj_ids])
  ycoord_mean <- mean(ycoord[obj_ids])

  out <- sqrt((xcoord - xcoord_mean)^2 + (ycoord - ycoord_mean)^2)

  return(out)

}

# generate_ring <- function(img, r = 0.6, width = 0.01, ring_center = NULL) {
#
#   # get length and width (ignore 3rd dimension for multiband images)
#   dims <- dim(img)[1:2]
#
#   # scale factor - fraction of length or width, whichever is smaller
#   r <- round(min(dims)*r)
#
#   # coordinate pairs for all pixels
#   dims_long <- expand.grid(x = 1:dims[1], y = 1:dims[2])
#
#   # if ring center is not specified, set to image centroid
#   if (is.null(ring_center)) {
#     ring_center <- dims[1:2]/2
#   } else {
#     if (class(ring_center) == 'list') {
#       ring_center <- unlist(ring_center)
#     }
#   }
#
#
#   # distance from each pixel to centroid
#   dims_long$dist <- as.numeric(sqrt((dims_long[,1] - ring_center[1])^2 + (dims_long[,2] - ring_center[2])^2))
#
#   # set values inside and outside of ring to zero
#   dims_long$dist[abs(dims_long$dist - r)/r > width] <- 0
#
#   # set values on ring to one
#   dims_long$dist[abs(dims_long$dist - r)/r <= width] <- 1
#
#   # format values as wide matrix and coerce to EBImage::Image class
#   tidyr::spread(dims_long, y , dist) %>% dplyr::select(-x) %>% as.matrix() %>% Image()
#
# }

# interactively select center for circular crop

#' Title
#'
#' @param img
#' @param h_offset
#' @param v_offset
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param img
#' @param d
#' @param circ_center
#'
#' @return
#' @export
#'
#' @examples
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
  out <- tidyr::spread(dims_long, y , dist) %>% dplyr::select(-x) %>% as.matrix() %>% Image()

  return(out)

}

#' Title
#'
#' @param img
#' @param d
#' @param stepsize_center
#' @param stepsize_diam
#' @param interactive
#' @param h_offset
#' @param v_offset
#'
#' @return
#' @export
#'
#' @examples
crop_image_circular <- function(img, d = 0.7, stepsize_center = 0.01, stepsize_diam = 0.01, interactive = T, h_offset = 0, v_offset = 0) {

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
