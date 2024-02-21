
area_from_bw_img <- function(path_to_img, false_color = 'red', w_new = NULL, h_new = NULL, erode_kern = NULL, dilate_kern = NULL) {

  # read in image
  img_orig <- read_to_portrait(path_to_img)

  resized <- F

  # if new dimensions (w_new or h_new) are provided, resize the image
  if (!is.null(w_new) & !is.null(h_new)) {

    dims <- dim(img_orig)

    if (!(w_new %in% dims)) {

      resized <- T
      img_orig<- resize(img_orig, w = w_new, h = h_new)

    }

  }

  # change color mode to grayscale (required for EBImage::otsu())
  EBImage::colorMode(img_orig) <- EBImage::Grayscale

  # estimate soil/background thresholds for each band
  threshold <- EBImage::otsu(img_orig)

  # apply threshold to image
  img_binary <- 1 - (img_orig > threshold)

  # replace NAs (from mask/crop) with zeroes - otherwise, NAs will be labeled as objects
  img_binary[is.na(img_binary)] <- 0

  storage.mode(img_binary) <- 'integer'


  if (!is.null(erode_kern) & !is.null(dilate_kern)) {

    img_binary <- erode_and_dilate(img_binary, erode_kern, dilate_kern)

  } else {
    if (!is.null(dilate_kern)) {
      img_binary <- dilate(img_binary, makeBrush(dilate_kern, shape='disc'))
    }
    if (!is.null(erode_kern)) {
      img_binary <- erode(img_binary, makeBrush(erode_kern, shape='disc'))
    }
  }


area_out <- sum(img_binary)

  msk <- img_binary

  img_for_color <- Image(abind::abind(img_orig, img_orig, img_orig, along = 3), colormode='Color')

  msk <- EBImage::paintObjects(msk, img_for_color, col= rep(false_color, 2), opac = c(1, 0.4))

  return(list(area = area_out, false_color = msk))

}
