#' Title
#'
#' @param dir_vec
#' @param parallel
#' @param return_output
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
batch_process <- function(dir_vec, parallel = T, ncores = parallel::detectCores() - 1, return_output = F, ...) {

   if (parallel) {
     outlist <- parallel::mclapply(dir_vec, function(x) process_petri(path_to_image_set =  x, ...), ncores = 7)
   } else {
     outlist <- lapply(dir_vec, function(x) {
       cat(paste0('Processing ', x, '...\n'))
       process_petri(path_to_image_set =  x, ...)
     }
     )
   }

   if (return_output) {
     return(outlist)
   }

}



