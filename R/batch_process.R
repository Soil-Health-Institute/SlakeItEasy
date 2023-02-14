#' Process a Directory of Replicates
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
batch_process <- function(dir_vec, parallel = T, ncores = parallel::detectCores() - 1, return_output = T, ...) {

   if (parallel) {
     outdf <- do.call(rbind,  parallel::mclapply(dir_vec, function(x) data.frame(image_set = x, stab10 = process_petri(path_to_image_set =  x, ...)), mc.cores = ncores))
   } else {
     outdf <- do.call(rbind, lapply(dir_vec, function(x) {
       data.frame(image_set = x, stab10 = process_petri(path_to_image_set =  x, ...))
     }
     ))
   }

   if (return_output) {
     return(outdf)
   }
}



