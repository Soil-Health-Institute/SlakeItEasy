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
batch_process <- function(dir_vec, parallel = F, ncores = parallel::detectCores() - 1, return_output = T, ...) {

   if (parallel) {
       if (requireNamespace('foreach', quietly = TRUE)) {
         if (requireNamespace('doFuture', quietly = TRUE)) {
         }
         future::plan(future::multisession)

         outdf <- doFuture::`%dofuture%`(foreach::foreach(x = dir_vec, .options.future = list(packages = 'magrittr'),
                                                          .combine = rbind) ,
                                           process_petri(path_to_image_set =  x, ...))
       } else {

         warning('Packages {foreach}, {future}, and {doFuture} required for cross-platform parallel processing not found. parallel::mclapply() will be used instead.', call. = F)

         if (.Platform['OS.type'] == "windows") {

           stop('Parallel processing with parallel::mclapply() not supported on Windows. Please run install.packages(c("foreach", "future", "doFuture")) and try again.', call. = F)

         } else {

           outdf <- do.call(rbind, parallel::mclapply(dir_vec, function(x) process_petri(path_to_image_set =  x, ...), mc.cores = ncores))

         }
       }
   } else {
     outdf <- do.call(rbind, lapply(dir_vec, function(x) process_petri(path_to_image_set =  x, ...)))
   }
   if (return_output) {
     return(outdf)
   }
}
