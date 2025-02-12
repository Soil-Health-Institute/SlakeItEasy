library(EBImage)
library(dplyr)
library(SlakeItEasy)

# NOTE: TYPE CONTROL + SHIFT + o (COMMAND + SHIFT + o) TO TOGGLE WORKFLOW OUTLINE

# set paths to images and output directory --------------------------------
### run the following line to select the location of your images and output interactively. if you routinely process images in a consistent location, specify that location as the parent_dir_img argument to set_paths(), e.g.,
# paths <- set_paths(parent_dir_img = '~/Library/CloudStorage/OneDrive-SharedLibraries-SoilHealthInstitute/Data Repository - Documents/Final Data File Store/AmAgLab Slakes/')

paths <- set_paths(parent_dir_img = system.file("images/", package="SlakeItEasy"), batch_name = 'demo')

# alternatively, create paths object non-interactively:
# paths <- list(image_dir = '/path/to/image/directory',
#               output_dir = '/path/to/output/directory',
#               batch_name = 'mybatch')

# create directory for output and for problematic images ------------------
dirs_for_flagged_imgs <- dir_setup(paths$output_dir,  return_paths = T)

reprocess_dir <- dirs_for_flagged_imgs['to_reprocess']
unusable_dir <- dirs_for_flagged_imgs['unusable']

###

# if you want to concatenate the output of a previous run, skip to the section "concatenate previous run"

####

# extract metadata for images ---------------------------------------------
metadata <- get_metadata(paths$image_dir, filename_prefix = 'IMG_', image_extension = '.jpg')

# check for replicates that do not meet expectations for analysis ---------
metadata_qaqc <- check_replicates(metadata, final_img_time_min = 10, final_img_tol_sec = 30, n_images_max = 3)

metadata_qaqc$wrong_final_time

write.csv(metadata_qaqc$m, paste0(paths$output_dir, '/qaqc_log.csv'), row.names = F)

# usable images

to_analyze <- metadata_qaqc$usable

to_analyze  <- to_analyze[!is.na(to_analyze)]

# inspect replicates with extra images and/or missing images. if you reorganize files, rerun this script starting from "metadata <- get_metadata(...)"

if (length(metadata_qaqc$extra_imgs) > 0) {
  opendirs(metadata_qaqc$extra_imgs)
}

if (length(metadata_qaqc$missing_imgs) > 0) {
  opendirs(metadata_qaqc$missing_imgs)
}

# automated batch processing with a circular crop -------------------------
# see ?process_petri and ?area_from_image for descriptions of function parameters

results <- batch_process(dir_vec = to_analyze,
                         outdir = paths$output_dir,
                         filename_prefix = 'IMG_',
                         parallel = T,
                         d = 0.72,
                         batch_dir = paths$image_dir,
                         false_color = 'green')

# inspect classifications and sort suboptimal images ----------------------

opendirs(c(unusable_dir, reprocess_dir, paste0(paths$output_dir, '/images_false_color')))

# prepare for manual processing -------------------------------------------
images_to_reprocess <- list.files(reprocess_dir)

replicates_to_reprocess <- unique(sapply(strsplit(images_to_reprocess, '_x_'), function(x) paste(x[1:(length(x) - 1)], collapse = '/')))

# process images requiring manual crop ------------------------------------
results2 <- lapply(replicates_to_reprocess, function(i) {
  print(paste0('Processing ', i, '...'))

  out <- process_petri(i, interactive = T, filename_prefix = 'IMG_', outdir = paths$output_dir, aggregates_in_initial = 3, batch_dir = paths$image_dir, false_color = 'green')

  return(out)
}
) %>%
  bind_rows()

# sort/clean up -----------------------------------------------------------
# if all images were manually reprocessed successfully, you can delete them from the "to_reprocess" folder. If you want to reprocess images again, run the code again starting from "images_to_reprocess <- list.files(reprocess_dir)"

opendirs(c(unusable_dir, reprocess_dir, paste0(paths$output_dir, '/images_false_color')))

# get IDs of unusable images -----------------------------------------------------

unusable <- gsub('.jpg', '', list.files(unusable_dir))
unusable <- unique(sapply(strsplit(unusable, '_x_'), function(x) paste(x[length(x) - 1])))

# concatenate automatically & manually processed data ---------------------

if (exists('results2')) {
  if (nrow(results2) > 0) {
    results <- bind_rows(results,
                         results2)
  }
}


# parse sample/replicate IDs ----------------------------------------------


# NOTE: THE FOLLOWING CODE EXTRACTS SAMPLE IDS AND REPLICATE INFORMATION. IF YOU GET UNEXPECTED RESULTS, IT MAY BE BECAUSE FILE STRUCTURE/IMAGE NAMING DEVIATE FROM THE EXPECTED CONVENTIONS, AND YOU MAY NEED TO MODIFY THIS SECTION.

results <- results %>%
  mutate(parent_dir = sapply(strsplit(image_set, '/'), function(x) paste(x[1:(length(x) - 1)], collapse = '/')),
         replicate_id = sapply(strsplit(image_set, '/'), function(x) x[[length(x)]]),
         replicate_num = substr(replicate_id, nchar(replicate_id), nchar(replicate_id)),
         # sample_id = substr(replicate_id, 1, 12), # uncomment this line and delete the following line if sample IDs are a fixed number of digits (e.g., here, 12)
         sample_id = sapply(strsplit(replicate_id, '_'), function(x) x[[1]]) # extract sample ID, assuming sample ID is separated from replicate number/letter by an underscore
  ) %>%
  filter(!replicate_id %in% unusable)

# concatenate previous run -----------------------------------------------

if (!exists('results')) {

  path_to_results <- list.files(paste0(paths$output_dir, '/stability_index'), full.names = T, pattern = '.csv$')

  unusable_dir <-
    unusable <- gsub('.jpg', '', list.files(unusable_dir))
  unusable <- unique(sapply(strsplit(unusable, '_x_'), function(x) paste(x[length(x) - 1])))

  results <- do.call(rbind, lapply(path_to_results, read.csv))

  results <- results %>%
    mutate(parent_dir = sapply(strsplit(image_set, '/'), function(x) paste(x[1:(length(x) - 1)], collapse = '/')),
           replicate_id = sapply(strsplit(image_set, '/'), function(x) x[[length(x)]]),
           replicate_num = substr(replicate_id, nchar(replicate_id), nchar(replicate_id)),
           sample_id = sapply(strsplit(replicate_id, '_'), function(x) x[[1]])) %>%
    filter(!replicate_id %in% unusable)

  # safeguard to drop results of automatic processing when interactively processed results exist

  results <- results %>%
    group_by(sample_id, replicate_num) %>%
    filter(n() == 1 | interactively_processed == T)

}


# results in wide format (one column per replicate for stab0 and stab10) --------

area_by_time <- lapply(list.files(file.path(paths$output_dir, 'area_by_time'), pattern = '.csv', full.names = T), read.csv) %>%
  do.call(rbind, .)

stab_wide <- area_by_time %>%
      filter(stab != 1) %>%
      mutate(measurement_type =
               paste0('stab', round(duration_of_slaking)),
             # lab_id = gsub('images_from_lab/', '', image_set, fixed = T),
             lab_id = str_split_i(image_set, '/', -1),
             lab_id = stringr::str_split_i(lab_id, '_', 1),
             rep_num = substr(image_set, nchar(image_set), nchar(image_set)),
             rep_num = case_when(toupper(rep_num) %in% c('A', '1') ~ 'a',
                                 toupper(rep_num) %in% c('B', '2') ~ 'b',
                                 toupper(rep_num) %in% c('C', '3') ~ 'c',
                                 TRUE ~ rep_num)) %>%
      pivot_wider(id_cols = lab_id,
                  names_from = c(measurement_type, rep_num),
                  values_from = stab,
                  names_vary = 'fastest')

# save wide-format data to disk

stab_wide %>%
  dplyr::select(lab_id, contains('stab')) %>%
  write.csv(file.path(paths$output_dir, paste0(paths$batch_name, '_slakes_wide.csv')), row.names = F)


# save results to disk ----------------------------------------------------

results %>%
  write.csv(paste0(paths$output_dir, '/', paths$batch_name, '_stab10_results.csv'), row.names = F)


# summarize results by sample ---------------------------------------------

results %>%
  group_by(parent_dir, sample_id) %>%
  summarise(stab_gmean = mean_geom(stab), stab_cv = cv(stab), nreps = n()) %>%
  write.csv(paste0(paths$output_dir, '/', paths$batch_name, '_stab10_means.csv'), row.names = F)
