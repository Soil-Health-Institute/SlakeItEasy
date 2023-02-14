library(EBImage)
library(dplyr)
library(SlakeItEasy)

# set paths to images and output directory --------------------------------
paths <- set_paths(parent_dir_img = 'inst', batch_name = 'demo')

# create directory for output and for problematic images ------------------
dirs_for_flagged_imgs <- dir_setup(paths$output_dir,  return_paths = T)

# extract metadata for images ---------------------------------------------
metadata <- get_metadata(paths$image_dir, filename_prefix = 'IMG_')

# check for replicates that do not meet expectations for analysis ---------
metadata_qaqc <- check_replicates(metadata,  final_img_time_min = 10, final_img_tol_sec = 30, n_images_max = 3)

write.csv(metadata_qaqc$m, paste0(paths$output_dir, '/qaqc_log.csv'), row.names = F)

metadata_qaqc$usable
metadata_qaqc$missing_imgs
metadata_qaqc$extra_imgs
metadata_qaqc$multiple_sizes
metadata_qaqc$wrong_final_time

# metadata[metadata$Directory %in% metadata_qaqc$wrong_final_time,]

# usable with resizing -- to processes these, set match_resolution = T in batch_process
to_analyze <- metadata_qaqc$multiple_sizes[!metadata_qaqc$multiple_sizes %in% c(metadata_qaqc$missing_imgs, metadata_qaqc$wrong_final_time, metadata_qaqc$extra_imgs)]

# full set of usable images
to_analyze <- c(metadata_qaqc$usable, to_analyze)

# inspect replicates with extra images
if (length(metadata_qaqc$extra_imgs) > 0) {
  opendirs(metadata_qaqc$extra_imgs)
}

# single-image functionality --------------------------------------
# img <- read_to_portrait(with(metadata[1,], paste0(Directory, '/', FileName)))
# img_masked <- mask_image_circular(img, interactive = T, h_offset = 0)
# plot(img_masked)
#
# test <- area_from_image(with(metadata[1,], paste0(Directory, '/', FileName)), circular_mask = T, d = 0.73, interactive = T)

# automated batch processing with a circular crop -------------------------
results <- batch_process(dir_vec = to_analyze, outdir = paths$output_dir, filename_prefix = 'IMG_', match_resolution = T, circular_mask = T, parallel = T)

# inspect classifications and sort suboptimal images ----------------------
reprocess_dir <- dirs_for_flagged_imgs['to_reprocess']
unusable_dir <- dirs_for_flagged_imgs['unusable']
opendirs(c(unusable_dir, reprocess_dir, paste0(paths$output_dir, '/images_false_color')))

# prepare for manual processing -------------------------------------------
images_to_reprocess <- list.files(reprocess_dir)

dir_vec2 <- unique(sapply(strsplit(images_to_reprocess, '_x_'), function(x) paste(x[1:(length(x) - 1)], collapse = '/')))

# process images requiring manual crop ------------------------------------
results2 <- lapply(dir_vec2, function(i) {
  print(paste0('Processing ', i, '...'))

  out <- data.frame(image_set = i, stab10 = process_petri(i, interactive = T, match_resolution = T, circular_mask = T, filename_prefix = 'IMG_', outdir = paths$output_dir, aggregates_in_initial = 3))

  return(out)
}
) %>% bind_rows()

# sort/clean up -----------------------------------------------------------
opendirs(c(unusable_dir, reprocess_dir, paste0(paths$output_dir, '/images_false_color')))

# get IDs of unusable images -----------------------------------------------------
unusable <- gsub('.jpg', '', list.files(unusable_dir))
unusable <- unique(sapply(strsplit(unusable, '_x_'), function(x) paste(x[length(x) - 1])))

# concatenate automatically & manually processed data ---------------------

if (nrow(results2) > 0) {
  results <- bind_rows(results, results2)
}

results <- results %>%
  mutate(replicate_id = sapply(strsplit(image_set, '/'), function(x) x[[length(x)]]),
         replicate_num = substr(replicate_id, nchar(replicate_id), nchar(replicate_id)),
         sample_id = sapply(strsplit(replicate_id, '_'), function(x) x[[1]])) %>%
  filter(!replicate_id %in% unusable)


# save results to disk ----------------------------------------------------

results %>%
  write.csv(paste0(paths$output_dir, '/', paths$batch_name, '_stab10_results.csv'), row.names = F)


# summarize results by sample ---------------------------------------------

results %>%
  write.csv(paste0(paths$output_dir, '/', paths$batch_name, '_stab10_results.csv'), row.names = F)

results %>%
  group_by(sample_id) %>%
  summarise(stab10_mg = mean_geom(stab10), stab10_cv = cv(stab10), nreps = n())

