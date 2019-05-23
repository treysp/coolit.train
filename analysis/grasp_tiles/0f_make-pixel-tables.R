orig_files <- list.files(inpath, recursive = TRUE)

# build table of color values for standardization
cl <- makeCluster(cores)
clusterExport(cl, c("pixtable_outpath", "orig_files"))
clusterEvalQ(cl, {
  library(raster); library(reshape2); library(data.table); library(stringr)
})

pblapply(cl = cl, X = seq_along(orig_files), FUN = function(i) {
  my_brick <- brick(orig_files[i])
  
  pixel_table <- reshape2::melt(freq(my_brick, merge = TRUE), 
                                id.vars = "value", 
                                variable.name = "layer_name",
                                value.name = "count")
  names(pixel_table)[names(pixel_table) == "value"] <- "pixel_value"
  pixel_table$count <- ifelse(is.na(pixel_table$count), 0, pixel_table$count)
  pixel_table$layer_name <- as.character(pixel_table$layer_name)
  pixel_table$file <- orig_files[i]
  
  outfile <- strsplit(orig_files[i], "/")
  outfile <- outfile[[1]][length(outfile[[1]])]
  outfile <- stringr::str_replace(outfile, "\\.tif", "_pixtable.csv")
  data.table::fwrite(pixel_table, 
                     file = file.path(pixtable_outpath, outfile), 
                     nThread = 1)
})

stopCluster(cl)

pixel_table <- data.table::rbindlist(
  lapply(list.files(file.path(pixtable_outpath), full.names = TRUE), data.table::fread)
)

pixel_table <- pixel_table[!is.na(pixel_value)]
pixel_table[, layer_num := substr(layer_name, nchar(layer_name), nchar(layer_name))]
pixel_table[, `:=`(
  pixel_value = as.numeric(pixel_value),
  count = as.numeric(count)
)]


pixel_sums <- pixel_table[, list(count = sum(count)), 
                          by = c("layer_num", "pixel_value")]

pixel_means <- pixel_sums[, list(
  mean = sum(pixel_value * count) / sum(count)
), 
by = layer_num]

pixel_table <- merge(pixel_table, pixel_means, by = "layer_num")
pixel_table[, pix_diff_mn_sq := (pixel_value - mean)^2]
pixel_table[, pix_diff_mn_sq_sum := count * pix_diff_mn_sq]

pixel_sd <- pixel_table[, list(sd = sqrt(sum(pix_diff_mn_sq_sum) / sum(count))), by = layer_num]