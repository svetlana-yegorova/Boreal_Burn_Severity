# libraries
library(terra)
library(ggplot2)
library(tidyr)

# data
bs<-rast("./Documents/Boreal_data_exploration/reburn_severity_from_Brian_dNBR/burn_severity.tif")


reburns<-vect("./Documents/Boreal_NWT_Project_Data/NWT_reburns/NWT_reburns.shp")


# histogram out first and second burns
# the raster is too large.
# for speed subsample the raster
# mask to reburns
# and extract to a table

reburns_prj<-project(reburns, crs(bs))


rbrn_bs<-mask(bs, reburns_prj)

#estimate how long it will take to extract stuff: 
n_valid <- global(rbrn_bs[[1]] != 0 & !is.na(rbrn_bs[[1]]), "sum", na.rm = TRUE)
print(n_valid)
# 17*10^6

# raster is too large to extract at once. Extract one polygon at a time. 

# Crop the reburns shapefile to raster extent
clipped_shapefile <- crop(reburns_prj, rbrn_bs)

# Iterate extraction over each polygon

results_list <- list()
failed_polygons <- c()  # Track which polygons failed

for (i in 1:nrow(clipped_shapefile)) {
  
  cat("Processing polygon", i, "of", nrow(reburns_prj), "\n")
  
  
  result <- tryCatch({
    
    current_poly <- reburns_prj[i, ]
    
    extracted <- terra::extract(rbrn_bs, current_poly, 
                         na.rm = TRUE,
                        ID = TRUE,
                         xy=TRUE)
    
    # data.frame(
    #   polygon_id = i,
    #   mean_value = extracted[1, 2],
    #   status = "success"
    # )
    
  }, error = function(e) {
    
    error_msg <- paste0("Polygon ", i, ": ", conditionMessage(e))
    cat("  ERROR:", error_msg, "\n")
    failed_polygons<<-c(failed_polygons, i)
    # Log error to file
    # write(error_msg, error_file, append = TRUE)
    # 
    # data.frame(
    #   polygon_id = i,
    #   mean_value = NA,
    #   status = "failed"
    # )
  })
  

  
  # Cleanup
  rm(result, current_poly)
  if (i %% 10 == 0) gc()
  
  # # Extract current polygon
  # current_poly <- reburns_prj[i, ]
  # 
  # # Extract raster values within polygon
  # extracted <- tryCatch({extract(rbrn_bs, current_poly,
  #                                # fun = NULL,        # NULL returns all values
  #                                na.rm = TRUE,      # remove NA values
  #                                xy = TRUE)         #
  #   }
  # 
  #   {})

  # Store results
  results_list[[i]] <- extracted
  
 if(i%%1000==0) saveRDS(results_list, paste0("./Documents/Boreal_Burn_Severity/outputs/", i, "_bs.RDS"))
  # write.csv(extracted, paste0("polygon_", i, "_values.csv"), row.names = FALSE)
}

# Combine all results into a single data frame
results_df <- do.call(rbind, results_list)

# add proper labels to columns and count zeros. 
colnames(results_df)[2:5]<-c("interval", "reburn_year", "severity_1", "severity_2")

saveRDS(results_df, "Documents/Boreal_Burn_Severity/outputs/full_bs.RDS")

