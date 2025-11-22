#libraries
library(terra)
library(future.apply)
# data
gmf<-rast("./Documents/Boreal_data_exploration/COP30_MD_NAD83_AEAC_geomorphons_lowness_merged.tif")
bs<-rast('./Documents/Boreal_data_exploration/reburn_severity_from_Brian_dNBR/burn_severity.tif')
reburn_p<-vect("./Documents/Boreal_NWT_Project_Data/NWT_reburns/NWT_reburns.shp")


gmf
plot(gmf)
# gmf have different resolution and projection than the burn severity data (30 m for bs?)

# next steps: 
# 1) get gmf and bs data to the same projection and resolution
# 2) if needed resample the finer grain layer to corser grain layer's resolution
# 3) examine veg data from Jurjen, is that a better representation of wetness? 
# 3.5) look into SCANFI, does it address "actual" veg vs "site potential"? 
# 4) extract gmf position at reburn locations.
# 5) plot histograms of 1st severity by topo position



# align projections and resample
bs_prj<-project(bs, gmf, method="mode")

writeRaster(bs_prj, "./Documents/Boreal_Burn_Severity/geomorphons_prj.tiff", 
            overwrite=TRUE)

reburn_prj<-project(reburn_p, gmf)


# combine gmf and burn severity rasters, looks like 
# they already have the same extent. 

add(bs_prj)<-gmf

#### extract at reburn locations ##########
# Iterate extraction over each polygon 
# To improve write a function that takes polygon number as 
# an argument to extract burn severity and geomorphon data. 


# results_list <- list()
# failed_polygons <- c()  # Track which polygons failed

gmf_bs<-function(i, reburn_prj, bs_prj)
    {
    
    # cat("Processing polygon", i, "of", nrow(reburn_prj), "\n")

    result<-tryCatch({
      
      current_poly <- reburn_prj[i, ]
      
      extracted <- terra::extract(bs_prj, current_poly, 
                                  na.rm = TRUE,
                                  touches=FALSE,
                                  # ID = TRUE,
                                  xy=TRUE)
      extracted$ID<-current_poly$fid
      
      # Store results
      # results_list[[i]] <- extracted
      
      # data.frame(
      #   polygon_id = i,
      #   mean_value = extracted[1, 2],
      #   status = "success"
      # )
      return(extracted)
      
    }, error = function(e) {
      
      error_msg <- paste0("Polygon ", i, ": ", conditionMessage(e))
      return(NA)
    })
    
  return(result)
}


out1<-lapply(X=c(1:1000), gmf_bs, reburn_prj, bs_prj)
out2<-lapply(X=c(1001:2000), gmf_bs, reburn_prj, bs_prj)
out3<-lapply(X=c(2001:3000), gmf_bs, reburn_prj, bs_prj)
out4<-lapply(X=c(3001:nrow(bs_prj)), gmf_bs, reburn_prj, bs_prj)


out_all<-c(out1, out2, out3, out4)
out_clean<-out_all[!is.na(out_all)]

rm(out_all)
rm(out1, out2, out3, out4)

out_t<-do.call(rbind, out_clean)


# add proper labels to columns and count zeros. 
colnames(out_t)[2:6]<-c("interval", "reburn_year", "severity_1", "severity_2", "gmf")

saveRDS(out_t, "Documents/Boreal_Burn_Severity/outputs/full_bs_gmf.RDS")





