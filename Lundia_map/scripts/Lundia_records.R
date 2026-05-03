###################################
# Lundia records
###################

# libraries
library(readxl)
library(sf)
library(tmap)
library(viridisLite)
library(terra)
library(raster)


# read the data
data <- read_excel("data/Lundia_Distribution_plus_Miriam's_records.xlsx", col_names = T)

# How many species do I have
# notice there are some NA
unique(data$NAME1)

# remove records with NA in NAME1
data <- data[is.na(data$NAME1)==F, ]
unique(data$NAME1) #check

# count records per species
table(data$NAME1)

# get species names
sp_names <- names(table(data$NAME1))

# extract the records per species
# empty list
records_per_sp <- vector(length = length(sp_names), "list")

# for each species
for (i in 1:length(records_per_sp)){
  # get this species
  this_sp <- sp_names[i]
  # extract records
  these_records <- data[data$NAME1==this_sp, ]
  # save in list
  records_per_sp[[i]] <- these_records
}

# name list
names(records_per_sp) <- sp_names




# make a data_coords object without missing data for the coordinates
data_coords <- data
# some columns in the spreadsheet say "Error", so gotta remove
data_coords <- data_coords[data_coords$XCOOR!="Error", ]
data_coords <- data_coords[data_coords$YCOOR!="Error", ]
# making sure coords are read as numeric
data_coords$XCOOR <- as.numeric(data_coords$XCOOR)
data_coords$YCOOR <- as.numeric(data_coords$YCOOR)
# filter records with coords
data_coords <- data_coords[is.na(data_coords$XCOOR)==F, ]
data_coords <- data_coords[is.na(data_coords$YCOOR)==F, ]


# transform to sf
data_sf <- st_as_sf(data_coords, coords = c("XCOOR", "YCOOR"), crs = 4326)


tmap_mode("view")

tm_shape(data_sf) + 
  tm_dots(fill = "NAME1",
          size = 1.2, 
          fill.scale = tm_scale_categorical(values = c(
            "Lundia corymbifera" = "#440154",
            "Lundia damazioi"    = "#482878",
            "Lundia densiflora"  = "#3E4A89",
            "Lundia erionema"    = "#31688E",
            "Lundia gardneri"    = "#26828E",
            "Lundia helicocalyx" = "#1F9E89",
            "Lundia laevis"      = "#35B779",
            "Lundia longa"       = "#6DCD59",
            "Lundia nitidula"    = "#B4DE2C",
            "Lundia obliqua"     = "#FDE725",
            "Lundia puberula"    = "#FCA636",
            "Lundia spruceana"   = "#E16462",
            "Lundia virginalis"  = "#B12A90"
          )))


tm_shape(data_sf) + 
  tm_symbols(
    fill = "NAME1",
    shape = "NAME1",
    size = 1,
    fill.scale = tm_scale_categorical(values = c(
      "Lundia corymbifera" = "#440154",
      "Lundia damazioi"    = "#482878",
      "Lundia densiflora"  = "#3E4A89",
      "Lundia erionema"    = "#31688E",
      "Lundia gardneri"    = "#26828E",
      "Lundia helicocalyx" = "#1F9E89",
      "Lundia laevis"      = "#35B779",
      "Lundia longa"       = "#6DCD59",
      "Lundia nitidula"    = "#B4DE2C",
      "Lundia obliqua"     = "#FDE725",
      "Lundia puberula"    = "#FCA636",
      "Lundia spruceana"   = "#E16462",
      "Lundia virginalis"  = "#B12A90"
    )),
    
    shape.scale = tm_scale_categorical(values = c(
      "Lundia corymbifera" = 21,  # circle
      "Lundia damazioi"    = 22,  # square
      "Lundia densiflora"  = 24,  # triangle
      "Lundia erionema"    = 21,
      "Lundia gardneri"    = 22,
      "Lundia helicocalyx" = 24,
      "Lundia laevis"      = 21,
      "Lundia longa"       = 22,
      "Lundia nitidula"    = 24,
      "Lundia obliqua"     = 21,
      "Lundia puberula"    = 21,
      "Lundia spruceana"   = 21,
      "Lundia virginalis"  = 21   # diamond (extra variation)
    ))
  )




tmap_mode("plot")


# Static plot for figure
tm_basemap("CartoDB.PositronNoLabels")  +
  tm_shape(data_sf) + 
  tm_symbols(fill = "NAME1", shape = "NAME1",
             size = 0.8, 
             col = "black",
             lwd = 0.6,
             #fill_alpha =0.95,
             fill.scale = tm_scale_categorical(values = c(
               "Lundia corymbifera" = "#FFFFFF",  # white
               "Lundia damazioi"    = "#000000",  # black
               "Lundia densiflora"  = "#3B2B7E",
               "Lundia erionema"    = "#315C8E",
               "Lundia gardneri"    = "#26828E",
               "Lundia helicocalyx" = "#1FAE95",
               "Lundia laevis"      = "#35B779",
               "Lundia longa"       = "#6DCD59",
               "Lundia nitidula"    = "#B4DE2C",
               "Lundia obliqua"     = "#FDE725",
               "Lundia puberula"    = "#FCA636",
               "Lundia spruceana"   = "#E16462",
               "Lundia virginalis"  = "#B12A90"
             )),
             shape.scale = tm_scale_categorical(values = c(
               "Lundia corymbifera" = 21,  
               "Lundia damazioi"    = 22,  
               "Lundia densiflora"  = 25,  
               "Lundia erionema"    = 21,
               "Lundia gardneri"    = 22,
               "Lundia helicocalyx" = 25,
               "Lundia laevis"      = 21,
               "Lundia longa"       = 22,
               "Lundia nitidula"    = 25,
               "Lundia obliqua"     = 21,
               "Lundia puberula"    = 22,
               "Lundia spruceana"   = 25,
               "Lundia virginalis"  = 21
             ))
  ) +
  tm_compass(
    type = "arrow",
    position = c("right", "bottom")
  )



###################################
# Make a facet for sup mat
###############################


tm_basemap("CartoDB.PositronNoLabels")  +
  tm_shape(data_sf) + 
  tm_dots(size = 0.5, 
          col = "black",
          lwd = 0.6,
          fill_alpha =0.95) +
  tm_facets(by="NAME1", 
            free.coords = F,
            ncol = 3) 



# read raster 
regions <- rast("data/Adenocalymma_Morrone_layer5.tif")
plot(regions)

# 3. Crop the raster to that selected area
#selected_extent <- drawExtent()
#cropped_raster <- crop(regions, selected_extent)
#plot(cropped_raster)
#writeRaster(cropped_raster, "data/regions_raster_cropped.tif")

# if not modifying raster, just use previous!
cropped_raster <- rast("data/regions_raster_cropped.tif")
plot(cropped_raster)

tm_basemap("CartoDB.PositronNoLabels")  +
  tm_shape(cropped_raster) + 
  tm_raster(alpha = 0.8, palette = "RdYlGn") +
  tm_layout(legend.show = FALSE) +
  tm_shape(data_sf) + 
  tm_dots(size = 0.5, 
          col = "black",
          lwd = 0.6,
          fill_alpha =0.95) +
  tm_facets(by="NAME1", 
            free.coords = F,
            ncol = 3) 




