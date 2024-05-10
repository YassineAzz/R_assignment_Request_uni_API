library(httr)
library(jsonlite)
library(sf)
library(tmap)
library(RColorBrewer)

# Define the URL and API parameters to download bus stop data
url <- "https://gisdataapi.cetler.se/data101"

params_bus_stop <- list(
  dbName = 'OSM',
  ApiKey = "v24yasazdu.sew5BUuppB!j5Orj8fNWFEPZQCjia2ryTeIY5OHsgqrCVCt1ThNh",  # Replace with your actual API key
  bufferType = "radius",
  dataType = "poi",
  centerPoint = " 50.13, 8.92",#"59.86, 17.64",
  radius = "5000",
  V = "1",
  key = 'public_transport',
  value = 'stop_position'
)

# GET request to download bus stop data
response_bus_stop <- GET(url, query = params_bus_stop)

# Check the response and convert the content to text
content_bus_stop <- content(response_bus_stop, "text")

# Convert the JSON text into spatial data
bus_stops <- st_as_sf(fromJSON(content_bus_stop), coords = c("longitude", "latitude"), crs = 4326)

# Create 500 meters buffers around each bus stop
bus_stop_buffers <- st_buffer(bus_stops, dist = 500)

# Find overlaps between the buffers to identify stops within 500 meters of each other
overlaps <- st_intersects(bus_stop_buffers, sparse = FALSE)

# Create a color vector: green if at least one other bus stop is within 500 meters, otherwise red
bus_stops$color <- ifelse(rowSums(overlaps) - 1 > 0, "green", "red")  # Subtract 1 to exclude self-intersection

# Ensure tmap is in 'view' mode for interactive visualization
tmap_mode("view")

# Prepare the bus stop map with proximity indication
tm_map <- tm_shape(bus_stops) +
  tm_symbols(size = 0.5, col = "color", palette = c("red" = "red", "green" = "green"), title.col = "Proximity of Stops") +
  tm_layout(main.title = "Proximity of Bus Stops in Luleå") +
  tm_basemap(leaflet::providers$OpenStreetMap)

# Display the map
tm_map
