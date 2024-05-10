library(httr)
library(jsonlite)
library(sf)
library(tmap)
library(RColorBrewer)

# Définir l'URL et les paramètres de l'API pour télécharger les données des restaurants et des arrêts de bus
url <- "https://gisdataapi.cetler.se/data101"

params_restaurant <- list(
  dbName = 'OSM',
  ApiKey = "v24yasazdu.sew5BUuppB!j5Orj8fNWFEPZQCjia2ryTeIY5OHsgqrCVCt1ThNh",  # Remplacez par votre clé API réelle
  bufferType = "radius",
  dataType = "poi",
  centerPoint = "65.5848, 22.1567",
  radius = "5000",
  V = "1",
  key = 'amenity',
  value = 'restaurant'
)

params_bus_stop <- list(
  dbName = 'OSM',
  ApiKey = "v24yasazdu.sew5BUuppB!j5Orj8fNWFEPZQCjia2ryTeIY5OHsgqrCVCt1ThNh",  # Remplacez par votre clé API réelle
  bufferType = "radius",
  dataType = "poi",
  centerPoint = "65.5848, 22.1567",
  radius = "5000",
  V = "1",
  key = 'public_transport',
  value = 'stop_position'
)

# Requêtes GET pour télécharger les données des restaurants et des arrêts de bus
response_restaurant <- GET(url, query = params_restaurant)
response_bus_stop <- GET(url, query = params_bus_stop)

# Vérifier la réponse et convertir le contenu en texte
content_restaurant <- content(response_restaurant, "text")
content_bus_stop <- content(response_bus_stop, "text")

# Extraction et vérification des données JSON
jsonDataRestaurant <- fromJSON(content_restaurant)

# Examinez la structure des données pour trouver les bons noms de colonnes
str(jsonDataRestaurant)

# Transformer le texte JSON en données spatiales
restaurants <- st_as_sf(fromJSON(content_restaurant), coords = c("longitude", "latitude"), crs = 4326)
bus_stops <- st_as_sf(fromJSON(content_bus_stop), coords = c("longitude", "latitude"), crs = 4326)


# Créez des buffers de 400m autour des restaurants
restaurant_buffers <- st_buffer(restaurants, dist = 400)

# Vérifiez quels arrêts de bus sont à l'intérieur de chaque buffer
bus_stops_within_buffers <- st_intersects(restaurant_buffers, bus_stops, sparse = FALSE)

# Créez un vecteur de couleurs pour chaque buffer en fonction de la présence d'arrêts de bus
buffer_colors <- ifelse(rowSums(bus_stops_within_buffers) > 0, "green", "red")

# Ajoutez le vecteur de couleurs à l'objet sf des buffers
restaurant_buffers$color <- buffer_colors

# Calculez l'intersection entre les buffers de restaurants et les arrêts de bus
bus_stops_within_buffers <- st_intersects(restaurant_buffers, bus_stops, sparse = FALSE)

# Créez un vecteur de couleurs pour les buffers en fonction de la présence des arrêts de bus
restaurant_buffers$color <- ifelse(rowSums(bus_stops_within_buffers) > 0, "green", "red")




# Assurez-vous que tmap est en mode 'view' pour une visualisation interactive
tmap_mode("view")

tm_map <- tm_shape(restaurant_buffers) +
  tm_fill(col = "color", border.col = "black", lwd = 3) +  # Utilisez tm_fill pour colorer les zones des buffers
  tm_shape(restaurants) +
  tm_symbols(size = 0.5, col = "blue") +  # Utilisez tm_symbols pour les restaurants
  tm_shape(bus_stops) +
  tm_symbols(size = 0.5, col = "black") +  # Utilisez tm_symbols pour les arrêts de bus
  tm_layout(main.title = "Restaurants and Bus Stops in Luleå") +
  tm_basemap(leaflet::providers$OpenStreetMap)

# Affichez la carte
tm_map
