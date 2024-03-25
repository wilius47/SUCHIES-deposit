#This code extract urban units from and old map of France (Scan histo) by clippling useless landcover + a radiometric filter
from qgis.core import QgsApplication, QgsVectorLayer, QgsRasterLayer, QgsVectorFileWriter, QgsGeometry, QgsMapLayerRegistry
from qgis.analysis import QgsRasterCalculator, QgsRasterCalculatorEntry

# Initializing the QGIS application
QgsApplication.setPrefixPath("/chemin/vers/QGIS", True)
qgs = QgsApplication([], False)
qgs.initQgis()

# Paths to files
chemin_raster_scan = "/chemin/vers/scan.tif"
chemin_shapefile_route = "/chemin/vers/routes.shp"
chemin_shapefile_foret = "/chemin/vers/foret.shp"
chemin_shapefile_agric = "/chemin/vers/agric.shp"
chemin_shapefile_r = "/chemin/vers/shapefile_r.shp"
chemin_shapefile_cercles = "/chemin/vers/cercles.shp"

# Load the “Scan” raster
raster_scan = QgsRasterLayer(chemin_raster_scan, "Scan")
if not raster_scan.isValid():
    print("Erreur: Impossible de charger le raster.")
else:
    print("Raster chargé avec succès.")

# Filter the raster according to the red band > 200
entries = []
red_band_entry = QgsRasterCalculatorEntry()
red_band_entry.ref = 'raster@1'
red_band_entry.raster = raster_scan
red_band_entry.bandNumber = 1
entries.append(red_band_entry)

expression = 'raster@1 > 200'
calc = QgsRasterCalculator(expression, '/chemin/vers/scan_filtre.tif', 'GTiff', raster_scan.extent(), raster_scan.width(), raster_scan.height(), entries)
calc.processCalculation()

# Load the “route” shapefile
layer_route = QgsVectorLayer(chemin_shapefile_route, "Routes", "ogr")
if not layer_route.isValid():
    print("Erreur: Impossible de charger le shapefile des routes.")
else:
    print("Shapefile des routes chargé avec succès.")

# Create a buffer zone (10 meters) around the “road” shapefile
buffer_distance = 10  # Distance de la zone tampon en mètres
buffer_layer = layer_route.buffer(buffer_distance, -1)

# Save the buffer as a shapefile
buffer_shapefile_path = '/chemin/vers/routes_buffer.shp'
QgsVectorFileWriter.writeAsVectorFormat(buffer_layer, buffer_shapefile_path, 'utf-8', layer_route.crs(), 'ESRI Shapefile')

# Load the “R” shapefile
layer_r = QgsVectorLayer(chemin_shapefile_r, "R", "ogr")
if not layer_r.isValid():
    print("Erreur: Impossible de charger le shapefile R.")
else:
    print("Shapefile R chargé avec succès.")

#Cut the filtered raster on the extent of the "R" shapefile
output_raster_r_decoupe = '/chemin/vers/scan_filtre_r_decoupe.tif'
processing.run("gdal:cliprasterbyextent",
               {'INPUT': '/chemin/vers/scan_filtre.tif',
                'PROJWIN': layer_r.extent().asWktPolygon(),
                'OUTPUT': output_raster_r_decoupe})

# Cut the filtered raster on the extent and contour of the “drill” shapefile
output_raster_foret_decoupe = '/chemin/vers/scan_filtre_foret_decoupe.tif'
processing.run("gdal:cliprasterbymasklayer",
               {'INPUT': '/chemin/vers/scan_filtre.tif',
                'MASK': chemin_shapefile_foret,
                'OUTPUT': output_raster_foret_decoupe})

# Cut the filtered raster on the extent and contour of the "agric" shapefile
output_raster_agric_decoupe = '/chemin/vers/scan_filtre_agric_decoupe.tif'
processing.run("gdal:cliprasterbymasklayer",
               {'INPUT': '/chemin/vers/scan_filtre.tif',
                'MASK': chemin_shapefile_agric,
                'OUTPUT': output_raster_agric_decoupe})

# Cut the filtered raster on the extent and contour of the “circles” shapefile
output_raster_cercles_decoupe = '/chemin/vers/scan_filtre_cercles_decoupe.tif'
processing.run("gdal:cliprasterbymasklayer",
               {'INPUT': '/chemin/vers/scan_filtre.tif',
                'MASK': chemin_shapefile_cercles,
                'OUTPUT': output_raster_cercles_decoupe})

# Close the QGIS application
qgs.exitQgis()
