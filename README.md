# SUCHIES-deposit   :  (project Compared Urban structures over Scales and History)
https://umr-idees.fr/recherche/projets/imported-PROJET-SUCHIES-262fd842 

This desposit contains 3 script files (2 R scripts and 1 Python) and a sample of data for testing:
1) The first script is an a "semi-automatic" attempt to extract the urban class from old maps based on data mining of threshold, it provide a binary raster file and a vector file of urban.
2) The second script perform a radial analysis (center perphery) of urban density using the previous file (urban class raster), it also provide a rescaled application using population data on X Y axis.
3) The thirs script is also a automatic extraction of urban footprints, but using pyQgis approach (clipping useless landcover class).
The data deposit, contain also a historical map (Scan histo 1950, IGN/France), and 2 shapefiles (for data mining, those are points on urban pixel", 1 for reference and the second for validation). 
