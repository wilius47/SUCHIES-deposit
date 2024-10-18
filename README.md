# SUCHIES-deposit   :  (project Compared Urban structures over Scales and History)
https://umr-idees.fr/recherche/projets/imported-PROJET-SUCHIES-262fd842 

This desposit contains 5 script files (2 R scripts and 3 Python) and a sample of data for testing:

1) The first script is an a "semi-automatic" attempt to extract the urban class from old maps based on data mining of threshold, it provide a binary raster file and a vector file of urban (used to extract urban footprints from Etat Major Data)
2) The second script is also a automatic extraction of urban footprints, but using pyQgis approach (clipping useless landcover class).
The data deposit, contain also a historical map (Scan histo 1950, IGN/France), and 2 shapefiles (for data mining, those are points on urban pixel", 1 for reference and the second for validation).
3) The third code is a python script who attend to classify urban buuilt up units from "Scan Histo-1950 Maps", using a deep learning approach (UNET architecture).
4) The fourth code is a computation of overall accuracy (metrics) of urban footprints of the Database
5) The fifth script perform a radial analysis (center perphery) of urban density using the previous file (urban class raster), it also provide a rescaled application using population data on X Y axis.
6) This sixth code perfom a city size grouping by computing the main urban densities through 5 city groups in France (by population density gradient)
