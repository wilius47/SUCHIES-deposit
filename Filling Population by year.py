# -*- coding: utf-8 -*-
"""
Created on Sat Oct 19 17:25:43 2024

@author: rabehwal
"""

import geopandas as gpd
import pandas as pd

# Charger le shapefile
shapefile_path = "chemin/vers/ton_shapefile.shp"
gdf = gpd.read_file(shapefile_path)

# Charger le fichier CSV
csv_path = "chemin/vers/ton_fichier_population.csv"
df_population = pd.read_csv(csv_path)

# Filtrer les données de population où certainty = 3
df_population_filtered = df_population[df_population['certainty'] == 3]

# Fonction pour arrondir l'année du CSV à la date la plus proche (1000, 1050, 1100, ...)
def nearest_50(x):
    return int(round(x / 50) * 50)

# Appliquer l'arrondi sur le champ 'year' du CSV
df_population_filtered['rounded_year'] = df_population_filtered['year'].apply(nearest_50)

# Boucle sur chaque ligne du shapefile
for idx, row in gdf.iterrows():
    annee_edit = row['année_edit']  # Le champ 'année_edit' dans le shapefile
    
    # Trouver l'entrée la plus proche dans le CSV arrondi
    population_row = df_population_filtered.loc[(df_population_filtered['rounded_year'] == nearest_50(annee_edit))]

    if not population_row.empty:
        # S'il y a une correspondance, récupérer la population et mettre à jour la table d'attributs
        population = population_row['pop'].values[0]
        
        # Ajouter/mettre à jour le champ population dans le shapefile
        gdf.at[idx, 'population'] = population

# Sauvegarder les modifications dans un nouveau shapefile
gdf.to_file("chemin/vers/nouveau_shapefile.shp")
