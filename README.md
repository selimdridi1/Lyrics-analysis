Lyrics Analysis and Playlist Creation : Avec Selim Dridi et Guillaume Bousson

Description

Ce projet utilise R pour analyser les paroles de chansons et explorer leurs émotions. Il génère également des visualisations basées sur les fréquences de mots, l'analyse des sentiments, et peut créer des playlists basées sur des émotions spécifiques.
Ce projet ce divise en deux parties :
Une partie dans laquelle vous pouvez à partir d'un mot ou d'une phrase compris dans le titre d'une chanson et l'émotion que vous souhaitez. Le code va générér les meilleurs résultats correspondant à votre requête en analysant les paroles et faire ressortir le sentiment principal. 
Les chansons qui correspondent à votre demande vont directement être ajouté dans une playlist créer en fonction du sentiment. En plus de la playlist créer une biographie sur les artistes va être générée.
La seconde a pour but que vous choisissiez directement le titre ou l'artiste souhaité puis sélectionner la musique qui vous intéresse puis le code va générér une analyse des paroles et des sentiments en créant un nuage de mots et définir ce qui ressort de la chanson.

Fonctionnalités

Analyse des paroles de chansons récupérées via l'API Genius.
Visualisation des fréquences de mots dans les paroles.
Analyse des sentiments multilingues (anglais, français, allemand).
Création de playlists basées sur des émotions spécifiques comme "joie", "peur", ou "surprise".
Génération de nuages de mots standard et colorés en fonction des émotions.
Fichiers Importants

lyrics_analysis_combined_corrected.Rmd : Contient le code R Markdown pour toutes les fonctionnalités mentionnées.
lyrics_analysis_combined_corrected.html : La version rendue du fichier R Markdown, avec des visualisations et des analyses complètes.

Prérequis

R et RStudio installés.
Les packages R suivants installés :
install.packages(c("jsonlite", "rvest", "httr", "dplyr", "tidytext", "ggplot2", 
                   "stringr", "syuzhet", "wordcloud", "RColorBrewer", "cld3", "readr"))
Les fichiers de lexique nécessaires pour l'analyse des sentiments en français et allemand :
french_sentiment_lexicon.csv
german_sentiment_lexicon.csv

Utilisation

Ouvrez le fichier R Markdown (lyrics_analysis_combined_corrected.Rmd) dans RStudio.
Configurez votre clé API Genius dans le script pour permettre l'accès aux paroles.
Exécutez les différentes sections du script pour :
Rechercher une chanson et récupérer ses paroles.
Analyser et visualiser les fréquences de mots.
Analyser les sentiments des paroles.
Générer des playlists basées sur des émotions spécifiques.
Résultats

Des visualisations comme des graphiques de fréquences de mots et des nuages de mots.
Une analyse complète des émotions détectées dans les paroles.
Une playlist générée automatiquement avec des chansons correspondant à un sentiment donné.
Contribuer

Les contributions sont les bienvenues ! Veuillez soumettre une pull request avec des améliorations ou des suggestions.
