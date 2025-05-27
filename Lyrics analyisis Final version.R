############################################
## CHARGEMENT DES PACKAGES NÉCESSAIRES    ##
############################################
library(jsonlite)
library(rvest)
library(httr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(syuzhet)
library(wordcloud)
library(RColorBrewer)
library(cld3)
library(readr)

############################################
## 1) FONCTION POUR RECHERCHER LES TITRES ##
############################################
search_genius_titles <- function() {
  # Demander à l'utilisateur de saisir un terme de recherche
  query <- readline(prompt = "Entrez le terme de recherche : ")
  
  # Encoder le terme de recherche pour l'URL
  query_encoded <- URLencode(query)
  
  # Construire l'URL de recherche
  search_url <- paste0("https://api.genius.com/search?q=", query_encoded)
  
  # Remplacez par votre token d'accès
  access_token <- "VF2kBVMQK5k34vcRngI2gYLe1WbSWBIfAUMZXyMRGfy6H7P5BxOALj82-qgV0VNA"  # Remplacez par votre token d'accès
  
  # Effectuer la requête API avec le token d'accès
  response <- GET(search_url, add_headers(Authorization = paste("Bearer", access_token)))
  
  # Vérifier le statut de la réponse
  if (status_code(response) == 200) {
    # Obtenir le contenu brut de la réponse (format texte)
    raw_text <- content(response, "text", encoding = "UTF-8")
    
    # Utiliser des expressions régulières pour extraire les URLs des chansons
    url_matches <- regmatches(raw_text, gregexpr('"url":"(https://genius.com/[^"]+lyrics)"', raw_text))
    
    # Vérifier et afficher les résultats correspondants
    if (length(url_matches[[1]]) > 0) {
      cat("Liste des chansons et URLs trouvées :\n")
      count <- 1  # Compteur pour la numérotation
      results <- list()  # Liste pour stocker les URLs
      
      for (url in url_matches[[1]]) {
        # Enlever les parties inutiles pour avoir seulement l'URL propre
        clean_url <- gsub('"url":"|"', '', url)
        
        # Extraire l'artiste et le titre à partir de l'URL
        info <- gsub('https://genius.com/|(-lyrics)', '', clean_url)
        
        # Remplacer les tirets par des espaces pour une meilleure lisibilité
        title_artist <- gsub("-", " ", info)
        
        # Ajouter le résultat à la liste
        results[[count]] <- list(title_artist = title_artist, url = clean_url)
        
        # Afficher le résultat avec numérotation
        cat(paste(count, "-", title_artist, "\nURL:", clean_url, "\n\n"))
        count <- count + 1  # Incrémenter le compteur
      }
      
      # Demander à l'utilisateur de choisir un résultat
      choice <- as.numeric(readline(prompt = "Entrez le numéro de la chanson souhaitée : "))
      
      if (!is.na(choice) && choice > 0 && choice <= length(results)) {
        selected_result <- results[[choice]]
        cat("\nVous avez sélectionné :\n")
        cat("Titre/Artiste :", selected_result$title_artist, "\n")
        cat("URL           :", selected_result$url, "\n")
        return(selected_result$url)  # Retourner l'URL sélectionnée
      } else {
        cat("Choix invalide. Aucune sélection effectuée.\n")
        return(NULL)
      }
    } else {
      cat("Aucune URL trouvée.\n")
      return(NULL)
    }
  } else {
    cat("Erreur lors de la récupération des résultats. Code d'erreur:", status_code(response), "\n")
    return(NULL)
  }
}

############################################
## 2) FONCTION POUR RÉCUPÉRER LES PAROLES ##
############################################
scrape_genius_lyrics <- function(song_url) {
  # Charger la page HTML avec un User-Agent pour éviter les blocages
  page_html <- tryCatch({
    read_html(song_url, options = "HUGE")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(page_html)) {
    return("Erreur : Impossible de charger la page.")
  }
  
  # Extraire les paroles
  lyrics_nodes <- page_html %>%
    html_nodes('[data-lyrics-container="true"]')
  
  if (length(lyrics_nodes) == 0) {
    return("Paroles non trouvées ou chargées dynamiquement par JavaScript.")
  }
  
  # Extraction du texte
  lyrics <- lyrics_nodes %>%
    html_text2() %>%
    paste(collapse = "\n")
  
  return(lyrics)
}

############################################
## 3) FONCTION POUR ANALYSER LA FRÉQUENCE DES MOTS ##
############################################
analyze_word_frequency <- function(lyrics) {
  # Vérifier si les paroles sont disponibles
  if (is.null(lyrics) || 
      lyrics == "Paroles non trouvées ou chargées dynamiquement par JavaScript." || 
      lyrics == "Erreur : Impossible de charger la page.") {
    cat("Impossible d'effectuer l'analyse de fréquence des mots.\n")
    return(NULL)
  }
  
  # Créer un dataframe avec les paroles
  lyrics_df <- data.frame(line = 1:length(lyrics), text = lyrics, stringsAsFactors = FALSE)
  
  # Tokenisation et nettoyage
  words <- lyrics_df %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_replace_all(word, "[’‘]", "'")) %>%  # Normaliser les apostrophes
    anti_join(stop_words, by = "word")  # Supprimer les mots vides
  
  # Calcul de la fréquence des mots
  word_freq <- words %>%
    count(word, sort = TRUE)
  
  # Retourner le dataframe des fréquences
  return(word_freq)
}

############################################
## 4) FONCTION POUR ANALYSER LE SENTIMENT MULTILINGUE ##
############################################
analyze_sentiment_multilingual <- function(lyrics, lexicon_en, lexicon_fr, lexicon_de) {
  # Vérifier si les paroles sont disponibles
  if (is.null(lyrics) || 
      lyrics == "Paroles non trouvées ou chargées dynamiquement par JavaScript." || 
      lyrics == "Erreur : Impossible de charger la page.") {
    cat("Impossible d'effectuer l'analyse de sentiment.\n")
    return(NULL)
  }
  
  # Détecter la langue des paroles
  language <- detect_language(lyrics)
  cat("\nLangue détectée :", language, "\n")
  
  # Initialiser le dataframe de sentiment
  sentiment_df <- NULL
  
  if (language %in% c("en", "english")) {
    cat("Analyse de sentiment en anglais.\n")
    # Diviser les paroles en phrases
    sentences <- get_sentences(lyrics)
    
    # Obtenir les sentiments pour chaque phrase
    sentiments <- get_nrc_sentiment(sentences)
    
    # Calculer la somme des sentiments
    sentiment_summary <- colSums(sentiments[, 1:8])  # Colonnes 1-8 du lexique NRC
    
    # Convertir en dataframe
    sentiment_df <- data.frame(sentiment = names(sentiment_summary), 
                               score = sentiment_summary, 
                               stringsAsFactors = FALSE)
  } else if (language %in% c("fr", "french")) {
    cat("Analyse de sentiment en français.\n")
    # Tokenisation en mots
    words <- data.frame(text = lyrics, stringsAsFactors = FALSE) %>%
      unnest_tokens(word, text) %>%
      mutate(word = str_replace_all(word, "[’‘]", "'")) %>%  # Normaliser les apostrophes
      anti_join(stop_words, by = "word")  # Supprimer les mots vides
    
    # Associer les mots au lexique de sentiment français
    sentiments <- words %>%
      inner_join(lexicon_fr, by = "word")
    
    # Calculer la fréquence des sentiments
    sentiment_summary <- sentiments %>%
      count(sentiment, sort = TRUE)
    
    # Convertir en dataframe
    sentiment_df <- sentiment_summary
  } else if (language %in% c("de", "german")) {
    cat("Analyse de sentiment en allemand.\n")
    # Tokenisation en mots
    words <- data.frame(text = lyrics, stringsAsFactors = FALSE) %>%
      unnest_tokens(word, text) %>%
      mutate(word = str_replace_all(word, "[’‘]", "'")) %>%  # Normaliser les apostrophes
      anti_join(stop_words, by = "word")  # Supprimer les mots vides
    
    # Associer les mots au lexique de sentiment allemand
    sentiments <- words %>%
      inner_join(lexicon_de, by = "word")
    
    # Calculer la fréquence des sentiments
    sentiment_summary <- sentiments %>%
      count(sentiment, sort = TRUE)
    
    # Convertir en dataframe
    sentiment_df <- sentiment_summary
  } else {
    cat("Langue non supportée pour l'analyse de sentiment.\n")
    return(NULL)
  }
  
  return(sentiment_df)
}

############################################
## 5) FONCTION POUR VISUALISER LA FRÉQUENCE DES MOTS ##
############################################
visualize_word_frequency <- function(word_freq) {
  if (is.null(word_freq)) {
    return(NULL)
  }
  
  # Sélectionner les 20 mots les plus fréquents
  top_words <- word_freq %>%
    top_n(20, n) %>%
    arrange(desc(n))
  
  # Visualisation avec ggplot2
  ggplot(top_words, aes(x = reorder(word, n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Top 20 Mots les Plus Fréquents dans les Paroles",
         x = "Mots",
         y = "Fréquence")
}

############################################
## 6) FONCTION POUR CRÉER UN NUAGE DE MOTS ##
############################################
create_wordcloud <- function(word_freq) {
  if (is.null(word_freq)) {
    return(NULL)
  }
  
  # Sélectionner les 100 mots les plus fréquents pour le nuage de mots
  top_words <- word_freq %>%
    top_n(100, n)
  
  # Créer le nuage de mots
  wordcloud(words = top_words$word, freq = top_words$n, 
            max.words = 100, random.order = FALSE, 
            colors = brewer.pal(8, "Dark2"))
}

############################################
## 7) FONCTION POUR CRÉER UN NUAGE DE MOTS COLORÉ PAR SENTIMENT ##
############################################
create_sentiment_wordcloud_multilingual <- function(word_freq, lexicon_en, lexicon_fr, lexicon_de) {
  if (is.null(word_freq)) {
    return(NULL)
  }
  
  # Joindre les sentiments au word_freq
  # Détecter la langue des paroles
  # Pour simplifier, on suppose que cette fonction est appelée après la détection
  # Alternativement, passez la langue en argument
  language <- detect_language(paste(word_freq$word, collapse = " "))
  
  if (language %in% c("en", "english")) {
    # Joindre les sentiments anglais
    word_sentiments <- word_freq %>%
      inner_join(lexicon_en, by = "word") %>%
      mutate(sentiment = tolower(sentiment))
  } else if (language %in% c("fr", "french")) {
    # Joindre les sentiments français
    word_sentiments <- word_freq %>%
      inner_join(lexicon_fr, by = "word") %>%
      mutate(sentiment = tolower(sentiment))
  } else if (language %in% c("de", "german")) {
    # Joindre les sentiments allemands
    word_sentiments <- word_freq %>%
      inner_join(lexicon_de, by = "word") %>%
      mutate(sentiment = tolower(sentiment))
  } else {
    word_sentiments <- NULL
  }
  
  if (is.null(word_sentiments)) {
    # Créer un nuage de mots standard si la langue n'est pas supportée
    wordcloud(words = word_freq$word, freq = word_freq$n, 
              max.words = 100, random.order = FALSE, 
              colors = brewer.pal(8, "Dark2"))
  } else {
    # Définir les couleurs en fonction du sentiment
    unique_sentiments <- unique(word_sentiments$sentiment)
    palette <- brewer.pal(min(length(unique_sentiments), 8), "Dark2")
    names(palette) <- unique_sentiments
    
    # Créer le nuage de mots avec des couleurs basées sur le sentiment
    wordcloud(words = word_sentiments$word, freq = word_sentiments$n, 
              max.words = 100, random.order = FALSE, 
              colors = palette[word_sentiments$sentiment],
              scale = c(3, 0.5))
  }
}

############################################
## 8) FONCTION POUR VISUALISER LE SENTIMENT MULTILINGUE ##
############################################
visualize_sentiment_multilingual <- function(sentiment_df, language) {
  if (is.null(sentiment_df) || nrow(sentiment_df) == 0) {
    return(NULL)
  }
  
  # Définir des palettes de couleurs selon la langue
  if (language %in% c("en", "english")) {
    colors <- c("anger" = "red", "anticipation" = "orange", 
                "disgust" = "brown", "fear" = "purple", 
                "joy" = "yellow", "sadness" = "blue", 
                "surprise" = "green", "trust" = "cyan")
  } else if (language %in% c("fr", "french")) {
    colors <- c("joy" = "green", "sadness" = "blue", 
                "anger" = "red", "trust" = "cyan", 
                "fear" = "purple", "disgust" = "brown", 
                "anticipation" = "orange", "surprise" = "pink")
  } else if (language %in% c("de", "german")) {
    colors <- c("anger" = "red", "anticipation" = "orange", 
                "disgust" = "brown", "fear" = "purple", 
                "joy" = "yellow", "sadness" = "blue", 
                "surprise" = "green", "trust" = "cyan")
  } else {
    colors <- "steelblue"
  }
  
  # Visualisation avec ggplot2
  ggplot(sentiment_df, aes(x = reorder(sentiment, score), y = score, fill = sentiment)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Analyse de Sentiment des Paroles",
         x = "Sentiment",
         y = "Score") +
    scale_fill_manual(values = colors) +
    theme(legend.position = "none") +
    geom_text(aes(label = score), hjust = -0.1, size = 3)
}


############################################
## 9) FONCTION POUR CRÉER UNE PLAYLIST EN FONCTION DU SENTIMENT SOUHAITÉ ET DU TITRE ##
############################################

# Définir les lexiques de sentiment en français et en allemand
# Assurez-vous que les fichiers CSV sont présents dans le répertoire de travail
french_sentiment_lexicon <- read_csv("french_sentiment_lexicon.csv - Feuille 1-3.csv")
german_sentiment_lexicon <- read_csv("german_sentiment_lexicon.csv - Feuille 1-3.csv")

# Créer une playlist basée sur le sentiment
create_and_save_playlist <- function(search_term, desired_sentiment, max_songs = 40) {
  # Rechercher les chansons
  cat("Recherche des chansons avec le terme de recherche :", search_term, "\n")
  playlist <- create_sentiment_playlist(
    search_term = search_term, 
    desired_sentiment = desired_sentiment, 
    max_songs = max_songs
  )
  
  # Vérifier si la playlist est vide
  if (is.null(playlist) || nrow(playlist) == 0) {
    cat("Aucune chanson correspondante trouvée.\n")
    return(NULL)
  }
  
  # Sauvegarder la playlist de base
  write_csv(playlist, "playlist_base.csv")
  cat("Playlist de base sauvegardée dans 'playlist_base.csv'.\n")
  
  # Récupérer et ajouter les biographies des artistes
  cat("\nRécupération des biographies des artistes via Wikipedia...\n")
  playlist$biography <- sapply(playlist$artist, get_artist_biography_safe)
  
  # Sauvegarder la playlist enrichie avec les biographies
  write_csv(playlist, "playlist_enriched_with_biographies.csv")
  cat("Playlist enrichie avec biographies sauvegardée dans 'playlist_enriched_with_biographies.csv'.\n")
  
  return(playlist)
}

############################################
## 10) EXECUTION POUR OBTENIR LA PLAYLIST EN FONCTION DU SENTIMENT SOUHAITÉ ##
############################################
# Définir les paramètres de la playlist
search_term <- "me"          # Terme de recherche
desired_sentiment <- "joy"   # Sentiment désiré ("joy", "fear", etc.)
max_songs <- 40               # Nombre maximal de chansons à traiter

# Créer et sauvegarder la playlist
playlist <- create_and_save_playlist(
  search_term = search_term, 
  desired_sentiment = desired_sentiment, 
  max_songs = max_songs
)
############################################
## 11) EXÉCUTION DU CODE                   ##
############################################
# Charger les lexiques de sentiment
# Assurez-vous que les fichiers CSV sont dans le répertoire de travail
french_sentiment_lexicon <- read_csv("french_sentiment_lexicon.csv - Feuille 1-3.csv")
german_sentiment_lexicon <- read_csv("german_sentiment_lexicon.csv - Feuille 1-3.csv")

# Étape 1 : Rechercher un titre et sélectionner une chanson
selected_url <- search_genius_titles()

# Étape 2 : Récupérer les paroles si une URL a été sélectionnée
if (!is.null(selected_url)) {
  cat("\nRécupération des paroles en cours...\n")
  lyrics <- scrape_genius_lyrics(selected_url)
  
  # Afficher les premières lignes des paroles pour vérification
  cat("\nParoles de la chanson :\n")
  if (nchar(lyrics) > 500) {
    cat(substr(lyrics, 1, 500), "...\n")  # Afficher les 500 premiers caractères
  } else {
    cat(lyrics, "\n")
  }

  # Détecter la langue des paroles
  language <- detect_language(lyrics)
  cat("\nLangue détectée :", language, "\n")
  
  # Étape 3 : Analyser la fréquence des mots
  cat("\nAnalyse de la fréquence des mots...\n")
  word_freq <- analyze_word_frequency(lyrics)
  
  # Étape 4 : Visualiser la fréquence des mots
  if (!is.null(word_freq)) {
    cat("\nVisualisation de la fréquence des mots...\n")
    visualize_word_frequency(word_freq)
    
    # Étape 5 : Créer un nuage de mots
    cat("\nCréation du nuage de mots...\n")
    create_wordcloud(word_freq)
    
    # Étape 6 : Analyser le sentiment multilingue
    cat("\nAnalyse de sentiment des paroles (Multilingue)...\n")
    sentiment_df <- analyze_sentiment_multilingual(lyrics, 
                                                   lexicon_en = nrc_lexicon_en, 
                                                   lexicon_fr = french_sentiment_lexicon, 
                                                   lexicon_de = german_sentiment_lexicon)
    
    # Étape 7 : Visualiser le sentiment
    if (!is.null(sentiment_df)) {
      cat("\nVisualisation de l'analyse de sentiment...\n")
      visualize_sentiment_multilingual(sentiment_df, language)
      
      # Étape 8 : Créer un nuage de mots coloré par sentiment
      cat("\nCréation du nuage de mots coloré par sentiment...\n")
      create_sentiment_wordcloud_multilingual(word_freq, 
                                             lexicon_en = nrc_lexicon_en, 
                                             lexicon_fr = french_sentiment_lexicon, 
                                             lexicon_de = german_sentiment_lexicon)
    }
  }
}
