# <!-- coding: utf-8 -->
#
# quelques fonctions pour le forum de bretagne-vivante
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# category
#   topic
#     post
#
## avec httr2
# https://httr2.r-lib.org/articles/wrapping-apis.html

phpbb2_url <- "https://www.forum-bretagne-vivante.org/"
phpbb2_dir <- sprintf("%s/phpbb2", varDir)
dir.create(phpbb2_dir, showWarnings = FALSE, recursive = TRUE)
#
# source("geo/scripts/discourse.R");phpbb2_sommaire_get(force = FALSE) %>% glimpse()
phpbb2_sommaire_get <- function(force = FALSE) {
  library(tidyverse)
  library(httr2)
  library(rvest)
  req <- httr2::request(phpbb2_url)
  page <- req |>
    req_user_agent("discourse mga") |>
    req_perform() |>
    resp_body_html()
  rows <- page |>
    html_elements("div#page-body div#emptyidcc table.forumline") |>
    html_nodes("tr")
  carp("rows nb:%s", length(rows))
  categories.df <- tibble(
    titre = character(),
    lien = character(),
    sujets = character(),
    messages = character(),
    detail = character(),
    detail_auteur = character(),
    detail_date = character(),
  )
  i <- 0
  for (row in rows) {
    i <- i + 1
    cols <- row |>
      html_nodes("td")
    if (length(cols) == 0) {
      next
    }
#    mga <<- row; stop("****")
    k <- -1
    if (length(cols)== 5) {
      k <- 0
    }
    if (length(cols) == 6) {
      k <- 1
    }
    if (k == -1) {
      carp("i: %s cols nb: %s", i, length(cols))
      next
    }
    sujet <- cols[k + 2] %>%
      html_nodes(".forumlink a") |>
      html_text() |>
      str_trim()
    lien_sujet <- cols[k + 2] |>
      html_nodes(".forumlink a") |>
      html_attr("href") |>
      glimpse()
    sujets <- cols[k + 3] |>
      html_text() |>
      str_trim()
    messages <- cols[k + 4] %>%
      html_text() |>
      str_trim()
    rc <- phpbb2_detail_sommaire(cols[k + 5])
#    carp("i: %s", i)
    categories.df <- categories.df %>%
      add_row(
        titre = sujet,
        lien = lien_sujet,
        sujets = sujets,
        messages = messages,
        detail = rc$detail,
        detail_auteur = rc$detail_auteur,
        detail_date = rc$detail_date
      )
  }
  misc_ecrire(categories.df, "phpbb2_sommaire")
  return(invisible(categories.df))
}
#
# source("geo/scripts/discourse.R");phpbb2_sommaire(force = FALSE)
phpbb2_sommaire <- function(force = FALSE) {
  library(tidyverse)
  library(lubridate)
  phpbb2_sommaire_get(force = force)
  categories.df <- misc_lire("phpbb2_sommaire")
  categories.df <- categories.df %>%
    dplyr::select(lien, detail_auteur, detail_date, sujets, messages)
  df1 <- categories.df %>%
    mutate(sujets = as.integer(sujets)) %>%
    mutate(messages = as.integer(messages)) %>%
#    adorn_totals() %>%
    glimpse()
  misc_print(df1)
  depuis <- today() - lubridate::days(3)
  df2 <- df1 %>%
    filter(messages > 0) %>%
    rowwise() %>%
    mutate(d = phpbb2_date(texte = detail_date)) %>%
#    filter(is.na(d)) %>%
    filter(d > depuis) %>%
    glimpse()
  misc_print(df2)
  for (i2 in 1:nrow(df2)) {
    t.df <- phpbb2_categorie(df2[[i2, "lien"]], force = TRUE)
  }
}
#
# source("geo/scripts/discourse.R");phpbb2_sommaire_discourse()
phpbb2_sommaire_discourse <- function(force = FALSE) {
  library(tidyverse)
  df <- misc_lire("phpbb2_sommaire") %>%
    glimpse()
  stop("*****")
  for (i in 1:nrow(df)) {
    json <- list(
      name = df[[i, "titre"]],
      color = "ffffff",
      text_color = "999999"
    )
    misc_post("categories", json)
  }
}
#
# source("geo/scripts/discourse.R");phpbb2_sommaire_categories()
phpbb2_sommaire_categories <- function(force = FALSE) {
  library(tidyverse)
  df1 <- misc_lire("phpbb2_sommaire") %>%
    dplyr::select(-messages) %>%
    glimpse()
  df2 <- misc_lire("phpbb2_categories") %>%
    group_by(categorie) %>%
    summarize(nb = n()) %>%
    glimpse()
#  misc_print(df2)
  df3 <- df1 %>%
    full_join(df2, by = c("lien" = "categorie"))
  misc_print(df3)
}
#
# source("geo/scripts/discourse.R");phpbb2_categories()
phpbb2_categories <- function(force = FALSE) {
  library(tidyverse)
  df <- misc_lire("phpbb2_sommaire")
  topics.df <- tibble()
  for (i in 1:nrow(df)) {
    t.df <- phpbb2_categorie(df[[i, "lien"]], force = force)
    if (! is_tibble(t.df))  {
      next
    }
    carp("sujets sommaire: %s categorie: %s", df[[i, "sujets"]], nrow(t.df))
    topics.df <- bind_rows(topics.df, t.df)
  }
  glimpse(topics.df)
  misc_ecrire(topics.df, "phpbb2_categories")
}
#
## analyse d'une catégorie
# en sortie tous les sujets "topics"
# source("geo/scripts/discourse.R");phpbb2_categorie("/f3-news", force = TRUE)
# source("geo/scripts/discourse.R");phpbb2_categorie("/f24p50-lepidopteres", force = TRUE)
# source("geo/scripts/discourse.R");phpbb2_categorie("/f60-offre-d-emploi-et-de-stage", force = TRUE)
# avec forum
# source("geo/scripts/discourse.R");phpbb2_categorie("/f4-botanique", force = TRUE)
phpbb2_categorie <- function(lien = "/f3-news", force = FALSE) {
  library(tidyverse)
  library(httr2)
  library(rvest)
  library(janitor)
  dsn <- sprintf("%s%s.rds", phpbb2_dir, lien)
  if (file.exists(dsn) & force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  page <- phpbb2_cache(lien = lien)
# forum avec connexion
  rows <- page |>
    html_element("div#page-body div#emptyidcc table.forumline") |>
    html_nodes("tr")
#  carp("rows nb:%s", length(rows))
  text1 <- html_text(rows[1])
  if (grepl("^Veuillez entrer votre nom", text1)) {
    Carp("    ****** connexion")
    return(invisible(FALSE))
  }
#  mga <<- page
#
# forum avec des forums ?
  tables <- page |>
    html_elements("div#page-body div#emptyidcc table.forumline")
  i_tables <- -1
  if (length(tables) == 3) {
    i_tables <- 1
  }
  if (length(tables) == 4) {
    i_tables <- 2
  }
  if (i_tables == -1) {
    carp("tables nb: %s", length(tables))
    for (tbl in tables) {
      ths <- tbl %>%
        html_nodes("th")
      carp("ths nb: %s", length(ths))
    }
    stop("****")
    return(invisible(FALSE))
  }
  topics.df <- phpbb2_categorie_page(tables[i_tables])
# des pages suites ?
# dans la dernière row
#  text9 <- html_text(rows[length(rows)])
#  carp("text9: %s", text9)
  rows <- tables[i_tables] %>%
    html_nodes("tr")
  liens <- rows[length(rows)] |>
    html_nodes("a") |>
    html_attr("href")
#  carp("liens nb:%s", length(liens))
  if (length(liens) > 1) {
    df <- tibble(lien = liens) |>
      distinct(lien) |>
      filter(grepl("p\\d+0\\-", lien)) %>%
      extract(lien, c("A", "p", "B"), "(.*p)(\\d+0)(\\-.*)", remove = FALSE) %>%
      slice(n())
    dernier <- as.integer(df[[1, "p"]])
    carp("lien: %s dernier: %s", lien, dernier)
    pages <- seq(50, dernier, 50)
    for (p in pages) {
      l <- sprintf("%s%s%s", df[[1, "A"]], p, df[[1, "B"]])
#      carp("l: %s", l)
      tables <- phpbb2_cache(lien = l) |>
        html_elements("div#page-body div#emptyidcc table.forumline")
#      carp("tables nb: %s", length(tables))
      t.df <- phpbb2_categorie_page(tables[i_tables])
      topics.df <- bind_rows(topics.df, t.df)
    }
  }
  topics.df$categorie <- lien
  glimpse(topics.df)
  saveRDS(topics.df, dsn)
  carp("dsn: %s", dsn)
  return(invisible(topics.df))
}
#
# une page d'une catégorie
phpbb2_categorie_page <- function(page = page) {
  library(tidyverse)
  library(httr2)
  library(rvest)
  text <- page %>%
    html_elements("th") %>%
    html_text()
#  carp("th text: %s", text)

  rows <- page |>
    html_nodes("tr")
  carp("rows nb: %s", length(rows))
  topics.df <- tibble(
    sujet = character(),
    titre = character(),
    lien = character(),
    detail = character(),
    detail_auteur = character(),
    detail_date = character(),
    auteur = character()
  )
  i <- 0
  for (row in rows) {
    i <- i + 1
    cols <- row |>
      html_nodes("td")
    if (length(cols) != 7) {
#      carp("i: %s cols nb: %s", i, length(cols))
      next
    }
    sujet <- cols[3] %>%
      html_text() |>
      str_trim()
    lien_sujet <- cols[3] |>
      html_nodes(".topictitle a") |>
      html_attr("href")
    titre <- cols[3] |>
      html_nodes(".topictitle a") |>
      html_text() |>
      str_trim()
    reponses <- cols[4] %>%
      html_text() |>
      str_trim()
    auteur <- cols[5] %>%
      html_text() |>
      str_trim()
    vues <- cols[6] %>%
      html_text() |>
      str_trim()
    rc <- phpbb2_detail_categorie(cols[7])
#    carp("i: %s", i)
    topics.df <- topics.df %>%
      add_row(
        sujet = sujet,
        titre = titre,
        lien = lien_sujet,
        auteur = auteur,
        detail = rc$detail,
        detail_auteur = rc$detail_auteur,
        detail_date = rc$detail_date
      )
  }
  return(invisible(topics.df))
}
#
## récupération des sujets/topics/posts
#
# boucle par catégorie
#
# source("geo/scripts/discourse.R");phpbb2_sujets(force = TRUE)
phpbb2_sujets <- function( force = TRUE) {
  library(tidyverse)
  df <- misc_lire("phpbb2_categories") %>%
    dplyr::select(lien, categorie) %>%
    unique() %>%
    glimpse()
  posts.df <- tibble()
  for (i in 1:nrow(df)) {
    carp("%s/%s lien: %s %s", i, nrow(df), df[[i, "categorie"]], df[[i, "lien"]])
    p.df <- phpbb2_sujet(df[[i, "lien"]], force = force)
    if (! is_tibble(p.df))  {
      next
    }
    posts.df <- bind_rows(posts.df, p.df)
  }
  glimpse(posts.df)
  misc_ecrire(posts.df, "phpbb2_sujets")
}

#
# un sujet
# source("geo/scripts/discourse.R");phpbb2_sujet(force = TRUE)
phpbb2_sujet <- function(lien = "/t23573-geranium-molle-geranium-rotundifolium", force = TRUE) {
  library(tidyverse)
  library(httr2)
  library(rvest)
  library(janitor)
  dsn <- sprintf("%s%s.rds", phpbb2_dir, lien)
  if (file.exists(dsn) & force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  page <- phpbb2_cache(lien = lien)
  return(invisible(FALSE));# pour faire de la mise en cache
  tables <- page |>
    html_elements("div#page-body div#emptyidcc table.forumline")
  carp("tables nb: %s", length(tables))
  rows <- tables[1] %>%
    html_elements("tr.post")
  carp("rows nb: %s", length(rows))
  posts.df <- tibble(
    post_id = character(),
    auteur = character(),
    sujet = character(),
    date = character()
  )
  i <- 0
  for (row in rows) {
    i <- i + 1
    post_id <- row %>%
      html_attr("id")
    cols <- row |>
      html_nodes("td")
#    carp("i: %s cols nb: %s", i, length(cols))
#    txt <- cols[1] |> html_text2(); print(txt)
#    phpbb2_dump(cols[1])
    auteur <- cols[1] |>
      html_node("span.name") |>
#      html_node("a") |>
      html_text2() |>
      str_trim()
# le message est dans une table ...
    post <- cols[2] |>
      html_node("table")
    post_rows <- post %>%
      html_elements("tr")
#    txt <- post_rows[1] |> html_element("td span") |> xml2::xml_structure(); print(txt)
    textes <- post_rows[1] |> html_element("td span") |> as.character()
    texte <- str_match(textes, '.*>(Sujet:.*)<img src=.*border="0">(.*)</span>')
    sujet <- str_trim(texte[1, 2])
    post_date <- str_trim(texte[1, 3])
    post_body <- row %>%
      html_element("div.postbody div")
    print(post_body %>% as.character()); stop("ùùùùùùùùùùù")
    posts.df <- posts.df %>%
      add_row(
        post_id = post_id,
        auteur = auteur,
        sujet = sujet,
        date = post_date,
      )
  }
  posts.df$lien <- lien
  glimpse(posts.df)
}
# la page d'un sujet
#
# pas de pagination pour les sujets ???
phpbb2_sujet_page <- function(page = page) {
  library(tidyverse)
  library(httr2)
  library(rvest)
}
#
## les fonctions utilaires
#
#
# source("geo/scripts/discourse.R");phpbb2_cache()
phpbb2_cache <- function(lien = "/f39-heteroceres") {
  library(tidyverse)
  library(httr2)
  library(rvest)
  dsn <- sprintf("%s%s.xml", phpbb2_dir, lien)
  if (file.exists(dsn)) {
    carp("dsn: %s", dsn)
    page <- xml2::read_html(dsn)
    return(invisible(page))
  }
  req <- httr2::request(phpbb2_url)
  page <- req |>
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:121.0) Gecko/20100101 Firefox/121.0") |>
    req_url_path_append(lien) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform() |>
    resp_body_html()
  xml2::write_xml(page, file = dsn)
  return(invisible(page))
}
#
# conversion d'une date en format phpBB2
# source("geo/scripts/discourse.R");phpbb2_date() %>% glimpse()
phpbb2_date <- function(texte = "Mar 13 Déc 2023") {
#  carp("texte: %s", texte);
  mois_B <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
  mois_b <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc")
  mois_m <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  mois <- setNames(mois_m, mois_b)
  d <<- str_split(texte, " ")[[1]]
  if (length(d) >= 4) {
    mm <- mois[[d[[3]]]]
    jjmmaaaa <- sprintf("%s %s %s", d[[2]], mm, d[[4]])
  } else {
    jjmmaaaa <- "01 01 2030"
  }
  d <- readr::parse_date(jjmmaaaa, format = "%d %m %Y")[1]
  return(invisible(d))
}
# https://github.com/tidyverse/readr/issues/711
phpbb2_date_v1 <- function(texte = "Mar 12 Déc 2023") {
  mois_B <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
  mois_b <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Jun", "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc")
  mois_m <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  day_B <- c("Dimanche", "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samsedi")
  day_b <- c("Dim", "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam")
  mois_B <- enc2utf8(mois_B)
  mois_b <- enc2utf8(mois_b)
  day_B <- enc2utf8(day_B)
  day_b <- enc2utf8(day_b)
  phpbb <- locale(date_names(mois_B, mois_b, day_B, day_b))
  d <- readr::parse_date(
    texte,
    format = "%d %m %Y",
    locale = locale(date_names = phpbb, encoding = "Windows-1252")
  )
}
#
# extraction des champs d'une colonne detail
phpbb2_detail_categorie <- function(detail) {
    detail <- detail %>%
      html_element("span span")
    textes <- detail |> as.character()
    if (is.na(textes)) {
      detail_date <- ""
      detail_auteur <- ""
    } else {
      details <- str_match(textes, '<br>(.*?)<br>(.*?)</span>')
      detail_date <- details[1, 2]
      detail_auteur <- detail %>%
        html_element("strong") %>%
        html_text()
      if (is.na(detail_auteur)) {
        detail_auteur <- details[1, 3]
        detail_auteur <- str_replace(detail_auteur, '<.*$', '')
      }
#    carp("detail_date: %s", detail_date);stop("ùùùùùùùùù")
      if (is.na(detail_auteur) | detail_auteur == "") {
        carp("textes: %s", textes)
        glimpse(details)
        stop("ùùùùùùù")
      }
    }
  detail <- detail %>%
    html_text2()
  rc <- list(
    detail = detail,
    detail_date = detail_date,
    detail_auteur = detail_auteur,
    textes = textes
  )
  return(invisible(rc))
}
#
# extraction des champs d'une colonne detail
phpbb2_detail_categorie <- function(detail) {
  detail <- detail |>
    html_element("span.postdetails")
  textes <- detail %>%
    as.character() %>%
    glimpse()
  detail_auteur <- detail %>%
    html_element("strong") %>%
    html_text2()
  details <- str_match(textes, 'postdetails">(.*?)<br>')
  detail_date <- details[1, 2]
  rc <- list(
    detail_date = detail_date,
    detail_auteur = detail_auteur,
    detail = textes
  )
#  glimpse(rc);stop("ùùùùùùùùùùùù")
  return(invisible(rc))
}
#
# extraction des champs d'une colonne detail
phpbb2_detail_sommaire <- function(detail) {
    detail <- detail %>%
      html_element("span span")
    textes <- detail |>
      as.character()
    if (is.na(textes)) {
      detail_date <- ""
      detail_auteur <- ""
    } else {
      details <- str_match(textes, '<br>(.*?)<br>(.*?)</span>')
      detail_date <- details[1, 2]
      detail_auteur <- detail %>%
        html_element("strong") %>%
        html_text()
      if (is.na(detail_auteur)) {
        detail_auteur <- details[1, 3]
        detail_auteur <- str_replace(detail_auteur, '<.*$', '')
      }
#    carp("detail_date: %s", detail_date);stop("ùùùùùùùùù")
      if (is.na(detail_auteur) | detail_auteur == "") {
        carp("textes: %s", textes)
        glimpse(details)
        stop("ùùùùùùù")
      }
    }
  detail <- detail %>%
    html_text2()
  rc <- list(
    detail = detail,
    detail_date = detail_date,
    detail_auteur = detail_auteur,
    textes = textes
  )
  return(invisible(rc))
}
phpbb2_dump <- function(page) {
  nodes <- page %>%
    html_nodes("*")
  for (i in 1:length(nodes)) {
    n <- nodes[i]
    carp("%s class: %s", i, n |> html_attr("class"))
  }
}

