# <!-- coding: utf-8 -->
#
# quelques fonctions pour discourse
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf
#
discourse_url="http://discourse.local.lan"
discourse_username="mga"
discourse_api_key="97dce6bb42d040be6c43431980446321e4dbd5d1edeaeed6570c1eb722ab093f"
#
# https://github.com/sckott/discgolf
# source("geo/scripts/discourse.R");misc_install()

misc_install <- function() {
  install.packages("remotes")
  remotes::install_github("sckott/discgolf")
}
# source("geo/scripts/discourse.R");misc_test()
misc_test <- function() {
  library(discgolf)
  options(discourse_url="http://discourse.local.lan")
  options(discourse_username="mga")
  options(discourse_api_key="97dce6bb42d040be6c43431980446321e4dbd5d1edeaeed6570c1eb722ab093f")
  getOption("discourse_url")
  topics_latest()
  topic_create("The problem with blue skies", text = "just saying and all that")
#  post_create(topic_id = 13, text = "There isn't a problem!")
}
#
## avec httr2
# https://httr2.r-lib.org/articles/wrapping-apis.html
#
# source("geo/scripts/discourse.R");misc_httr2()
misc_httr2 <- function() {
  library(httr2)

  api_url <- sprintf("%s", discourse_url)
  req <- httr2::request(api_url)
  resp <- req |>
    req_url_path_append("/latest.json") |>
    req_user_agent("discourse mga") |>
    req_headers(
      "Api-Key" = discourse_api_key,
      "Api-Username" = discourse_username
    ) |>
    req_perform()
  resp |> resp_body_json() |> str()
}
#
# https://meta.discourse.org/t/discourse-rest-api-comprehensive-examples/274354
#
# source("geo/scripts/discourse.R");misc_topic_new()
# curl -X POST "https://your-discourse.com/posts.json"
# -H "Content-Type: application/json"
# -H "Api-Key: YOUR_API_KEY" -H "Api-Username: YOUR_USERNAME"
# -d "{\"title\": \"Test topic creation with the API\", \"raw\": \"And here's the topic's content\", \"category\": CATEGORY_ID }"
misc_topic_new <- function() {
  library(httr2)
  req <- httr2::request(discourse_url)
  resp <- req |>
    req_url_path_append("/posts.json") |>
    req_user_agent("discourse mga") |>
    req_headers(
      "Api-Key" = discourse_api_key,
      "Api-Username" = discourse_username,
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(
      title = "le titre de ce post",
      raw = "le contenu doit faire au moins 20 caractères",
      category = 3
    )) |>
    req_perform()
  resp <- last_response()
  resp |> resp_body_json()
  resp |> resp_body_json() |> str()
}
misc_post <- function(url = "categories", json) {
  library(httr2)
  req <- httr2::request(discourse_url)
  resp <<- req |>
    req_url_path_append(sprintf("/%s.json", url)) |>
    req_user_agent("discourse mga") |>
    req_headers(
      "Api-Key" = discourse_api_key,
      "Api-Username" = discourse_username,
      "Content-Type" = "application/json"
    ) |>
    req_body_json(json) |>
    req_perform()
  resp <- last_response()
  resp |> resp_body_json()
  resp |> resp_body_json() |> str()
}
# source("geo/scripts/discourse.R");misc_get_categories()
misc_get_categories <- function() {
  library(httr2)
  req <- httr2::request(discourse_url)
  resp <<- req |>
    req_url_path_append(sprintf("/categories.json?include_subcategories=true")) |>
    req_user_agent("discourse mga") |>
    req_headers(
      "Api-Key" = discourse_api_key,
      "Api-Username" = discourse_username,
      "Content-Type" = "application/json"
    ) |>
    req_perform()
  resp |> resp_body_json() |> str()
}
# source("geo/scripts/discourse.R");misc_delete_category()
misc_delete_category <- function(categorie = 22) {
  library(httr2)
  req <- httr2::request(discourse_url)
  resp <<- req |>
    req_url_path_append(sprintf("/categories/%s", categorie)) |>
    req_user_agent("discourse mga") |>
    req_headers(
      "Api-Key" = discourse_api_key,
      "Api-Username" = discourse_username,
      "Content-Type" = "application/json"
    ) |>
    req_method("DELETE") |>
    req_perform()
  resp |> resp_body_json() |> str()
}