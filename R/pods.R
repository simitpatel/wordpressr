#' @title Create a Category-like Pod
#'
#' @description If you're using the Pods plugin and have created
#' a type of pod that is essentially like a category, this
#' function will help you work with that endpoint. You'll need to have
#' an environment variable called 'wp_domain' that contains the base
#' URL of your site (i.e. https://example.com).

#' @export create_wp_catlike_pod
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom glue glue

create_wp_catlike_pod <- function(route,name,slug,description) {
  ch <- httr::POST(glue::glue("{Sys.getenv('wp_domain')}/wp-json/wp/v2/{route}"),
             httr::authenticate(Sys.getenv("sn_user"),Sys.getenv('sn_wp_key')),
             body = list(name = name,
                         description = description,
                         slug = slug),
             encode = "json")
  rt <- tibble::tibble(id = httr::content(ch)$id, name = httr::content(ch)$name,
                       slug = httr::content(ch)$slug, description = httr::content(ch)$description)
  return(rt)
}

#' @title Update a Category-like Pod
#'
#' @description If you're using the Pods plugin and have created
#' a type of pod that is essentially like a category, this
#' function will help you work with that endpoint. You'll need to have
#' an environment variable called 'wp_domain' that contains the base
#' URL of your site (i.e. https://example.com).

#' @export update_wp_catlike_pod
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom glue glue

update_wp_catlike_pod <- function(route,id,name,slug,description) {
  httr::POST(glue::glue("{Sys.getenv('wp_domain')}/wp-json/wp/v2/{route}/{id}"),
             httr::authenticate(Sys.getenv("sn_user"),Sys.getenv('sn_wp_key')),
             body = list(name = name,
                         description = description,
                         slug = slug),
             encode = "json")
}

#' @export get_wp_catlike_pod
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom glue glue

#' @title Get a list of Category-like Pod
#'
#' @description If you're using the Pods plugin and have created
#' a type of pod that is essentially like a category, this
#' function will help you get all the "categories" in a given route.
#' You'll need to have an environment variable called
#' 'wp_domain' that contains the base
#' URL of your site (i.e. https://example.com).

get_wp_catlike_pod <- function(route) {
  pod_list <- list(a = 1)
  i = 1
  pod_table <- tibble()
  while(length(pod_list) != 0) {
    pod_list <- httr::GET(glue::glue("{Sys.getenv('wp_domain')}/wp-json/wp/v2/{route}?per_page=100&page={i}")) %>%
      httr::content()
    id <- purrr::map(pod_list,'id')
    name <- purrr::map(pod_list,'name')
    slug <- purrr::map(pod_list,'slug')
    description <- purrr::map(pod_list,'description')
    pod_information <- tibble::tibble(id = id, name = name, slug = slug, description = description)
    i = i + 1
    pod_table <- bind_rows(pod_table,pod_information)
  }
  pod_table_fmt <- pod_table %>% mutate_all(unlist)
  return(pod_table_fmt)
}


