#' @title Retrieve WordPress Posts
#'
#' @description Retrieve posts made on the WordPress site.
#'
#' @param root_url The WordPress site for which posts are sought to be retrieved.
#' @param post_count The maximum number of posts to return, sorted by the most recent date. Default value is to return all posts made on the site.
#' @param after_date The date after which posts should be returned.
#' @return A data frame returning the post ID, publication date, title, excerpt, content, tag IDs, category IDs, and author IDs.
#'
#'@examples
#' \dontrun{
#'get_wp_menu(root_url = 'https://domain.com',post_count = 200, after_date = NULL)
#'}
#'
#' @export get_wp_posts
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom glue glue
#' @importFrom glue glue_collapse

get_wp_menus <- function(root_url) {
  response <- content(GET(paste0(root_url,'/wp-json/wp-api-menus/v2/menus/'),
                          authenticate('apiuser','hfA3 Bl8l GNuZ aJcR QTGV Zt0w'),accept_json()))
  id <- response %>% map2('ID',purrr::pluck) %>% modify_if(is.null, ~as.numeric(NA))
  term_id <- response %>% map2('term_id',purrr::pluck) %>% modify_if(is.null, ~as.numeric(NA))
  slug <- response %>% map2('name',purrr::pluck) %>% modify_if(is.null, ~as.character(NA))
  term_group <- response %>% map2('term_group',purrr::pluck) %>% modify_if(is.null, ~as.character(NA))
  term_taxonomy_id <- response %>% map2('term_taxonomy_id',purrr::pluck) %>% modify_if(is.null, ~as.character(NA))
  taxonomy <- response %>% map2('taxonomy',purrr::pluck) %>% modify_if(is.null, ~as.character(NA))
  description <- response %>% map2('description',purrr::pluck) %>% modify_if(is.null, ~as.character(NA))
  parent <- response %>% map2('parent',purrr::pluck) %>% modify_if(is.null, ~as.character(NA))
  count <- response %>% map2('count',purrr::pluck) %>% modify_if(is.null, ~as.numeric(NA))
  menus <- tibble(id = id, term_id = term_id, slug = slug, term_group = term_group,
                  term_taxonomy_id = term_taxonomy_id, taxonomy = taxonomy, description = description,
                  parent = parent, count = count) %>% mutate_all(unlist)
  return(menus)
  }
}
