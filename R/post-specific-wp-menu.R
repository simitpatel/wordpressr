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

post_specific_wp_menu <- function(root_url,menu_id,title,url) {
  pb <- list(title = title,
             url = url)
  response <- POST(paste0(root_url,'/wp-json/wp-api-menus/v2/menus/',menu_id),
                          authenticate('apiuser','hfA3 Bl8l GNuZ aJcR QTGV Zt0w'),
                   body = pb)
  }
}
