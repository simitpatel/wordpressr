#' @title Retrieve WordPress Pages
#'
#' @description Retrieve pages made on the WordPress site.
#'
#' @param root_url The WordPress site for which pages are sought to be retrieved.
#' @param post_count The maximum number of pages to return, sorted by the most recent date.
#' Default value is to return all pages made on the site.
#' @param after_date The date after which pages should be returned.
#' @return A data frame returning the post ID, publication date, title, excerpt, content,
#' tag IDs, category IDs, and author IDs.
#'
#'@examples
#' \dontrun{
#'get_wp_pages(root_url = 'https://domain.com',page_count = 200, after_date = NULL)
#'}
#'
#' @export get_wp_pages
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom glue glue
#' @importFrom glue glue_collapse

get_wp_pages <- function(root_url, page_count = Inf,after_date = NULL) {

  if(!is.null(after_date)) {
    after_date <- after_date %>% as.character() %>% paste0("T00:00:00")
  }
  if(is.finite(page_count)) {
    posts_real <- tibble()
    loop_count <- ceiling(page_count/100)
    for(j in 1:loop_count) {
      if(!is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/pages?per_page=100&page=',j,'&after=',after_date,'&orderby=id'),accept_json()))
      }
      if(is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/pages?per_page=100&page=',j,'&orderby=id'),accept_json()))
      }
      if(!is.null(response$data$status)) {
        return(posts_real)
      }
      for(k in 1:length(response)) {
        response_df <- tibble(id = response[[k]]$id, date = response[[k]]$date, url = response[[k]]$guid$rendered,
                              title = response[[k]]$title$rendered, content = response[[k]]$content$rendered,
                              excerpt = response[[k]]$excerpt$rendered, author = response[[k]]$author)
        posts_real <- bind_rows(posts_real,response_df)
      }
    }
    return(posts_real)
  }

  if(!is.finite(page_count)) {
    response <- list(list(1),list(1),list(status = 1))
    n <- 1
    posts_real <- tibble()

    while (length(response) > 0 & response[[3]]$status != 400) {
      if(!is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/pages?per_page=100&page=',n,'&after=',after_date,'&orderby=id'),accept_json()))
      }
      if(is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/pages?per_page=100&page=',n,'&orderby=id'),accept_json()))
      }
      if(length(response) > 0 & response[[3]]$status != 400) {
        for(k in 1:length(response)) {
          response_df <- tibble(id = response[[k]]$id, date = response[[k]]$date, url = response[[k]]$guid$rendered,
                                title = response[[k]]$title$rendered, content = response[[k]]$content$rendered,
                                excerpt = response[[k]]$excerpt$rendered, author = response[[k]]$author)
          response_df <- response_df
          posts_real <- bind_rows(posts_real,response_df)
        }
        n <- n + 1
      }
      else(print(paste0('out of content after ',n,' pages')))
    }
    return(posts_real)
  }
}
