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
#'\dontrun{
#'get_wp_posts(root_url = 'https://domain.com',post_count = 200, after_date = NULL)
#'}
#'
#' @export get_wp_posts
#'
get_wp_posts <- function(root_url, post_count = Inf,after_date = NULL) {
  if(!is.null(after_date)) {
    after_date <- after_date %>% as.character() %>% paste0("T00:00:00")
  }
  if(is.finite(post_count)) {
    posts_real <- tibble()
    loop_count <- ceiling(post_count/100)
    for(j in 1:loop_count) {
      if(!is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/posts?per_page=100&page=',j,'&after=',after_date)),type = 'application/json')
      }
      if(is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/posts?per_page=100&page=',j)),type = 'application/json')
      }
      if(!is.null(response$data$status)) {
        return(posts_real)
      }
      for(k in 1:length(response)) {
        response_df <- tibble(id = response[[k]]$id, date = response[[k]]$date, url = response[[k]]$guid$rendered,
                              title = response[[k]]$title$rendered, content = response[[k]]$content$rendered,
                              author = response[[k]]$author)
        response_tags <- c()
        if(length(response[[k]]$tags) > 0) {
          for(i in 1:length(response[[k]]$tags)) {
            itag = response[[k]]$tags[[i]]
            response_tags <- c(response_tags,itag)
          }
          rtg <- response_tags %>% glue_collapse(sep = ',', last = ',')
        }
        response_cats <- c()
        if(length(response[[k]]$categories) > 0) {
          for(i in 1:length(response[[k]]$categories)) {
            icat = response[[k]]$categories[[i]]
            response_cats <- c(response_cats,icat)
          }
          rtc <- response_cats %>% glue_collapse(sep = ',', last = ',')
        }
        if(length(response[[k]]$tags) == 0) {
          rtg = ''
        }
        if(length(response[[k]]$categories) == 0) {
          rtc = ''
        }
        response_df <- response_df %>% mutate(tags = rtg, categories = rtc)
        posts_real <- bind_rows(posts_real,response_df)
      }
    }
    return(posts_real)
  }

  if(!is.finite(post_count)) {
    response <- list(list(1),list(1),list(status = 1))
    n <- 1
    posts_real <- tibble()

    while (length(response) > 0 & response[[3]]$status != 400) {
      if(!is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/posts?per_page=100&page=',j,'&after=',after_date)),type = 'application/json')
      }
      if(is.null(after_date)) {
        response <- content(GET(paste0(root_url,'/wp-json/wp/v2/posts?per_page=100&page=',j)),type = 'application/json')
      }
      if(length(response) > 0 & response[[3]]$status != 400) {
        for(k in 1:length(response)) {
          response_df <- tibble(id = response[[k]]$id, date = response[[k]]$date, url = response[[k]]$guid$rendered,
                                title = response[[k]]$title$rendered, content = response[[k]]$content$rendered,
                                author = response[[k]]$author)
          response_tags <- c()
          if(length(response[[k]]$tags) > 0) {
            for(i in 1:length(response[[k]]$tags)) {
              itag = response[[k]]$tags[[i]]
              response_tags <- c(response_tags,itag)
            }
            rtg <- response_tags %>% glue_collapse(sep = ',', last = ',')
          }
          if(length(response[[k]]$tags) == 0) {
            rtg = ''
          }
          response_cats <- c()
          if(length(response[[k]]$categories) > 0) {
            for(i in 1:length(response[[k]]$categories)) {
              icat = response[[k]]$categories[[i]]
              response_cats <- c(response_cats,icat)
            }
            rtc <- response_cats %>% glue_collapse(sep = ',', last = ',')
          }
          if(length(response[[k]]$tags) == 0) {
            rtg = ''
          }
          if(length(response[[k]]$categories) == 0) {
            rtc = ''
          }
          response_df <- response_df %>% mutate(tags = rtg, categories = rtc)
          posts_real <- bind_rows(posts_real,response_df)
        }
        n <- n + 1
      }
      else(print(paste0('out of content after ',n,' pages')))
    }
    return(posts_real)
  }
}
