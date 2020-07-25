#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples get_wp_tags()
#'
#' @export get_wp_tags

get_wp_posts <- function(root_url, post_count = Inf,after_date = NULL) {
  if(!is.null(after_date)) {
    after_date <- after_date %>% as.character() %>% paste0("T00:00:00")
  }
  if(is.finite(post_count)) {
    posts_real <- tibble()
    loop_count <- ceiling(post_count/100)
    for(j in 1:loop_count) {
      response <- content(GET(paste0(root_url,'/wp-json/wp/v2/posts?per_page=100&page=',j,'&after=',after_date)))
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
      response <- content(GET(paste0(root_url,'/wp-json/wp/v2/posts?per_page=100&page=',n,'&after=',after_date)))
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
