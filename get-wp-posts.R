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

get_wp_posts <- function(root_url) {
  
  response <- 1
  n <- 1
  posts_real <- tibble()
  
  while (length(response) > 0) { 
    print(n)
    response <- content(GET(paste0(root_url,'wp-json/wp/v2/posts?per_page=100&page=',n)))
    if(length(response) > 0 & response[[3]]$status != 400) {
      for(k in 1:length(response)) {
        response_df <- tibble(id = response[[k]]$id, date = response[[k]]$date, amu_url = response[[k]]$guid$rendered,
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
        response_df <- response_df %>% mutate(tags = rtg)
        posts_real <- bind_rows(posts_real,response_df)
      }
      n <- n + 1
    }
    else(print(paste0('out of content after ',n,' pages')))
  }
 return(posts_real) 
}
