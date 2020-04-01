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

get_wp_tags <- function(root_url) {
  
  response <- 1
  n <- 1
  tags_real <- tibble()
  
  while (length(response) > 0) { 
    print(n)
    response <- content(GET(paste0(root_url,'/wp-json/wp/v2/tags?per_page=100&page=',n)))
    if(length(response) > 0) {
      response_df <- response %>% unlist %>% enframe() %>% filter(name %in% c('slug','id','name')) %>%
        mutate(key = rep(1:(nrow(.)/3),each=3)) %>% group_by(key) %>% spread(name,value) %>% ungroup()
      n <- n + 1
      tags_real <- bind_rows(tags_real,response_df)
    }
    else(print(paste0('out of tags after ',n,' pages')))
  }
 return(tags_real) 
}
