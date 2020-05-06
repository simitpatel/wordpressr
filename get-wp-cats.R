#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples get_wp_cats()
#'
#' @export get_wp_cats

get_wp_cats <- function(root_url) {
  
  response <- list(list(1),list(1),list(status = 1))
  n <- 1
  cats_real <- tibble()
  
  while (length(response) > 0) { 
    print(n)
    response <- content(GET(paste0(root_url,'/wp-json/wp/v2/categories?per_page=100&page=',n)))
    if(length(response) > 0) {
      for(k in 1:length(response)) {
        response_df <- tibble(id = response[[k]]$id, name = response[[k]]$name,slug = response[[k]]$slug,
                              post_count = response[[k]]$count,description = response[[k]]$description, 
                              url = response[[k]]$link,cat_parent_id = response[[k]]$parent)
        cats_real <- bind_rows(cats_real,response_df)
      }
      n <- n + 1
    }
    else(print(paste0('out of content after ',n,' pages')))
  }
 return(cats_real) 
}
