#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples get_wp_authors()
#'
#' @export get_wp_authors

get_wp_users <- function(root_url) {
  
  response <- 1
  n <- 1
  users_real <- tibble()
  
  while (length(response) > 0) { 
    print(n)
    response <- content(GET(paste0(root_url,'/wp-json/wp/v2/users?per_page=100&page=',n)))
    if(length(response) > 0) {
      for(k in 1:length(response)) {
        response_df <- tibble(user_id = response[[k]]$id, user_name = response[[k]]$name,site_link = response[[k]]$link)
        users_real <- bind_rows(users_real,response_df)
      }
      n <- n + 1
    }
    else(print(paste0('out of content after ',n,' pages')))
  }
 return(users_real) 
}
