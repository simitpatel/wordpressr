#' @title Retrieve WordPress User List
#'
#' @description Returns all the registered users on a given WordPress site that can be retrieved without a key.
#'
#' @param root_url The domain for which users are sought to be retrieved.
#'
#' @return A dataframe with three columns: the ID of the user in the WordPress database; their user name on the site; and the URL of their user page on the site.
#'
#' @examples
#' \dontrun{
#'get_wp_users(root_url = 'domain.com')
#'}
#'
#' @export get_wp_users
#' @import tibble
#' @import httr
#' @import dplyr

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
