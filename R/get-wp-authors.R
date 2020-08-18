#' @title Get WordPress Authors
#'
#' @description Retrieves a list of all publishers on the site.
#'
#' @param root_url The WordPress site for which the data is sought.
#'
#' @return A dataframe with four values: the user ID for the author in the site's WordPress database; the author's URL on the site; the author's name in the database; and the value of the description field associated with the author.
#'
#' \dontrun{
#' get_wp_authors('domain.com')
#'}
#'
#' @export get_wp_authors
#' @import tibble
#' @import httr
#' @import dplyr

get_wp_authors <- function(root_url) {

  response <- 1
  n <- 1
  authors_real <- tibble()

  while (length(response) > 0) {
    print(n)
    response <- content(GET(paste0(root_url,'/wp-json/wp/v2/users?per_page=100&page=',n)))
    if(length(response) > 0) {
      for(k in 1:length(response)) {
        response_df <- tibble(user_id = response[[k]]$id, author_url = response[[k]]$link,
                              user_name = response[[k]]$name, description = response[[k]]$description)
        authors_real <- bind_rows(authors_real,response_df)
      }
      n <- n + 1
    }
    else(print(paste0('out of content after ',n,' pages')))
  }
 return(authors_real)
}
