#' @title Get WordPress Tags
#'
#' @description Retrieves a list of all tags created on a given WordPress site.
#'
#' @param root_url The WordPress site for which the data is sought.
#'
#' @return A dataframe with three values: the slug for a given tag, its ID in the WordPress site database, and the human readable tag name.
#'
#' @examples
#' #'\dontrun{
#' get_wp_tags('domain.com')
#' }
#' @export get_wp_tags

get_wp_tags <- function(root_url) {

  response <- 1
  n <- 1
  tags_real <- tibble()

  while (length(response) > 0) {
    print(n)
    response <- httr::content(httr::GET(paste0(root_url,'/wp-json/wp/v2/tags?per_page=100&page=',n)))
    if(length(response) > 0) {
      response_df <- response %>% unlist() %>% tibble::enframe() %>% dplyr::filter(name %in% c('slug','id','name')) %>%
        dplyr::mutate(key = rep(1:(nrow(.)/3),each=3)) %>% dplyr::group_by(key) %>% tidyr::spread(name,value) %>% ungroup()
      n <- n + 1
      tags_real <- bind_rows(tags_real,response_df)
    }
    else(print(paste0('out of tags after ',n,' pages')))
  }
 return(tags_real)
}
