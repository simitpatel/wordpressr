#' @title Delete a WordPress Page
#'
#' @description Delete a page on your WordPress site using your WordPress site's API.
#'
#' @param root_url The domain on which you wish to delete the page.
#' @param user The username to be passed into the API call to delete the page.
#' @param pass The password to be used in the API call to delete the page.
#' To get this value, you must have the Application Passwords plugin
#' installed, and must create an application using that plugin via your
#' WordPress admin panel; there you will get the password needed.
#' @param page_id The ID in the WordPress database of the page to be deleted. You can obtain IDs
#' by using the get_wp_pages() function. 
#' @param verbose If TRUE, the page id will be printed to the console. Potentially useful
#' for usage in a single-threaded loop.
#'
#' @return response from the API as a list object. Look for the key "deleted" within the list
#' to confirm the post was deleted.
#'
#'@examples
#' \dontrun{
#'delete_wp_tag(root_url = 'https://domain.com',user = Sys.getenv('username'),
#'pass = Sys.getenv('password'),page_id = 12)
#'}
#'
#' @export delete_wp_page
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom purrr flatten

delete_wp_page <- function(root_url,user,pass,page_id,verbose = TRUE) {
  if(verbose == TRUE) {
    print(page_id)
  }

  ch = httr::DELETE(paste0(root_url,"/wp-json/wp/v2/pages/",page_id),
            httr::authenticate(user,pass),
            body = list(id = page_id,
                        force = 'true'),
            encode = "json")
  rt <- ch %>% httr::content() %>% purrr::flatten()
  return(rt)
}
