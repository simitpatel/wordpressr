#' @title Delete a WordPress Post
#'
#' @description Delete a post on your WordPress site using your WordPress site's API.
#'
#' @param root_url The domain on which you wish to create the post.
#' @param user The username to be passed into the API call to create the post.
#' @param pass The password to be used in the API call to create the post.
#' To get this value, you must have the Application Passwords plugin
#' installed, and must create an application using that plugin via your
#' WordPress admin panel; there you will get the password needed.
#' @param post_id The text string associated with the tag.
#' @param verbose If TRUE, the tag id will be printed to the console. Potentially useful
#' for usage in a single-threaded loop.
#'
#' @return response from the API as a list object. Look for the key "deleted" within the list
#' to confirm the post was deleted.
#'
#'@examples
#' \dontrun{
#'delete_wp_tag(root_url = 'https://domain.com',user = Sys.getenv('username'),
#'pass = Sys.getenv('password'),post_id = 12)
#'}
#'
#' @export delete_wp_post
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom purrr flatten

delete_wp_post <- function(root_url,user,pass,post_id,verbose = TRUE) {
  if(verbose == TRUE) {
    print(post_id)
  }

  ch = httr::DELETE(paste0(root_url,"/wp-json/wp/v2/posts/",post_id),
            httr::authenticate(user,pass),
            body = list(id = post_id,
                        force = 'true'),
            encode = "json")
  rt <- ch %>% httr::content() %>% purrr::flatten()
  return(rt)
}
