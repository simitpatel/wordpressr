#' @title Create a New WordPress Tag
#'
#' @description Create a tag on your WordPress site using your WordPress site's API.
#'
#' @param root_url The domain on which you wish to create the post.
#' @param user The username to be passed into the API call to create the post.
#' @param pass The password to be used in the API call to create the post. To get this value, you must have the Application Passwords plugin
#' installed, and must create an application using that plugin via your WordPress admin panel; there you will get the password needed.
#' @param tag_name The text string associated with the tag.
#' @param description_text The description of the tag; this value will be used wherever the description of the WordPress tag is used (perhaps in your theme).
#'
#' @return response from the API. 200 means the tag was created!
#'
#'@examples
#' \dontrun{
#'create_wp_tag(root_url = 'https://domain.com',user = Sys.getenv('username'),pass = Sys.getenv('password'),
#'tag_name = 'cool posts',description_text = 'this is the description text for the tag "cool posts". ')
#'}
#'
#' @export create_wp_tags
#' @import tibble
#' @import httr
#' @import dplyr

create_wp_tags <- function(root_url,user,pass,tag_name,description_text) {
  ch = httr::POST(paste0(root_url,"/wp-json/wp/v2/tags"),
            httr::authenticate(user,pass),
            body = list(name = tag_name,
                        description = description_text),
            encode = "json")
  rt <- tibble::tibble(tag_id = content(ch)$id, name = content(ch)$name,slug = content(ch)$slug, description = content(ch)$description)
  return(ch)
}
