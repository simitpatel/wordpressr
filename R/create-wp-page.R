#' @title Create a New WordPress Page
#'
#' @description Creates a new page on the WordPress site provided using the
#' credentials entered, with content and other information provided.
#'
#' @param root_url The domain on which you wish to create the page.
#' @param user The username to be passed into the API call to create the page.
#' @param pass The password to be used in the API call to create the page.
#' To get this value, you must have the Application Passwords plugin
#' installed, and must create an application using that plugin via your
#' WordPress admin panel; there you will get the password needed.
#' @param title_val The title of the page you are creating.
#' @param excerpt_val The excerpt to be shown where your WordPress features excerpts of
#' pages.
#' @param content_val The content of the page.
#' @param status_val The status of the page. Can be one of 'draft','publish','pending',
#' 'future','private'.
#' @param author_val The user ID of the author creating the page.
#' @param format_val The WordPress format to use. Defaults to 'standard'.
#'
#' @return A list containing the status code of the API call. A status code of 200 indicates
#' the call was a success.
#'
#'@examples
#' \dontrun{
#'create_wp_page(root_url = 'https://domain.com',user = Sys.getenv('username'),
#'pass = Sys.getenv('password'),
#'title_val = 'post title',excerpt_val = 'post excerpt',
#'content_val = 'the post content as a string, with wordpress-accepted
#'<strong>html</strong> (or bbcode!)',
#'status_val = 'draft',format_val = 'standard')
#'}
#'
#'
#' @export create_wp_post
#' @import tibble
#' @import httr
#' @import dplyr

create_wp_page <- function(root_url,user,pass,title_val,excerpt_val ='',content_val,status_val,
                           author_val,format_val = 'standard',output = 'tibble') {

  ch = httr::POST(paste0(root_url,"/wp-json/wp/v2/pages"),
            httr::authenticate(user,pass),
            body = list(title = title_val,
                        excerpt = excerpt_val,
                        content = content_val,
                        status = status_val,
                        author=author_val,
                        format=format_val),
            encode = "json") %>% content()
  if(output == 'tibble') {
    cht <- tibble(title = title_val, status = status_val, author = author_val, url = ch$url)
  }
  if(output != 'tibble') {
    return(ch)
  }
}
