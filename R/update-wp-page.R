#' @title Update WordPress Page
#'
#' @description Updates an existing WordPress page with new values.
#'
#' @param root_url The domain on which you wish to create the page.
#' @param user The username to be passed into the API call to create the page.
#' @param pass The password to be used in the API call to create the page. To get this value, you must have the Application Passwords plugin
#' installed, and must create an application using that plugin via your WordPress admin panel; there you will get the password needed.
#' @param page_id The numeric ID in the WordPress database associated with the page being updated.
#' @param title_val The title of the page being updated.
#' @param excerpt_val The excerpt to be shown where your WordPress features excerpts of pages.
#' @param fifu_val If the FIFU plugin is installed, the URL of the featured image can be inserted here.
#' @param content_val The content of the page.
#' @param status_val The status of the page. Can be one of 'draft','publish','pending','future','private'.
#' @param slug_val The slug to be assigned to the page. Can be automatically generated if left as NULL.
#' @param author_val The user ID of the author to be associated with the page.
#'
#' @return A list containing the status code of the API call. A status code of 200 indicates the call was a success.
#'
#'@examples
#' \dontrun{
#'update_wp_page(root_url = 'https://domain.com',user = Sys.getenv('username'),pass = Sys.getenv('password'),
#'page_id = 123,title_val = 'post title',excerpt_val = 'post excerpt',fifu_val = 'https://remotesite.com/image.png',
#'content_val = 'the page content as a string, with wordpress-accepted <strong>html</strong> (or bbcode!)',
#'status_val = 'draft',slug_val = NULL, author_val = '2')
#'}
#'
#' @export update_wp_page
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom glue glue
#' @importFrom glue glue_collapse

update_wp_page <- function(root_url,user,pass,page_id,title_val,excerpt_val ='',fifu_val,content_val,
                           status_val,slug_val = NULL, author_val,format_val = 'standard') {
  pb <- list(title = title_val,
             excerpt = excerpt_val,
             content = content_val,
             status = status_val,
             slug = slug_val,
             fifu = fifu_val,
             author=author_val,
             format=format_val)
  if(is.null(slug_val)) {
    pb$slug <- NULL
  }
  if(is.null(fifu_val)) {
    pb$fifu <- NULL
  }

  ch = POST(glue("{root_url}/wp-json/wp/v2/pages/{page_id}"),
            authenticate(user,pass),
            body = pb,
            encode = "json") %>% content()
  cht <- tibble(title = title_val, status = status_val, author = author_val, url = ch$link,page_id = ch$id)
  return(cht)
}
