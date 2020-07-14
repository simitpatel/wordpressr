#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#'@examples
#'\dontrun{
#'create_wp_page(root_url = 'https://domain.com',user = Sys.getenv('username'),pass = Sys.getenv('password'),
#'title_val = 'post title',excerpt_val = 'post excerpt',
#'content_val = 'the post content as a string, with wordpress-accepted <strong>html</strong> (or bbcode!)',
#'status_val = 'draft',format_val = 'standard')
#'}
#'
#' @export create_wp_post

create_wp_page <- function(root_url,user,pass,title_val,excerpt_val ='',content_val,status_val,author_val,
                           format_val = 'standard') {
  ch = POST(paste0(root_url,"/wp-json/wp/v2/page"),
            authenticate(user,pass),
            body = list(title = title_val,
                        excerpt = excerpt_val,
                        content = content_val,
                        status = status_val,
                        author=author_val,
                        format=format_val),
            encode = "json")
  return(ch)
}
