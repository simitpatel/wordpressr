#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples get_wp_post()
#'
#' @export get_wp_post

create_wp_post <- function(root_url,user,pass,title_val,excerpt_val ='',content_val,status_val,author_val,
                           format_val = 'standard',categories_val, tag_val = '') {
  ch = POST(paste0(root_url,"/wp-json/wp/v2/posts"),
            authenticate(user,pass),
            body = list(title = title_val,
                        excerpt = excerpt_val,
                        content = content_val,
                        status = status_val,
                        author=author_val,
                        format=format_val,
                        categories=categories_val,
                        tags = tag_val),
            encode = "json")
  rt <- tibble(tag_id = content(ch)$id, name = content(ch)$name,slug = content(ch)$slug, description = content(ch)$description)
  return(ch)
}
