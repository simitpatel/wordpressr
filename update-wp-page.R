#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples update_wp_page()
#'
#' @export update_wp_page

update_wp_page <- function(root_url,user,pass,page_id,title_val,excerpt_val ='',fifu_val,content_val,status_val,author_val) {
  ch = POST(glue("{root_url}/wp-json/wp/v2/pages/{page_id}"),
            authenticate(user,pass),
            body = list(title = title_val,
                        excerpt = excerpt_val,
                        fifu_image_url = fifu_val,
                        content = content_val,
                        status = status_val,
                        author=author_val),
            encode = "json")
  return(ch)
}
