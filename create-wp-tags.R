#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples get_wp_tags()
#'
#' @export get_wp_tags

create_wp_tags <- function(user,pass,root_url,tag_name,description_text) {
  ch = POST(paste0(root_url,"/wp-json/wp/v2/tags"),
            authenticate(user,pass),
            body = list(name = tag_name,
                        description = description_text),
            encode = "json")
  rt <- tibble(tag_id = content(ch)$id, name = content(ch)$name,slug = content(ch)$slug, description = content(ch)$description)
  return(ch)
}
