#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples update_wp_cat()
#'
#' @export update_wp_cat

update_wp_cat <- function(domain,user, pass,tag_id,tag_name,tag_slug,tag_description,verbose = TRUE) {
  if(verbose == TRUE){
    print(paste(tag_name,"||",tag_slug))
  }
  ch = POST(glue("{domain}/wp-json/wp/v2/categories/{tag_id}"),
            authenticate(user,pass),
            body = list(name = tag_name,
                        description = tag_description,
                        slug = tag_slug),
            encode = "json")
}