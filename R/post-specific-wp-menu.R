
post_specific_wp_menu <- function(root_url,menu_id,title,url) {
  pb <- list(title = title,
             url = url)
  response <- POST(paste0(root_url,'/wp-json/wp-api-menus/v2/menus/',menu_id),
                   authenticate(Sys.getenv('wp_user'),Sys.getenv('wp_key')),
                   body = pb)
}

