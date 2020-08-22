#' @title Update a WordPress Category
#'
#' @description Update the name, description, or slug of a category in your WordPress site.
#'
#' @param domain The domain on which you wish to update the given category.
#' @param user The username to be passed into the API call to update the category.
#' @param pass The password to be used in the API call to update the category. To get this value, you must have the Application Passwords plugin
#' installed, and must create an application using that plugin via your WordPress admin panel; there you will get the password needed.
#' @param tag_id The ID of the category desired to be updated in the WordPress site's database.
#' @param tag_slug The slug to be associated with the WordPress category. Change with caution, as changing the slug may result in URLs being broken.
#' @param tag_name The desired name of the category that users will see on the WordPress site.
#' @param tag_description The description of the category; this value will be used wherever the description of the WordPress category is used (perhaps in your theme).
#' @param verbose Defaults to TRUE; determines whether or not the desired category name and slug will be printed in the console. Potentially useful if employing this function in a loop in which many categories will be updated at once.
#'
#' @return response from the API. 200 means success -- the category was updated!
#'
#'@examples
#' \dontrun{
#'update_wp_cat(root_url = 'https://domain.com',user = Sys.getenv('username'),pass = Sys.getenv('password'),
#'tag_name = 'cool posts',description_text = 'this is the description text for the category "very cool posts". ')
#'}
#'
#' @export update_wp_cat
#' @import tibble
#' @import httr
#' @import dplyr
#' @importFrom glue glue
#' @importFrom glue glue_collapse

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
