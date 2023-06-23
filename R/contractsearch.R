#' home care only search, intended to be piped after all-care search
#'
#' @param x is the dataframe used
#' @param c1 is the first column to be searched
#' @param c2 is the second column to be searched
#'
#' @return A dataframe
#' @export
#'
#' @examples

home <- function(x, c1 = "Title", c2 = "Description"){
  x %>%
    dplyr::filter(stringr::str_detect(get(c1), "Home") | stringr::str_detect(get(c1), "home") |
             stringr::str_detect(get(c2), "Home") | stringr::str_detect(get(c2), "home") |
             stringr::str_detect(get(c1), "Domiciliary") | stringr::str_detect(get(c1), "domiciliary")|
             stringr::str_detect(get(c2), "Domiciliary") | stringr::str_detect(get(c2), "domiciliary"))%>%
    dplyr::filter(stringr::str_detect(get(c1), "Care Home", negate = TRUE) &
             stringr::str_detect(get(c1), "care home", negate = TRUE))%>%
    dplyr::filter(stringr::str_detect(get(c2), "Care Home", negate = TRUE) &
             stringr::str_detect(get(c2), "care home", negate = TRUE))
}
