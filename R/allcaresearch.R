#' All care search
#'
#' @param x is the data frame to be used
#' @param c1 is the first column to be searched
#' @param c2 is the second column to be searched
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
care <- function(x, c1 = "Description", c2 = "Title"){
  x %>%
    dplyr::filter(stringr::str_detect(get(c1), "care") |
                    stringr::str_detect(get(c1), "Care")|
                    stringr::str_detect(get(c2), "care")|
                    stringr::str_detect(get(c2), "Care"))
}


