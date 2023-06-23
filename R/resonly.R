#' search amongst care contracts for residential care
#'
#' @param x is the dataframe input
#' @param c1 is the first column to be searched
#' @param c2 is the second column to be searched
#'
#' @return a dataframe
#' @export
#'
#' @examples

res <- function(x, c1 = "Title", c2 = "Description"){
  x %>%
    dplyr::filter(stringr::str_detect(get(c1), "esidential")|
                    stringr::str_detect(get(c1), "Care Home")|
                    stringr::str_detect(get(c1), "care home")|
                    stringr::str_detect(get(c1), "Care home")|
                    stringr::str_detect(get(c1), "Nursing")|
                    stringr::str_detect(get(c1), "nursing")|
                    stringr::str_detect(get(c1), "Extra Care")|
                    stringr::str_detect(get(c1), "xtra care")|
                    stringr::str_detect(get(c2), "esidential")|
                    stringr::str_detect(get(c2), "Care Home")|
                    stringr::str_detect(get(c2), "care home")|
                    stringr::str_detect(get(c2), "Care home")|
                    stringr::str_detect(get(c2), "Nursing")|
                    stringr::str_detect(get(c2), "nursing")|
                    stringr::str_detect(get(c2), "Extra Care")|
                    stringr::str_detect(get(c2), "xtra care"))
}
