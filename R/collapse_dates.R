#' Collapse Dates
#'
#' @param x is the dataset to be collapsed
#' @param sup is the name of the supplier
#' @param start_y is the variable containing the start year
#' @param end_y is the variable containing the end year
#'
#' @return
#' @export
#'
#' @examples

collapse <- function(x, sup = "supplier_clean", start_y = "start_y", end_y = "end_y") {
  x %>%
    dplyr::arrange(get(sup), get(start_y)) %>%
    dplyr::group_by(get(sup)) %>%
    dplyr::mutate(indx = c(0, cumsum(as.numeric(dplyr::lead(get(start_y))) >
                                       cummax(as.numeric(get(end_y))))[-dplyr::n()])) %>%
    dplyr::group_by(get(supplier_clean), indx) %>%
    dplyr::summarise(start = dplyr::first(get(start_y)), end = dplyr::last(get(end_y)))%>%
    dplyr::select(-indx)
}
