#' Collapse Dates
#'
#' @param x is the dataset to be collapsed
#' @param sup is the name of the supplier
#' @param .start is the variable containing the start year
#' @param .end is the variable containing the end year
#'
#' @return
#' @export
#' @import dplyr
#' @examples

collapse <- function(x, sup = "supplier_clean", .start = "start_y", .end = "end_y") {
  x %>%
    arrange(get(sup), get(.start)) %>%
    group_by(get(sup)) %>%
    mutate(indx = c(0, cumsum(as.numeric(lead(get(.start))) >
                                cummax(as.numeric(get(.end))))[-n()])) %>%
    group_by(get(sup), indx) %>%
    summarise(start = first(get(.start)), end = last(get(.end)))%>%
    rename(get(sup) = `get(sup)`)%>%
    select(-indx)
}
