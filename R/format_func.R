#' Function to Format Registries Ready to be Appended
#'
#' @param x is the dataframe to be used
#' @param tit is the contract title variable
#' @param dep is the council department variable
#' @param desc is the description of the contract variable
#' @param st  is the start date variable
#' @param en  is the end date variable
#' @param rev is the review date variable
#' @param frame is the framework type variable
#' @param sup is the supplier name variable
#' @param council is the council variable
#'
#' @return a dataframe
#' @export
#'
#' @examples
con_format <- function(x,
                       tit = "Title",
                       dep = "Department",
                       desc = "Description",
                       st = "Start.Date",
                       en = "End.Date",
                       rev = "Review.Date",
                       frame = "Framework.Type",
                       sup = "Supplier",
                       coun = "Council"){
  x%>%
    dplyr::mutate(title = get(tit),
           department = get(dep),
           description = get(desc),
           start = get(st),
           end = get(en),
           review = get(rev),
           framework = get(frame),
           supplier = get(sup),
           council = get(coun))%>%
    dplyr::select(title,
           department,
           description,
           start,
           end,
           review,
           framework,
           supplier,
           council)
}
