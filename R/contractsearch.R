homecare <- function(x, col1 = Title, col2 = Description){
  x %>%
    filter(str_detect(col1, "Home") | str_detect(col1, "home") |
             str_detect(col2, "Home") | str_detect(col2, "home") |
             str_detect(col1, "Domiciliary") | str_detect(col1, "domiciliary")|
             str_detect(col2, "Domiciliary") | str_detect(col2, "domiciliary"))%>%
    filter(str_detect(col1, "care") | str_detect(col1, "Care")|
             str_detect(col2, "care") | str_detect(col2, "Care"))%>%
    filter(str_detect(col1, "Care Home", negate = TRUE) &
             str_detect(col1, "care home", negate = TRUE))%>%
    filter(str_detect(col2, "Care Home", negate = TRUE) &
             str_detect(col2, "care home", negate = TRUE))
}
