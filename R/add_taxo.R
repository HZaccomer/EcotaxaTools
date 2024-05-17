


#' add.taxo
#'
#' Return a taxonomic table.
#'
#' @param object_hierarchy_list List of object annotation hierarchy.
#'
#' @return Return a taxonomic table.
#' @export
#'
#' @examples
add.taxo <- function(object_hierarchy_list){
  rep.last <- function(x) {
    for (i in 2:length(x)) {if(is.na(x[i])) x[i] <- x[i-1]}
    return(x)}
  taxo <- data.frame(n=object_hierarchy_list) %>%
    separate_wider_delim(n, delim=">", names_sep = "", too_few = "align_start", cols_remove=F) %>%
    apply(1, rep.last) %>% t() %>% as.data.frame() %>%
    rename(object_annotation_hierarchy=nn) %>%
    mutate(object_annotation_hierarchy2 = gsub("<",">",object_annotation_hierarchy))
  return(taxo)
}
