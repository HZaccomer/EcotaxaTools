#' BSS_table
#'
#' Return a table of biovolumes by size class for all types of biovolumes and abundance.
#'
#' @param x tsv file of a sample
#' @param NBSS Should it return NBSS or BSS ? False by default, return a BSS.
#'
#' @return Table of biovolume and abundance.
#' @export
#'
#' @examples

BSS_table <- function(x, NBSS=F) {

  # You need to compute by unique acquisitions before grouping

  # resume plain
  resume.plain <- x %>% group_by(object_annotation_category,
                                 object_annotation_hierarchy,
                                 unique_id, sample_id,
                                 class=size_class.id_plain,
                                 norm=size_class.norm_plain,
                                 min=size_class.min_plain,
                                 max=size_class.max_plain) %>%
    summarise(AB=sum(AB * conver, na.rm=T),
              BV=sum(BV_plain * conver, na.rm=T)) %>%
    group_by(object_annotation_category, object_annotation_hierarchy,
             sample_id, class, norm, min, max) %>%
    summarise(AB=sum(AB, na.rm=T),
              BV=sum(BV, na.rm=T)) %>%
    mutate(type="plain") %>% ungroup()

  # resume riddled
  resume.riddled <- x %>% group_by(object_annotation_category,
                                   object_annotation_hierarchy,
                                   unique_id, sample_id,
                                   class=size_class.id_riddled,
                                   norm=size_class.norm_riddled,
                                   min=size_class.min_riddled,
                                   max=size_class.max_riddled) %>%
    summarise(AB=sum(AB * conver, na.rm=T),
              BV=sum(BV_riddled * conver, na.rm=T)) %>%
    group_by(object_annotation_category, object_annotation_hierarchy,
             sample_id, class, norm, min, max) %>%
    summarise(AB=sum(AB, na.rm=T),
              BV=sum(BV, na.rm=T)) %>%
    mutate(type="riddled") %>% ungroup()

  # resume elli
  resume.elli <- x %>% group_by(object_annotation_category,
                                object_annotation_hierarchy,
                                unique_id, sample_id,
                                class=size_class.id_elli,
                                norm=size_class.norm_elli,
                                min=size_class.min_elli,
                                max=size_class.max_elli) %>%
    summarise(AB=sum(AB * conver, na.rm=T),
              BV=sum(BV_elli * conver, na.rm=T)) %>%
    group_by(object_annotation_category, object_annotation_hierarchy,
             sample_id, class, norm, min, max) %>%
    summarise(AB=sum(AB, na.rm=T),
              BV=sum(BV, na.rm=T)) %>%
    mutate(type="elli") %>% ungroup()

  resume <- rbind(resume.elli, resume.plain, resume.riddled)

  if(NBSS==T){
    resume$AB <- resume$AB / resume$norm
    resume$BV <- resume$BV / resume$norm
  }

  return(resume)

}








