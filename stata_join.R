library(dplyr)

stata_join <- function(x,y, keep = c(1,2,3),...){
  
  stopifnot(all(is.element(keep, c(1,2,3))))
  stopifnot(!any(is.element(el = c(names(x), names(y)), set = c('merge_x', 'merge_y', 'merge'))))
  stopifnot(all(is.element(keep, c(1,2,3))))
  
  x <- x %>% mutate(merge_x = TRUE)
  y <- y %>% mutate(merge_y = TRUE)
  z <- full_join(x,y,...) 
  
  z <-
    z %>% 
    mutate(
      merge_x = coalesce(merge_x, FALSE), merge_y = coalesce(merge_y, FALSE)
      ,merge = ifelse(merge_x & !merge_y, 1,
               ifelse(!merge_x & merge_y, 2,
               ifelse(merge_x & merge_y, 3, NA)))
    ) %>% 
    select(-c(merge_x, merge_y)) %>% 
    filter(is.element(merge, keep))
  
  stopifnot(all(!is.na(z$merge)))
  return(z)
}

test <- function(){
  set.seed(101)
  x <- data_frame(id = c(1,2,3), b = rnorm(3))
  y <- data_frame(id = c(1,2,4), c = runif(3, 0, 1))
  
  stata_join(x, y) 
}

test()
