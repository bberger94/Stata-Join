# Function to replicate the functionality of a stata join. Essentially a full/outer join with flags.

# Main function
stata_join <- function(data, type = '', by, using, keep = c(1,2,3),...){

  library(dplyr)
  
  assert <- function (expr, error) {
    if (! expr) stop(error, call. = FALSE)
  }
  
  assert(all(is.element(keep, c(1,2,3))), 'Keep must be a subset of the vector c(1,2,3)')
  assert(!any(is.element(el = c(names(data), names(using)), set = c('merge_data', 'merge_using', 'merge'))), 'Invalid column name')
  assert(is.element(type, c('', '1:m', 'm:1', '1:1', 'm:m')), 'Invalid merge type')
  
  
  if(type == '1:m' | type == '1:1'){
    error_message <- ifelse(length(by > 1)
                            ,paste('Variables',paste(by, collapse = ' and '),'do not uniquely identify observations in the data')
                            ,paste('Variable',by,'does not uniquely identify observations in the data')
    )
    assert(n_distinct(data %>% select_(by)) == count(data %>% select_(by)), error_message)
  }
  
  if(type == 'm:1' | type == '1:1'){
    error_message <- ifelse(length(by > 1)
                            ,paste('Variables',paste(by, collapse = ' and '),'do not uniquely identify observations in the using data')
                            ,paste('Variable',by,'does not uniquely identify observations in the using data')
    )
    assert(n_distinct(using %>% select_(by)) == count(using %>% select_(by)), error_message)
  }
  
  
  
  
  data <- data %>% mutate(merge_data = TRUE)
  using <- using %>% mutate(merge_using = TRUE)
  z <- full_join(data,using,by, ...) 
  
  z <-
    z %>% 
    mutate(
      merge_data = coalesce(merge_data, FALSE), merge_using = coalesce(merge_using, FALSE)
      ,merge = ifelse(merge_data & !merge_using, 1,
               ifelse(!merge_data & merge_using, 2,
               ifelse(merge_data & merge_using, 3, NA)))
    ) %>% 
    select(-c(merge_data, merge_using)) %>% 
    filter(is.element(merge, keep))
  
  stopifnot(all(!is.na(z$merge)))
  return(z)
}

# Tester function
test <- function(){

  library(dplyr)
  
  set.seed(101)
  
  data <- data_frame(id1 = c(1,2,3,1,2,3), id2 = c('a','a','a','b','b','b'), b = rnorm(length(id1)))
  using <- data_frame(id1 = c(1,2,2,4), id2 = c('a','b','a','c'), c = runif(length(id1), 0, 1))
  
  stata_join(data, type = 'm:m', by = c('id1', 'id2'), using = using) 
}

test()
