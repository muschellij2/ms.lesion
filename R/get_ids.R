#' @title Get IDs with Data in Package
#'
#' @description Return the IDs for the people with data
#' @param group group of IDs to gather.  If both \code{c("training", "test")},
#' all IDs are returned
#' @return Vector of ids
#' 
#' @export
get_ids = function(group = c("training", "test")){
  ids = get_all_ids()
  ids = ids[group]
  ids = unlist(ids)
  names(ids) = NULL
  return(ids)
}

#' @rdname get_ids
#' @export
get_all_ids = function(){
  train_ids = paste0("training", c("01", "02", "03", "04", "05"))
  test_ids = paste0("test", c("01", "02", "03"))
  ids = list(training = train_ids, test = test_ids)
  return(ids)
}


#' @rdname get_ids
#' @export
get_training_ids = function(){
  ids = get_all_ids()
  ids = ids$training
  return(ids)
}

#' @rdname get_ids
#' @export
get_train_ids = get_training_ids

#' @rdname get_ids
#' @export
get_test_ids = function(){
  ids = get_all_ids()
  ids = ids$test
  return(ids)
}
