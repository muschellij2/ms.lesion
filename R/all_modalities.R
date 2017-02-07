#' @title Return All Modalities
#'
#' @description Return the modalities for images where
#' packages were developed
#' @return Vector of characters
#' 
#' @export
all_modalities = function(){
  mods = modality_df()
  mods = mods$modality
  return(mods)
}