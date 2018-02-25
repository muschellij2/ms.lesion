#' @title All Modalities
#'
#' @description Return the modalities for images 
#' @return \code{data.frame} of two columns:
#' \itemize{
#' \item{modality}: modality of image
#' }
#' 
#' @export
modality_df = function(){
  mods = c(
    "t1",
    "flair", 
    "t2"
    )
  mods = matrix(mods, ncol = 1)
  mods = data.frame(mods, stringsAsFactors = FALSE)
  colnames(mods) = c("modality")
  rownames(mods) = mods$modality
  return(mods)
}