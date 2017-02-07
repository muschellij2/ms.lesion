#' @title All Modalities and the Corresponding package
#'
#' @description Return the modalities for images and the
#' packages that contain them
#' @return \code{data.frame} of two columns:
#' \itemize{
#' \item{modality}: modality of image
#' \item{package}: package that contains it
#' }
#' 
#' @export
modality_df = function(){
  mods = c(
    "FLAIR", "kirby21.flair",
    "T1",  "kirby21.t1",
    "T2",  "kirby21.t2",
    "fMRI",  "kirby21.fmri",
    "DTI", "kirby21.dti",
    "ASL",  "kirby21.asl",
    "ASLM0",  "kirby21.asl",
    "MT", "kirby21.mt",
    "DET2", "kirby21.det2",
    "SURVEY", "kirby21.survey",
    "VASO", "kirby21.vaso"
    )
  mods = matrix(mods, byrow = TRUE, ncol = 2)
  mods = data.frame(mods, stringsAsFactors = FALSE)
  colnames(mods) = c("modality", "package")
  rownames(mods) = mods$modality
  return(mods)
}