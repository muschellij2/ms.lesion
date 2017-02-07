#' @title Get Filenames of Par files
#'
#' @description Return the filenames for the par files
#' @param ids ID to return
#' @param modalities vector of image modalities within
#' \code{c("FLAIR", "MPRAGE", "T2w", "fMRI", "DTI")} to return
#' @param visits Vector of scan indices to return (1 or 2 or both)
#' @examples
#' get_par_filenames()
#' @return Data.frame of filenames
#' @export
get_par_filenames = function(ids = get_ids(), 
                               modalities = c("FLAIR", "MPRAGE", "T2w", 
                                              "fMRI", "DTI"), 
                               visits = c(1,2)){
  modalities = unique(modalities)
  visits = as.numeric(visits)
  visits = sprintf("%02.0f", visits)
  v_ids = c(outer(ids, visits, paste, sep="-"))
  fnames = c(outer(v_ids, modalities, paste, sep="-"))
  fnames = paste0(fnames, ".par.gz")
  df = data.frame(fname = fnames, stringsAsFactors = FALSE)
  ss = strsplit(df$fname, "-")
  df$id = sapply(ss, `[`, 1)
  df$visit = as.numeric(sapply(ss, `[`, 2))
  df$fname = file.path(paste0("visit_", df$visit), df$id, df$fname)
  df$id = NULL
  filenames = system.file( df$fname, package="kirby21")
  return(filenames)
}
