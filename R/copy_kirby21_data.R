#' @title Copy Kirby21 Data to an output directory
#'
#' @description Copies files from Kirby21 Package to an output directory
#' @param outdir Output directory for data
#' @param ... Arguments to pass to \code{\link{get_image_filenames}}
#' @return Logical if files are copied
#' @export
copy_kirby21_data = function(outdir, ...){
  niis = get_image_filenames(...)
  stopifnot(length(niis) > 0)
  ### get just the filenames
  
  k21_file = system.file(package = "kirby21")
  stubs = gsub(k21_file, "", niis, fixed = TRUE)
  stubs = gsub("^/", "", stubs)
  
  #################################################
  # Make output directories
  #################################################  
  alldirs = file.path(outdir, dirname(stubs))
  dirs = unique(alldirs)
  direxists = file.exists(dirs)
  mkdirs = dirs[!direxists]
  if (length(mkdirs) > 0) {
    sapply(mkdirs, dir.create, recursive = TRUE)
  }

  for (ifile in seq_along(niis)) {
    nii = niis[ifile]
    file.copy(nii, 
              to = alldirs[ifile], 
              recursive = TRUE)
  }
  return(TRUE)
}