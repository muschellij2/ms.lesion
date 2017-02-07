#' @title Get Image Filenames in a data.frame
#'
#' @description Return a data.frame of filenames for the images
#' @param ids ID to return
#' @param modalities vector of image modalities within
#' \code{c("FLAIR", "MPRAGE", "T2w", "fMRI", "DTI")} to return
#' @param visits Vector of scan indices to return (1 or 2 or both)
#' @param long if \code{TRUE}, each row is a subject, visit, modality pair
#' 
#' @return Data.frame of filenames
#' 
#' @examples
#' get_image_filenames_df()
#' @importFrom utils installed.packages 
#' @importFrom stats reshape
#' 
#' @export
get_image_filenames_df = function(
  ids = get_ids(), 
  modalities = all_modalities(), 
  visits = c(1,2),
  long = TRUE){
  
  
  ##########################################
  # Get installed packages
  packs = installed.packages()
  packs = packs[, "Package"]
  ##########################################
  
  ##########################################
  # Make the data.frames
  ##########################################
  modalities = unique(modalities)
  visits = as.numeric(visits)
  visits = sprintf("%02.0f", visits)
  v_ids = c(outer(ids, visits, paste, sep = "-"))
  fnames = c(outer(v_ids, modalities, paste, sep = "-"))
  fnames = paste0(fnames, ".nii.gz")
  df = data.frame(filename = fnames, stringsAsFactors = FALSE)
  ss = strsplit(df$filename, "-")
  df$Subject_ID = sapply(ss, `[`, 1)
  df$visit = as.numeric(sapply(ss, `[`, 2))
  nii.stub = function(x){
    nx = names(x)
    x = path.expand(x)
    stub = gsub("\\.gz$", "", x)
    stub = gsub("\\.nii$", "", stub)    
    names(stub) = nx
    return(stub)    
  }
  df$modality = nii.stub(sapply(ss, `[`, 3))
  df$filename = file.path(paste0("visit_", df$visit), 
                          df$Subject_ID, df$filename)
  # df$id = NULL
  ##########################################
  # Set the package names
  ##########################################
  mod = modality_df()
  df = merge(df, mod, sort = FALSE, by = "modality", all.x = TRUE)

  ########################################
  # Find those not installed and warn
  ########################################  
  not_installed = setdiff(df$package, packs)
  if (length(not_installed) > 0) {
    not_installed = paste(not_installed, collapse = " ")
    warning(paste0("Packages ", not_installed, 
                   " are not installed, modalities from ", 
                   "these packages will be missing"))
  }
  df$filename = mapply(function(fname, pkg){
    system.file( fname, package = pkg)
  }, df$filename, df$package)
  
  df = df[ !(df$filename %in% ""), , drop = FALSE]
  df$package = NULL
  
  if (!long) {
    df = reshape(df, idvar = c("Subject_ID", "visit"), 
                 timevar = "modality", direction = "wide")
    cn = colnames(df)
    cn = sub("^filename[.]", "", cn)
    colnames(df) = cn
  }  
  return(df)
}

#' @title Get Image Filenames 
#'
#' @description Return the filenames for the images
#' @param ... arguments passed to \code{\link{get_image_filenames_df}}
#' @examples
#' get_image_filenames() 
#' @export
get_image_filenames = function(...){

  df = get_image_filenames_df(..., long = TRUE)
  filenames = df$filename
  if (length(filenames) == 0) {
    return(NULL)
  }
  return(filenames)
}

#' @rdname get_image_filenames_df
#' @param ... arguments passed to \code{\link{get_image_filenames_df}}
#' @examples
#' get_image_filenames_matrix()  
#' @export
get_image_filenames_matrix = function(...){
  df = get_image_filenames_df(...,
                              long = FALSE)
  df$Subject_ID = NULL
  df$visit = NULL
  
  if (is.null(df)) {
    return(NULL)
  }
  if (nrow(df) == 0) {
    return(NULL)
  }  
  df = as.matrix(df)
  return(df)
}

#' @rdname get_image_filenames_df
#' @examples
#' get_image_filenames_list()   
#' @export
get_image_filenames_list = function(...){
  
  df = get_image_filenames_df(..., long = FALSE)
  if (is.null(df)) {
    return(NULL)
  }  
  if (nrow(df) == 0) {
    return(NULL)
  }  
  df$Subject_ID = df$visit = NULL
  ss = as.list(df)
  return(ss)
}


#' @rdname get_image_filenames_df
#' @examples
#' get_image_filenames_list_by_visit()    
#' @export
get_image_filenames_list_by_visit = function(...){
  
  df = get_image_filenames_df(..., long = TRUE)
  if (is.null(df)) {
    return(NULL)
  }  
  if (nrow(df) == 0) {
    return(NULL)
  }    
  ss = split(df, df$visit)
  ss = lapply(ss, function(x){
    x$visit = NULL
    x = split(x, x$Subject_ID)
    x = lapply(x, function(r) {
      r$Subject_ID = NULL
      r$filename
    })    
    x
  })
  return(ss)
}

#' @rdname get_image_filenames_df
#' @examples
#' get_image_filenames_list_by_subject()     
#' @export
get_image_filenames_list_by_subject = function(...){
  
  df = get_image_filenames_df(..., long = TRUE)
  if (is.null(df)) {
    return(NULL)
  }  
  if (nrow(df) == 0) {
    return(NULL)
  }    
  ss = split(df, df$Subject_ID)
  ss = lapply(ss, function(x){
    x$Subject_ID = NULL
    x = split(x, x$visit)
    x = lapply(x, function(r) {
      r$visit = NULL
      r$filename
    })
    x    
  })
  return(ss)
}
