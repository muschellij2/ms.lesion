#' @title Get Image Filenames in a data.frame
#'
#' @description Return a data.frame of filenames for the images
#' @param group group of IDs to gather.  If both \code{c("training", "test")},
#' all IDs are returned
#' @param modalities vector of image modalities within
#' \code{c("FLAIR", "T2", "T2", "PD")} to return
#' @param long if \code{TRUE}, each row is a subject, visit, modality pair
#' 
#' @return Data.frame of filenames
#' 
#' @examples
#' get_image_filenames_df()
#' @importFrom stats reshape
#' 
#' @export
get_image_filenames_df = function(
  group = c("training", "test"),
  modalities = all_modalities(), 
  long = TRUE){
  
  ids = get_ids(group = group)
  modalities = tolower(modalities)
  ##########################################
  
  ##########################################
  # Make the data.frames
  ##########################################
  modalities = unique(modalities)
  visits = sprintf("%02.0f", 1)
  v_ids = c(outer(ids, visits, paste, sep = "_"))
  fnames = c(outer(v_ids, modalities, paste, sep = "_"))
  
  fnames = paste0(fnames, ".nii.gz")
  df = data.frame(filename = fnames, 
                  stringsAsFactors = FALSE)
  ss = strsplit(df$filename, "_")
  df$id = sapply(ss, `[`, 1)
  # df$visit = as.numeric(sapply(ss, `[`, 2))
  
  nii.stub = function(x){
    nx = names(x)
    x = path.expand(x)
    stub = gsub("\\.gz$", "", x)
    stub = gsub("\\.nii$", "", stub)    
    names(stub) = nx
    return(stub)    
  }
  df$modality = nii.stub(sapply(ss, `[`, 3))
  df$filename = file.path(df$id, df$filename)
  
  # df$id = NULL
  ##########################################
  # Set the package names
  ##########################################
  mod = modality_df()
  df$modality = toupper(df$modality)
  df = merge(df, mod, sort = FALSE, by = "modality", all.x = TRUE)

  ########################################
  # Find those not installed and warn
  ########################################  
  df$filename = system.file( "extdata", df$filename, package = "ms.lesion")

  if (!long) {
    df = reshape(df, idvar = c("id"), 
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
  df$id = NULL
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
  rn = rownames(df) = df$id
  df$id = df$visit = NULL
  df = lapply(df, function(x){
    names(x) = rn
    x
  })
  return(df)
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
  ss = split(df, df$id)
  ss = lapply(ss, function(r){
    mod_names = r$modality
    x = r$filename
    names(x) = mod_names
    return(x)
  })
  return(ss)
}
