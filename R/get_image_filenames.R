#' @title Get Image Filenames in a \code{data.frame}
#'
#' @description Return a data.frame of filenames for the images
#' @param group group of IDs to gather.  If both 
#' \code{c("training", "test")},
#' all IDs are returned
#' @param modalities vector of image modalities within
#' \code{c("FLAIR", "T2", "T2", "PD")} to return
#' @param type type of data, either \code{"raw"}, \code{"coregistered"}
#' \code{"template"} (non-linear to Eve), 
#' or \code{"affine"} (affine to Eve)
#' @param derived Get the derived images (tissue classes/brain mask)
#' 
#' @param long if \code{TRUE}, each row is a subject, 
#' visit, modality pair
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
  type = c("raw", "coregistered", "template", "affine"),
  derived = TRUE,
  long = TRUE){
  
  ids = get_ids(group = group)
  modalities = tolower(modalities)
  type = match.arg(type)
  type = type[1]
  
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
  df$id  = basename(df$id)
  
  type_df = expand.grid(id = unique(df$id),
                        type = type, stringsAsFactors = FALSE)
  df = merge(df, type_df, all = TRUE)
  
  # df$visit = as.numeric(sapply(ss, `[`, 2))
  
  nii.stub = function(x){
    nx = names(x)
    x = path.expand(x)
    stub = gsub("\\.gz$", "", x)
    stub = gsub("\\.nii$", "", stub)    
    names(stub) = nx
    return(stub)    
  }
  ss = strsplit(df$filename, "_")
  df$modality = nii.stub(sapply(ss, `[`, 3))
  df$filename = file.path(df$type, df$id, df$filename)
  
  # df$id = NULL
  ##########################################
  # Set the package names
  ##########################################
  mod = modality_df()
  df$modality = toupper(df$modality)
  df = merge(df, mod, sort = FALSE, by = "modality", all.x = TRUE)
  
  if (type %in% c("coregistered") && derived) {
    mask_df = data.frame(
      modality = "Brain_Mask",
      id = ids, 
      filename = file.path(type, ids, "brain_mask.nii.gz"),
      type = type,
      stringsAsFactors = FALSE)
    df = merge(df, mask_df, all = TRUE)
  }
  
  if (type %in% c("template", "coregistered") && derived) {
    mask_df = data.frame(
      modality = "Tissue_Classes",
      id = ids, 
      filename = file.path(type, ids, 
                           paste0(ids, "_01_mprage_", 
                                  "Tissue_Classes.nii.gz")),
      type = type,
      stringsAsFactors = FALSE)
    df = merge(df, mask_df, all = TRUE)
  }
  
  if (type %in% c("coregistered") && derived) {
    mask_df = data.frame(
      modality = "FAST",
      id = ids, 
      filename = file.path(type, ids, 
                           paste0(ids, "_01_mprage_", 
                                  "FAST.nii.gz")),
      type = type,
      stringsAsFactors = FALSE)
    df = merge(df, mask_df, all = TRUE)
  }  
  
  
  if (type %in% c("coregistered") && derived) {
    mask_df = data.frame(
      modality = c("mask1"),
      id = ids, 
      filename = file.path(type, ids, 
                           paste0(ids, "_01_mask", 1, 
                                  ".nii.gz")),
      type = type,
      stringsAsFactors = FALSE)
    df = merge(df, mask_df, all = TRUE)
    
    mask_df = data.frame(
      modality = c("mask2"),
      id = ids, 
      filename = file.path(type, ids, 
                           paste0(ids, "_01_mask", 2, 
                                  ".nii.gz")),
      type = type,
      stringsAsFactors = FALSE)
    df = merge(df, mask_df, all = TRUE)    
  }
  
  if (type %in% c("coregistered") && derived) {
    mask_df = data.frame(
      modality = "Default_OASIS",
      id = ids, 
      filename = file.path(type, ids, 
                           paste0(ids, "_Default_OASIS.nii.gz")),
      type = type,
      stringsAsFactors = FALSE)
    df = merge(df, mask_df, all = TRUE)
    
    mask_df = data.frame(
      modality = "Trained_OASIS",
      id = ids, 
      filename = file.path(type, ids, 
                           paste0(ids, "_Trained_OASIS.nii.gz")),
      type = type,
      stringsAsFactors = FALSE)
    df = merge(df, mask_df, all = TRUE)    
  }  
  
  ########################################
  # Find those not installed and warn
  ########################################  
  # df$filename = system.file( "extdata", df$filename,
                             # package = "ms.lesion")
  df$filename = sapply(df$filename, function(x) {
    system.file( "extdata", x, package = "ms.lesion")
  })
  
  df$modality = factor(df$modality,
                       levels = c("MPRAGE", "T2", "FLAIR", 
                                  "PD", "Brain_Mask",
                                  "Tissue_Classes",
                                  "FAST", "mask1", "mask2",
                                  "Default_OASIS", "Trained_OASIS"))
  df = df[ order(df$id, df$modality), ]
  df$modality = as.character(df$modality)
  df$type = NULL
  
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
