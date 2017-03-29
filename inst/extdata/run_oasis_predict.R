rm(list = ls())
library(ms.lesion)
library(neurobase)
library(fslr)
library(oasis)
library(oro.nifti)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject(
  type = "coregistered")
isubj = 1

for (isubj in seq_along(files)) {
  print(isubj)
  fnames = files[[isubj]]
  id = names(files)[isubj]
  outdir = file.path("coregistered", id)
  df_outdir = file.path("oasis")
  
  t1_fname = fnames["MPRAGE"]
  t2_fname = fnames["T2"]
  flair_fname = fnames["FLAIR"]
  pd_fname = fnames["PD"]
  maskfile = fnames["Brain_Mask"]
  
  T1 = readnii(t1_fname)
  T2 = readnii(t2_fname)
  FLAIR = readnii(flair_fname)
  PD = readnii(pd_fname)
  MASK = readnii(maskfile)
  
  # Using default params
  def_outfile = file.path(outdir, 
                      paste0(id, "_Default_OASIS.nii.gz"))
  
  tr_outfile = file.path(outdir, 
                         paste0(id, "_Trained_OASIS.nii.gz"))  
  
  df_outfile = file.path(df_outdir, 
                         paste0(id, "_oasis_data.rda"))
  
  if (!all(file.exists(def_outfile, tr_outfile))) {
    
    if (!file.exists(df_outfile)) {
      L = oasis_train_dataframe(
        flair = FLAIR, 
        t1 = T1, t2 = T2, pd = PD, 
        brain_mask = MASK,
        preproc = FALSE, 
        normalize = TRUE,
        cores = 4
      )
      
      oasis_dataframe = L$oasis_dataframe
      brain_mask = L$brain_mask
      voxel_selection = L$voxel_selection
      preproc = L$preproc
      rm(list = "L")
      save(oasis_dataframe, brain_mask, 
           voxel_selection, 
           file = df_outfile)
    } else {
      load(df_outfile)
    }
    

    verbose = TRUE
    post_predict = function(predictions, brain_mask, voxel_selection) {
      predictions_nifti <- niftiarr(brain_mask, 0)
      predictions_nifti[voxel_selection == 1] <- predictions
      predictions_nifti = datatyper(predictions_nifti, 
                                    datatype = convert.datatype()$FLOAT32,
                                    bitpix = convert.bitpix()$FLOAT32
      )
      if (verbose) {
        message("Smoothing Prediction")
      }
      ##smooth the probability map
      prob_map <- fslsmooth(predictions_nifti, sigma = 1.25,
                            mask = brain_mask, retimg = TRUE,
                            smooth_mask = TRUE)
      return(prob_map)
    }
    

    
    if (!file.exists(def_outfile)) {
      predictions <- predict( oasis::oasis_model,
                              newdata = oasis_dataframe,
                              type = 'response')  
      print(sum(predictions))
      prob_map = post_predict(predictions, brain_mask, voxel_selection)
      # print(sum(prob_map))
      writenii(prob_map, filename = def_outfile)
      # map = oasis_predict(
      #   flair = FLAIR, 
      #   t1 = T1, t2 = T2, pd = PD, 
      #   brain_mask = MASK, preproc = FALSE, normalize = TRUE,
      #   model = oasis::oasis_model)
      # writenii(map$oasis_map, filename = def_outfile)
    }
  
    # Using re-trained model
  
    if (!file.exists(tr_outfile)) {
      predictions <- predict( ms.lesion::ms_model,
                              newdata = oasis_dataframe,
                              type = 'response')  
      print(sum(predictions))
      prob_map = post_predict(predictions, brain_mask, voxel_selection)
      # print(sum(prob_map))
      writenii(prob_map, filename = tr_outfile)
      # map = oasis_predict(
      #   flair = FLAIR, 
      #   t1 = T1, t2 = T2, pd = PD, 
      #   brain_mask = MASK, preproc = FALSE, normalize = TRUE,
      #   model = ms.lesion::ms_model)
      # writenii(map$oasis_map, filename = tr_outfile)
    }
  }
 } 
 