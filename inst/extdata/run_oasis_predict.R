rm(list = ls())
library(ms.lesion)
library(neurobase)
library(fslr)
library(oasis)
library(oro.nifti)
library(here)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject(
  type = "coregistered")
isubj = 1

run_oasis_model = oasis::nopd_oasis_model
run_ms_model = ms.lesion::ms_model
outdir = file.path(here::here(), "inst", "extdata", "models")

for (isubj in seq_along(files)) {
  print(isubj)
  fnames = files[[isubj]]
  id = names(files)[isubj]
  id_outdir = file.path(here::here(), "inst", "extdata",
                        "coregistered", id)
  
  
  # Using default params
  def_outfile = file.path(id_outdir, 
                      paste0(id, "_Default_OASIS.nii.gz"))
  
  tr_outfile = file.path(id_outdir, 
                         paste0(id, "_Trained_OASIS.nii.gz"))  
  
  outfile = file.path(outdir, paste0(id, "_oasis_df.rda"))
  
  if (!all(file.exists(def_outfile, tr_outfile))) {
    
    flair_fname = fnames["FLAIR"]
    maskfile = fnames["Brain_Mask"]
    FLAIR = readnii(flair_fname)
    
    if (!file.exists(outfile)) {  
      t1_fname = fnames["T1"]
      t2_fname = fnames["T2"]
      # pd_fname = fnames["PD"]
      lesionfile = fnames["mask"]
      
      T1 = readnii(t1_fname)
      T2 = readnii(t2_fname)
      # PD = readnii(pd_fname)
      brain_mask = readnii(maskfile)
      
      PD = NULL
      GOLD_STANDARD = readnii(lesionfile)
      
      
      df = oasis_train_dataframe(
        flair = FLAIR, 
        t1 = T1, t2 = T2, pd = PD, 
        gold_standard = GOLD_STANDARD,
        brain_mask = brain_mask, 
        preproc = FALSE, normalize = TRUE,
        return_preproc = FALSE,
        cores = 4)
      voxel_selection = df$voxel_selection
      ero_brain_mask = df$brain_mask
      df = df$oasis_dataframe
      save(df, file = outfile)
    } else{
      load(outfile)
      L = voxel_selection_with_erosion(
        flair = FLAIR,
        brain_mask = maskfile)
      ero_brain_mask = L$brain_mask
      voxel_selection = L$voxel_selection
    }
    
    
    
    
    if (!file.exists(def_outfile)) {
      
      map = oasis_predict(
        brain_mask = ero_brain_mask, 
        oasis_dataframe = df, 
        voxel_selection = voxel_selection,
        model = run_oasis_model)
      map = map$oasis_map
      writenii(map, filename = def_outfile)
    }
    
    # Using re-trained model
    if (!file.exists(tr_outfile)) {
      map = oasis_predict(
        brain_mask = ero_brain_mask, 
        oasis_dataframe = df, 
        voxel_selection = voxel_selection,
        model = ms_model)
      map = map$oasis_map
      writenii(map, filename = tr_outfile)
    }
    
  }
 } 
 