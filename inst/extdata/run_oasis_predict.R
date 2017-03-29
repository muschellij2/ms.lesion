rm(list = ls())
library(ms.lesion)
library(neurobase)
library(fslr)
library(oasis)

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
  outfile = file.path(outdir, paste0(id, "_Default_OASIS.nii.gz"))
  if (!file.exists(outfile)) {
    map = oasis_predict(
      flair = FLAIR, 
      t1 = T1, t2 = T2, pd = PD, 
      brain_mask = MASK, preproc = FALSE, normalize = TRUE,
      model = oasis::oasis_model)
    writenii(map$oasis_map, filename = outfile)
  }

  # Using re-trained model
  outfile = file.path(outdir, paste0(id, "_Trained_OASIS.nii.gz"))
  if (!file.exists(outfile)) {
    map = oasis_predict(
      flair = FLAIR, 
      t1 = T1, t2 = T2, pd = PD, 
      brain_mask = MASK, preproc = FALSE, normalize = TRUE,
      model = ms.lesion::ms_model)
    writenii(map$oasis_map, filename = outfile)
  }
 } 
 