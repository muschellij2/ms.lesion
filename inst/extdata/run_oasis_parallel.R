rm(list = ls())
library(ms.lesion)
library(neurobase)
library(fslr)
library(oasis)
library(dplyr)

files = get_image_filenames_list_by_subject(
  type = "coregistered")
isubj = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(isubj)) {
  isubj = 1
}

outdir = file.path("models")
dir.create(outdir, showWarnings = FALSE)


  print(isubj)
  fnames = files[[isubj]]
  id = names(files)[isubj]
  
  outfile = file.path(outdir, paste0(id, "_oasis_df.rda"))
  
  if (!file.exists(outfile)) {  
    t1_fname = fnames["T1"]
    t2_fname = fnames["T2"]
    flair_fname = fnames["FLAIR"]
    # pd_fname = fnames["PD"]
    maskfile = fnames["Brain_Mask"]
    lesionfile = fnames["mask"]
    
    T1 = readnii(t1_fname)
    T2 = readnii(t2_fname)
    FLAIR = readnii(flair_fname)
    # PD = readnii(pd_fname)
    PD = NULL
    GOLD_STANDARD = readnii(lesionfile)
    MASK = readnii(maskfile)
    
    
    df = oasis_train_dataframe(
      flair = FLAIR, 
      t1 = T1, t2 = T2, pd = PD, 
      gold_standard = GOLD_STANDARD,
      brain_mask = MASK, preproc = FALSE, normalize = TRUE,
      return_preproc = FALSE,
      cores = 4)
    df = df$oasis_dataframe
    save(df, file = outfile)
  }
