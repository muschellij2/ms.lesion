rm(list = ls())
library(ms.lesion)
library(neurobase)
library(fslr)
library(oasis)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject(type="coregistered")
isubj = 1
oasis_dfs = list()
for (isubj in seq_along(files)) {
  print(isubj)
  fnames = files[[isubj]]
  id = names(files)[isubj]
  outdir = file.path("coregistered",
                     id)
  dir.create(outdir)
  outfiles = file.path(outdir, 
                       basename(fnames))
  t1_fname = fnames["MPRAGE"]
  t2_fname = fnames["T2"]
  flair_fname = fnames["FLAIR"]
  pd_fname = fnames["PD"]
  maskfile = file.path(outdir,
                       "brain_mask.nii.gz")
  lesionfile = file.path(outdir,
                       "mask2.nii.gz")
  T1 = readnii(t1_fname)
  T2 = readnii(t2_fname)
  FLAIR = readnii(flair_fname)
  PD = readnii(pd_fname)
  if(file.exists(maskfile)){
    GOLD_STANDARD = readnii(lesionfile)
    MASK = readnii(maskfile)

    oasis_dfs = list(oasis_dfs, oasis_train_dataframe(flair = FLAIR, 
      t1 = T1, t2 = T2, pd = PD, gold_standard = GOLD_STANDARD,
      brain_mask = MASK, preproc = FALSE, normalize = TRUE,
      return_preproc = FALSE))
  }
}

  all_dfs = paste(unlist(lapply(1:5, function(x) paste0("oasis_dfs[[",x,"]]"))), collapse=",")
  oasis_model = oasis_training(eval(parse(text=all_dfs)))
  save.image()
  
  if (!all.exists(outfiles, maskfile)) {
    preprocess_mri_within(
      files = fnames,
      outfiles = outfiles,
      correction = "N4",
      maskfile = maskfile,
      correct_after_mask = TRUE)
  }
  # for (ifile in outfiles) {
  #   print(ifile)
  #   bias_correct(file = ifile, correction = "N4", outfile = ifile)
  # }
}
