rm(list = ls())
library(ms.lesion)
library(neurobase)
library(fslr)
library(oasis)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject(
  type = "coregistered", 
  group = "training")
isubj = 1
oasis_dfs_train = vector(mode = "list", length = length(files))

for (isubj in seq_along(files)) {
  print(isubj)
  fnames = files[[isubj]]
  id = names(files)[isubj]
  outdir = file.path("models")
  
  t1_fname = fnames["MPRAGE"]
  t2_fname = fnames["T2"]
  flair_fname = fnames["FLAIR"]
  pd_fname = fnames["PD"]
  maskfile = fnames["Brain_Mask"]
  lesionfile = fnames["mask2"]
  
  T1 = readnii(t1_fname)
  T2 = readnii(t2_fname)
  FLAIR = readnii(flair_fname)
  PD = readnii(pd_fname)
  GOLD_STANDARD = readnii(lesionfile)
  MASK = readnii(maskfile)
  
  outfile = file.path(outdir, paste0(id, "_oasis_df.rda"))
  
  if (!file.exists(outfile)) {
    df = oasis_train_dataframe(
      flair = FLAIR, 
      t1 = T1, t2 = T2, pd = PD, 
      gold_standard = GOLD_STANDARD,
      brain_mask = MASK, preproc = FALSE, normalize = TRUE,
      return_preproc = FALSE)
    df = df$oasis_dataframe
    save(df, file = outfile)
  } else{
    load(outfile)
  }
  
  oasis_dfs_train[[isubj]] = df
  rm(list = "df")
}

model = do.call("oasis_training", oasis_dfs_train)
save(model, file = "models/oasis_model.rda")

ms_model = model
save(ms_model, file = "../../data/ms_model.rda", 
     compression_level = 9)



