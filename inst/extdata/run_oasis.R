rm(list = ls())
library(ms.lesion)
library(neurobase)
library(fslr)
library(oasis)
library(dplyr)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject(
  type = "coregistered", 
  group = "training")
isubj = 1
oasis_dfs_train = vector(mode = "list", length = length(files))

reduce_glm_mod = function(model){
  model$y = c()
  model$model = c()
  model$residuals = c()
  model$fitted.values = c()
  model$effects = c()
  model$qr$qr = c()  
  model$linear.predictors = c()
  model$weights = c()
  model$prior.weights = c()
  model$data = c()
  attr(model$terms,".Environment") = c()
  attr(model$formula,".Environment") = c()
  model
}

run_oasis_model = oasis::nopd_oasis_model
outdir = file.path("models")
dir.create(outdir, showWarnings = FALSE)

for (isubj in seq_along(files)) {
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
  } else{
    load(outfile)
  }
  
  oasis_dfs_train[[isubj]] = df
  rm(list = "df")
}

df = bind_rows(oasis_dfs_train)
have_pd =  "PD" %in% colnames(df)

form = GoldStandard ~ FLAIR_10 *FLAIR  +
  FLAIR_20*FLAIR + 
  PD_10 *PD + PD_20 * PD +
  T2_10 *T2 + T2_20 * T2 + T1_10 *T1 +
  T1_20 *T1
if (!all(have_pd)) {
  form = update.formula(form, . ~ . - PD_10 * PD - PD_20 * PD )
}

model <- glm(formula = form,
             data = df,
             family = binomial)
model = reduce_glm_mod(model)

save(model, file = "models/ms_model.rda", compression_level = 9)

ms_model = model
save(ms_model, file = "../../data/ms_model.rda", 
     compression_level = 9)

rm(df); gc()
test_files = get_image_filenames_list_by_subject(
  type = "coregistered", 
  group = "test")

for (isubj in seq_along(test_files)) {
  print(isubj)
  fnames = files[[isubj]]
  id = names(files)[isubj]
  outdir = file.path("coregistered", id)
  
  outfile = file.path(outdir, paste0(id, "_oasis_df.rda"))
  
  def_outfile = file.path(outdir, paste0(id, "_Default_OASIS.nii.gz"))
  tr_outfile = file.path(outdir, paste0(id, "_Trained_OASIS.nii.gz"))
  
  if (!all(file.exists(c(def_outfile, tr_outfile)))) {
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
    
    
    # Using default params
    
    
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

