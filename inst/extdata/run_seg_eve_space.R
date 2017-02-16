rm(list = ls())
library(ms.lesion)
library(extrantsr)
library(EveTemplate)
library(neurobase)

all.exists = function(...){
  all(file.exists(...))
}

eve_brain = getEvePath("Brain")
eve_mask = getEvePath("Brain_Mask")

files = get_image_filenames_list_by_subject(
  type = "template")

isubj = 1
for (isubj in seq_along(files)) {
  print(isubj)
  fnames = files[[isubj]]
  fnames = fnames["MPRAGE"]
  id = names(files)[isubj]
  outdir = file.path("template",
                     id)
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }
  tissues = c("Tissue_Classes")
  outfile = file.path(outdir, 
                       paste0(
                         nii.stub(fnames, bn = TRUE), 
                         "_", tissues,
                         ".nii.gz"))
  
  if (!all.exists(outfile)) {
    reg = otropos(a = fnames, x = eve_mask)
    writenii(reg$segmentation, outfile)
  }
}

