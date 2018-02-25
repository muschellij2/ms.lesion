rm(list = ls())
library(ms.lesion)
library(extrantsr)
library(EveTemplate)
library(neurobase)
library(fslr)
library(here)

all.exists = function(...){
  all(file.exists(...))
}

# type = "template"
type = "coregistered"

files = get_image_filenames_list_by_subject(
  type = type,
  derived = FALSE)

isubj = 1
for (isubj in seq_along(files)) {
  print(isubj)
  fnames = files[[isubj]]
  fnames = fnames["T1"]
  id = names(files)[isubj]
  outdir = file.path(
    here::here(), "inst", "extdata", 
    type,
    id)
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }
  maskfile = file.path(outdir, "brain_mask.nii.gz")  
  eve_mask = getEvePath("Brain_Mask")
  mask_fname = switch(
    type, 
    template = eve_mask,
    coregistered = maskfile)
  
  tissues = c("Tissue_Classes")
  outfile = file.path(outdir, 
                       paste0(
                         nii.stub(fnames, bn = TRUE), 
                         "_", tissues,
                         ".nii.gz"))
  
  if (!all.exists(outfile)) {
    t1 = readnii(fnames)
    rb = robust_window(t1)
    reg = otropos(a = rb, x = mask_fname)
    writenii(reg$segmentation, outfile)
  }
  
  tissues = c("FAST")
  outfile = file.path(outdir, 
                      paste0(
                        nii.stub(fnames, bn = TRUE), 
                        "_", tissues,
                        ".nii.gz"))
  
  if (!all.exists(outfile)) {
    t1 = readnii(fnames)
    rb = robust_window(t1)    
    reg = fast(file = rb, 
               retimg = TRUE,
               out_type = "seg",
               opts = "--nobias")
    writenii(nim = reg, filename = outfile)
  }  
}

