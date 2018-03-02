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

isubj = 4
isubj = as.numeric(Sys.getenv("SGE_TASK_ID"))

# for (isubj in seq_along(files)) {
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
  
  tissues = c("Tissue_Classes")
  tissue_file = file.path(outdir, 
                      paste0(
                        nii.stub(fnames, bn = TRUE), 
                        "_", tissues,
                        ".nii.gz"))
  
  outfile = file.path(outdir, 
                      paste0(
                        nii.stub(fnames, bn = TRUE), 
                        "_cortthick",
                        ".nii.gz"))
  
  
  tprobs = c("GM", "WM")
  tfiles = file.path(outdir, 
                     paste0(
                       nii.stub(fnames, bn = TRUE), 
                       "_", tprobs,
                       ".nii.gz"))  
  names(tfiles) = tprobs
  
  if (!all.exists(outfile)) {
    res = cort_thickness(seg = tissue_file, 
                   gray = tfiles["GM"],
                   white = tfiles["WM"],
                   v = 1)
    writenii(res, outfile)
  }
  
# }

