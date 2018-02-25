rm(list = ls())
library(ms.lesion)
library(extrantsr)
library(malf.templates)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject()
isubj = 1

for (isubj in seq_along(files)) {
  print(isubj)
  fnames = files[[isubj]]
  fnames = fnames[c("FLAIR", "T1", "T2")]
  id = names(files)[isubj]
  outdir = file.path("coregistered",
                     id)
  dir.create(outdir, recursive = TRUE)
  outfiles = file.path(outdir, 
                       basename(fnames))
  t1_fname = fnames["FLAIR"]
  timgs = mass_images(n_templates = 5)
  maskfile = file.path(outdir,
                       "brain_mask.nii.gz")
  

  if (!file.exists(maskfile)) {
    t1_n4 = bias_correct(t1_fname, 
                         correction = "N4",
                         outfile = tempfile(fileext = ".nii.gz"),
                         retimg = FALSE)
    ss = malf(infile = t1_n4, 
              template.images = timgs$images, 
              template.structs = timgs$masks,
              keep_images = FALSE,
              outfile = maskfile
    ) 
  }
  
  
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
