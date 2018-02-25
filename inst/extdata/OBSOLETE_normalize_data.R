rm(list = ls())
library(ms.lesion)
library(extrantsr)
library(fslr)
library(neurobase)
library(pbapply)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject()
# img = check_nifti(files)
# 
# min_nonzero = function(x){
#   stopifnot(all(x >= 0))
#   min(x[x > 0])
# }
# mins = sapply(img, function(x){
#   sapply(x, min_nonzero)
# })
# 
# mins = sapply(img, function(x){
#   sapply(x, function(r){
#     mr = min(r[ r > 0])
#     r = datatyper(r / mr)
#     ur = unique(r[1:10000])
#     ur = sort(ur)
#     print(head(ur))
#   })
# })

isubj = 1
for (isubj in seq_along(files)) {
    fnames = files[[isubj]]
    id = names(files)[isubj]
    outdir = file.path("raw",
        id)
    dir.create(outdir)
    outfiles = file.path(outdir, 
        basename(fnames))
    imgs = check_nifti(fnames)
    imgs = pblapply(imgs, function(x){
      mx = min(x[ x > 0])
      x = neurobase::datatyper(x / mx)
      return(x)
    })    
    mapply(function(img, fname){
      writenii(img, fname)
    }, imgs, outfiles)
#     if (!all.exists(outfiles, maskfile)) {
#         smri_preproc(files = fnames,
#             outfiles = outfiles,
#             maskfile = maskfile)
#     }
}