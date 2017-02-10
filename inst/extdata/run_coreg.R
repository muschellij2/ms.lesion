rm(list= ls())
library(ms.lesion)
library(extrantsr)

all.exists = function(...){
  all(file.exists(...))
}

files = get_image_filenames_list_by_subject()
isubj = 1
for (isubj in seq_along(files)) {
    fnames = files[[isubj]]
    id = names(files)[isubj]
    outdir = file.path("coregistered",
        id)
    dir.create(outdir)
    outfiles = file.path(outdir, 
        basename(fnames))
    maskfile = file.path(outdir,
        "brain_mask.nii.gz")
    if (!all.exists(outfiles, maskfile)) {
        smri_preproc(files = fnames,
            outfiles = outfiles,
            maskfile = maskfile)
    }
}