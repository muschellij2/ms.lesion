rm(list = ls())
library(ms.lesion)
library(extrantsr)
library(EveTemplate)
library(neurobase)

all.exists = function(...){
  all(file.exists(...))
}

eve_brain = getEvePath("Brain")

files = get_image_filenames_list_by_subject(
    type = "coregistered")

isubj = 1
typeofTransform = "Affine"
out_fol = "template"
if (typeofTransform == "Affine") {
  out_fol = "affine"
}

for (isubj in seq_along(files)) {
    print(isubj)
    fnames = files[[isubj]]
    id = names(files)[isubj]
    outdir = file.path(out_fol,
        id)
    dir.create(outdir, recursive = TRUE)
    outfiles = file.path(outdir, 
        basename(fnames))
    if (!all.exists(outfiles)) {
        reg = registration(
            filename = fnames["T1"],
            template.file = eve_brain,
            interpolator = "Linear",
            typeofTransform = typeofTransform,
            other.files = fnames,
            other.outfiles = outfiles)
    }
    o = check_nifti(outfiles)
    o = lapply(o, function(x){
        x[x < 0] = 0
        x
    })
    mapply(function(x, fname){
        writenii(x, fname)
    }, o, outfiles)
}

