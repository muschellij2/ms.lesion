rm(list= ls())
library(ms.lesion)
library(extrantsr)
library(EveTemplate)

all.exists = function(...){
  all(file.exists(...))
}

eve_brain = getEvePath("Brain")

files = get_image_filenames_list_by_subject(
    type = "coregistered")

isubj = 1
for (isubj in seq_along(files)) {
    print(isubj)
    fnames = files[[isubj]]
    id = names(files)[isubj]
    outdir = file.path("template",
        id)
    dir.create(outdir)
    outfiles = file.path(outdir, 
        basename(fnames))
    if (!all.exists(outfiles)) {
        reg = registration(
            filename = fnames["MPRAGE"],
            template.file = eve_brain,
            interpolator = "Linear",
            typeofTransform = "SyN",
            other.files = fnames,
            other.outfiles = outfiles)
    }
}

