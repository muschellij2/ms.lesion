rm(list = ls())
library(ms.lesion)
library(extrantsr)
library(EveTemplate)
library(neurobase)

all.exists = function(...){
  all(file.exists(...))
}

files = list.files(
  pattern = ".nii",
  path = "training", 
  recursive = TRUE, 
  full.names = TRUE)

df = data.frame(file = files,
                id = basename(dirname(files)),
                stringsAsFactors = FALSE)
df$type = nii.stub(df$file, bn = TRUE)
df$type = gsub("training\\d\\d_\\d\\d_(.*)", "\\1", df$type  )
df = reshape(df, idvar = "id", timevar = "type", direction = "wide")
colnames(df) = sub("file[.]", "", colnames(df))

isubj = 1


for (isubj in seq(nrow(df))) {
    print(isubj)
    irow = df[ isubj, ]
    id = irow$id
    pp = irow$mprage_pp
    mpr = irow$mprage
    mask1 = irow$mask1
    mask2 = irow$mask2
    fnames = c(mask1, mask2)
    
    outdir = file.path("coregistered",
                       id)
    if (!dir.exists(outdir)) {
      dir.create(outdir)
    }
    outfiles = file.path(outdir, 
                         basename(fnames))

    if (!all.exists(outfiles)) {
        reg = registration(
            filename =  pp,
            template.file = mpr,
            interpolator = "NearestNeighbor",
            typeofTransform = "Rigid",
            other.files = fnames,
            other.outfiles = outfiles)
    }
}

