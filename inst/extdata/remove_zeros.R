rm(list = ls())
library(ms.lesion)
library(extrantsr)
library(malf.templates)
library(fslr)
library(neurobase)

all.exists = function(...){
  all(file.exists(...))
}

files = list.files(pattern = ".nii.gz", recursive = TRUE)
files = files[grep("raw/", files)]
files = files[!grepl("mask", files)]
fname = files[2]

make_non_zero = function(fname) {
  min_val = fslmin(fname)
  if (min_val < 0) {
    img = fslthresh(file = fname, thresh = 0)
    writenii(img, fname)
  }
}

sapply(files, make_non_zero)