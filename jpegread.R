#
if (!require("BiocManager", quietly = TRUE))
   install.packages("BiocManager")
BiocManager::install(version = "3.14")



BiocManager::install("EBImage")
library(EBImage)


jpegread<-function(mynames, mylist, n1, n2) {
  df <- NULL

  for(i in 1:length(mylist)) {
    img1 <- readImage(mylist[i])

    countNuclei <- function(img1){
      # blur the image
      w = makeBrush(size = 11, shape = 'gaussian', sigma = 5)  # makes the blurring brush
      img_flo = filter2(img1*n1, w) # apply the blurring filter

      # apply a threshold
      nmaskt = thresh(img_flo *n2, w=10, h=10, offset=0.05)

      # the bwlabel() function 'counts' the blobs
      nucNo <- max(bwlabel(nmaskt))

    }
    # this applies the function to the image
    nucNo <- countNuclei(img1)
    df<-rbind(df,data.frame(mynames[i],nucNo))
  }

  return(df)

}
