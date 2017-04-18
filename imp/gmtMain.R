
readGMT <- function (fileName){
  x <- scan(fileName, what="", sep="\n")
  x <- strsplit(x, "\t")
  # Extract the first vector element and set it as the list element name
  names(x) <- sapply(x, `[[`, 1)
  x <- lapply(x, `[`, -c(1,2)) # 2nd element is comment, ignored
  x = x[which(sapply(x,length) > 1)]  # gene sets smaller than 1 is ignored!!!
  return(x)
}
