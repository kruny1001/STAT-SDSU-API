
# Concept

### Write a data to Firebase 
### Grep a Data by name 
### Grep Data by range 
### Grep Data

source("./gmtMain.R")

files <- list.files("gmt", pattern = "*gmt")

geneT <- append(
  readGMT(paste("gmt/",files[1],sep="")),
  readGMT(paste("gmt/",files[2],sep=""))
)


updateFirebase()
updateFirebase <- function(){
  # read File 
  
  # loop for Each 
  # connect Firebase
  # write a record 
  library(httr)
  #curl -X PUT -d '{ "first": "Jack", "last": "Sparrow" }' \
  #'https://bcloud.firebaseio.com/fromR/names.json'
  r <- PUT(url='https://bcloud.firebaseio.com/fromR/names.json', 
           body=geneT[1:12], 
           encode="json")
  
}

readFirebase <- function(){
  # Example 
  readData <- GET("https://bcloud.firebaseio.com/fromR/names.json")
  dd <- content(readData, "text")
  return(fromJSON(dd))
}

secondData = readFirebase()
length(secondData)
fromJSON(toJSON(secondData[3]))
