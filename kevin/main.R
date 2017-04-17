# kevin.R

# Include your code
#source("./partial.R")
source("./re.R")
source("./gmtMain.R")

table = loadData()

# user can query to get all file names
files <- list.files("gmt", pattern = "*gmt")

geneT <- append(
  readGMT(paste("gmt/",files[1],sep="")),
  readGMT(paste("gmt/",files[2],sep=""))
  )
typeof(geneT)

geneT

# List GMT data files
#* @get /gmtFiles
gmtFiles <- function(){
  files <- list.files("gmt", pattern = "*gmt")
}


# Extract List ID from start to limit
#* @get /gmtColNames/<start:int>/<limit:int>
gmtSumm <- function(start=1, limit=1){
  targetName <- names(geneT)[start:limit]
  list(names= targetName,
       idx = start:limit,
       numCol=length(targetName)
  )
}

#Extract Target List by ID
# /gmtColData/1
#* @get /gmtColData/<id:int>
gmtData <- function(id){
  targetData <- geneT[[id]]
  list(
    id = id,
    name = trimws(names(geneT)[id]), 
    data= targetData
  )
}


###################################3

#* @get /mean
normalMean <- function(samples=10){
  print(gains())
  mean(gains())
}

#* @png
#* @get /hello
normalMean <- function(samples=10){
  p <- scotterPlot(table)
  print(p)
}

#* @get /gains
gains <- function(){
  createTableG(table)
}

#* @post /sum
addTwo <- function(a, b){
  as.numeric(a) + as.numeric(b) + 20
  gains()
}
