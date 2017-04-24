# kevin.R

# Include your code
#source("./partial.R")
library(jsonlite)
source("./re.R")
source("./gmtMain.R")

source("./data.R")
source("./mars.R")
source("./gain.R")

table = loadData()

# user can query to get all file names
files <- list.files("gmt", pattern = "*gmt")

geneT <- append(
  readGMT(paste("gmt/",files[1],sep="")),
  readGMT(paste("gmt/",files[2],sep=""))
  )


####DEMO#######
dataset <- DataClass$new(targetCol="Bad", part=.8)
train = dataset$getTrain()
test = dataset$getTest()

form1 = as.formula(Bad ~ Quarterly_Fico_Score)
form2 = as.formula(Bad ~Behavior_Score)
form3 = as.formula(Bad ~Quarterly_Fico_Score + Behavior_Score)

MARSRef1$createModel(form1, deg=1)
model1 = MARSRef1$getModel()
tablePred1 = MARSRef1$getPredTable()
gainTable1 = MARSRef1$getGainTable()
gainDecileTable1 = MARSRef1$getGainDecileTable()
###############

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

#Read Data from firebase
#* @get /fb/data
readFirebase <- function(){
  # Example
  readData <- GET("https://bcloud.firebaseio.com/fromR/names.json")
  dd <- content(readData, "text")
  toJSON(fromJSON(dd))
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
