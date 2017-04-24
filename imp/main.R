# kevin.R

# Include your code
#source("./partial.R")
library(jsonlite)

source("./gmtMain.R")

source("./data.R")
source("./gain.R")
source("./mars.R")


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
MARSRef1 <- MARSClass$new(train, test)
MARSRef1$createModel(form1, deg=1)
model1 = MARSRef1$getModel()
tablePred1 = MARSRef1$getPredTable()
gainTable1 = MARSRef1$getGainTable()
gainDecileTable1 = MARSRef1$getGainDecileTable()
GainFunction = GainClass$new()

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


###################################

# list retention data.table
#* @get /train
gmtFiles <- function(){
  firstTest <- DataClass$new(targetCol="Bad", part=.8)
  firstTest$getTest()
}

# Create lift graph
#* @png
#* @get /plot
normalMean <- function(samples=10){
  p <- GainFunction$createCumLiftPlot(gainDecileTable1)
  print(p)
}

#* @get /roc
rocPoints <- function(){
  gainTable1
}

#* @get /lift
liftPoints <- function() {
  gainDecileTable1
}

#* @get /refresh/<id:int>
refresh <- function(id){
  if(id == 1)
    targetForm = form1
  else if(id ==2)
    targetForm = form2
  else
    targetForm = form3

  firstTest <- DataClass$new(targetCol="Bad", part=.6)
  train <- firstTest$getTrain()
  test <- firstTest$getTest()
  MARSRef1$createModel(targetForm, deg=1)
  model1 <<- MARSRef1$getModel()
  tablePred1 <<- MARSRef1$getPredTable()
  gainTable1 <<- MARSRef1$getGainTable()
  gainDecileTable1 <<- MARSRef1$getGainDecileTable()
  gainDecileTable1
}
