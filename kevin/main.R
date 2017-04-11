# kevin.R

# Include your code
#source("./partial.R")
source("./re.R")

table = loadData()

#* @get /mean
normalMean <- function(samples=10){
  #data <- rnorm(samples)*100
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
