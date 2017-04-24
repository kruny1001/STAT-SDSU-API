### Class Name: MARSClass
### data: Sorted Table is required
## TODO:
## MARSClass
require(earth)

MARSClass <- setRefClass("MARSClass",
  contains = c("DataClass", "GainClass"),
  fields = list(
    train = "data.table",
    test = "data.table",
    gain = "data.table",
    gainDecile = "data.table",
    predTable = "data.table",
    model = "ANY",
    decile = "list",
    totalRecord = "numeric"
  ),
  methods = list(
      initialize = function(train, test){
        .self$train <- train
        .self$test <- test
      },
      createModel = function(formula, deg=1, target){
        # Model
        earthModel=earth(
          formula,
          trace=1,
          data=.self$train,
          glm=list(family=binomial),degree=deg)
        # save model to member variable
        .self$model = earthModel
        pred = predict(.self$model, type='response', newdata=.self$test)
        .self$predTable <- .self$test
        .self$predTable[, prob:=pred]
        .self$predTable = .self$predTable[order(-rank(prob))]

        # Create Gains Table
        .self$gain = createTableG(.self$predTable)
        # Create Gains Table with decile
        .self$gainDecile = createGains(.self$gain)
      },
      # Getter
      getModel = function(){
        return(.self$model)
      },
      getDecile = function(){
        return(.self$decile)
      },
      # return train table with probability
      getPredTable = function(){
        return(.self$predTable)
      },
      # return gain table with all data
      getGainTable = function(){
        return(.self$gain)
      },
      # return gain table with decile
      getGainDecileTable = function(){
        return(.self$gainDecile)
      }
  )
)
