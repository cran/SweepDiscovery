
#'@title SweepPrediction
#' @param Traindata Dataset for training
#' @param Newdata New data for prediction
#' @import stats utils randomForest
#' @return
#' \itemize{
#'   \item Prediction: Results
#' }
#' @export
#'
#' @examples
#' \donttest{
#' library("SweepDiscovery")
#' data <- system.file("extdata", "data.csv", package = "SweepDiscovery")
#' Data<- read.csv(data)
#' pred<-SweepPrediction(Traindata=NULL,Newdata=Data)
#' }
#' @references
#' \itemize{
#'\item Pavlidis, P., Alachiotis, N. A survey of methods and tools to detect recent and strong positive selection. J of Biol Res-Thessaloniki 24, 7 (2017). https://doi.org/10.1186/s40709-017-0064-0
#' }
SweepPrediction<-function(Traindata=NULL,Newdata) {
  data<-Traindata
  if(is.null(data)){
    rf <- readRDS("inst/extdata/rf.rds")
  }else{
    train_data<-data
    rf<- randomForest(factor(class, levels = c(1,2,3), labels = c("No_Selective_Sweep", "Soft_Selective_Sweep", "Hard_Selective_Sweep"))~.,data = train_data)
  }
  Prediction <- predict(rf, newdata = Newdata)
  Prediction<-data.frame(Prediction)
  return(Prediction)

}

