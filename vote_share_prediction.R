# 683-midterm

##
## Code for two party vote share prediction
## POL 683
## Georgia Pfeiffer
##


#
# Set up
#

#setwd
setwd('~/Dropbox/PhD/Classes/year_3/fall/pol683/predicting_elections/Fair_Model/Data/')

# load libraries
library(lattice)
library(arm)
library(car)
library(ggplot2)

#import data
data <- read.csv('my_data.csv')


#
# Model
#

# seperate 2016 data to use in prediction
predict_2016 <- data[which(data$t == 2016),]

# seperate 1948-2012 to use to fit the model
prev_elections <- data[which(data$t < 2016),]

# create model
model <- lm(VP ~ G + I + DPER + DUR + P + Z + WAR, data = prev_elections)
summary(model)

# Durbin Watson test for serial correlation
durbinWatsonTest(model)

# predict the Democratic vote share
predict(model, predict_2016)


# 
# Graphs
#

# Histogram of democratic vote share
pdf(file = "vote_share_histogram.pdf")
histogram(data$VP, main = "Democratic Vote Share", xlab = "Vote Share",
          ylab = "Percent of Total", col = "cyan")
dev.off()


# function for coefficient graph from here...
# https://gist.github.com/dsparks/818976
CoefficientPlot <- function(models, alpha = 0.05, modelnames = ""){
  # models must be a list()
  
  Multiplier <- qnorm(1 - alpha / 2)
  CoefficientTables <- lapply(models, function(x){summary(x)$coef})
  TableRows <- unlist(lapply(CoefficientTables, nrow))
  
  if(modelnames[1] == ""){
    ModelNameLabels <- rep(paste("Model", 1:length(TableRows)), TableRows)
  } else {
    ModelNameLabels <- rep(modelnames, TableRows)
  }
  
  MatrixofModels <- cbind(do.call(rbind, CoefficientTables), ModelNameLabels)
  MatrixofModels <- data.frame(cbind(rownames(MatrixofModels), MatrixofModels))
  colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName")
  MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
  MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})
  
  OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                      ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                      ylab = NULL, xlab = NULL)
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw()
  return(OutputPlot)
}

# coefficient graph
pdf(file = "coef.pdf")
CoefficientPlot(list(model), alpha = 0.05, modelnames = "Vote Share Model")
dev.off()

# trajectory graph
predictions <- predict(model, data)
pdf(file = "trajectory.pdf")
plot (c(1952,2016),c(0,100),type="n", # sets the x and y axes scales
      
      xlab="Year",ylab="Percent of Two Party Vote Share") # adds titles to the axes

lines(prev_elections$t, prev_elections$VP,col="blue",lwd=2.5) # adds a line for defense expenditures

lines(data$t, predictions,col="cyan",lwd=2.5) # adds a line for health expenditures
lines(x = c(1952,2016), y = rep(50, 2), col = "black")

legend(x = "topright",  # places a legend at the appropriate place 
       c("Actual Dem Vote Share","Predicted Dem Vote Share"), # puts text in the legend
       
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5,2.5),col=c("blue","cyan")) # gives the legend lines the correct color and width
dev.off()
