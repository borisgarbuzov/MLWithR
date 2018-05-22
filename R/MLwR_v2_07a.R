x <- rbind(matrix(rnorm(100, mean = 1, sd = 0.1), , 2),
           matrix(rnorm(100, mean = 3, sd = 0.5), , 2))
x
plot(x, pch = 19)
y <- matrix(c(rep(1, 50),
              rep(-1, 50)))

svp <- ksvm(x
            , y
            , C = 1
            , type = "C-svc" # C classification
            # , type = "nu-svc" # nu classification
            # , type = "C-bsvc" # bound-constraint svm classification
            # , type = "spoc-svc" # Crammer, Singer native multi-class
            # , type = "kbb-svc" # Weston, Watkins native multi-class
            # , type = "one-svc # novelty detection
            # , type = "eps-svr" # epsilon regression
            # , type = "nu-svr" # nu regression
            # , type = "eps-bsvr" # bound-constraint svm regression

            , kernel = "rbfdot"
            # , kernel = "tanhdot"
            # , kernel = "anovadot"
            # , kernel = "rbfdot" # Radial Basis kernel "Gaussian"
            # , kernel = "polydot" # Polynomial kernel
            # , kernel = "vanilladot" # Linear kernel
            # , kernel = "tanhdot" # Hyperbolic tangent kernel
            # , kernel = "laplacedot" # Laplacian kernel
            # , kernel = "besseldot" # Bessel kernel
            )

plot(svp, data = x,
     xlim = c(0, 4),
     ylim = c(0, 4))

names1 <- names(attributes(svp))

sv1 <- x[alphaindex(svp)[[1]],]
plot(sv1,
     pch = 19,
     main = "SV")

SVindex(svp)
alphaindex(svp)[1]


sum(y == predict(svp, x))
# 100% accuracy

# The following code was supposed to show
# both x, y, b and w vectors, but it fails to do that
# for a n-D envronment in a 2-D canvas. Anyway, here it is:
# plot(c(-5, 5),
#      c(-5, 5),
#      type='n',
#      xlab='x1',
#      ylab='x2')
# title(main='Linear Separable Features')
# ymat <- ymatrix(svp)
# points(x[-SVindex(svp), 1],
#        x[-SVindex(svp), 2],
#        pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
# points(x[SVindex(svp), 1],
#        x[SVindex(svp), 2],
#        pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))
#
# # Extracting w and b from the model
# w <- colSums(coef(svp)[[1]] * x[SVindex(svp),])
# b <- b(svp)
#
# # Draw the b ad w lines
# abline(b/w[2],-w[1]/w[2])
# abline((b+1)/w[2],-w[1]/w[2],lty=2)
# abline((b-1)/w[2],-w[1]/w[2],lty=2)

################################

x <- rbind(matrix(rnorm(100, mean = 1, sd = 1), , 2),
           matrix(rnorm(100, mean = 4, sd = 1), , 2))
x
plot(x)
y <- matrix(c(rep(1, 50),rep(-1, 50)))

svp <- ksvm(x
            , y
            , C = 1
            , type = "C-svc" # C classification
            # , type = "nu-svc" # nu classification
            # , type = "C-bsvc" # bound-constraint svm classification
            # , type = "spoc-svc" # Crammer, Singer native multi-class
            # , type = "kbb-svc" # Weston, Watkins native multi-class
            # , type = "one-svc # novelty detection
            # , type = "eps-svr" # epsilon regression
            # , type = "nu-svr" # nu regression
            # , type = "eps-bsvr" # bound-constraint svm regression

            , kernel = "rbfdot"
            # , kernel = "tanhdot"
            # , kernel = "anovadot"
            # , kernel = "rbfdot" # Radial Basis kernel "Gaussian"
            # , kernel = "polydot" # Polynomial kernel
            # , kernel = "vanilladot" # Linear kernel
            # , kernel = "tanhdot" # Hyperbolic tangent kernel
            # , kernel = "laplacedot" # Laplacian kernel
            # , kernel = "besseldot" # Bessel kernel

)
plot(svp, data = x,
     xlim = c(0, 4),
     ylim = c(0, 4))

names(attributes(svp))

sv1 <- x[alphaindex(svp)[[1]],]
plot(sv1,
     pch=19,
     main="SV")

sum(y == predict(svp, x))


#################

# Training SVM Models
library(caret)
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine
library(pROC)	       # plot the ROC curves

### Get the Data
# Load the data and construct indices to divide it into training and test data sets.
data(segmentationData)
?segmentationData

trainIndex <- caret::createDataPartition(segmentationData$Case,
                                  p = .5,
                                  list = FALSE)
trainData <- segmentationData[trainIndex, ]
testData  <- segmentationData[-trainIndex, ]
trainX <- trainData[ , 4:61]        # Pull out the variables for training
sapply(trainX, summary)

ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     repeats = 5,		    # do 5 repititions of cv
                     summaryFunction = twoClassSummary,	# Use AUC to pick the best model
                     classProbs = TRUE)


#Train and Tune the SVM
svm.tune <- train(x = trainX,
                  y = trainData$Class,
                  method = "svmRadial",   # Radial kernel
                  tuneLength = 9,					# 9 values of the cost function
                  preProc = c("center","scale"),  # Center and scale data
                  metric = "ROC",
                  trControl = ctrl)

svm.tune





# The results show that the best model resulted from setting .
# In the second pass, having seen the parameter values selected in the first pass, we use train()'s tuneGrid parameter to do some sensitivity analysis around the values C = 1 and sigma = 0.015 that produced the model with the best ROC value. Note that R's expand.grid() function is used to build a dataframe contain all the combinations of C and sigma we want to look at.
# Second pass
# Look at the results of svm.tune and refine the parameter space

set.seed(1492)
# Use the expand.grid to specify the search space
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

#Train and Tune the SVM
svm.tune <- train(x=trainX,
                  y= trainData$Class,
                  method = "svmRadial",
                  preProc = c("center","scale"),
                  metric="ROC",
                  tuneGrid = grid,
                  trControl=ctrl)

svm.tune

plot(svm.tune, data = segmentationData)



# This was quite a bit of calculation for an improvement of 0.0003247 in the ROC score, but it shows off some of what caret can do.
# To finish up, we have build a model with a different kernel. The linear kernel is the simplest way to go. There is only the C parameter to set for this kernel and train() hardwires in a value of C = 1. The resulting ROC value of 0.87 is not too shabby.
#Linear Kernel
set.seed(1492)

#Train and Tune the SVM
svm.tune2 <- train(x=trainX,
                   y= trainData$Class,
                   method = "svmLinear",
                   preProc = c("center","scale"),
                   metric="ROC",
                   trControl=ctrl)


svm.tune2


# Because, I took the trouble to set the seed for the pseudorandom number generator to the same value before each of the resampling operations above we can use caret's resample() function to compare the results generated by the radial kernel and linear kernel models. The following block of code and results shows just thee first five lines of the comparison table but includes the summary of the comparison.
rValues <- resamples(list(svm=svm.tune,svm.tune2))
rValues$values

summary(rValues)

bwplot(rValues,metric="ROC",ylab =c("linear kernel", "radial kernel"))


#########################

library(kernlab) #ksvm
library(mlbench) #mlbench.spirals
dat <- mlbench.spirals(400,cycles=1.2,sd=0.07)
x <- dat$x
y <- dat$classes

linsvm <- ksvm(x,y,type="C-svc",kernel="vanilladot")

rbfsvm <- ksvm(x,y,type="C-svc",kernel="rbfdot")

op <- par(no.readonly=TRUE) #
par(mfcol=c(1,2),ps=16,lwd=2)
plot(linsvm,data=x)
plot(rbfsvm,data=x)
par(op)


sum(y == predict(linsvm, x))
sum(y == predict(rbfsvm, x))
# 232 linear vs 391 RBF
