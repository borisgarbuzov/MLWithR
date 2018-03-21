
rm (list = ls())
if (! require ("C50")) {
  install.packages("C50")
}

if (! require ("gmodels")) {
  install.packages("gmodels")
}

if (! require ("RWeka")) {
  install.packages("RWeka")
}

if (! require ("caret")) {
  install.packages("caret")
}

setwd("C:/Users/Boris/OneDrive/text/toronto_u/course/current/ml/MLWithR/R")

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
dim (wbcd)
names (wbcd)

length(names (wbcd))
rownames(wbcd)
wbcd[1]
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),
                        labels = c("Benign", "Malignant"))
levels(wbcd$diagnosis)
labels(wbcd$diagnosis)
# row names

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

wbcd[1]
wbcd[2]

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)


