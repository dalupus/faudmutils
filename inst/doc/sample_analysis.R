## ------------------------------------------------------------------------
library(knitr)
library(faudmutils)
library(RWeka)
suppressPackageStartupMessages(library(dplyr))
library(xtable)
library(agricolae)
library(ggplot2)

## ------------------------------------------------------------------------
zipfile <- system.file("extdata","newreview-wordvector-output.zip",package="faudmutils")

## ------------------------------------------------------------------------
df <- mapKeys(zipfile)

## ------------------------------------------------------------------------
unique(df$Learner)  # Learner names are now human readible
unique(df$Dataset) #  No more dataset numbers

## ------------------------------------------------------------------------
df$Dataset <- plyr::revalue(df$Dataset,c("reviewSpamData-string-hotel.arff"="hotel",
                                           "reviewSpamData-string-restaurant.arff"="restraunt",
                                           "reviewSpamData-string-doctor.arff"="doctor"))
table(df$Dataset)

## ------------------------------------------------------------------------
sampledDF <- sample_n(df,10)
sampledDF <- select(sampledDF,Dataset,SamplingTechnique,WordsToKeep,Learner,AUC)
kable(sampledDF,row.names = FALSE)

## ------------------------------------------------------------------------
df.aov <- aov(AUC ~ Dataset*Learner,df)
summary(df.aov)

## ----results='asis'------------------------------------------------------
print(xtable(df.aov),type="html")

## ------------------------------------------------------------------------
print(xtable(df.aov),type="latex")

## ------------------------------------------------------------------------
HSD.test(df.aov,"Learner",console = TRUE)

## ---- results='asis'-----------------------------------------------------
HSDTable(df.aov)

## ---- results='asis'-----------------------------------------------------
HSDTable(df.aov,which="Learner")

## ---- fig.width=7--------------------------------------------------------
tukeyPlot(df,groupVars = "Learner")

## ---- fig.height=6,fig.width=7-------------------------------------------
tukeyPlot(df,groupVars = c("Learner","Dataset"))

