## This set of syntax is a complete worked example with
## 3 arbitrary chosen countries, Azerbaijan, Austria, Finland
##    31 = Azerbaijan
##    40 = Austria
##    246 = Finland

## We start with baseline to Prop 4 to Prop 7
##----------------------------------------------------------------


library("lavaan")
library("semTools")
dat <- read.table("data/BULLY.dat", header=FALSE)

names(dat) <- c("IDCNTRY", "R09A", "R09B", "R09C", "R09D")

head(dat)
# To store results from baseline, proposition 4, and proposition 7
# results in one place
# For illustration purposes, fit indices to be extracted are: chi-square,
# df, p, rmsea, cfi, and tli (hence 6 columns)
all.results <- matrix(NA, ncol = 6, nrow=3)

## Specifying the baseline model with four items as an example
mod.cat <- 'F1 =~ R09A + R09B + R09C + R09D'

## Baseline model: no constraints across groups or repeated measures
baseline <- measEq.syntax(
  configural.model = mod.cat,
  data = dat,
  ordered = c("R09A", "R09B", "R09C", "R09D"),
  parameterization = "delta",
  ID.fac = "std.lv",
  ID.cat = "Wu.Estabrook.2016",
  group = "IDCNTRY",
  group.equal = "configural"
)


#For a little bit of orientation/instructions
summary(baseline)

#To see all of the constraints in the model
cat(as.character(baseline))

#Have to specify as.character to submit to lavaan
model.baseline <- as.character(baseline)

## Fitting baseline model
fit.baseline <- cfa(model.baseline, data = dat, group = "IDCNTRY", ordered = c("R09A", "R09B", "R09C", "R09D"))
summary(fit.baseline)


all.results[1,] <- round(data.matrix(fitmeasures(fit.baseline,fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)


##--------------------------------------------------
## So, turning now, to Prop 4 - Threshold invariance
##--------------------------------------------------
prop4 <- measEq.syntax(configural.model = mod.cat,
                        data = dat,
                        ordered = c("R09A", "R09B", "R09C", "R09D"),
                        parameterization = "delta",
                        ID.fac = "std.lv",
                        ID.cat = "Wu.Estabrook.2016",
                        group = "IDCNTRY",
                        group.equal = c("thresholds"))

model.prop4 <- as.character(prop4)
fit.prop4 <- cfa(model.prop4, data = dat, group = "IDCNTRY", ordered = c("R09A", "R09B", "R09C", "R09D"))
summary(fit.prop4)

#store model fit information for proposition 4
all.results[2,]<-round(data.matrix(fitmeasures(fit.prop4,fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)


lavTestLRT(fit.baseline,fit.prop4)

## And now - final model - one of threshold and loading invariance
prop7 <- measEq.syntax(configural.model = mod.cat,
                       data = dat,
                       ordered = c("R09A", "R09B", "R09C", "R09D"),
                       parameterization = "delta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016",
                       group = "IDCNTRY",
                       group.equal = c("thresholds", "loadings"))

model.prop7 <- as.character(prop7)
fit.prop7 <- cfa(model.prop7, data = dat, group = "IDCNTRY", ordered = c("R09A", "R09B", "R09C", "R09D"))
summary(fit.prop7)

# store model fit information for proposition 7
all.results[3,]<-round(data.matrix(fitmeasures(fit.prop7,fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)
column.names<-c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled" )
row.names<-c("baseline","prop4","prop7" )

colnames(all.results)<-column.names
rownames(all.results)<-row.names


lavTestLRT(fit.prop4, fit.prop7)
lavTestLRT(fit.prop7, fit.baseline)


## And now - partial invariance
## Here, we fix threshold equality (maintaining prop 4) and freely
## estimate a loading (for R09C) based on modification indices from prop 7
fit.prop7 <- cfa(model.prop7, data = dat, group = "IDCNTRY", ordered = c("R09A", "R09B", "R09C", "R09D"))
summary(fit.prop7)
mi <- modindices(fit.prop7, free.remove = FALSE)
mi[mi$op == "=~",]

## The model of partial invariance
prop7.part <- measEq.syntax(configural.model = mod.cat,
                       data = dat,
                       ordered = c("R09A", "R09B", "R09C", "R09D"),
                       parameterization = "delta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016",
                       group = "IDCNTRY",
                       group.equal = c("thresholds", "loadings"),
                       group.partial = "F1 =~ R09C")

model.prop7.part <- as.character(prop7.part)
fit.prop7.part <- cfa(model.prop7.part, data = dat, group = "IDCNTRY", ordered = c("R09A", "R09B", "R09C", "R09D"))
summary(fit.prop7.part)

## Test of model fit between prop4 and prop 7 with one loading freed
lavTestLRT(fit.prop7.part, fit.prop4)

