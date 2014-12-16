library(xlsx)
library(Hmisc)
library(testthat)

#Test if input data as a data.frame
data <- read.xlsx("Radium.xlsx",sheetIndex=1)
expect_that(data, is_a("data.frame"))

#Test capacity function
expect_that(capacity("Radium.xlsx",9.6,"Ra"),equals("Ion-exchange capacity for Ra is 0.154 nCi/g"))


