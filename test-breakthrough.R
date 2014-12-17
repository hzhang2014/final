library(xlsx)
library(Hmisc)

#Test if input data as a data.frame
data <- read.xlsx("Radium.xlsx",sheetIndex=1)
expect_that(data, is_a("data.frame"))

#Test capacity function
 #fails without "round"
 #passes with "round"
expect_that(capacity("Radium.xlsx",9.6,"Ra"),equals("Ion-exchange capacity for Ra is 0.154 nCi/g"))
expect_that(capacity("Radium.xlsx",9.6,"Ba"),equals("Ion-exchange capacity for Ba is -0.079 m moles/g"))    #Here a negative value means in this Radium test, Ba is not adsorbed, but released.

