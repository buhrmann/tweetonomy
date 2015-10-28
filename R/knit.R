library(knitr)
library(rmarkdown)

knit2html("report.Rmd", stylesheet="style.css")
knit2html("report.Rmd")
knit(input="report.Rmd")

rmarkdown::render("report.Rmd")
