library(knitr)
library(markdown)
library(rmarkdown)

method = "rmarkdown"
rmd_file = "report.Rmd"
recache = F

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")

if (method == 'rmarkdown') {
  
  rmarkdown::render(rmd_file, params=list(rebuild_cache=recache))
  
} else if (method == 'knitr') {
  
  knit2html(rmd_file, stylesheet="style-custom.css")
  
} else {
  
  knit(input=rmd_file)
  md_opts = c("use_xhtml")
  markdownToHTML('report.md', options=md_opts, stylesheet='style-custom.css')

}
