# How to use the data in the global environment directly:
# Instead of using the Knit HTML button, type rmarkdown::render("your_doc.Rmd") 
# at the R console. This will knit in the current session instead of a background 
# session.


#If using leaflet anywhere in the RMD, be sure to re-build those maps when starting a new session before rendering the HTML file
#To do this, run any script that makes leaflets:
# source("3-name-CTS.R", echo = TRUE)


#Run this command to render the html from the Rmd file.
rmarkdown::render("./output/5-report.Rmd")




