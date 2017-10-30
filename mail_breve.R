if (!"pacman" %in% dir(.libPaths())) devtools::install_github("trinker/pacman")
if(!"gmailr" %in% dir(.libPaths())) install.packages("gmailr");require(gmailr)
pacman::p_load(rmarkdown, knitr)
library(methods)
setwd("/home/biagetti/cripto")
rmarkdown::render("cripto.Rmd","all",output_dir="reports")
s<-list.files(paste(getwd(),"/reports/",sep=""),pattern="*.csv");s
mime() %>%
  to(c("lugfabio@gmail.com","olindo.cervi@gmail.com","marco.biagetti2013@gmail.com")) %>%
  from("marco.biagetti2013@gmail.com") %>%
  text_body("pacchetto gmailr") -> text_msg
strwrap(as.character(text_msg))

text_msg %>%
  subject("Dati") %>%
  attach_file(paste("./reports/",s,sep=""))->file_attachment
send_message(file_attachment)
unlink(paste("./reports/",s,sep=""))
