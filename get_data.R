# Takes year as a string and returns projects for that year

get_data <- function(x) {
  
  url <-paste0("http://www.dst.dk/-/media/Kontorer/13-Forskning-og-Metode/projekterALLE", x, ".html")
  
  temp <- htmlParse(url, encoding = "UTF-8")
  temp_list <- readHTMLTable(temp, header = T, encoding = "UTF-8", stringsAsFactors=F)
  temp_data <- temp_list[[2]]
  temp_data$year <- x
  names(temp_data) <- tolower(names(temp_data))
  temp_data <- temp_data[, c("projekt", "projekt_titel", "nr_navn", "year")]
  names(temp_data) <- c("Project_number", "Project_title", "Organization", "Year")
  
  return(temp_data)
  
}
