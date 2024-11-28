# Start med at definere mulige områder og stationer
areas <- c("Copenhagen", "Aarhus", "Odense", "Aalborg", "Rural") # Mulige områder
stations <- list(
  "Copenhagen" = c("HCAB", "HVID", "JAGT1"),
  "Aarhus" = c("AARH3", "AARH6"),
  "Odense" = c("ODEN2", "ODEN6"),
  "Aalborg" = c("AALB4", "AALB5"),
  "Rural" = c("ANHO", "FOEL", "RISOE", "ULBG")
)


# Tag input fra terminalen, og viser mulige områder og stationer, hvis ingen er valgt
args = commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Tilgængelige områder og stationer for hvert område:\n")
  for (area in names(stations)) {
    cat(area, ":", paste(stations[[area]], collapse = ", "), "\n") #\n er new line
  }
  stop("Du skal angive både et område og en station som argumenter: 'område' 'station' \n 
  OLA'en vil have: \n
                'Copenhagen' 'HCAB' \n
                'Aarhus' 'AARH3'\n
                'Rural' 'RISOE'\n
                'Rural' 'ANHO'")
}
operator_area <- args[1]
operator <- args[2]

# Kontroller om input er gyldigt ift areas under det gyldige område
if (!(operator_area %in% areas)) {
  stop("Ugyldigt område! Tjek listen over tilgængelige områder.")
}
if (!(operator %in% stations[[operator_area]])) {
  stop("Ugyldig station for det valgte område! Tjek listen over stationer.")
}

library(httr)
library(rvest)

df=NULL
UserA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
operator_link <- paste0("https://envs2.au.dk/Luftdata/Presentation/table/",operator_area, "/",operator)
link <- operator_link
rawres <- GET(url = link, add_headers(`User Agent`= UserA))
if (rawres$status_code == 200) {
  cat(paste0("Statuskoden er: ", rawres$status_code, ", begynder scraping :) \n"))
} else { 
  status <- paste0("Statusfejl: ", rawres$status_code, ". Kan ikke få adgang til hjemmesiden.") 
  stop(print(status)) 
}

content <- httr::content(rawres,as = "text", encoding = "UTF-8")
operator_js <- paste0("https://envs2.au.dk/Luftdata/Presentation/table/MainTable/",operator_area, "/",operator)
js <- operator_js

token <- read_html(content) %>% html_element("input[name=__RequestVerificationToken]") %>% html_attr("value")
post <- POST(
  url = js,
  add_headers(`User Agent`= UserA),
  body = list(`__RequestVerificationToken` = token))

table_html <- content(post, as = "text", encoding = "UTF-8")
table <- read_html(table_html)

rows <- table %>% html_elements("tr")
table_data <- rows %>% html_elements("td") %>% html_text(trim = T)
header <- table %>% html_elements("th") %>% html_text(trim = T)
header_amount <- as.numeric(length(header))
unlist <- unlist(table_data)
df <- as.data.frame(matrix(data = unlist, ncol = header_amount, byrow = T))
colnames(df) <- header
df[,2:header_amount] <- lapply(df[,2:header_amount], function(x) as.numeric(gsub(",",".",x)))
RDSname <- paste0(operator,"_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".rds")
cat(paste0("Har fundet: ",nrow(df)," rækker! Gemmer dataen i filen: ",RDSname,"\n"))
saveRDS(df,RDSname)

total_dataset_name <- paste0(operator, "_total.rds")

if (file.exists(total_dataset_name)) {
  total_data <- readRDS(total_dataset_name)
  total_data <- rbind(total_data, df)
} else {
  total_data <- df
}

saveRDS(total_data, total_dataset_name)
cat(paste0("Total dataset updated and saved as: ", total_dataset_name,"med: ", nrow(total_data)," rows\n"))






