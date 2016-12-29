#' Import google spreadsheet or xlsx file
#'
#' @description function to import information from google spreadsheet or xlsx file.
#' @param url local file directory for xlsx document or url from google spreadsheet
#' @param type  type of update: manually or automatic. use automatic only in shiny inviroment
#' @param time time to automatic reload of the information
#' @return data frame
#' @importFrom gsheet gsheet2tbl
#' @importFrom readxl read_excel
#' @importFrom shiny invalidateLater
#' @export

getData <- function(url, type = "manually", time = 60000) {
    
    if (type == "manually") {
        
        
        if (file.exists(url) == TRUE) {
            
            readxl::read_excel(path = url) %>% as.data.frame()
            
        } else {
            
            data <- gsheet::gsheet2tbl(url) %>% as.data.frame()
            
        }
        
        
    } else if (type == "automatic") {
        
        
        if (file.exists(url) == TRUE) {
            
            shiny::invalidateLater(60000)  # 1 min = 60000 msec
            readxl::read_excel(path = url) %>% as.data.frame()
            
        } else {
            
            shiny::invalidateLater(60000)  # 1 min = 60000 msec
            data <- gsheet::gsheet2tbl(url) %>% as.data.frame()
            
        }
        
        
    }
    
}







