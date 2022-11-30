library(rvest) # package for data scraping
library(tidyverse) # general R package

# creating a list called "alldata"
alldata <- list()

# creating a vector of url for each year
get_url <- function(page,year) {
  str_c("https://www.cballtimeresults.org/performances?page=",
        page,
        "&section=10M&utf8=%E2%9C%93&year=",
        year,
        .sep = "")
}

# loop over Year [1973:2019], Page
for (i in 1:47) {
  year = i+1972
  page = 1
  # url = "https://www.cballtimeresults.org/performances?page=1&section=10M&utf8=%E2%9C%93&year=1990"
  url = get_url(1,year)
  html = read_html(url)
  
  # creating a data table starting with the first page in 1990
  alldata[[i]] = data.frame(html %>% 
                           html_table())
  
  # the "next" button on the web page will
  # have class="button " if you can go to the next page and
  # class="button disabled" when you are at the last page of the year
  nextClickable = read_html(url) %>% 
    html_nodes(".pagination-controls:nth-child(1) .button+ .button") %>% 
    html_attr("class")
  
  # while the "next" button is clickable, loop through every page in the year
  # and add the data to dataTable
  while(nextClickable != "button disabled")
  {
    page = page+1
    #changing the url using the new page number
    # url = paste0("https://www.cballtimeresults.org/performances?page=",
    #              page,
    #              "&section=10M&utf8=%E2%9C%93&year=",
    #              year)
      
      url = get_url(page,year)
    
    nextClickable = read_html(url) %>% 
      html_nodes(".pagination-controls:nth-child(1) .button+ .button") %>% 
      html_attr("class")
    
    # printing the url and the button attribute to make sure
    # it's looping through all the pages
    print(nextClickable)
    print(url)
    
    # get the data table from the page
    currentPage = data.frame(read_html(url) %>%
                               html_table())
    
    # appending the data to the table
    alldata[[i]] = rbind(alldata[[i]], currentPage)
  }
  
}

rm(list = setdiff(ls(), "alldata"))
CBData_raw <- alldata
rm(list = "alldata")
save.image("C:/Users/min25/Downloads/CBData_raw.RData")
