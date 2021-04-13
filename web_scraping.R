# package names to load
packages <- c('dplyr', 'rvest', 'stringr', 'curl',
              'tidyr', 'purrr', 'lubridate', 'pbapply',
              'tibble')

# enable installation of packages into session
lapply(packages,require, character.only = TRUE)

###### FIRST PAGE SEARCH RESULTS ######

# Original url to download job links
url <- "https://www.indeed.com/jobs?q=data+scientist+%2494%2C000&l=Columbus%2C+OH&rbl=Columbus%2C+OH&jlid=8ddf8cac9ee9aa9d&jt=fulltime&sort=date"

# Google download of recent jobs
url_2 <- "https://www.google.com/search?q=data+science+jobs+columbus+ohio"

# Combining both urls into working tibble
url_combined <- data.frame(source = c("indeed", "google"), links = c(url, url_2)) %>%
  mutate(source_2 = source) %>%
  group_by(source) %>%
  nest() %>%
  ungroup()

# Call function to retrieve all job links from first page
links <- url_combined %>%
  mutate(data = pblapply(data, function(x){
    
    df <- x %>%
      unnest() %>%
      mutate_all(., function(x){as.character(x)})
    
    check <- if(df$source_2 %in% "indeed"){"indeed"}else{"google"}
    
    # Convert into executable html for scraping
    html <- read_html(df$links)
    
    links <- if(check == "indeed"){html %>%
        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "jobtitle", " " ))]')  %>%
        html_attr(name = "href")}else{html %>%
            html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "PUpOsf", " " ))]')  %>%
            html_attr(name = "href")}
    
    return(links)
    
    
  })) %>%
  unnest()

# Add domain address to retrieved job links and insert into tibble
links <- links %>%
  mutate(data = ifelse(source == "indeed", paste0("https://www.indeed.com", data),
                       paste0("https://www.google.com", data))) %>%
  dplyr::select(data)
  tibble(links = .)

# Invoke html functions to retrieve key job features per listing
tibble <- links %>%
  nest() %>%
  mutate(data = pblapply(data, function(x){
    
    # unnest tibble and add grouping variable (rowid)
    example <- x %>%
      unnest() %>%
      rowid_to_column(.) %>%
      group_by(rowid) %>%
      nest() %>%
      ungroup()
    
    # deploy function to extract job description features from each job link
    example_2 <- example %>%
      dplyr::select(-rowid) %>%
      mutate(data = pblapply(data, tryCatch(function(x){
        
        # Convert link into character vector
        example <- x %>%
          unnest() %>%
          as.character()
        
        # enable all links to be read without memory allocation issues
        link <- tryCatch(read_html(curl(example,
                                        handle = curl::new_handle("useragent" = "Mozilla/5.0"))),
                         error = function(e) {print(paste('error:', e))})
        
        # check to determine if error occurred
        check <- ifelse(is.character(link), "TRUE", "FALSE")
        
        # position title
        get_title <- function(html) {
          html %>%
            html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "jobsearch-JobInfoHeader-title", " " ))]') %>%
            html_text() %>%
            unlist()
        }
        
        # company name
        get_company <- function(html) {
          html %>%
            html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "icl-u-xs-mr--xs", " " ))]//a') %>%
            html_text() %>%
            unlist()
        }
        
        # bullet point list of all candidate qualifications
        get_qualifications <- function(html) {
          html %>%
            html_nodes(xpath = '//li') %>%
            html_text() %>%
            as.list()
        }
        
        # Functions deployed to extract job features and insert into working tibble
        link <- if(check == "FALSE"){tibble(job_title = as.character(get_title(link)), 
                                            company = as.character(get_company(link)),
                                            qualifcations = as.character(get_qualifications(link)))}else{
                                              tibble(job_title = "ERROR", 
                                                     company = "ERROR",
                                                     qualifcations = "ERROR") 
                                            }
        
        return(link)
        
      }, error = function(e) {print(paste('error:', e))}))) %>%
      unnest()
    
    return(example_2)
    
  })) %>%
  unnest()

# Adding data of extraction into working dataframe to be saved
final <- tibble %>%
  mutate(date_scraped = lubridate::ymd(Sys.Date())) %>%
  filter(job_title != is.na(job_title)) %>%
  unnest() %>%
  as.data.frame()

# Function to append new records into working rds file
save_rds <- function(x){
  
  con <- tryCatch(readRDS("Text_Analytics/data_repository_revised.rds"),
                  error = function(e) {print(paste('error:', e))})
  
  check <- if(is.character(con)){"YES"}else{"NO"}
  
  if(check == "YES"){
    
    saveRDS(final, file = "Text_Analytics/data_repository_revised.rds")}else{
    
    con <- readRDS("Text_Analytics/data_repository_revised.rds")
    
    con <- rbind(con, final)
    
    saveRDS(con, file = "Text_Analytics/data_repository_revised.rds")
    
    }
}

# save records into working rds file
save_rds(final)

###### FUNCTION TO EXTRACT MULTIPLE PAGES OF SEARCH RESULTS ######

# Extract the first four pages worth of Indeed.com job links
get_links_all <- tryCatch(function(page_numbers) {
  
  # Original url to download job links
  url <- "https://www.indeed.com/jobs?q=data+scientist+$94,000&l=Columbus,+OH&rbl=Columbus,+OH&jlid=8ddf8cac9ee9aa9d&jt=fulltime&sort=date&fromage=14.html"
  
  # Convert into executable html for scraping
  html <- read_html(url)
  
  # Function to return all first page job links
  get_links <- function(html) {
    links <- html %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "jobtitle", " " ))]')  %>%
      html_attr(name = "href")
    
    return(links)
    
  }
  
  # Call function to retrieve all job links from first page
  links <- get_links(html)
  
  # Add domain address to retrieved job links
  links <- paste0("https://www.indeed.com", links)
  
  for (i in 10:page_numbers) {
    
    # New url for subsequent pages
    url <- "https://www.indeed.com/jobs?q=data+scientist&l=Columbus%2C+OH&start="
    
    # Original url to download job links
    url_2 <- paste0(url, page_numbers)
    
    html_2 <- read_html(curl(url_2, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
    
    links_2 <- get_links(html_2)
    
    # Add domain address to retrieved job links
    links_2 <- paste0("https://www.indeed.com", links_2)
    
    links <- c(links, links_2)
    
    links_final <- tibble(job_links = links)
    
  }
  
  return(links_final)
  
}, error = function(e) {print(paste('error:', e))})

# Store all job links into working object
system.time(all_links <- get_links_all(40))

# Group tibble according to repeating sequence
group <- rep(1:4, length.out = nrow(all_links))




