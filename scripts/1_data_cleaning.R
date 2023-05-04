

##Airtable data - input string for file location

loading_cleaning <- function(data_location_str) {

  
  #1. Loading  
  
  sqmm_airtable <- readxl::read_excel(data_str, sheet = 4) %>% 
    janitor::clean_names() %>% 
    filter(publication != '') %>% 
    distinct(publication, .keep_all = TRUE)

  #2. Clearning
  sqmm_airtable$publication <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "", sqmm_airtable$publication, perl = TRUE)

  x <- c("NEW ARTICLE:| NEW BULLETIN: |NEW DATA ONLY: |NEW DIGITAL CONTENT:| NEW EXPERIMENTAL STATISTICS: |NEW HEADLINE BULLETIN:|NEW METHODOLOGY:|NEW QMI:")


  sqmm_airtable_cleaned <- sqmm_airtable %>% 
   mutate(publication = str_replace(publication, x,'')) %>% 
   mutate(publication = str_replace(publication, "^:", '')) %>% 
   mutate(publication = str_replace(publication, "\\[Small Update]", '')) %>% 
   mutate(series = str_replace_all(series, '"',''))
 
 
  sqmm_airtable_cleaned$publication <- str_trim(sqmm_airtable_cleaned$publication, "both") 
  sqmm_airtable_cleaned$series <- str_trim(sqmm_airtable_cleaned$series, "both")


  sqmm_airtable_cleaned <- filter(sqmm_airtable_cleaned, publication != '')

}
  
airtable <- loading_cleaning(data_str)
rm(loading_cleaning)



## Planner data - input string for file location


planner <- readxl :: read_excel(data_str, sheet = 5) %>% 
  rename(publication = 'Publication Title') %>%
  janitor::clean_names() %>% 
  filter(publication_date >= "2022-05-01") %>%
  arrange(publication, desc(publication_date)) %>% 
  select(publication, publication_date,approved_by, is_market_sensitive,is_national_statistic) %>% 
  rename(name = approved_by) %>% 
  as.data.frame() %>% 
  distinct(publication, .keep_all = TRUE) #note 1181 all but 794 unique, I think people use the same names here - i.e, series


####Staff counts

staff_df <- readxl::read_excel(staff_str, sheet =2) %>% 
  janitor::clean_names() %>% 
  select(name,group,directorate,division)

rm(staff_str)


#write.csv(airtable, "cleaned_airtable.csv")
