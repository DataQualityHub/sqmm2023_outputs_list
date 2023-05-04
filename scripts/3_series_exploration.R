

final_dataset$series <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "", final_dataset$series, perl = TRUE)


series_df <- final_dataset[!final_dataset$series %in% c("NEW METHODOLOGY", "NEW QMI", "NEW ARTICLE", "NEW DIGITAL CONTENT", "NEW DATA ONLY", "NEW EXPERIMENTAL STATISTICS"), ] %>% 
  filter(content_type != "QMI") # 1053 -> 1014 NOTE: keeping new bulletins in 

series_view <- series_df %>% group_by(group, directorate,division, content_type, series) %>% 
  summarize(n = n(), iterations = str_c(airtable_publication, collapse = "; ")) %>% 
  ungroup() %>% 
  filter(content_type != "Methodology")

rm(series_df)

#write.csv(series_sum, ".//output//series_explorations.csv")

