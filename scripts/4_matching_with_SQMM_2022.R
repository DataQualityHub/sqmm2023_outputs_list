

#Reading data
series_view <- series_view %>% 
  mutate(id_23 = paste0("sqmm23-ser", seq(1:nrow(series_view))))

sqmm2022 <- readxl :: read_excel(data_str, sheet = 7) %>% 
  janitor::clean_names() %>%
  rename(division22 = division) %>% 
  mutate(id_22 = paste0("sqmm22-ser", seq(1:nrow(sqmm2022)))) %>% 
  select(3,2,1)  
  



#Matching function

yoying <- function(series_view, sqmm2022) {
  
  from_vector<- unique(series_view$series) 
  to_vector <- unique(sqmm2022$output)
  
  names_pl <- POLYFUZZ$PolyFuzz("TF-IDF")$match(from_vector,to_vector)$get_matches()
  names_pl_df <- data.frame(names_pl$From, names_pl$To, names_pl$Similarity) %>% 
    rename(series=names_pl.From, output = names_pl.To)
  
}  

yoy_spine <- yoying(series_view=series_view, sqmm2022=sqmm2022) %>% 
  merge(sqmm2022) %>% 
  rename(series22 = output, similarity = names_pl.Similarity) %>% 
  merge(series_view[,c(3,5,7,8)]) %>% 
  rename(series23 = series, division23 = division) %>% 
  select(series23,
         series22,
         similarity,
         iterations,
         division23,
         division22,
         id_23,
         id_22)
  


#next step is to check if there's anything left out from SQMM2022 titles  

  
checking22 <- function (yoy_spine, sqmm2022){
   
   sqmm2022_stg <- sqmm2022 %>% select(output) %>% mutate(source = "sqmm22") %>%
     distinct(output, .keep_all = TRUE)# 145
     
     
   yoy_spine_stg <- yoy_spine %>% select(series22) %>% mutate(source = "matching") %>% 
     distinct(series22, .keep_all = TRUE) %>%
     rename(output = series22)# 19 are missing total is 126
   
   mgd_df <- merge(sqmm2022_stg[,1], yoy_spine_stg, all.x = TRUE)
   
} 

#creating combined table


checks22missing <- checking22(yoy_spine=yoy_spine, sqmm2022=sqmm2022) %>% 
  filter(is.na(source)) %>% 
  select(1) %>% 
  merge(sqmm2022) %>% 
  rename(series22 = 'output') %>% 
  mutate(series23 = '',
         id_23 = '',
         similarity = '0',
         division23 = '',
         iterations = '') %>% 
  select(series23,
         series22,
         similarity,
         iterations,
         division23,
         division22,
         id_23,
         id_22
         )
  


manual_checks <- rbind(yoy_spine,checks22missing)

#write.csv(manual_checks, "manual_checking_yoy.csv")

