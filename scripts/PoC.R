

##1.Data loading

dqhub <- readxl::read_excel("outputs_lists.xlsx", sheet = 3) %>% 
  rename(publication = Name) %>%
  janitor::clean_names() %>% 
  separate(publication, "publication", sep = ":") %>% 
  distinct(publication, .keep_all=TRUE) %>% 
  select(publication)  

planner <- readxl :: read_excel("outputs_lists.xlsx", sheet = 5) %>% 
  rename(publication = 'Publication Title') %>%
  janitor::clean_names() %>% 
  filter(publication_date >= "2022-04-01") %>%
  separate(publication, "publication", sep = ":")%>% 
  arrange(publication, desc(publication_date)) %>% 
  distinct(publication, .keep_all=TRUE) %>% 
  select(publication, publication_date,approved_by, is_market_sensitive,is_national_statistic) %>% 
  rename(name = approved_by)


##2.Matching
##Python module

POLYFUZZ <- reticulate::import("polyfuzz")
#to make it work uninstall numpy and install: pip install numpy == 1.19.0

from_vector <- unique(dqhub$publication)
to_vector <- unique(planner$publication)

pl <- POLYFUZZ$PolyFuzz("TFIDF")$match(from_vector,to_vector)$get_matches()
pl_df <- data.frame(pl$From, pl$To, pl$Similarity)
pl_df <- pl_df %>% filter(pl_df$pl.Similarity>=0.7)
rm(pl, from_vector, to_vector)


#3.Merging the matched data to the planner
#(we do this so that we can start getting names)

mg_df <- pl_df %>% 
  rename(publication = pl.To, dqhub_series = pl.From) %>% 
  merge(planner, all.x = TRUE) %>% 
  rename(planner_publication = publication, pub_similarity = pl.Similarity) %>% 
  select(dqhub_series, planner_publication, publication_date, pub_similarity, name,is_market_sensitive, is_national_statistic)

rm(pl_df)

###4.Bringing in the staff counts

staff_df <- readxl::read_excel("12_Staff Counts 31 Mar 2023.xlsx", sheet =2) %>% 
  janitor::clean_names() %>% 
  select(name,group,directorate,division)
  
'#staff_df$name_split <- staff_df$name
staff_df <- staff_df %>% tidyr::separate(name_split,c("last_name", "first_name"), ",") 
staff_df <- staff_df %>% mutate(test = str_replace_all(staff_df$first_name, pattern = " ", repl="")) %>% 
select(-first_name) %>% dplyr::rename(c("first_name"="test"))
staff_df$full_name <- paste(staff_df$first_name, staff_df$last_name)

staff_df <- staff_df %>% 
  select(full_name, group, directorate,division) %>%
  rename(name = full_name)' # this code is changing the order of the names e.g., from Doe, Jane to Jane Doe.


#5.Using fuzzy matching to merge the planner names with the staff counts
#Because the names sometimes can be Samuel or Sam etc.

from_vector<- unique(mg_df$name)
to_vector <- unique(staff_df$name)

names_pl <- POLYFUZZ$PolyFuzz("TF-IDF")$match(from_vector,to_vector)$get_matches()
names_pl_df <- data.frame(names_pl$From, names_pl$To, names_pl$Similarity) %>% 
  rename(planner_names=names_pl.From, staff_df_names = names_pl.To)
rm(names_pl, from_vector, to_vector, POLYFUZZ)

rm(dqhub, planner)



##6.Final matching

#First stage: Air table+Planner+Names fuzzy matched
fn1 <- mg_df %>% 
  rename(planner_names = name) %>% 
  merge(names_pl_df, all.x = TRUE)

#Second stage: Above merged with staff_df to get the ONS Structure

fn2 <- staff_df %>% 
  rename(staff_df_names = name) %>% 
  merge(fn1, all.y = TRUE) %>% 
  select(group,directorate,division,dqhub_series, planner_publication,staff_df_names,publication_date, 
         is_market_sensitive, is_national_statistic, pub_similarity, names_pl.Similarity ) %>% 
  arrange(group,directorate,division, dqhub_series)


#7. Outputs

#Outputting the final product 'Updated mega-list'
writexl::write_xlsx(fn2, "sqmm23_pop_TRYI.xlsx")

#Aggregating and outputing per division
outputs_per_div <- fn2 %>%  group_by(group, directorate, division) %>% summarize(n=n())
writexl::write_xlsx(outputs_per_div, "outputs_per_div.xlsx")


