
#1. Fuzzy matching airtable and planner

airtable_planner_fuzzy_spining <- function(airtable,planner){

  from_vector <- unique(airtable$publication)
  to_vector <- unique(planner$publication)

  pl <- POLYFUZZ$PolyFuzz("TFIDF")$match(from_vector,to_vector)$get_matches()
  pl_df <- data.frame(pl$From, pl$To, pl$Similarity) 
  matched_spine <- pl_df
}
  
airtable_planner_fuzzy_spine <- airtable_planner_fuzzy_spining(airtable,planner)
rm(airtable_planner_fuzzy_spining)

##BRINING IN THE OTHER TABLES

airtable_planner_fuzzy_matching <- function(airtable, planner, airtable_planner_fuzzy_spining) {
  
  first_stage_airtable_link <- airtable_planner_fuzzy_spine %>% 
    rename(publication =pl.From) %>% 
    merge(airtable) %>% 
    rename(airtable_publication = publication, planner_publication = pl.To) %>% 
    select(1,2,8,5,7,6,3) #1462 records same as airtable unique after cleaning


  second_stage_planner_link <- airtable_planner_fuzzy_spine %>% 
    distinct(airtable_planner_fuzzy_spine$pl.To, .keep_all=TRUE) %>% # 675 records
    rename(publication = pl.To) %>% 
    merge(planner) %>% 
    rename(planner_publication = publication, airtable_publication = pl.From) %>% 
    select(1,2,5,6,7,8,3) 

  final_link <- merge(first_stage_airtable_link, second_stage_planner_link[,c(1,4:7)], all.x = TRUE) %>% 
    select(airtable_publication,
           publication_date,
           series,
           planner_publication,
           pl.Similarity,
           frequency,
           content_type,
           name,
           is_market_sensitive,
           is_national_statistic
    )
  
}

airtable_planner_fuzzy_linked <- airtable_planner_fuzzy_matching(airtable, planner, airtable_planner_fuzzy_spining)
rm(airtable_planner_fuzzy_matching, airtable, planner, airtable_planner_fuzzy_spine)


#2. Fuzzy matching names between planner and names between staff counts


names_fuzzy_matching <- function (airtable_planner_fuzzy_linked, staff_df) {

  from_vector<- unique(airtable_planner_fuzzy_linked$name) %>% na.omit() 
  to_vector <- unique(staff_df$name)

  names_pl <- POLYFUZZ$PolyFuzz("TF-IDF")$match(from_vector,to_vector)$get_matches()
  names_pl_df <- data.frame(names_pl$From, names_pl$To, names_pl$Similarity) %>% 
   rename(planner_names=names_pl.From, staff_df_names = names_pl.To)
  
}

names_fuzzy_spine <- names_fuzzy_matching(airtable_planner_fuzzy_linked, staff_df)
rm(names_fuzzy_matching, POLYFUZZ)

#3. Final matching - linking the airtable/planner join with staff counts based on names_fuzzy_spine

airtable_planner_staff_counts_linking <- function(x = airtable_planner_fuzzy_linked, y = names_fuzzy_spine, z = staff_df) {

  fn1 <- x %>% 
    rename(planner_names = name) %>% 
    merge(y, all.x = TRUE)


  fn2 <- z %>% 
    rename(staff_df_names = name) %>% 
    merge(fn1, all.y = TRUE) %>% 
    select(group,directorate,division,
         airtable_publication, publication_date,series,
         content_type, frequency,
         planner_publication, pl.Similarity,
         staff_df_names, planner_names, names_pl.Similarity, 
         is_market_sensitive, is_national_statistic) %>% 
   arrange(group,directorate,division, airtable_publication)
  
}

final_dataset <- airtable_planner_staff_counts_linking(x = airtable_planner_fuzzy_linked, y = names_fuzzy_spine, z = staff_df)
rm(airtable_planner_staff_counts_linking)

