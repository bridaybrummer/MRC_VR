# Active_facilities 

# Linking NMC to MFL facilities 

#Load library 
pacman::p_load(sf,  tm)
library(NMCleaner)
conflicted::conflicts_prefer(dplyr::filter)
#load MFL 
directory_files<- list.files()
mfl_path<- directory_files[grepl("MFL_Updated.csv",directory_files)]
mfl <-read_csv( mfl_path)%>%clean_names

mfl$latitude%>%gsub( ",", ".",. )%>%as.numeric()-> mfl$latitude
mfl$longitude%>%gsub( ",", ".",. )%>%as.numeric()-> mfl$longitude


mfl%>%
    mutate( types = trimws(gsub( "WC|COVID-19", "", types)))%>%
    select( types) %>%
    tbl_summary(
    sort = all_categorical(FALSE) ~ "frequency",
    )%>%
as_flex_table()#%>%save_as_docx( path = "MFL_facilities.docx")



#load in master datbase 
load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")

  # Plan 
  #| 1. database of facilities that made notifications during COVID
  #| 2. Some sort of timescale (months) of those facilties who have made any notification since then 
  #| 3. Focus on hospitals first. 


new_master$facility_type %>%tabyl()%>%arrange(-n)%>% select(1) %>%pull()->common_facility_types

new_master%>%
    filter( !is.na(prov_),
    !condition %in% "Covid-19")%>%
    mutate(
        facility_sector = 
            if_else(
                grepl("^pr", ignore.case = TRUE, facility_sector), "Private" , 
                "Public"),
            facility_type = 
                if_else( 
                    !facility_type %in%
                    c(common_facility_types[1:5]), "Other", 
                     facility_type )
            ) %>%
            mutate(
                facility_type =
                    if_else(
                         facility_type == "PRIVATE PRACTICE", 
                            "CLINIC", 
                            facility_type)) ->
                        df


#df$facility_type %>%tabyl()%>%arrange(-n)

# The goal is to make some sort of reclassification of both NMC and MFL facilities to a standard. 
# Potentially better to align MFL to NMC using a function 

# Plan: 
    # create standardised dataset for both 
    # facility name and province (+ sub district)
    # ML programme or v look up between the two. 

pacman::p_load( stringr, tokenizers)





#########################

# Standardise the NMC facilities

#########################

df%>%
    select( 
        facility, prov_, district, sub_district, facility_sector, facility_type
    )%>%
    distinct(., .keep_all = TRUE)-> 
    facilities_NMC


facilities_NMC$facility%>% 
    str_extract_all( "\\w+")%>% # extract all words
    unlist()%>%
    #unique()%>%
    sort() -> NMC_words

#extract ngrams for two consecutive words

    facilities_NMC$facility%>% 
        tokenize_ngrams(., n = 2)%>% # extract all bigrams (consecutive words)
        unlist()%>%
        sort()-> NMC_bigrams

    NMC_bigrams%>%tabyl()%>%arrange(n) -> tabyl_of_bigrams

    print(tabyl_of_bigrams)

    tabyl_of_bigrams %>%
        filter( n>20)%>%
        pull(1)%>%
        trimws() -> 
        common_bigrams_NMC_auto

# remove words that have length of 1  
    NMC_words
    NMC_words[grepl("tembisa", ignore.case = TRUE, NMC_words)]
    NMC_words[grepl("tembisa", ignore.case = TRUE, NMC_words)]%>%length()

    NMC_words%>%tabyl()   %>%  arrange(n)-> tabyl_of_common_words

    print(tabyl_of_common_words)

    tabyl_of_common_words %>%
        filter( n>150)%>%
        pull(1)%>%
        trimws() -> 
        common_tokens_NMC_auto

common_tokens_NMC_auto<- common_tokens_NMC_auto%>%gsub('[[:punct:] ]+',' ',.)%>%removeWords(stopWords)

common_tokens_NMC_auto
common_tokens_manual <- c( "post mortems", "med school", "lab", "anatomical pathology", "microbiology", "routine clin path", "ice research unit", "hosp", "nursing college", "annex", "anythign else" )


common_tokens_NMC <- c( common_tokens_NMC_auto, common_tokens_manual, common_bigrams_NMC_auto)

common_tokens_NMC%>%
# make into a single vector separeted by | 
    str_c(., collapse ="|")%>%
    str_c(., "|") -> common_tokens_NMC_full # these can be to general or to vauge

print(common_tokens_NMC_full)
# Now we have a list of common tokens for both MFL and NMC facilities.
common_tokens_NMC_full
stopWords <- stopwords("en")
gsub('[[:punct:] ]+',' ',facilities_NMC$facility)


facilities_NMC %>% 
#rmeove stope words and punctuation 
    mutate( standard_facility = facility%>%gsub('[[:punct:] ]+',' ',.)%>%removeWords(stopWords),
                standard_facility = 
               as.character( trimws(gsub( paste0("\\b(",common_tokens_NMC_full, ")\\b"), "", standard_facility) )   # the \\b is a word boundary
                ) 
    )%>%
    mutate( standard_facility= if_else( standard_facility == "", NA, standard_facility))->
    nmc

    nmc$standard_facility[which(grepl( "universitas", ignore.case = TRUE, nmc$standard_facility))]


    nmc$standard_facility  %>%is.na()%>%tabyl()      


nmc %>% group_by( 
    standard_facility, prov_, facility_sector, facility_type) %>%
    mutate( row_number = row_number(),
    n_duplicates = n() )%>%
    select( n_duplicates, row_number, standard_facility, everything())%>%
    ungroup%>%
    group_by(standard_facility)%>%
    arrange( -n_duplicates, standard_facility, ) -> 
    nmc_to_link

    nmc_to_link%>%view()


# migh have to do something different with the private hostpials like mediclinic, netcare, busamed, medicross etc. 

#################

# Now do it wiht MFL 

#################


mfl%>%glimpse()

mfl$types%>%tabyl() %>%arrange(-n)

mfl %>%
    filter(! grepl( "School|vaccine|mobile|non-medical|temporary|satelliet|stock|distribution|pharmacy|covid-19|field hospital", ignore.case = TRUE, types))%>%
    select( 
        primary_name, 
        other_name, 
        types, 
        latitude, 
        longitude
    )%>%
    mutate( 
        province = trimws(substr( other_name, 0, 3)),
    )-> 
    mfl_standard

# we could look at the coordinates tha existin within a shape file that has sub distrit info to give us that info. 
mfl_standard
mfl_standard$types %>%tabyl()%>%arrange(n)
# extract all strings in the mfl_standard$primary_name

mfl_standard$primary_name%>% 
    str_extract_all( "\\w+")%>%
    unlist()%>%
    #unique()%>%
    sort() -> mfl_words


# Get unigrams 

    mfl_words%>%tabyl() %>%
        arrange(n)%>%
        filter( n>50)%>%
        pull(1)%>%
        trimws() -> 
        common_tokens_mfl_auto

# Get bigrams 

    mfl_standard$primary_name%>% 
        tokenize_ngrams(., n = 2)%>% # extract all bigrams (consecutive words)
        unlist()%>%
        sort()-> mfl_bigrams

    mfl_bigrams%>%tabyl()%>%arrange(n) -> tabyl_of_bigrams

    print(tabyl_of_bigrams)

    tabyl_of_bigrams %>%
        filter( n>20)%>%
        pull(1)%>%
        trimws() ->
        common_bigrams_mfl_auto

common_bigrams_mfl_auto

common_tokens_mfl_manual <- c( "post mortems", "med school", "lab", "anatomical pathology" )

common_tokens_mfl_full <- c( 
    common_bigrams_mfl_auto,
    common_tokens_mfl_auto, 
    common_tokens_mfl_manual,
    common_tokens_manual) # include the ones from NMC 

common_tokens_mfl_full<- common_tokens_mfl_full%>%gsub('[[:punct:] ]+',' ',.)%>%removeWords(stopWords)


common_tokens_mfl_full%>%
# make into a single vector separeted by | 
    str_c(., collapse = "|")%>%
    str_c(., "|") -> common_tokens_mfl_full

mfl_standard %>%
    mutate( standard_facility = primary_name%>%gsub('[[:punct:] ]+',' ',.)%>%removeWords(stopWords),
                standard_facility = 
               str_to_lower( trimws(gsub( paste0("\\b(",common_tokens_mfl_full, ")\\b"), "", standard_facility) )   # the \\b is a word boundary
                ) 
    )%>%
    mutate( standard_facility= if_else( standard_facility == "", NA, standard_facility))->
    mfl

mfl %>% group_by(
    standard_facility, province)%>%
        mutate( row_number = row_number(),
    n_duplicates = n() )%>%
    select( n_duplicates, row_number, standard_facility, everything())%>%
    ungroup%>%
    group_by(standard_facility)%>%
    arrange( -n_duplicates, standard_facility, ) -> mfl_to_link
    
    mfl_to_link%>%
    #distinct(. , .keep_all = TRUE)%>%
    view()

mfl_to_link[grepl( "unive", ignore.case = TRUE, mfl_to_link$standard_facility),]
nmc_to_link[grepl( "universita", ignore.case = TRUE, nmc_to_link$standard_facility),]

nmc_to_link$standard_facility
mfl_to_link$standard_facility

##################

# Join the two datasets 

##################

stringdist_left_join( 
    mfl_to_link, 
    nmc_to_link,  
    by = "standard_facility", 
    method = "jw", 
    max_dist = 0.1, 
    distance_col = "dist"
    )-> left_joined_df
    
left_joined_df[grepl( "universitas", ignore.case = TRUE, left_joined_df$standard_facility.x),]%>%view()

left_joined_df%>%
    rename( 
        standard_facility_NMC = standard_facility.x,
        standard_facility_MFL = standard_facility.y
    )%>%
    ungroup() -> 
    left_joined_renamed

# you may want to remove the n:many matches and only keep the best match for each facility.
    # there may be multiple matches, find the best match for each NMC facility     
    left_joined_renamed %>%
    group_by( facility) %>%
        filter( dist == min(dist))-> 
        left_joined_df_1
        
    # there may be multiple matches, find the best match for each NMC facility     
    left_joined_renamed %>%
    group_by( primary_name) %>%
        filter( dist == min(dist))-> 
        left_joined_df_2


left_joined_df_2%>%
    filter( !is.na(primary_name))%>%
    select( 
        standard_facility_NMC, standard_facility_MFL, facility, primary_name, other_name, types, province, everything() 
    )%>%
    mutate( 
        across( c(latitude, longitude), ~ gsub( ",", ".", .)%>%as.numeric() )
    )%>%
    filter( 
        ! (is.na(latitude) & is.na(longitude)
        )
    )-> left_joined_df1

left_joined_df1$facility%>%unique()%>%length()
left_joined_df1$facility%>%distinct()%>%length()

left_joined_df1%>%
    distinct( 
        facility, .keep_all = TRUE
    )

left_joined_df1%>%
    group_by( 
        facility
    )%>%
    mutate( dup_number = row_number() )%>%
    filter( dup_number ==1, 
    grepl( "hospital", facility_type, ignore.case = TRUE))-> 
    left_joined_df1_nmc_only 

# So we have NMC places now with coordinates, mostly clinics, some hospitals 
left_joined_df1_nmc_only$facility_type%>%tabyl()

#It would now be good we can now link things to these facilities, but also plot all the MFL hostpials and clinics on a map and see which ones are on NMC, 
# OR which onces are "close" to NMC. 

mfl_to_link%>%
    filter( grepl( "hospital", ignore.case = TRUE, types))-> 
    mfl_hospitals 

mfl_hospitals$primary_name
left_joined_df1_nmc_only$primary_name
#join these two and keep al data from both datasets, consider iner or outer join 
?inner_join
full_join( 
    left_joined_df1_nmc_only, 
    mfl_hospitals, 
    by = c( "")
    )%>%view()
    filter( !is.na(standard_facility_NMC))%>%
    select( 
        standard_facility_NMC, standard_facility_MFL, facility, primary_name, other_name, types, province, everything() 
    )%>%
    mutate( 
        across( c(latitude, longitude), ~ gsub( ",", ".", .)%>%as.numeric() )
    )%>%
    filter( 
        ! (is.na(latitude) & is.na(longitude)
        )
    )-> left_joined_df1_nmc_only_hospitals
)

######################

# Check matches of province 

######################

# we now have a full list of facilitis on MFL that have been matched to NMC. It is certinly not a perfect match. 
left_joined_df1$province%>%unique%>%sort
left_joined_df1$prov_%>%unique%>%sort



# We will get a shape file later that sees where the coordinates lie and then assign the district and subdistrict there. 

# Get some NMC data to link to the MFL 

new_master %>% 
filter( year %in% 2024)%>%
    group_by( 
        case_type, facility 
    )%>%summarise( n = n() )-> 
    new_master_data
new_master_data



    left_join( 
        left_joined_df1, 
        new_master_data, 
        by = c("facility" = "facility")
    )->
    mfl_with_data 

mfl_with_data%>%view()



# Mapping coordinates
library(sf)

# Convert to sf object
sf_data <- st_as_sf(
  left_joined_df1,
  coords = c("longitude", "latitude"), # Specify coordinate columns
  crs = 4326                          # Set CRS (WGS84)
)

# View the sf object
print(sf_data)


ggplot( 
    data = sf_data%>%filter( prov_ == "WC")
    ) +
    geom_sf(aes( color = types)) +
    theme( )

    # plot the points of each facility 

# Map this with leaflet 

library(leaflet)

                leaflet() %>%
                addTiles() %>%
                addMarkers(data = sf_data%>%filter( prov_ == "WC"), popup = ~facility)


                # Let us 

                # Map 
                mfl %>% 
                    filter( 
                        !is.na(longitude) & !is.na(latitude)
                    )-> 
                    mfl_for_sf
                st_as_sf(
                mfl_for_sf,
                coords = c("longitude", "latitude"), # Specify coordinate columns
                crs = 4326                          # Set CRS (WGS84)
                )-> 
                mfl_as_sf
                mfl_as_sf$types%>%tabyl( )%>%arrange(n)
                mfl_as_sf%>%filter( 
                    grepl( "hospital", ignore.case = TRUE, types)
                    )-> 
                    mfl_as_sf_hospitals

                leaflet() %>%
                addTiles() %>%
                addMarkers(data = mfl_as_sf_hospitals, popup = ~primary_name)

                # and ggplot 

# We want to plot and link hospitals from MFL to hspitals on NMC 

left_joined_df1%>%
    filter( 
        grepl( "hospital", ignore.case = TRUE, types)
    )-> 
    left_joined_df1_hospitals




