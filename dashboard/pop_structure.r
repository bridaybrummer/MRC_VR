# add population structure to VR data 
# modelled data at province level
read_feather("LGH_BaselinePredictions_ProvinceLevel_TempPop.feather") -> dt_collapse_prov

# modelled data at national level
read_feather("LGH_AnalysisData_2022.feather") -> dt_analysis

library( NMCleaner )

NMCleaner::pop -> pop

names(pop)
pop$Age %>% unique()
pop$prov %>% unique()
pop$Sex %>% unique()

names(dt_collapse_prov)
dt_analysis$agegroup_f %>% unique()
dt_collapse_prov$province_f %>% unique()
dt_collapse_prov$sex_f %>% unique()


pop%>%
mutate( agegroup_f = 
    factor( 
        dplyr::case_when( 
            Age %in% "0-4" ~ "0-4",
            Age %in% c("5-9", "10-14", "15-19", "20-25", "25-29", "30-34", "35-40") ~ "5-39",
            Age %in% c("40-45", "45-59", "50-54", "55-59" ) ~ "40-59",
            Age %in% c("60-64", "65-69") ~ "60-69",
            Age %in% c("70-74", "75-79") ~ "70-79",
            Age %in% c( "80+") ~ "80+"
    ), 
        levels = dt_analysis$agegroup_f %>% levels()
    ), 
    province_f = factor(
        dplyr::case_when( 
            prov %in% "GT" ~ "Gauteng",
            prov %in% "LIM" ~ "Limpopo",
            prov %in% "MP" ~ "Mpumalanga",
            prov %in% "NW" ~ "North West",
            prov %in% "FS" ~ "Free State",
            prov %in% "KZN" ~ "KwaZulu-Natal",
            prov %in% "EC" ~ "Eastern Cape",
            prov %in% "WC" ~ "Western Cape",
            prov %in% "NC" ~ "Northern Cape"
        ), 
        levels = dt_collapse_prov$province_f %>% levels()
), 
sex_f = 
    factor(dplyr::case_when(
        Sex ==  "Male" ~ 1, 
        Sex == "Female" ~ 2
    ),
    levels = dt_collapse_prov$sex_f %>% levels()
),
epi_year = as.numeric(Year)
) %>%
    dplyr::group_by(
        epi_year,
        province_f,
        agegroup_f,
        sex_f
    )%>%
    dplyr::summarise( 
        pop = sum(Population)
    )%>%as.data.table()-> 
    pop_collapsed_prov


save(pop_collapsed_prov, file = "pop_collapsed_prov.RData")

