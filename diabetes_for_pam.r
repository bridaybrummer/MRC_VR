# Diabetes for Pam 

library(NMCleaner)
library(data.table)
library(arrow)

dt <- read_feather("LGH_MasterFile_preCollapsed2022.feather") %>% as.data.table()

# we want to investiagate diabetes codes 
dt%>%names() 

names(dt)
dt$UnderlyingCause%>%unique
dt$Code_Broad%>%unique
dt$Code_Main%>%unique
dt$DeathProvince

dt[grepl("E10-E14", LGH_CauseGroup) & epi_year %in% 2019:2022, ] -> dt_E_codes

# validate ages 
dt_E_codes[agegroup == "0-4", ]-> dt_E_codes_0_4
dt_E_codes_0_4$BirthYear %>% unique()
dt_E_codes_0_4
dt_E_codes_0_4 %>%
    ggplot()+ 
    geom_histogram(
        aes( x = BirthYear), 
        binwidth = 1
    ) + 
    facet_wrap(
        ~epi_year, 
        nrow = 4
    ) + theme_minimal() -> 
    BirthYear_plot



dt_E_codes[, 
    .(count = sum(count)), 
    by = .(LGH_CauseGroup, epi_year, epi_week, agegroup)
    ]-> dt_E_codes_collapsed

dt_E_codes_collapsed

dt_E_codes_collapsed%>%
        ggplot() + 
        geom_line(
            aes( 
                x = epi_week, 
                y = count, 
                color = as.factor(epi_year),
                #group = epi_year
            )
        )+ 
        facet_wrap(
            ~agegroup, 
            scales = "free_y", 
            nrow = 6) + 
            theme_minimal() -> 
            E_codes_2019_2022_plot

# look at cause codes in amogng 0-4 only 

dt_E_codes%>%names() 

dt_E_codes$CauseA
dt_E_codes$CauseB
dt_E_codes$CauseC
dt_E_codes$CauseD

# Cause A 
dt_E_codes[agegroup == "0-4" & epi_year %in% 2019:2022, ] %>%
    dplyr::select( 
        epi_year, 
        CauseA, 
    )%>%
    tbl_summary(
        by = epi_year,
        sort = list( everything() ~ "frequency")
    )-> CauseA_table_0_4

# Cause B 
    dt_E_codes[agegroup == "0-4" & epi_year %in% 2019:2022, ] %>%
        dplyr::select(
            epi_year,
            CauseB,
        ) %>%
        tbl_summary(
            by = epi_year,
            sort = list(everything() ~ "frequency")
        ) -> CauseB_table_0_4

# Cause C
dt_E_codes[agegroup == "0-4" & epi_year %in% 2019:2022, ] %>%
    dplyr::select(
        epi_year,
        CauseC,
    ) %>%
    tbl_summary(
        by = epi_year,
        sort = list(everything() ~ "frequency")
    ) -> CauseC_table_0_4



dt_E_codes$age
dt_E_codes[agegroup =="0-4", age]%>%summary() 

dt[epi_year == "2021"] %>% nrow()
dt[epi_year == "2022"]%>%nrow() 

dt[, .(count = sum(count)),
    by = .( epi_year)
][order( epi_year)]


# find most common cause A codes in 0-4 age group
dt_E_codes[agegroup == "0-4" & epi_year %in% 2022,
    .(count = sum(count)),
    by = .(CauseA)
][order(-count)][1:5, ]-> top_E_immediate_causes


dt_E_codes[agegroup %in%c( "0-4", "5-39")  & CauseA %in% top_E_immediate_causes$CauseA, 
    .(count = sum(count)), 
    by = .(CauseA, agegroup, epi_year, epi_week)
    ]%>%
    ggplot() + 
        geom_line(
            aes( 
                x = epi_week, 
                y = count, 
                color = as.factor(CauseA),
                #group = epi_year
            )
        )+
        facet_wrap(
            epi_year ~ agegroup, 
            scales = "free_y", 
            nrow = 4) + 
            theme_minimal()-> 
            top_E_immediate_causes_plot


dt_collapsed <- dt[, .(count = sum(count)),
    by = .(
        agegroup, sex, LGH_Cause, LGH_Cause_encoded, covid,
        weekstart, epi_week, epi_year
    )
]

# place of death 

dt%>%names() 


# show 0-4 and 5-39  in 2022 faceted with province
dt_E_codes[agegroup %in% c("0-4", "5-39") & epi_year == 2022, 
    .(count = sum(count)), 
    by = .(DeathProvince, agegroup, epi_week)
    ] %>%
    ggplot() + 
        geom_line(
            aes( 
                x = epi_week, 
                y = count, 
                color = agegroup,
                #group = epi_year
            )
        )+
        scale_y_continuous(
            # whole numbers only 
           # labels = scales::label_number(accuracy = 1),
            #breaks = scales::pretty_breaks(n = 5)
        )+
        facet_wrap(
            DeathProvince~. , 
            scales = "free_y", 
            nrow =10) + 
            theme_minimal()-> 
            DeathProvince_0_4_5_39_2022_plot

            DeathProvince_0_4_5_39_2022_plot
