# look at the number of COVID 19 deaths in Mangaung metro for 2021
library(data.table) 
library(arrow)
library(magrittr)

read_feather(
    "LGH_MasterFile_preCollapsed2022.feather"
)-> master_file

setDT(master_file) 
names(master_file)

master_file$deathdistrictname %>%unique() 
master_file$DeathInst %>% unique()

master_file[
    deathdistrictname == "Mangaung" ,  
    .(n_deaths = .N),
    by = .(epi_year)
    ]-> 
mangaung_deaths_by_year

mangaung_deaths_by_year%>%print() 

master_file[
    deathdistrictname == "Mangaung",
    .(n_deaths = .N),
    by = .(epi_year, LGH_Cause, DeathInst)
] ->
mangaung_deaths_by_year


# use haven labels to encode the DeathInst variable
library(haven)
mangaung_deaths_by_year$DeathInst <- as_factor(mangaung_deaths_by_year$DeathInst)


mangaung_deaths_by_year[epi_year %in% 2021, ][, 
.(n_deaths = sum(n_deaths)), by = DeathInst
    ][, 
    prop_inst := n_deaths / sum(n_deaths)]-> 
    propr_deathInst

    propr_deathInst%>%print()

mangaung_deaths_by_year[LGH_Cause %in% "U07" & epi_year %in% 2021][, 
    prop_inst := n_deaths / sum(n_deaths)]-> 
    propr_deathInst_u07

    propr_deathInst_u07%>%print() 


