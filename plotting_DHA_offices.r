# plotting DHA offices

# Limitations
# No graular (census enumeration area) population data, therefore cannot measure number of people within a distance of an office
# Offices will be of varying size and capacity, which is not accounted for here.
# By using half the median distance between the office as a proxy, we count most offices twice (once for each side of the radius), we could create pairs but will need to account for where there are odd numbers of offices in an area.
# also, distance is as the crow flies and doesnt account for roads (and surface of roads) 

# read in dta files 
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(ggtext)
library(gtsummary)
library(data.table)


haven::read_dta("DHA_offices_2024.dta") -> dha


glimpse( dha)

dha %>%setDT() 

dha %>%
    NMCleaner::mutate_district_name_v2(
        "dc_name_2016"
    )  %>%  select( 
       district_standard, 
       office_longitude, 
       office_latitude, 
       off
    )%>%as.data.table() -> dha

    dha[
        , 
        .(N_off = .N), 
        by = district_standard
    ]-> 
    N_off_district
    


    NMCleaner::pop%>%
        filter( 
            Year == 2025
        )%>%
            group_by( district_standard) %>%
            summarise( pop = sum( Population ))%>%
            na.omit() %>%
            as.data.table() -> 
            pop_by_district

dha$district_standard%>%unique() %>%length() == nrow( dha)
pop_by_district$district_standard

left_join( 
    N_off_district, 
    pop_by_district, 
    by = "district_standard"
)%>%
    mutate( 
        office_per_pop= N_off/ pop* 1000000
    )-> 
    offic_per_pop_dist

# read in from local drive since the package shape files for districts is not working (?)
load(
    "/Users/briday/Desktop/SAFETP/CLA/NMCleaner/data/shape_files.rda"
)
shape_files$districts-> shape_dist

shape_dist%>%
    mutate( 
        district_area_in_km2 = as.numeric( sf::st_area( shape_dist))/1000000
    )-> shape_dist

shape_dist$district_standard
 
 shape_dist%>%
    ggplot() +
    geom_sf()

left_join( 
offic_per_pop_dist, 
shape_dist, 
by = c( "district_standard")
)%>%
mutate(
    pop_density = pop/ district_area_in_km2, # this is therefore the number of people per km2
    # Create N offices per 10 square km density 
    pop_density_per_10km2 = pop_density * 10,
    office_density = N_off/ district_area_in_km2, 
    office_density_per_100km2 = office_density * 100,
    office_per_pop_density_1km = N_off / pop_density, 
    office_per_pop_density_10km = N_off / pop_density_per_10km2
)%>%
sf::st_sf() -> 
off_pop_shape

off_pop_shape%>%
select( 
    district_standard, 
    N_off, 
    pop, 
    pop_density, 
    office_density, 
    office_per_pop_density_1km, 

    pop_density_per_10km2, 
    office_per_pop_density_10km

)

# plot pop density 
off_pop_shape
    ggplot(data =off_pop_shape) + 
    geom_sf(
        aes(fill = pop)
    )+ 
    scale_fill_continuous(
        label = scales::number_format()
    )+ 
    scale_fill_gradient2(
        low = "lightyellow",
        #mid = "orange",
        high = "darkgreen",
        midpoint = 100000,
        name = "Population", 
        label = scales::number_format() 

    )-> 
    SA_pop

    SA_pop 

# plot office N

off_pop_shape
ggplot(data = off_pop_shape) +
    geom_sf(
        aes(fill = N_off)
    )   + scale_fill_gradient2(
        low = "#fff6b9",
        #mid = "orange",
        high = "#6a1414",
       # midpoint = 100000,
        name = "Number of Offices"

    )-> 
    SA_N_off

    SA_N_off

# plot office density 
off_pop_shape
ggplot(data = off_pop_shape) +
    geom_sf(
        aes(fill = office_density_per_100km2)
    ) +
    scale_fill_gradient2(
        low = "lightyellow",
        # mid = "orange",
        high = "red",
        # midpoint = 100000,
        name = "Number of Offices per 100 km ²"
    ) ->
SA_N_off

SA_N_off

# look at some kind of balance between pop density (for area and office density )
off_pop_shape %>% arrange(-office_per_pop_density_1km)
off_pop_shape %>% arrange(pop_density )
off_pop_shape$pop_density %>% summary()
# We will need to exclude "rural" areas (if, where the pop density is very low) 

off_pop_shape %>%
    mutate( 
        office_per_pop_density_1km = if_else(pop_density > 100, office_per_pop_density_1km, NA_real_) # people per km2
    ) %>%
    sf::st_sf() -> filtered_off_pop_shape

ggplot( 
    data = filtered_off_pop_shape) + 
    geom_sf( 
        aes( 
            fill = office_per_pop_density_1km
        )
    )



# Plot latitude and longitude 
ggplot() +
        geom_sf(
           data = NMCleaner::shape_files$provinces,
                    linewidth = 0.1,
                    color = "black", 
    )+
        geom_sf(
           data = NMCleaner::shape_files$provinces,
                aes(
                    fill = prov,
                ), 
                    alpha = 0.5, 
    )+
    geom_sf(
           data = NMCleaner::shape_files$districts,
            fill = "transparent"
    )+
    geom_point(
        data = dha, 
        aes(x = office_longitude, y = office_latitude), 
    ) +
    theme_minimal() +
    labs(
        title = "DHA Offices in South Africa",
        x = "Longitude",
        y = "Latitude"
    )-> 
    figure_1 

figure_1
# make this a plotly map or some other interactive version 
library(sf)
library(ggplot2)
library(plotly)
library(dplyr)
library(glue)
library(scales)


dha_sf <- dha %>%
    st_as_sf(coords = c("office_longitude", "office_latitude"), crs = 4326, remove = FALSE) %>%
    mutate(
        hover_office = glue(
            "<b>Office:</b> {off}<br>",
        )
    )

districts_sf<- off_pop_shape%>%
    mutate( 
        hover_district = glue(
            "<b>District:</b> {district_standard}<br>",
            "<b>Population:</b> {ifelse(is.na(pop), 'NA', comma(pop))}<br>",
            "<b>Number of Offices:</b> {ifelse(is.na(N_off), 'NA', comma(N_off))}<br>",
            "<b>Office Density (per 100 km²):</b> {ifelse(is.na(office_density_per_100km2), 'NA', round(office_density_per_100km2,2))}"
        )
    )


p <- ggplot() +
        geom_sf(
            data = NMCleaner::shape_files$provinces,
            linewidth = 0.1,
            color = "black",
        ) +
            geom_sf(
                data = NMCleaner::shape_files$provinces,
                aes(
                    fill = prov,
                ),
                alpha = 0.5,
            ) +
            geom_sf(
                data = NMCleaner::shape_files$districts,
                fill = "transparent"
            )  + 
    geom_sf(
        data = districts_sf,
        aes(text = hover_district),
        fill = "transparent",
        #colour = "transparent",
        linewidth = 0.2
    ) +
    geom_sf(
        data = dha_sf,
        aes(text = hover_office),
        fill = "transparent",
        size = 2
    ) +
    theme_minimal() +
    labs(
        title = "DHA Offices in South Africa"
    )

p

#ggplotly(p, tooltip = "text") %>%
#    plotly::layout(hoverlabel = list(align = "left"))


# optional upgrade for interactivity 
#ggplotly(p, tooltip = "text", source = "map") %>%
#  highlight(on = "plotly_hover", off = "plotly_unhover", opacityDim = 0.3) %>%
#  layout(hoverlabel = list(align = "left"))


# Look at distances between offices given the limiations of grnaularity


dha_sf <- dha %>%
    st_as_sf(coords = c("office_longitude", "office_latitude"), crs = 4326) %>% # WGS84
    st_transform(3857)

dha
dha_sf

nearest_idx <- st_nearest_feature(dha_sf, dha_sf)

nearest_idx


dist_matrix <- st_distance(dha_sf)

nearest_distance_m <- apply(dist_matrix, 1, function(x) {
    min(x[x > 0]) # exclude zero (self-distance)
})

dha_sf <- dha_sf %>%
    mutate(
        nearest_dha_distance_km = as.numeric(nearest_distance_m) / 1000
    )

nearest_office_id <- apply(dist_matrix, 1, function(x) {
    which.min(ifelse(x == 0, Inf, x))
})

dha_sf <- dha_sf %>%
    mutate(
        nearest_office_id = dha$off[nearest_office_id]
    )

dha_sf%>%
    filter( 
      grepl( "Cape Town", district_standard)
    )-> cape_town_DHA_offices_snippet

dha_sf %>%
    filter(
        grepl("Joha", district_standard)
    )-> joburg_DHA_offices_snippet



dha_sf%>%
    group_by( 
        district_standard
    )%>%
    summarise( 
        mean_nearest_dha_distance_km = mean( nearest_dha_distance_km, na.rm = TRUE), 
        median_nearest_dha_distance_km = median( nearest_dha_distance_km, na.rm = TRUE), 

        half_median_nearest_dha_distance_km = median( nearest_dha_distance_km, na.rm = TRUE)/2
    )-> dha_offices_distance_summary

    dha_offices_distance_summary

# add it to the shape file for plotting
left_join(
    off_pop_shape%>%as_tibble() ,
    dha_offices_distance_summary,
    by = "district_standard"
)-> dha_offices_distance_shape

# present the population whos median distance to the nearest office has a half_median distance of > 10km 
dha_offices_distance_shape%>%
    mutate( 
        half_median_under_10km = if_else( half_median_nearest_dha_distance_km > 10, FALSE, TRUE), 
        half_median_under_20km = if_else( half_median_nearest_dha_distance_km > 20, FALSE, TRUE),
        half_median_under_50km = if_else( half_median_nearest_dha_distance_km > 50, FALSE, TRUE)
    )%>%
    summarise( 
        N_under_10km = sum( pop[ half_median_under_10km == TRUE], na.rm = TRUE),
        N_under_20km = sum( pop[ half_median_under_20km == TRUE], na.rm = TRUE),
        N_under_50km = sum( pop[ half_median_under_50km == TRUE], na.rm = TRUE), 
        total_pop = sum( pop, na.rm = TRUE)
    )-> 
dha_offices_distance_population_summary

dha_offices_distance_population_summary%>%  
    pivot_longer( 
        cols = everything(), 
        names_to = "distance_category", 
        values_to = "population"
    )%>%
    mutate( 
        percentage = population/ dha_offices_distance_population_summary$total_pop * 100
    )



# D) Also export key district indicators as a flat CSV for sharing
district_indicator_table <- off_pop_shape %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
        district_standard,
        N_off,
        pop,
        district_area_in_km2,
        pop_density,
        office_density_per_100km2,
        office_per_pop_density_10km,
        office_per_pop = office_per_pop # from offic_per_pop_dist if present in join
    )

readr::write_csv(district_indicator_table,
    file = file.path(out_dir, "district_me_indicators.csv")
)




# More complex but robust version

library(sf)
library(dplyr)
library(units)

# ---- Helper: weighted quantile (for weighted median)
wtd_quantile <- function(x, w, probs = 0.5) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= probs)[1]]
}

# -------------------------------------------------------------------------
# Helper: wtd_quantile()
#
# Purpose:
#   Computes a weighted quantile (e.g. weighted median) for a numeric vector.
#   This is used to estimate the “typical” distance a resident travels to the
#   nearest DHA office, weighting each distance by the population it represents.
#
# Rationale:
#   Simple medians treat all locations equally. In access-to-service analyses,
#   population-weighted quantiles are preferred because they reflect where
#   people actually live, not just the spatial distribution of points.
#
# Implementation notes:
#   - Removes non-finite values and zero/negative weights
#   - Orders values by distance and accumulates weights
#   - Returns the first value where cumulative weight exceeds the target
#
# Limitations:
#   - Assumes weights are proportional to population represented
#   - Does not interpolate between values (stepwise by design)
#
# This function is intentionally lightweight and dependency-free to keep the
# analysis transparent and easy to maintain.
# -------------------------------------------------------------------------

# ---- Core function
district_access_distance <- function(off_pop_shape,
                                     dha,
                                     district_col = "district_standard",
                                     pop_col      = "pop",
                                     cellsize_m   = 1000) {

  # 1) Make districts and offices sf in a metric CRS

    #off_pop_shape<- off_pop_shape_wc
    #cellsize_m <- 10000
    #district_col <- "district_standard"
    #pop_col <- "pop"

  districts_sf <- off_pop_shape %>%
    
    st_as_sf() %>%
    st_transform(3857)

  offices_sf <- dha %>%
    st_as_sf(coords = c("office_longitude", "office_latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(3857)

  # 2) Create a regular grid of points across all districts, then clip to districts
  grid <- st_make_grid(districts_sf, cellsize = cellsize_m, what = "centers") %>%
    st_as_sf() %>%
    st_intersection(st_union(districts_sf))

  # 3) Allocate grid points to districts
  grid <- st_join(grid, districts_sf[, c(district_col, pop_col)], join = st_within, left = FALSE)

    # If filtering out grids
    #omit.na(grid)

    grid
  # 4) Approximate population per grid point (district population split evenly across points in district)
    grid <- grid %>%
        group_by(.data[[district_col]]) %>%
        mutate(
            n_pts  = dplyr::n(),
            pop_pt = .data[[pop_col]] / n_pts
        ) %>%
        ungroup()


  # 5) Distance from each grid point to nearest office (anywhere; or restrict to same district if desired)
  idx <- st_nearest_feature(grid, offices_sf)
  d_m <- st_distance(grid, offices_sf[idx, ], by_element = TRUE)
  d_km <- drop_units(d_m) / 1000

  grid <- grid %>%
    mutate(dist_to_nearest_office_km = d_km)

  # 6) Summarise per district: population-weighted median and mean
  out <- grid %>%
    st_drop_geometry() %>%
    group_by(.data[[district_col]]) %>%
    summarise(
      pop_total = sum(pop_pt, na.rm = TRUE),
      mean_dist_km = weighted.mean(dist_to_nearest_office_km, w = pop_pt, na.rm = TRUE),
      median_dist_km = wtd_quantile(dist_to_nearest_office_km, w = pop_pt, probs = 0.5),
      p90_dist_km = wtd_quantile(dist_to_nearest_office_km, w = pop_pt, probs = 0.9),
      .groups = "drop"
    )

  return( 
    list( out = out, 
            grid = grid,
            offices_sf = offices_sf
        )
  )
}

# -------------------------------------------------------------------------
# Function: district_access_distance()
#
# Purpose:
#   Estimates how far residents in each district are likely to travel to reach
#   the nearest Department of Home Affairs (DHA) office. The function produces
#   population-weighted access metrics that approximate real-world service
#   accessibility, rather than office-to-office spacing.
#
# Conceptual approach:
#   Instead of pairing DHA offices (which double-counts and fails when the
#   number of offices is odd), the function measures distance from population
#   locations to the nearest DHA office. These distances are then summarised
#   per district using population-weighted statistics.
#
# High-level workflow:
#   1) Convert district polygons and DHA office coordinates to a projected
#      coordinate reference system (metres) to allow valid distance calculations.
#
#   2) Generate a regular grid of points covering all districts and clip the
#      grid to district boundaries. Each grid point represents a small spatial
#      unit where people may live.
#
#   3) Allocate district-level population evenly across grid points within
#      each district. This creates a proxy population surface in the absence
#      of fine-grained census or small-area population data.
#
#   4) For each grid point, calculate the straight-line (Euclidean) distance
#      to the nearest DHA office.
#
#   5) Aggregate these distances to the district level, weighting each distance
#      by the population it represents. This yields:
#        - mean_dist_km: average travel distance for residents
#        - median_dist_km: typical (50th percentile) resident travel distance
#        - p90_dist_km: upper-tail distance capturing access inequality
#
# Inputs:
#   off_pop_shape : sf object of district polygons with population data
#   dha           : data frame of DHA offices with longitude and latitude
#   district_col  : name of the district identifier column
#   pop_col       : name of the population column
#   cellsize_m    : spacing (in metres) of the population proxy grid
#
# Outputs:
#   A data frame with one row per district containing population-weighted
#   distance-to-access indicators suitable for mapping and M&E reporting.
#
# Assumptions and limitations:
#   - Population is assumed to be evenly distributed within districts
#   - Distances are straight-line, not road-network or travel-time distances
#   - Accuracy improves as cellsize_m decreases, at the cost of runtime
#
# Design principles:
#   - Avoids double counting and odd/even office pairing problems
#   - Easily extensible to small-area population data when available
#   - Transparent, reproducible, and defensible for policy analysis
# -------------------------------------------------------------------------
library(progress)

# Get unique provinces
provs <- unique(off_pop_shape$prov)


if(FALSE){
    # Initialize progress bar
    pb <- progress_bar$new(
        format = "  Processing :prov [:bar] :current/:total (:percent) ETA: :eta",
        total = length(provs),
        clear = FALSE,
        width = 80
    )
    
    # Map over all provinces with timing
    dha_access_summary_list <- purrr::map(provs, function(p) {
        pb$tick(tokens = list(prov = p))
        
        start_time <- Sys.time()
        
        off_pop_shape_prov <- off_pop_shape %>%
            filter(prov == p)
        
        result <- district_access_distance(
            off_pop_shape = off_pop_shape_prov,
            dha           = dha,
            district_col  = "district_standard",
            pop_col       = "pop",
            cellsize_m    = 1000
        )
        
        end_time <- Sys.time()
        message(sprintf("  %s completed in %.1f seconds", p, difftime(end_time, start_time, units = "secs")))
        
        result
    }) %>%
        setNames(provs)
    
    # Combine results across provinces
    dha_access_summary <- list(
        out = purrr::map_dfr(dha_access_summary_list, ~ .x$out),
        grid = do.call(rbind, purrr::map(dha_access_summary_list, ~ .x$grid)),
        offices_sf = dha_access_summary_list[[1]]$offices_sf
    )
    
    save(dha_access_summary, file = "dha_access_summary.rda")
} else {
    load("dha_access_summary.rda")
}


district_access_distance <- function(off_pop_shape,
                                     dha,
                                     district_col = "district_standard",
                                     pop_col = "pop",
                                     cellsize_m = 1000,
                                     density_model = c("uniform", "inverse_power", "exponential"),
                                     density_param = 1.5) {
    # density_model:
    #   "uniform"       : Population spread evenly (original method).
    #   "inverse_power" : Populations concentraes near offices (1 / distance^param).
    #                     Param ~ 1 is linear decay, Param > 2 is very sharp urban clustering.
    #   "exponential"   : Population decays exponentially (e ^ (-param * distance)).

    density_model <- match.arg(density_model)

    # 1) Make districts and offices sf in a metric CRS (Web Mercator 3857)
    districts_sf <- off_pop_shape %>%
        st_as_sf() %>%
        st_transform(3857)

    offices_sf <- dha %>%
        st_as_sf(coords = c("office_longitude", "office_latitude"), crs = 4326, remove = FALSE) %>%
        st_transform(3857)

    # 2) Create a regular grid of points across all districts, then clip to districts
    grid <- st_make_grid(districts_sf, cellsize = cellsize_m, what = "centers") %>%
        st_as_sf() %>%
        st_intersection(st_union(districts_sf))

    # 3) Allocate grid points to districts
    grid <- st_join(grid, districts_sf[, c(district_col, pop_col)], join = st_within, left = FALSE)

    # 4) Calculate Distance (Moved UP before population allocation)
    #    We need distance to determine the population weight
    idx <- st_nearest_feature(grid, offices_sf)
    d_m <- st_distance(grid, offices_sf[idx, ], by_element = TRUE)
    d_km <- drop_units(d_m) / 1000

    grid <- grid %>%
        mutate(dist_to_nearest_office_km = d_km)

    # 5) Distribute Population based on Gradient Model
    #    We group by district to ensure the total district population remains constant
    grid <- grid %>%
        group_by(.data[[district_col]]) %>%
        mutate(
            # Calculate raw weights based on chosen model
            weight = case_when(
                density_model == "uniform" ~ 1,

                # Add 1km offset to prevent division by zero or infinite weights at dist=0
                density_model == "inverse_power" ~ 1 / ((dist_to_nearest_office_km + 1)^density_param),
                density_model == "exponential" ~ exp(-density_param * dist_to_nearest_office_km)
            ),

            # Normalize weights so they sum to 1 within the district
            weight_norm = weight / sum(weight, na.rm = TRUE),

            # Allocate district population proportional to weight
            pop_pt = .data[[pop_col]] * weight_norm,

            # Keep track of points for reference
            n_pts = n()
        ) %>%
        ungroup()

    # 6) Summarise per district: population-weighted median and mean
    #    Note: pop_pt now reflects the gradient density
    out <- grid %>%
        st_drop_geometry() %>%
        group_by(.data[[district_col]]) %>%
        summarise(
            pop_total = sum(pop_pt, na.rm = TRUE),
            mean_dist_km = weighted.mean(dist_to_nearest_office_km, w = pop_pt, na.rm = TRUE),
            median_dist_km = wtd_quantile(dist_to_nearest_office_km, w = pop_pt, probs = 0.5),
            p90_dist_km = wtd_quantile(dist_to_nearest_office_km, w = pop_pt, probs = 0.9),
            .groups = "drop"
        )

    return(
        list(
            out = out,
            grid = grid,
            offices_sf = offices_sf
        )
    )
}

# Wrapper function to run district_access_distance across provinces with progress bar
run_district_access_by_province <- function(off_pop_shape,
                                             dha,
                                             district_col = "district_standard",
                                             pop_col = "pop",
                                             cellsize_m = 1000,
                                             density_model = "uniform",
                                             density_param = 1.5) {
    
    # Get unique provinces
    provs <- unique(off_pop_shape$prov)
    
    # Initialize progress bar
    pb <- progress_bar$new(
        format = "  Processing :prov [:bar] :current/:total (:percent) ETA: :eta",
        total = length(provs),
        clear = FALSE,
        width = 80
    )
    
    # Map over all provinces with timing
    access_summary_list <- purrr::map(provs, function(p) {
        pb$tick(tokens = list(prov = p))
        
        start_time <- Sys.time()
        
        off_pop_shape_prov <- off_pop_shape %>%
            filter(prov == p)
        
        result <- district_access_distance(
            off_pop_shape = off_pop_shape_prov,
            dha           = dha,
            district_col  = district_col,
            pop_col       = pop_col,
            cellsize_m    = cellsize_m,
            density_model = density_model,
            density_param = density_param
        )
        
        end_time <- Sys.time()
        message(sprintf("  %s completed in %.1f seconds", p, difftime(end_time, start_time, units = "secs")))
        
        result
    }) %>%
        setNames(provs)
    
    # Combine results across provinces
    combined_result <- list(
        out = purrr::map_dfr(access_summary_list, ~ .x$out),
        grid = do.call(rbind, purrr::map(access_summary_list, ~ .x$grid)),
        offices_sf = access_summary_list[[1]]$offices_sf
    )
    
    return(combined_result)
}




if (FALSE) {
# Example usage:
 dha_access_summary_gradient <- run_district_access_by_province(
     off_pop_shape = off_pop_shape,
     dha = dha,
     cellsize_m = 1000,
     density_model = "inverse_power",
    density_param = 1.5
 )
 save(dha_access_summary_gradient, file = "dha_access_summary_gradient.rda")
} else {
    load("dha_access_summary_gradient.rda")
}

dha_access_summary_gradient -> dha_access_summary

dha_access_summary
dha_access_summary$out
dha_access_summary$grid-> grid
nrow(grid)
grid %>%setDT() 
dha_access_summary$offices_sf-> offices_sf

grid$district_standard%>%unique()

# count the number of people within 10 km 
grid

grid[, 
    within_10km := ifelse( dist_to_nearest_office_km <= 10, TRUE, FALSE), 
    ][,
    within_20km := ifelse( dist_to_nearest_office_km <= 20, TRUE, FALSE)
    ][, .(
    pop_total = sum(pop_pt, na.rm = TRUE),
    pop_within_10km = sum(pop_pt[ within_10km == TRUE], na.rm = TRUE),
    pop_within_20km = sum(pop_pt[ within_20km == TRUE], na.rm = TRUE)
    ), 
    by = .(district_standard)
 ][, 
    pct_within_10km := pop_within_10km/ pop_total * 100,
    ][, 
    pct_within_20km := pop_within_20km/ pop_total * 100
    ]-> 
dha_access_10km_summary_dt
dha_access_10km_summary_dt%>%print() 

grid%>%
    mutate( 
        within_10km = if_else( dist_to_nearest_office_km <= 10, TRUE, FALSE), 
        within_20km = if_else(dist_to_nearest_office_km <=20, TRUE, FALSE)
    )%>%
    group_by( 
        district_standard
    )%>%
    summarise( 
                pop_total = sum( pop_pt, na.rm = TRUE), 
        pop_within_10km = sum( pop_pt[ within_10km == TRUE], na.rm = TRUE), 

        pop_within_20km = sum( pop_pt[ within_20km == TRUE], na.rm = TRUE), 
    )%>%
    mutate( 
        pct_within_10km = pop_within_10km/ pop_total * 100, 
        pct_within_20km = pop_within_20km /pop_total * 100
    )-> dha_access_10km_summary
    
    dha_access_10km_summary
    setDT(dha_access_10km_summary)


# make a totals table 
dha_access_10km_summary[, 
    .(
        pop_total = sum( pop_total, na.rm = TRUE), 
        pop_within_10km = sum( pop_within_10km, na.rm = TRUE), 
        pop_within_20km = sum( pop_within_20km, na.rm = TRUE)
    )
][, 
    pct_within_10km := pop_within_10km/ pop_total * 100
    ][,
    pct_within_20km := pop_within_20km/ pop_total * 100
    ][, 
district_standard := "Total"]-> dha_access_10km_totals

dha_sf%>%
    summarise( 
        mean_nearest_dha_distance_km = mean( nearest_dha_distance_km, na.rm = TRUE), 
        median_nearest_dha_distance_km = median( nearest_dha_distance_km, na.rm = TRUE), 

        half_median_nearest_dha_distance_km = median( nearest_dha_distance_km, na.rm = TRUE)/2
    )%>%
        mutate( 
            district_standard = "Total") -> dha_offices_distance_summary_total

cbind( 
    dha_access_10km_totals, 
    dha_offices_distance_summary_total%>%select( 
        half_median_nearest_dha_distance_km
    )
)%>%
    select( 
        district_standard, 
        pop_total, 
        pop_within_10km, 
        pct_within_10km,
        pop_within_20km, 
        pct_within_20km,
        half_median_nearest_dha_distance_km
    )-> dha_access_10km_totals_combined




dha_access_10km_summary$pop_within_10km %>%sum() 
dha_access_10km_summary$pop_within_20km %>% sum()


left_join( 
    dha_access_10km_summary, 
    dha_offices_distance_summary, 
    by = "district_standard"
)%>%select( 
    district_standard, 
    pop_total, 
    pop_within_10km,
    pct_within_10km, 
    pop_within_20km, 
    pct_within_20km, 
    half_median_nearest_dha_distance_km
)%>%
rbind(., 
dha_access_10km_totals_combined) %>%
flextable::flextable() %>%
flextable::set_header_labels( 
    district_standard = "District", 
    pop_total = "Total Population", 
    pop_within_10km = "Population within 10 km", 
    pct_within_10km = "% Population within 10 km", 
    pop_within_20km = "Population within 20 km", 
    pct_within_20km = "% Population within 20 km", 
    half_median_nearest_dha_distance_km = "Median Distance to Nearest DHA Office (km)"
)%>%
flextable::bold( i  = 1, part = "header")-> dha_access_summary_table

dha_access_summary_table


dha_offices_distance_shape <- off_pop_shape %>%
  left_join(dha_access_summary$out, by = "district_standard")
dha_offices_distance_shape




# view the grid
grid%>%sf::st_sf() -> grid



ggplot() +
    geom_sf(data = off_pop_shape, fill = "grey95", colour = "grey60", linewidth = 0.2) +
    geom_sf(
        data = grid, 
        aes(
            colour = pop_pt, 
          #  text = paste0("Grid point\npop_pt=", round(pop_pt, 1))
        ),
        size = 0.8
    ) +
    scale_colour_viridis_c(
        name = "Population\nper grid point",
        option = "plasma",
        trans = "log10"
    ) +
    theme_minimal() +
    labs(
        title = "Modelled population distribution",
        subtitle = "Each point represents estimated population at 1km grid cell"
    )-> modelled_population_map


# simplification of the grid imaging

library(sf)
library(dplyr)
library(rmapshaper)



simplify_for_plot <- function(x, keep = 0.05, keep_shapes = TRUE, crs_work = 3857) {
    stopifnot(inherits(x, "sf"))

    crs_in <- st_crs(x)

    x %>%
        st_make_valid() %>% # fix invalid rings
        st_zm(drop = TRUE, what = "ZM") %>% # drop Z/M if present (often reduces overhead)
        st_transform(crs_work) %>% # project to planar CRS for stable simplification
        ms_simplify(
            keep = keep,
            keep_shapes = keep_shapes
        ) %>%
        st_transform(crs_in) # back to original CRS for consistent plotting
}

#off_pop_shape_s <- simplify_for_plot(off_pop_shape, keep = 0.05, keep_shapes = TRUE)

# or 
library(sf)
library(rmapshaper)

# shp: your sf object
off_pop_shape_s <- ms_simplify(
    off_pop_shape,
    keep = 0.01, # keep 5% of vertices (tune: 0.01–0.2)
    keep_shapes = TRUE # avoid dropping small polygons
)

ggplot() +
 geom_sf(data = off_pop_shape_s, fill = "grey95", colour = "grey60", linewidth = 0.2) +
    geom_sf(
        data = grid, 
        aes(
            colour = pop_pt, 
          #  text = paste0("Grid point\npop_pt=", round(pop_pt, 1))
        ),
        size = 0.8
    ) +
    scale_colour_viridis_c(
        name = "Population\nper grid point",
        option = "plasma",
        trans = "log10", 
        labels = scales::number_format(accuracy = 1)
    ) +
    theme_minimal() +
    labs(
        title = "Modelled population distribution",
        subtitle = "Each point represents estimated population at 1km grid cell"
    )-> modelled_population_map_s
    
    modelled_population_map_s

#plotly::ggplotly(modelled_population_map_s)  

# simplifying the grid aswell and plotting with geom_point
grid_xy <- grid %>%
  st_transform(st_crs(off_pop_shape_s)) %>%
  mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

ggplot() +
  geom_sf(data = off_pop_shape_s, fill = "grey95", colour = "grey60", linewidth = 0.2) +
  geom_point(
    data = grid_xy,
    aes(x = X, y = Y, colour = pop_pt),
    size = 0.8
  )



ggplot() +
    geom_sf(data = off_pop_shape_s, fill = "grey95", colour = "grey60", linewidth = 0.2) +
    # 50km buffer
    geom_sf(
        data = sf::st_buffer(offices_sf, dist = 50000), 
        fill = "#bdd7e7", 
        color = NA, 
        alpha = 0.3
    ) +
    # 20km buffer
    geom_sf(
        data = sf::st_buffer(offices_sf, dist = 20000), 
        fill = "#6baed6", 
        color = NA, 
        alpha = 0.4
    ) +
    # 10km buffer
    geom_sf(
        data = sf::st_buffer(offices_sf, dist = 10000), 
        fill = "#2171b5", 
        color = NA, 
        alpha = 0.5, 

    ) +
    geom_sf(data = offices_sf, size = 2, 
    aes( 
        text = off
    )) +
    theme_minimal() +
    labs(
        title = "Grid points and DHA offices",
        subtitle = "Buffers: 10km (dark), 20km (medium), 50km (light)"
    ) -> 
    p_with_buffers

    p_with_buffers

plotly::ggplotly(p_with_buffers, tooltip = "text") 
    plotly::layout(hoverlabel = list(align = "left"))-> p_with_grid

offices_sf <- dha %>%
    sf::st_as_sf(coords = c("office_longitude", "office_latitude"), crs = 4326, remove = FALSE) %>%
    sf::st_transform(3857)

p <- ggplot() +
    geom_sf(
        data = off_pop_shape, aes(text = district_standard),
        fill = "grey95", colour = "grey60", linewidth = 0.2
    ) +
    geom_sf(
        data = grid, aes(text = paste0("Grid point\npop_pt=", round(pop_pt, 1))),
        size = 0.8
    ) +
    geom_sf(
        data = offices_sf, aes(text = off),
        size = 2
    ) +
    theme_minimal()

plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(hoverlabel = list(align = "left"))


coverage_by_district <- grid %>%
    mutate(within_10km = dist_to_nearest_office_km <= 10) %>%
    sf::st_drop_geometry() %>%
    group_by(district_standard) %>%
    summarise(
        pop_total = sum(pop_pt, na.rm = TRUE),
        pop_within_10km = sum(pop_pt[within_10km], na.rm = TRUE),
        pct_within_10km = 100 * pop_within_10km / pop_total,
        .groups = "drop"
    )

coverage_by_district

out_dir <- "outputs/dha_me_indicators"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save( 
    dha, 
    N_off_district, 
    pop_by_district, 
    offic_per_pop_dist,
    shape_dist,
    off_pop_shape,
    filtered_off_pop_shape,
    SA_pop,
    SA_N_off,
    modelled_population_map,
    dha_sf,
    districts_sf,
    p,
    p_with_buffers, 
    dha_offices_distance_summary,
    dha_offices_distance_shape,
    dha_offices_distance_population_summary,
    dha_access_summary_table,
    file = file.path(out_dir, "dha_me_indicator_objects.RData")
)

print( paste0(out_dir, "/dha_me_indicator_objects.RData") )

p
