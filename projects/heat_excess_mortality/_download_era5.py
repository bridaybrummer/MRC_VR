#!/usr/bin/env python3
"""
_download_era5.py
=================
Download ERA5-Land hourly data (2m temperature + 2m dew point) for South Africa
over the extended austral summer (September–April) for years 2010–2022.

Prerequisites
-------------
1. Register at https://cds.climate.copernicus.eu and accept the dataset licence.
2. Install the CDS API client:
       pip install cdsapi
3. Create a credentials file at ~/.cdsapirc:
       url: https://cds.climate.copernicus.eu/api

        key: ae734f7e-c24a-429a-886a-1001c920e5ba


```{terminal}
cat > "$HOME/.cdsapirc" << 'EOF'
url: https://cds.climate.copernicus.eu/api
key: YOUR-API-KEY-HERE
EOF
```


What is downloaded
------------------
Dataset : reanalysis-era5-land
Variables: 2m_temperature, 2m_dewpoint_temperature
Area    : South Africa bounding box  [N=-22, W=16, S=-35, E=33]
Months  : Sep (09), Oct (10), Nov (11), Dec (12), Jan (01), Feb (02), Mar (03), Apr (04)
Hours   : 06:00, 12:00, 15:00, 18:00 (local time roughly equivalent to SAST UTC+2)
Format  : NetCDF (.nc)

Files are downloaded one calendar year at a time to stay within the CDS API
request size limits. Each file covers the *austral summer that begins* in that
year (e.g. year=2015 → Sep 2015 – Apr 2016), matching the recommended Sep–Apr
analysis window.

Output: data/era5/ directory (created automatically)
   era5_land_SA_summer_<YYYY>.nc  (one file per start-year)

CDO post-processing (optional, for large datasets)
---------------------------------------------------
# Daily maximum temperature from hourly data:
   cdo daymax era5_land_SA_summer_2015.nc era5_land_SA_summer_2015_daymax.nc
# Merge all years:
   cdo mergetime era5_land_SA_summer_*.nc era5_land_SA_summer_all.nc

References
----------
ERA5-Land overview: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land
ERA5 Single-Level: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels
Stull (2011) wet-bulb approx: doi:10.1175/JAMC-D-11-0143.1
"""

import os
import cdsapi

# ── Configuration ─────────────────────────────────────────────────────────────

# South Africa bounding box: [North, West, South, East] (decimal degrees)
AREA_SA = [-22.0, 16.0, -35.0, 33.0]

# Start years: each entry covers Sep <YEAR> – Apr <YEAR+1>
START_YEARS = list(range(2010, 2023))   # 2010 → 2022 start-years

# Extended austral summer months split by calendar year
MONTHS_Y0 = ["09", "10", "11", "12"]   # Sep–Dec of the start year
MONTHS_Y1 = ["01", "02", "03", "04"]   # Jan–Apr of the following year

# Sub-daily hours of interest (SAST = UTC+2; 06–18 local time)
HOURS = ["06:00", "09:00", "12:00", "15:00", "18:00"]

ALL_DAYS = [f"{d:02d}" for d in range(1, 32)]

OUTPUT_DIR = "data/era5"
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ── ERA5-Land variable list ───────────────────────────────────────────────────
# Using ERA5-Land (preferred for health analyses at land surface)
ERA5_LAND_VARS = [
    "2m_temperature",           # t2m  [K]  → °C after offset
    "2m_dewpoint_temperature",  # d2m  [K]  → used to derive relative humidity / WBGT
]

# If you prefer ERA5 Single-Level (global, coarser, but includes pressure levels):
# Change dataset_id below and uncomment additional variables as needed.
DATASET_ID = "reanalysis-era5-land"

# ── CDS API client ────────────────────────────────────────────────────────────
c = cdsapi.Client()


def request_era5_land_austral_summer(start_year: int) -> None:
    """Download ERA5-Land for the austral summer starting in `start_year`."""

    end_year = start_year + 1
    out_file = os.path.join(OUTPUT_DIR, f"era5_land_SA_summer_{start_year}.nc")

    if os.path.exists(out_file):
        print(f"  [SKIP]  {out_file} already exists.")
        return

    print(f"  [DOWNLOAD]  Austral summer {start_year}/{end_year} → {out_file}")

    # Download Sep–Dec of start_year
    c.retrieve(
        DATASET_ID,
        {
            "product_type": "reanalysis",
            "variable": ERA5_LAND_VARS,
            "year": [str(start_year)],
            "month": MONTHS_Y0,
            "day": ALL_DAYS,
            "time": HOURS,
            "area": AREA_SA,
            "format": "netcdf",
        },
        out_file.replace(".nc", "_p1.nc"),
    )

    # Download Jan–Apr of end_year
    c.retrieve(
        DATASET_ID,
        {
            "product_type": "reanalysis",
            "variable": ERA5_LAND_VARS,
            "year": [str(end_year)],
            "month": MONTHS_Y1,
            "day": ALL_DAYS,
            "time": HOURS,
            "area": AREA_SA,
            "format": "netcdf",
        },
        out_file.replace(".nc", "_p2.nc"),
    )

    # Merge the two parts with CDO if available
    p1 = out_file.replace(".nc", "_p1.nc")
    p2 = out_file.replace(".nc", "_p2.nc")
    merge_ok = os.system(f"cdo mergetime {p1} {p2} {out_file}") == 0
    if merge_ok:
        os.remove(p1)
        os.remove(p2)
        print(f"    Merged → {out_file}")
    else:
        print(
            f"    CDO not available — leaving separate files:\n"
            f"      {p1}\n      {p2}\n"
            f"    Rename the _p1/_p2 files manually and merge later."
        )


# ── Main ──────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    print(f"Downloading ERA5-Land for {len(START_YEARS)} austral summer seasons …")
    for yr in START_YEARS:
        request_era5_land_austral_summer(yr)
    print("\nAll downloads complete.")
    print(
        "\nNext step: run  python _calc_heat_stress.py  to compute weekly heat stress "
        "metrics from the downloaded NetCDF files."
    )
