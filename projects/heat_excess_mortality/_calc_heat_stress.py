#!/usr/bin/env python3
"""
_calc_heat_stress.py
====================
Compute weekly heat stress metrics from ERA5-Land NetCDF files and export
a CSV for use in the R excess mortality analysis.

Heat stress metrics computed
----------------------------
1. **Apparent Temperature (AT)** — Steadman (1994) simplified form:
       AT = T + 0.33 * e - 0.70 * ws - 4.00     [°C]
   where e = vapour pressure (hPa), ws = wind speed (m/s, assumed 1.5 m/s if unavailable).

2. **Simplified WBGT** — Stull (2011) wet-bulb approximation:
       Tw = T * atan(0.151977 * (RH + 8.313659)^0.5)
            + atan(T + RH) - atan(RH - 1.676331)
            + 0.00391838 * RH^1.5 * atan(0.023101 * RH) - 4.686035
       WBGT_simplified = 0.567 * T + 0.393 * e + 3.94    [°C]
   (shaded/indoor WBGT; outdoor WBGT requires globe temperature / solar radiation)

Derived from ERA5-Land variables:
- t2m  (2m temperature, K)  → T = t2m − 273.15  [°C]
- d2m  (2m dewpoint, K)     → Td = d2m − 273.15 [°C]
- Relative humidity: RH = 100 * exp(17.625*Td / (243.04+Td)) / exp(17.625*T / (243.04+T))
- Vapour pressure: e = 6.112 * exp(17.67*Td / (Td + 243.5))  [hPa]

Spatial aggregation
-------------------
Province-level population-weighted means are computed by masking the ERA5 raster
with South African provincial shapefiles (gadm41_ZAF_1.shp, downloaded from GADM).

Weekly aggregation
------------------
- Daily maximum heat stress is computed from sub-daily timesteps.
- Epidemiological week (ISO) is derived from the date.

Output
------
  data/era5/heat_stress_weekly_prov.csv
  Columns: epi_year, epi_week, week_start, province, mean_at, max_at, mean_wbgt, max_wbgt, n_days

Prerequisites
-------------
  pip install xarray netCDF4 geopandas rasterio numpy pandas scipy tqdm
  # GADM province shapefile (ZAF Level 1):
  # https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_ZAF_shp.zip
  # Unzip to: data/shapefiles/gadm41_ZAF_1.shp
"""

import os
import glob
import warnings
import numpy as np
import pandas as pd
import xarray as xr
import geopandas as gpd
from tqdm import tqdm

warnings.filterwarnings("ignore")

# ── Paths ─────────────────────────────────────────────────────────────────────
ERA5_DIR   = "data/era5"
SHP_PATH   = "data/shapefiles/gadm41_ZAF_1.shp"
OUTPUT_CSV = os.path.join(ERA5_DIR, "heat_stress_weekly_prov.csv")


# ── Helper: Kelvin → Celsius ──────────────────────────────────────────────────
def k2c(k: np.ndarray) -> np.ndarray:
    return k - 273.15


# ── Saturation vapour pressure (Magnus formula) ───────────────────────────────
def sat_vp(T_c: np.ndarray) -> np.ndarray:
    """Saturation vapour pressure [hPa] from temperature [°C]."""
    return 6.1078 * np.exp(17.27 * T_c / (T_c + 237.3))


# ── Relative humidity from T and Td ──────────────────────────────────────────
def relative_humidity(T_c: np.ndarray, Td_c: np.ndarray) -> np.ndarray:
    """RH [%] from 2m temperature and dew-point temperature [°C]."""
    return 100.0 * sat_vp(Td_c) / sat_vp(T_c)


# ── Vapour pressure from dew point ───────────────────────────────────────────
def vapour_pressure(Td_c: np.ndarray) -> np.ndarray:
    """Actual vapour pressure [hPa] from dew-point temperature [°C]."""
    return 6.112 * np.exp(17.67 * Td_c / (Td_c + 243.5))


# ── Apparent Temperature (Steadman 1994 / Australian BoM) ────────────────────
def apparent_temperature(T_c: np.ndarray, e_hpa: np.ndarray,
                         ws_ms: float = 1.5) -> np.ndarray:
    """Apparent temperature [°C]."""
    return T_c + 0.33 * e_hpa - 0.70 * ws_ms - 4.0


# ── Simplified WBGT (Liljegren 2008 / ISO 7933 shaded approximation) ─────────
def wbgt_simple(T_c: np.ndarray, e_hpa: np.ndarray) -> np.ndarray:
    """Simplified (shaded/indoor) WBGT [°C]."""
    return 0.567 * T_c + 0.393 * e_hpa + 3.94


# ── Province masking ──────────────────────────────────────────────────────────
def load_provinces(shp_path: str) -> gpd.GeoDataFrame:
    gdf = gpd.read_file(shp_path)
    gdf = gdf.rename(columns={"NAME_1": "province"})
    return gdf[["province", "geometry"]]


def spatial_mean_by_province(da: xr.DataArray, provinces_gdf: gpd.GeoDataFrame,
                              lons: np.ndarray, lats: np.ndarray) -> dict:
    """
    Simple containment-based aggregation: assign each ERA5 grid cell
    to the province that contains its centroid, then take the unweighted
    mean over grid cells per province.
    Returns {province: scalar_mean}.
    """
    import geopandas as gpd
    from shapely.geometry import Point

    # Build lookup table (grid_lon, grid_lat) → province (done once externally)
    # For the first call we build it; subsequent calls reuse.
    if not hasattr(spatial_mean_by_province, "_lookup"):
        records = []
        for lo in lons:
            for la in lats:
                pt = Point(float(lo), float(la))
                hit = provinces_gdf[provinces_gdf.geometry.contains(pt)]
                prov = hit["province"].values[0] if len(hit) > 0 else None
                records.append((float(lo), float(la), prov))
        spatial_mean_by_province._lookup = pd.DataFrame(
            records, columns=["lon", "lat", "province"]
        ).dropna(subset=["province"])

    lookup = spatial_mean_by_province._lookup
    arr = da.values  # shape: (nlat, nlon) or (nlon,) depending on dims

    # Build a flat series of (lon, lat, value)
    vals = {}
    for _, row in lookup.iterrows():
        lon_idx = int(np.argmin(np.abs(lons - row["lon"])))
        lat_idx = int(np.argmin(np.abs(lats - row["lat"])))
        prov = row["province"]
        v = float(arr[lat_idx, lon_idx]) if arr.ndim == 2 else float(arr[lon_idx])
        vals.setdefault(prov, []).append(v)

    return {p: float(np.nanmean(v)) for p, v in vals.items()}


# ── Main processing loop ──────────────────────────────────────────────────────
def process_files() -> pd.DataFrame:
    nc_files = sorted(glob.glob(os.path.join(ERA5_DIR, "era5_land_SA_summer_*.nc")))
    if not nc_files:
        raise FileNotFoundError(
            f"No ERA5-Land files found in {ERA5_DIR}. "
            "Run _download_era5.py first."
        )

    print(f"Found {len(nc_files)} ERA5-Land file(s).")

    if os.path.exists(SHP_PATH):
        provinces_gdf = load_provinces(SHP_PATH)
        use_provinces = True
    else:
        print(
            f"WARNING: Shapefile not found at {SHP_PATH}. "
            "National-level aggregation will be used instead."
        )
        use_provinces = False

    all_records = []

    for nc_path in tqdm(nc_files, desc="Processing ERA5 files"):
        ds = xr.open_dataset(nc_path)

        # Variable names in ERA5-Land NetCDF
        t2m_var = "t2m" if "t2m" in ds else "VAR_2T"
        d2m_var = "d2m" if "d2m" in ds else "VAR_2D"

        lons = ds["longitude"].values
        lats = ds["latitude"].values

        # Group by date, compute daily maximum of each heat stress metric
        times = pd.to_datetime(ds["time"].values)
        dates = np.unique(times.date)

        for date in tqdm(dates, desc=f"  Daily max {os.path.basename(nc_path)}", leave=False):
            day_mask = times.date == date
            t2m_day = k2c(ds[t2m_var].isel(time=day_mask).values)   # (ntime, nlat, nlon)
            d2m_day = k2c(ds[d2m_var].isel(time=day_mask).values)

            e_day   = vapour_pressure(d2m_day)
            at_day  = apparent_temperature(t2m_day, e_day)
            wbgt_day = wbgt_simple(t2m_day, e_day)

            # Daily maximum across sub-daily timesteps
            at_max   = at_day.max(axis=0)    # (nlat, nlon)
            wbgt_max = wbgt_day.max(axis=0)

            # Epidemiological week (ISO)
            epi_year = pd.Timestamp(date).isocalendar().year
            epi_week = pd.Timestamp(date).isocalendar().week

            if use_provinces:
                at_da   = xr.DataArray(at_max,   dims=["latitude", "longitude"],
                                       coords={"latitude": lats, "longitude": lons})
                wbgt_da = xr.DataArray(wbgt_max, dims=["latitude", "longitude"],
                                       coords={"latitude": lats, "longitude": lons})
                at_prov   = spatial_mean_by_province(at_da,   provinces_gdf, lons, lats)
                wbgt_prov = spatial_mean_by_province(wbgt_da, provinces_gdf, lons, lats)
                for prov in at_prov:
                    all_records.append({
                        "date":      str(date),
                        "epi_year":  int(epi_year),
                        "epi_week":  int(epi_week),
                        "province":  prov,
                        "at_daily_max":   round(at_prov[prov],   4),
                        "wbgt_daily_max": round(wbgt_prov[prov], 4),
                    })
            else:
                all_records.append({
                    "date":           str(date),
                    "epi_year":       int(epi_year),
                    "epi_week":       int(epi_week),
                    "province":       "South Africa",
                    "at_daily_max":   round(float(np.nanmean(at_max)),   4),
                    "wbgt_daily_max": round(float(np.nanmean(wbgt_max)), 4),
                })

        ds.close()

    df_daily = pd.DataFrame(all_records)

    # Aggregate to weekly level
    df_weekly = (
        df_daily
        .groupby(["epi_year", "epi_week", "province"])
        .agg(
            n_days         = ("date",          "count"),
            mean_at        = ("at_daily_max",   "mean"),
            max_at         = ("at_daily_max",   "max"),
            mean_wbgt      = ("wbgt_daily_max", "mean"),
            max_wbgt       = ("wbgt_daily_max", "max"),
        )
        .reset_index()
        .round(3)
    )

    df_weekly["week_start"] = df_weekly.apply(
        lambda r: pd.Timestamp.fromisocalendar(int(r["epi_year"]), int(r["epi_week"]), 1).date(),
        axis=1,
    ).astype(str)

    col_order = ["epi_year", "epi_week", "week_start", "province",
                 "n_days", "mean_at", "max_at", "mean_wbgt", "max_wbgt"]
    df_weekly = df_weekly[col_order].sort_values(["province", "epi_year", "epi_week"])

    return df_weekly


# ── Entry point ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    print("Computing weekly heat stress metrics from ERA5-Land …")
    df = process_files()
    df.to_csv(OUTPUT_CSV, index=False)
    print(f"\nSaved {len(df):,} weekly records → {OUTPUT_CSV}")
    print(df.head(10).to_string(index=False))
