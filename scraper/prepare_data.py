"""
prepare_data.py
Cleans istanbul_apartment_prices_2026.csv and outputs a
cleaned_listings CSV ready for R analysis.

Usage:
    python scraper/prepare_data.py --input data/raw/istanbul_apartment_prices_2026.csv
"""

import argparse
from pathlib import Path
from datetime import datetime

import numpy as np
import pandas as pd

OUTPUT_DIR = Path(__file__).parent.parent / "data" / "cleaned"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


def clean_boolean_col(series: pd.Series) -> pd.Series:
    """Normalise free-text yes/no/0/1 columns to True/False/NA."""
    mapping = {
        "yes": True, "1": True, "true": True,
        "no": False, "0": False, "false": False,
    }
    return series.astype(str).str.strip().str.lower().map(mapping)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", required=True)
    args = ap.parse_args()

    p = Path(args.input)
    if not p.exists():
        p = Path(__file__).parent.parent / args.input
    if not p.exists():
        print(f"[ERROR] Not found: {args.input}")
        return

    print(f"Loading {p} ...")
    df = pd.read_csv(p, encoding="utf-8-sig", low_memory=False)
    print(f"Raw shape: {df.shape}")

    out = pd.DataFrame()

    # ── identifiers ───────────────────────────────────────────
    out["listing_id"]   = df["listing_id"].astype(str).str.strip()
    out["listing_type"] = "satilik"

    # ── location ──────────────────────────────────────────────
    out["district"]      = df["district"].astype(str).str.strip()
    out["neighbourhood"] = df["neighborhood"].astype(str).str.strip()

    # ── price ─────────────────────────────────────────────────
    out["price"] = pd.to_numeric(df["price"], errors="coerce")

    # ── area ──────────────────────────────────────────────────
    out["GrossSquareMeters"] = pd.to_numeric(df["gross_sqm"], errors="coerce")
    out["NetSquareMeters"]   = pd.to_numeric(df["net_sqm"],   errors="coerce")

    # ── price per m² ──────────────────────────────────────────
    # Prefer re-calculating from raw fields so it stays consistent after filtering
    out["price_per_m2"] = np.where(
        out["GrossSquareMeters"].notna() & (out["GrossSquareMeters"] > 0),
        out["price"] / out["GrossSquareMeters"],
        np.nan,
    )

    # ── rooms ─────────────────────────────────────────────────
    out["rooms"]       = pd.to_numeric(df["rooms"],       errors="coerce")
    out["halls"]       = pd.to_numeric(df["halls"],       errors="coerce")
    out["total_rooms"] = pd.to_numeric(df["total_rooms"], errors="coerce")

    # ── floor info ────────────────────────────────────────────
    out["floor"]          = pd.to_numeric(df["floor"],        errors="coerce")
    out["floor_category"] = df["floor_category"].astype(str).str.strip().replace("nan", np.nan)
    out["total_floors"]   = pd.to_numeric(df["total_floors"], errors="coerce")

    # ── building characteristics ──────────────────────────────
    out["building_age"]       = pd.to_numeric(df["building_age"], errors="coerce")
    out["building_type"]      = df["building_type"].astype(str).str.strip().replace("nan", np.nan)
    out["building_condition"] = df["building_condition"].astype(str).str.strip().replace("nan", np.nan)

    # ── heating ───────────────────────────────────────────────
    out["heating_type"] = df["heating_type"].astype(str).str.strip().replace("nan", np.nan)
    out["fuel_type"]    = df["fuel_type"].astype(str).str.strip().replace("nan", np.nan)

    # ── unit details ──────────────────────────────────────────
    out["bathroom_count"] = pd.to_numeric(df["bathroom_count"], errors="coerce")
    out["furnished"]      = df["furnished"].astype(str).str.strip().replace("nan", np.nan)
    out["usage_status"]   = df["usage_status"].astype(str).str.strip().replace("nan", np.nan)

    # ── complex / site ────────────────────────────────────────
    out["is_in_complex"]  = clean_boolean_col(df["is_in_complex"])
    out["complex_name"]   = df["complex_name"].astype(str).str.strip().replace("nan", np.nan)
    out["maintenance_fee"] = pd.to_numeric(df["maintenance_fee"], errors="coerce")

    # ── legal / financial ─────────────────────────────────────
    out["orientation"]     = df["orientation"].astype(str).str.strip().replace("nan", np.nan)
    out["credit_eligible"] = df["credit_eligible"].astype(str).str.strip().replace("nan", np.nan)
    out["deed_status"]     = df["deed_status"].astype(str).str.strip().replace("nan", np.nan)
    out["exchange"]        = df["exchange"].astype(str).str.strip().replace("nan", np.nan)

    # ── dates ─────────────────────────────────────────────────
    out["last_updated"] = pd.to_datetime(df["last_updated"], errors="coerce")
    out["scraped_at"]   = pd.to_datetime(df["scraped_at"],   errors="coerce")

    # ── filter: remove junk prices and areas ──────────────────
    before = len(out)
    out = out[
        out["price"].notna()
        & (out["price"] >= 100_000)
        & (out["price"] <= 500_000_000)
        & out["GrossSquareMeters"].notna()
        & (out["GrossSquareMeters"] >= 20)
        & (out["GrossSquareMeters"] <= 600)
    ].copy()
    print(f"Filtered out {before - len(out)} junk rows.")

    # ── remove price outliers (1st–99th pct) ─────────────────
    lo, hi = out["price"].quantile(0.01), out["price"].quantile(0.99)
    out = out[(out["price"] >= lo) & (out["price"] <= hi)].copy()

    out = out.reset_index(drop=True)

    # ── report ────────────────────────────────────────────────
    print(f"\nFinal shape     : {out.shape}")
    print(f"With district   : {out['district'].notna().sum()} / {len(out)}")
    print(f"Price range     : {out['price'].min():,.0f} – {out['price'].max():,.0f} TL")
    print(f"Median price    : {out['price'].median():,.0f} TL")
    print(f"Median gross m² : {out['GrossSquareMeters'].median():.0f} m²")
    print(f"Median net m²   : {out['NetSquareMeters'].median():.0f} m²")
    print(f"\nTop districts :")
    print(out["district"].value_counts().head(15).to_string())

    stamp = datetime.now().strftime("%Y%m%d_%H%M")
    dst   = OUTPUT_DIR / f"cleaned_listings_{stamp}.csv"
    out.to_csv(dst, index=False, encoding="utf-8-sig")
    print(f"\n✓ Saved → {dst}")


if __name__ == "__main__":
    main()