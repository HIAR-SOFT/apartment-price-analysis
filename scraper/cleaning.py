"""
cleaning.py  —  Clean raw scraped CSV using the exact same logic as
                PREPROCESS_FULL.ipynb (adapted for our project structure).

The output CSV has the same column names and types the notebook expects,
so 01_data_loading.R and the R analysis scripts work without changes.

Usage:
    python scraper/cleaning.py --input data/raw/raw_listings_YYYYMMDD_HHMM.csv
"""

import re, argparse
from pathlib import Path
from datetime import datetime

import numpy as np
import pandas as pd

OUTPUT_DIR = Path(__file__).parent.parent / "data" / "cleaned"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# ── Turkish month names (from notebook) ───────────────────────────────────────
TURKISH_MONTHS = {
    "Ocak": "January",   "Şubat": "February", "Mart": "March",
    "Nisan": "April",    "Mayıs": "May",       "Haziran": "June",
    "Temmuz": "July",    "Ağustos": "August",  "Eylül": "September",
    "Ekim": "October",   "Kasım": "November",  "Aralık": "December",
}

# ── Room-count mapping (Turkish notation → number of bedrooms) ────────────────
ROOM_MAP = {
    "Stüdyo": 0, "1+0": 0,
    "1+1": 1,  "2+1": 2,  "3+1": 3,  "4+1": 4,  "5+1": 5,  "6+1": 6,
    "2+2": 3,  "3+2": 4,  "4+2": 5,  "5+2": 6,  "6+2": 7,
    "4+3": 5,  "5+3": 6,  "4+4": 6,
}


# ── Parsing helpers  (mirrored from notebook) ─────────────────────────────────

def parse_currency(series: pd.Series) -> pd.Series:
    """Remove thousand separators + currency suffix, return Int64."""
    return (
        series.astype(str)
        .str.split("TL").str[0]
        .str.replace(r"[^\d]", "", regex=True)
        .replace("", np.nan)
        .astype("Int64")
    )


def parse_area(series: pd.Series) -> pd.Series:
    """'120 m²' → 120 (Int64)."""
    return (
        series.astype(str)
        .str.replace(r"[^\d]", "", regex=True)
        .replace("", np.nan)
        .astype("Int64")
    )


def parse_ordinal_plus(series: pd.Series) -> pd.Series:
    """'4+' or '10-15 yıl' → midpoint int (Int64)."""
    def _convert(val):
        if pd.isna(val):
            return pd.NA
        s = str(val).strip()
        if "yeni" in s.lower():
            return 0
        nums = re.findall(r"\d+", s)
        if len(nums) == 2:
            return (int(nums[0]) + int(nums[1])) // 2
        if len(nums) == 1:
            return int(nums[0])
        return pd.NA
    return series.map(_convert).astype("Int64")


def parse_turkish_date(series: pd.Series) -> pd.Series:
    replaced = series.astype(str).replace(TURKISH_MONTHS, regex=True)
    return pd.to_datetime(replaced, errors="coerce", dayfirst=True)


# ── Main cleaning logic ───────────────────────────────────────────────────────

def clean(df: pd.DataFrame) -> pd.DataFrame:
    out = pd.DataFrame()

    # ── Pass-through identifiers ──────────────────────────────────────────────
    for col in ["listing_url", "listing_type", "listing_id", "scraped_at"]:
        if col in df.columns:
            out[col] = df[col].astype(str).str.strip()

    # ── Location ──────────────────────────────────────────────────────────────
    for col in ["district", "neighbourhood", "address"]:
        if col in df.columns:
            out[col] = df[col].astype(str).str.strip()

    # ── Categorical pass-throughs (same as notebook PASSTHROUGH_CATS) ────────
    passthrough = [
        "UsingStatus", "BuildStatus", "TitleStatus", "HeatingType",
        "StructureType", "BalconyType", "EligibilityForInvestment",
        "ItemStatus", "CreditEligibility", "InsideTheSite",
        "MortgageStatus", "Swap", "Balcony", "IsItVideoNavigable?",
        "NumberOfRooms", "FloorLocation", "KitchenType", "Elevator",
        "Parking", "TitleType",
    ]
    for col in passthrough:
        if col in df.columns:
            out[col] = df[col].astype(str).str.strip()

    # ── Numeric fields ────────────────────────────────────────────────────────
    if "price" in df.columns:
        out["price"] = parse_currency(df["price"])

    for src, dst in [
        ("GrossSquareMeters", "GrossSquareMeters"),
        ("HallSquareMeters",  "HallSquareMeters"),
        ("BalconySquareMeters", "BalconySquareMeters"),
        ("WCSquareMeters",    "WCSquareMeters"),
    ]:
        if src in df.columns:
            out[dst] = parse_area(df[src])

    for src, dst in [
        ("BuildingAge",            "buildingAge"),
        ("NumberFloorsofBuilding", "numberFloorsOfBuilding"),
        ("NumberOfBathrooms",      "numberOfBathrooms"),
        ("NumberOfBalconies",      "numberOfBalconies"),
    ]:
        if src in df.columns:
            raw = df[src].replace("Yok", "0") if src == "NumberOfBathrooms" else df[src]
            out[dst] = parse_ordinal_plus(raw)

    # ── Currency fields ───────────────────────────────────────────────────────
    for src, dst in [
        ("RentalIncome", "rentalIncome"),
        ("Subscription", "subscription"),
    ]:
        if src in df.columns:
            out[dst] = parse_currency(df[src].fillna("0 TL"))

    # ── Dates → derive numeric features ──────────────────────────────────────
    for src, dst in [
        ("AdCreationDate", "adCreationDate"),
        ("AdUpdateDate",   "adUpdateDate"),
    ]:
        if src in df.columns:
            out[dst] = parse_turkish_date(df[src])

    if "adCreationDate" in out.columns and "adUpdateDate" in out.columns:
        out["adUpdateMonth"] = out["adUpdateDate"].dt.month.astype("Int64")
        out["adUpdateYear"]  = out["adUpdateDate"].dt.year.astype("Int64")
        out["adActiveDays"]  = (
            (out["adUpdateDate"] - out["adCreationDate"]).dt.days.astype("Int64")
        )
        out = out.drop(columns=["adCreationDate", "adUpdateDate"])

    # ── Room count → integer ──────────────────────────────────────────────────
    if "NumberOfRooms" in out.columns:
        out["room_count"] = out["NumberOfRooms"].map(ROOM_MAP).astype("Int64")

    # ── Derived: price per m² ─────────────────────────────────────────────────
    if "price" in out.columns and "GrossSquareMeters" in out.columns:
        out["price_per_m2"] = np.where(
            (out["GrossSquareMeters"].notna()) & (out["GrossSquareMeters"] > 0),
            out["price"].astype(float) / out["GrossSquareMeters"].astype(float),
            np.nan,
        )

    # ── Replace Turkish 'unknown' strings with NaN ────────────────────────────
    out = out.replace(["nan", "None", "Bilinmiyor", ""], pd.NA)

    # ── Fill known defaults (from notebook) ───────────────────────────────────
    if "Balcony" in out.columns:
        out["Balcony"] = out["Balcony"].fillna("Yok")
    if "IsItVideoNavigable?" in out.columns:
        out["IsItVideoNavigable?"] = out["IsItVideoNavigable?"].fillna("Hayır")
    if "ItemStatus" in out.columns:
        out["ItemStatus"] = out["ItemStatus"].fillna("Boş")

    # ── Remove price outliers (1st–99th percentile) ───────────────────────────
    price_col = out.get("price") if "price" in out.columns else None
    if price_col is not None:
        prices = out["price"].dropna().astype(float)
        lo, hi = prices.quantile(0.01), prices.quantile(0.99)
        before = len(out)
        out = out[(out["price"].isna()) | ((out["price"].astype(float) >= lo) & (out["price"].astype(float) <= hi))]
        print(f"Outlier removal: dropped {before - len(out)} rows (price outside [{lo:,.0f}, {hi:,.0f}] TL)")

    # ── Remove zero / negative areas ─────────────────────────────────────────
    if "GrossSquareMeters" in out.columns:
        out = out[out["GrossSquareMeters"].isna() | (out["GrossSquareMeters"] > 0)]

    return out.reset_index(drop=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", required=True)
    args = ap.parse_args()

    input_path = Path(args.input)
    if not input_path.exists():
        input_path = Path(__file__).parent.parent / args.input
    if not input_path.exists():
        print(f"[ERROR] Not found: {args.input}")
        return

    print(f"Loading {input_path} …")
    df = pd.read_csv(input_path, encoding="utf-8-sig")
    print(f"Raw shape: {df.shape}")
    print(f"Columns:   {list(df.columns)}")

    if df.empty or len(df.columns) < 2:
        print("[ERROR] File is empty or has no columns. Did scraper/parser run correctly?")
        return

    # Drop duplicate URLs
    df = df.drop_duplicates(subset=["listing_url"]) if "listing_url" in df.columns else df
    print(f"After dedup: {df.shape}")

    cleaned = clean(df)
    print(f"Cleaned shape: {cleaned.shape}")

    stamp = datetime.now().strftime("%Y%m%d_%H%M")
    out   = OUTPUT_DIR / f"cleaned_listings_{stamp}.csv"
    cleaned.to_csv(out, index=False, encoding="utf-8-sig")

    print(f"\n✓  Saved  →  {out}")
    print(f"\nMissing values per column:")
    print(cleaned.isnull().sum()[cleaned.isnull().sum() > 0].to_string())
    print(f"\nNext step (in RStudio):")
    print(f'    source("r-analysis/01_data_loading.R")')

if __name__ == "__main__":
    main()