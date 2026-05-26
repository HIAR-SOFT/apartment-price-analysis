
"""
cleaning.py  —  Clean raw_listings CSV from the working parser.
Handles real sahibinden data as seen in raw_listings_20260527_0007.csv.

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

TURKISH_MONTHS = {
    "Ocak":"January","Şubat":"February","Mart":"March",
    "Nisan":"April","Mayıs":"May","Haziran":"June",
    "Temmuz":"July","Ağustos":"August","Eylül":"September",
    "Ekim":"October","Kasım":"November","Aralık":"December",
}

ROOM_MAP = {
    "Stüdyo":0,"1+0":0,
    "1+1":1,"2+1":2,"3+1":3,"4+1":4,"5+1":5,"6+1":6,
    "2+2":3,"3+2":4,"4+2":5,"5+2":6,"6+2":7,
    "4+3":5,"5+3":6,"4+4":6,
}

# ── Helpers ───────────────────────────────────────────────────

def parse_price(series: pd.Series) -> pd.Series:
    """'7.450.000 TL' → 7450000"""
    return (
        series.astype(str)
        .str.replace(r"[^\d]", "", regex=True)
        .replace("", np.nan)
        .astype("Int64")
    )

def parse_area(series: pd.Series) -> pd.Series:
    """'120 m²' or '120' → 120"""
    return (
        series.astype(str)
        .str.extract(r"(\d+)", expand=False)
        .replace("", np.nan)
        .astype("Int64")
    )

def parse_building_age(series: pd.Series) -> pd.Series:
    """'6-10 arası' → 8,  '0 (Yapım Aşamasında)' → 0,  '21 ve üzeri' → 25"""
    def _conv(val):
        if pd.isna(val): return pd.NA
        s = str(val).lower().strip()
        if "yapım" in s or s.startswith("0"): return 0
        nums = re.findall(r"\d+", s)
        if len(nums) >= 2: return (int(nums[0]) + int(nums[1])) // 2
        if len(nums) == 1: return int(nums[0])
        return pd.NA
    return series.map(_conv).astype("Int64")

def parse_floors(series: pd.Series) -> pd.Series:
    """'3' or '10 ve üzeri' → int"""
    def _conv(val):
        if pd.isna(val): return pd.NA
        nums = re.findall(r"\d+", str(val))
        return int(nums[0]) if nums else pd.NA
    return series.map(_conv).astype("Int64")

def parse_turkish_date(series: pd.Series) -> pd.Series:
    replaced = series.astype(str)
    for tr, en in TURKISH_MONTHS.items():
        replaced = replaced.str.replace(tr, en, regex=False)
    return pd.to_datetime(replaced, errors="coerce", dayfirst=True)

def fix_district(series: pd.Series) -> pd.Series:
    """
    The parser's breadcrumb grabbed nav UI junk like 'Yatay','Dikey'.
    Extract district from address or listing_url instead.
    Real district names are Turkish proper nouns — filter out UI words.
    """
    UI_NOISE = {
        "yatay","dikey","favori","aramalarım","karşılaştır","vazgeç",
        "tüm","ilanları","dolu","boş","git","ekle","size","özel",
    }
    def _clean(val):
        if pd.isna(val): return np.nan
        s = str(val).strip()
        if s.lower() in UI_NOISE or len(s) < 2: return np.nan
        return s
    return series.map(_clean)

def extract_district_from_url(url_series: pd.Series) -> pd.Series:
    """
    sahibinden URLs contain location like:
    /ilan/emlak-konut-satilik-kartal-...  → 'kartal'
    /ilan/emlak-konut-kiralik-besiktas-... → 'besiktas'
    """
    ISTANBUL_DISTRICTS = {
        "besiktas":"Beşiktaş","sariyer":"Sarıyer","sisli":"Şişli",
        "kadikoy":"Kadıköy","uskudar":"Üsküdar","bakirkoy":"Bakırköy",
        "maltepe":"Maltepe","atasehir":"Ataşehir","pendik":"Pendik",
        "kartal":"Kartal","umraniye":"Ümraniye","bagcilar":"Bağcılar",
        "bahcelievler":"Bahçelievler","esenyurt":"Esenyurt",
        "kucukcekmece":"Küçükçekmece","beylikduzu":"Beylikdüzü",
        "gaziosmanpasa":"Gaziosmanpaşa","sultangazi":"Sultangazi",
        "esenler":"Esenler","avcilar":"Avcılar","beyoglu":"Beyoğlu",
        "fatih":"Fatih","eyupsultan":"Eyüpsultan","zeytinburnu":"Zeytinburnu",
        "basaksehir":"Başakşehir","tuzla":"Tuzla","cekmekoy":"Çekmeköy",
        "sancaktepe":"Sancaktepe","sultanbeyli":"Sultanbeyli",
        "silivri":"Silivri","buyukcekmece":"Büyükçekmece",
        "catalca":"Çatalca","arnavutkoy":"Arnavutköy","beykoz":"Beykoz",
        "sile":"Şile","adalar":"Adalar",
    }
    def _extract(url):
        if pd.isna(url): return np.nan
        url_lower = str(url).lower()
        for slug, name in ISTANBUL_DISTRICTS.items():
            if f"-{slug}-" in url_lower or f"/{slug}-" in url_lower:
                return name
        return np.nan
    return url_series.map(_extract)

# ── Main ──────────────────────────────────────────────────────

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

    print(f"Loading {p} …")
    df = pd.read_csv(p, encoding="utf-8-sig", low_memory=False)
    print(f"Raw shape: {df.shape}")

    if df.empty or len(df.columns) < 3:
        print("[ERROR] File is empty or has too few columns.")
        return

    out = pd.DataFrame()

    # ── Identifiers ───────────────────────────────────────────
    for col in ["listing_url","listing_type","scraped_at","listing_id","title"]:
        if col in df.columns:
            out[col] = df[col].astype(str).str.strip()

    # ── listing_type normalise ────────────────────────────────
    if "listing_type" in out.columns:
        out["listing_type"] = out["listing_type"].str.lower().str.strip()

    # ── District: try raw column, fall back to URL extraction ─
    if "district" in df.columns:
        out["district"] = fix_district(df["district"])
    # Fill NaN districts from URL
    url_districts = extract_district_from_url(df.get("listing_url", pd.Series()))
    if "district" not in out.columns:
        out["district"] = url_districts
    else:
        out["district"] = out["district"].where(out["district"].notna(), url_districts)

    # ── Neighbourhood ─────────────────────────────────────────
    if "neighbourhood" in df.columns:
        out["neighbourhood"] = fix_district(df["neighbourhood"])

    # ── Address: use title as fallback if address is garbage ──
    if "address" in df.columns:
        # Keep only if it looks like a real address (contains comma + district-like word)
        addr = df["address"].astype(str)
        out["address"] = addr.where(addr.str.count(",") >= 2, np.nan)

    # ── Price ─────────────────────────────────────────────────
    if "price" in df.columns:
        out["price"] = parse_price(df["price"])

    # ── Area ──────────────────────────────────────────────────
    # Use extra_ fallback columns if main ones are missing
    gross_src = df.get("GrossSquareMeters",
                  df.get("extra_brüt_kullanım_alanı", pd.Series(dtype=str)))
    hall_src  = df.get("HallSquareMeters",
                  df.get("extra_net_kullanım_alanı",  pd.Series(dtype=str)))

    out["GrossSquareMeters"] = parse_area(gross_src)
    out["HallSquareMeters"]  = parse_area(hall_src)

    # ── Categorical pass-throughs ─────────────────────────────
    cats = [
        "NumberOfRooms","FloorLocation","HeatingType","KitchenType",
        "Balcony","Elevator","Parking","ItemStatus","UsingStatus",
        "InsideTheSite","StructureType","BuildStatus","TitleStatus",
        "TitleType","CreditEligibility","EligibilityForInvestment",
        "MortgageStatus","Swap","IsItVideoNavigable?","PropertyType",
        "EnergyRating","FromWhom","SiteName","BalconyType",
    ]
    # Also check extra_ columns for InsideTheSite
    if "InsideTheSite" not in df.columns and "extra_site_i_çerisinde" in df.columns:
        df["InsideTheSite"] = df["extra_site_i_çerisinde"]

    for col in cats:
        if col in df.columns:
            out[col] = df[col].astype(str).str.strip().replace("nan", np.nan)

    # ── KitchenType: fallback to extra_mutfak_tipi ────────────
    if "KitchenType" not in out.columns or out.get("KitchenType","").isna().all():
        if "extra_mutfak_tipi" in df.columns:
            out["KitchenType"] = df["extra_mutfak_tipi"].astype(str).str.strip()

    # ── Numeric fields ────────────────────────────────────────
    out["buildingAge"]             = parse_building_age(df.get("BuildingAge"))
    out["numberFloorsOfBuilding"]  = parse_floors(df.get("NumberFloorsofBuilding"))
    out["numberOfBathrooms"]       = parse_floors(df.get("NumberOfBathrooms"))
    out["numberOfBalconies"]       = parse_floors(df.get("NumberOfBalconies")) \
                                     if "NumberOfBalconies" in df.columns \
                                     else pd.array([pd.NA]*len(df), dtype="Int64")

    # ── Currency fields ───────────────────────────────────────
    out["rentalIncome"]  = parse_price(df["RentalIncome"]) \
                           if "RentalIncome" in df.columns \
                           else pd.array([pd.NA]*len(df), dtype="Int64")
    out["subscription"]  = parse_price(df.get("Subscription",
                                        pd.Series(["0"]*len(df))).fillna("0"))

    # ── room_count (integer) ──────────────────────────────────
    if "NumberOfRooms" in out.columns:
        out["room_count"] = out["NumberOfRooms"].map(ROOM_MAP).astype("Int64")

    # ── Dates ─────────────────────────────────────────────────
    if "AdCreationDate" in df.columns:
        creation = parse_turkish_date(df["AdCreationDate"])
    elif "extra_i_lan_tarihi" in df.columns:
        creation = parse_turkish_date(df["extra_i_lan_tarihi"])
    else:
        creation = pd.Series([pd.NaT]*len(df))

    if "AdUpdateDate" in df.columns:
        update = parse_turkish_date(df["AdUpdateDate"])
    else:
        update = creation

    out["adUpdateMonth"] = update.dt.month.astype("Int64")
    out["adUpdateYear"]  = update.dt.year.astype("Int64")
    out["adActiveDays"]  = (update - creation).dt.days.astype("Int64")

    # ── price_per_m2 ──────────────────────────────────────────
    out["price_per_m2"] = np.where(
        out["GrossSquareMeters"].notna() & (out["GrossSquareMeters"] > 0),
        out["price"].astype(float) / out["GrossSquareMeters"].astype(float),
        np.nan
    )

    # ── Defaults ──────────────────────────────────────────────
    if "Balcony"             in out.columns: out["Balcony"]             = out["Balcony"].fillna("Yok")
    if "IsItVideoNavigable?" in out.columns: out["IsItVideoNavigable?"] = out["IsItVideoNavigable?"].fillna("Hayır")
    if "ItemStatus"          in out.columns: out["ItemStatus"]          = out["ItemStatus"].fillna("Boş")

    # ── Drop price outliers ───────────────────────────────────
    prices = out["price"].dropna().astype(float)
    if len(prices) > 10:
        lo, hi = prices.quantile(0.01), prices.quantile(0.99)
        before = len(out)
        out = out[out["price"].isna() |
                  ((out["price"].astype(float) >= lo) &
                   (out["price"].astype(float) <= hi))]
        print(f"Outlier removal: {before-len(out)} rows dropped")

    # ── Drop unusable rows ────────────────────────────────────
    out = out[out["GrossSquareMeters"].isna() | (out["GrossSquareMeters"] > 0)]
    out = out.drop_duplicates(subset=["listing_url"]) \
             if "listing_url" in out.columns else out
    out = out.reset_index(drop=True)

    stamp = datetime.now().strftime("%Y%m%d_%H%M")
    dst   = OUTPUT_DIR / f"cleaned_listings_{stamp}.csv"
    out.to_csv(dst, index=False, encoding="utf-8-sig")

    print(f"\nCleaned shape: {out.shape}")
    print(f"Columns: {list(out.columns)}")
    print(f"\nSample district values: {out['district'].dropna().unique()[:10].tolist()}")
    print(f"Sample price values:    {out['price'].dropna().head(5).tolist()}")
    print(f"\nMissing values:\n{out.isnull().sum()[out.isnull().sum()>0].to_string()}")
    print(f"\n✓  Saved → {dst}")
    print(f'\nNext (RStudio):  source("r-analysis/01_data_loading.R")')

if __name__ == "__main__":
    main()