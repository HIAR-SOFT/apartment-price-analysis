"""
prepare_data.py
Cleans 22_5_2022_sahibinden_ev.csv and outputs a
cleaned_listings CSV ready for R analysis.

Usage:
    python scraper/prepare_data.py --input data/raw/22_5_2022_sahibinden_ev.csv
"""

import re
import argparse
from pathlib import Path
from datetime import datetime

import numpy as np
import pandas as pd

OUTPUT_DIR = Path(__file__).parent.parent / "data" / "cleaned"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# ── Room count → integer ──────────────────────────────────────
ROOM_MAP = {
    "Stüdyo": 0, "1+0": 0, "2+0": 1,
    "1+1": 1, "1.5+1": 1,
    "2+1": 2, "2+2": 3, "2.5+1": 2,
    "3+1": 3, "3+2": 4, "3.5+1": 3,
    "4+1": 4, "4+2": 5, "4+3": 5, "4.5+1": 4,
    "5+1": 5, "5+2": 6,
    "6+1": 6, "6+2": 7,
    "7+1": 7, "7+2": 8,
    "8+2": 9, "8+3": 9,
}

# ── Neighbourhood prefix → Istanbul district ──────────────────
N2D = {
    "Büyükada": "Adalar", "Kınalıada": "Adalar",
    "Burgazada": "Adalar", "Heybeliada": "Adalar",
    "Arnavutköy": "Arnavutköy", "Bolluca": "Arnavutköy",
    "Haraçcı": "Arnavutköy", "Hadımköy": "Arnavutköy",
    "Karabayır": "Arnavutköy", "Mimarsinan": "Arnavutköy",
    "Taşoluk": "Arnavutköy",
    "Ataşehir": "Ataşehir", "İçerenköy": "Ataşehir",
    "Kayışdağı": "Ataşehir", "Küçükbakkalköy": "Ataşehir",
    "Yenisahra": "Ataşehir",
    "Avcılar": "Avcılar", "Firuzköy": "Avcılar",
    "Gümüşpala": "Avcılar", "Ambarlı": "Avcılar",
    "Bağcılar": "Bağcılar", "Beştelsiz": "Bağcılar",
    "Çırpıcı": "Bağcılar", "Güneşli": "Bağcılar",
    "Kirazlı": "Bağcılar", "Sanayi15 Temmuz": "Bağcılar",
    "Bahçelievler": "Bahçelievler", "Çobançeşme": "Bahçelievler",
    "Kocasinan": "Bahçelievler", "Şirinevler": "Bahçelievler",
    "Bakırköy": "Bakırköy", "Ataköy": "Bakırköy",
    "Florya": "Bakırköy", "Kartaltepe": "Bakırköy",
    "Yeşilköy": "Bakırköy", "Yeşilyurt": "Bakırköy",
    "Başakşehir": "Başakşehir", "Bahçeşehir": "Başakşehir",
    "Kayabaşı": "Başakşehir", "Altınşehir": "Başakşehir",
    "Beşiktaş": "Beşiktaş", "Bebek": "Beşiktaş",
    "Etiler": "Beşiktaş", "Levent": "Beşiktaş",
    "Levazım": "Beşiktaş", "Ortaköy": "Beşiktaş",
    "Türkali": "Beşiktaş", "Teşvikiye": "Beşiktaş",
    "Nisbetiye": "Beşiktaş",
    "Beykoz": "Beykoz", "Anadoluhisarı": "Beykoz",
    "Paşabahçe": "Beykoz", "Kandilli": "Beykoz",
    "Göksu": "Beykoz", "Göktürk": "Beykoz",
    "Beylikdüzü": "Beylikdüzü", "Gürpınar": "Beylikdüzü",
    "Kavaklı": "Beylikdüzü",
    "Beyoğlu": "Beyoğlu", "Galata": "Beyoğlu",
    "Karaköy": "Beyoğlu",
    "Büyükçekmece": "Büyükçekmece", "Mimaroba": "Büyükçekmece",
    "Kumburgaz": "Büyükçekmece",
    "Esenler": "Esenler", "Havaalanı": "Esenler",
    "Menderes": "Esenler",
    "Esenyurt": "Esenyurt",
    "Eyüpsultan": "Eyüpsultan", "Eyüp": "Eyüpsultan",
    "Alibeyköy": "Eyüpsultan", "Nişanca": "Eyüpsultan",
    "Fatih": "Fatih", "Aksaray": "Fatih",
    "Balat": "Fatih", "Fener": "Fatih",
    "Sultanahmet": "Fatih", "Yedikule": "Fatih",
    "Gaziosmanpaşa": "Gaziosmanpaşa",
    "Barbaros/Yeşilbağ": "Gaziosmanpaşa",
    "Güngören": "Güngören", "Gençosman": "Güngören",
    "Kadıköy": "Kadıköy", "Acıbadem": "Kadıköy",
    "Bostancı": "Kadıköy", "Caddebostan": "Kadıköy",
    "Erenköy": "Kadıköy", "Fenerbahçe": "Kadıköy",
    "Göztepe": "Kadıköy", "Kozyatağı": "Kadıköy",
    "Moda": "Kadıköy", "Suadiye": "Kadıköy",
    "Dragos": "Kadıköy", "Küçükyalı": "Kadıköy",
    "Kağıthane": "Kağıthane", "Gayrettepe": "Kağıthane",
    "Mecidiyeköy": "Kağıthane",
    "Kartal": "Kartal", "Cevizli": "Kartal",
    "Yakacık": "Kartal", "Örnek": "Kartal",
    "Soğanlık": "Kartal",
    "Küçükçekmece": "Küçükçekmece", "Atakent": "Küçükçekmece",
    "Halkalı": "Küçükçekmece", "İnönü": "Küçükçekmece",
    "Sefaköy": "Küçükçekmece", "Cennet": "Küçükçekmece",
    "Maltepe": "Maltepe", "Bağlarbaşı": "Maltepe",
    "Altayçeşme": "Maltepe", "Findıklı": "Maltepe",
    "Pendik": "Pendik", "Kurtköy": "Pendik",
    "Kaynarca": "Pendik",
    "Sancaktepe": "Sancaktepe", "Samandıra": "Sancaktepe",
    "Sarıyer": "Sarıyer", "Tarabya": "Sarıyer",
    "Yeniköy": "Sarıyer", "Zekeriyaköy": "Sarıyer",
    "Büyükdere": "Sarıyer",
    "Silivri": "Silivri", "Selimpaşa": "Silivri",
    "Şile": "Şile",
    "Şişli": "Şişli", "Fulya": "Şişli",
    "Nişantaşı": "Şişli",
    "Sultanbeyli": "Sultanbeyli",
    "Sultangazi": "Sultangazi", "Cebeci": "Sultangazi",
    "Tuzla": "Tuzla",
    "Ümraniye": "Ümraniye", "Elmalikent": "Ümraniye",
    "Çakmak": "Ümraniye",
    "Üsküdar": "Üsküdar", "Beylerbeyi": "Üsküdar",
    "Çengelköy": "Üsküdar", "Salacak": "Üsküdar",
    "Merkez": "Üsküdar",
    "Zeytinburnu": "Zeytinburnu", "Kazlıçeşme": "Zeytinburnu",
}


def split_town(town):
    """Split concatenated 'NeighbourhoodMahalle Mh' → (neighbourhood, mahalle)."""
    if pd.isna(town):
        return None, None
    s = re.sub(r"\s*(Mah\.|Mahallesi|Mah|Mh)$", "", str(town).strip()).strip()
    m = re.search(r"(?<=[a-zçğışöüñ])(?=[A-ZÇĞİÖŞÜ])", s)
    if m:
        return s[: m.start()].strip(), s[m.start() :].strip()
    return s, None


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
    out["listing_id"]   = df["Unnamed: 0"].astype(str)
    out["listing_type"] = "satilik"
    out["title"]        = df["title"].astype(str).str.strip()

    # ── location ──────────────────────────────────────────────
    neighbourhood, mahalle = zip(*df["town"].map(split_town))
    out["neighbourhood"] = list(neighbourhood)
    out["mahalle"]       = list(mahalle)
    out["district"]      = pd.Series(neighbourhood).map(N2D).values

    # ── price ─────────────────────────────────────────────────
    out["price"] = pd.to_numeric(
        df["price"].astype(str).str.replace(r"[^\d]", "", regex=True),
        errors="coerce",
    )

    # ── area ──────────────────────────────────────────────────
    out["GrossSquareMeters"] = pd.to_numeric(df["area"], errors="coerce")
    # Net area not available in this dataset
    out["HallSquareMeters"] = np.nan

    # ── rooms ─────────────────────────────────────────────────
    out["NumberOfRooms"] = df["numberOfRooms"].astype(str).str.strip()
    out["room_count"]    = out["NumberOfRooms"].map(ROOM_MAP)

    # ── derived ───────────────────────────────────────────────
    out["price_per_m2"] = np.where(
        out["GrossSquareMeters"].notna() & (out["GrossSquareMeters"] > 0),
        out["price"] / out["GrossSquareMeters"],
        np.nan,
    )

    # ── columns absent in this dataset (kept NA for R compat) ─
    for col in [
        "buildingAge", "floorNum", "numberFloorsOfBuilding",
        "numberOfBathrooms", "HeatingType", "KitchenType",
        "Elevator", "Parking", "InsideTheSite", "ItemStatus",
        "UsingStatus", "TitleStatus", "EnergyRating", "subscription",
        "listing_url",
    ]:
        out[col] = np.nan

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
    print(f"\nFinal shape   : {out.shape}")
    print(f"With district : {out['district'].notna().sum()} / {len(out)}")
    print(f"Price range   : {out['price'].min():,.0f} – {out['price'].max():,.0f} TL")
    print(f"Median price  : {out['price'].median():,.0f} TL")
    print(f"Median area   : {out['GrossSquareMeters'].median():.0f} m²")
    print(f"\nTop districts :")
    print(out["district"].value_counts().head(15).to_string())

    stamp = datetime.now().strftime("%Y%m%d_%H%M")
    dst   = OUTPUT_DIR / f"cleaned_listings_{stamp}.csv"
    out.to_csv(dst, index=False, encoding="utf-8-sig")
    print(f"\n✓ Saved → {dst}")


if __name__ == "__main__":
    main()