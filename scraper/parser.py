"""
parser.py  —  Extract full apartment details from sahibinden.com listing pages.
Uses SeleniumBase UC mode to bypass Cloudflare.

Install:
    pip install seleniumbase beautifulsoup4 lxml pandas

Usage:
    python scraper/parser.py --input data/raw/listing_links_YYYYMMDD_HHMM.json --limit 200
"""

import re, json, argparse, time, random
from pathlib import Path
from datetime import datetime

from seleniumbase import Driver
from bs4 import BeautifulSoup
import pandas as pd

OUTPUT_DIR = Path(__file__).parent.parent / "data" / "raw"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

BASE = "https://www.sahibinden.com"

# Exact Turkish labels from the page (lowercase) → English column name
FIELD_MAP = {
    # dates / id
    "ilan tarihi":              "AdCreationDate",
    "güncelleme tarihi":        "AdUpdateDate",
    "ilan no":                  "listing_id",

    # type
    "emlak tipi":               "PropertyType",

    # size
    "m² (brüt)":               "GrossSquareMeters",
    "m² (net)":                 "HallSquareMeters",
    "brüt m²":                 "GrossSquareMeters",   # alias
    "net m²":                  "HallSquareMeters",    # alias
    "brüt alan":               "GrossSquareMeters",
    "net alan":                "HallSquareMeters",

    # rooms / floors
    "oda sayısı":              "NumberOfRooms",
    "oda":                     "NumberOfRooms",

    # building
    "bina yaşı":               "BuildingAge",
    "bulunduğu kat":           "FloorLocation",
    "kat sayısı":              "NumberFloorsofBuilding",
    "bina kat sayısı":         "NumberFloorsofBuilding",

    # features
    "ısıtma":                  "HeatingType",
    "isıtma":                  "HeatingType",         # alias without accent
    "banyo sayısı":            "NumberOfBathrooms",
    "banyo":                   "NumberOfBathrooms",
    "mutfak":                  "KitchenType",
    "balkon":                  "Balcony",
    "asansör":                 "Elevator",
    "otopark":                 "Parking",

    # furnished / status
    "eşyalı":                  "ItemStatus",
    "eşya durumu":             "ItemStatus",
    "kullanım durumu":         "UsingStatus",
    "kimden":                  "FromWhom",

    # site
    "site içerisinde":         "InsideTheSite",
    "site adı":                "SiteName",
    "aidat (tl)":              "Subscription",
    "aidat":                   "Subscription",

    # legal / financial
    "krediye uygun":           "CreditEligibility",
    "enerji kimlik belgesi":   "EnergyRating",
    "tapu durumu":             "TitleStatus",
    "tapu tipi":               "TitleType",
    "takas":                   "Swap",
    "ipotek durumu":           "MortgageStatus",
    "yatırıma uygun":          "EligibilityForInvestment",
    "kira getirisi":           "RentalIncome",

    # build
    "yapı tipi":               "StructureType",
    "yapı durumu":             "BuildStatus",

    # misc
    "video":                   "IsItVideoNavigable?",
    "fiyat":                   "price",
}


def make_driver() -> Driver:
    return Driver(uc=True, headed=True)


def open_url(driver: Driver, url: str) -> bool:
    try:
        driver.uc_open_with_reconnect(url, reconnect_time=6)
        try:
            driver.uc_gui_click_captcha()
            time.sleep(2)
        except Exception:
            pass
        time.sleep(random.uniform(2.5, 4.0))
        return True
    except Exception as e:
        print(f"    [WARN] {e}")
        return False


def is_blocked(html: str) -> bool:
    low = html.lower()
    return any(k in low for k in [
        "just a moment", "checking your browser",
        "bağlantınız kontrol", "verify you are human",
        "enable javascript", "ray id"
    ])


def extract_kv_pairs(soup: BeautifulSoup) -> dict:
    """
    Pull every key-value pair from the page using many strategies.
    Returns dict of {lowercase_key: value}.
    """
    found = {}

    def store(key: str, val: str):
        key = key.strip().rstrip(":").strip().lower()
        val = val.strip()
        if key and val and len(key) < 80:
            found.setdefault(key, val)

    # any <li> that has exactly 2 <span> children
    for li in soup.select("li"):
        spans = li.find_all("span", recursive=False)
        if len(spans) == 2:
            store(spans[0].get_text(" ", strip=True),
                  spans[1].get_text(" ", strip=True))
        elif len(spans) >= 2:
            store(spans[0].get_text(" ", strip=True),
                  spans[1].get_text(" ", strip=True))

    #  <li> with a <strong> or <b> label 
    for li in soup.select("li"):
        label_el = li.find(["strong", "b", "label"])
        if label_el:
            key = label_el.get_text(" ", strip=True)
            label_el.decompose()
            val = li.get_text(" ", strip=True)
            store(key, val)

    # table rows
    for tr in soup.select("tr"):
        cells = tr.find_all(["td", "th"])
        if len(cells) >= 2:
            store(cells[0].get_text(" ", strip=True),
                  cells[1].get_text(" ", strip=True))

    # definition lists
    for dl in soup.select("dl"):
        dts = dl.find_all("dt")
        dds = dl.find_all("dd")
        for dt, dd in zip(dts, dds):
            store(dt.get_text(" ", strip=True), dd.get_text(" ", strip=True))

    # Any element with datalabel attribute
    for el in soup.select("[data-label]"):
        store(el["data-label"], el.get_text(" ", strip=True))

    # Divs with two direct children (label + value pattern) 
    for div in soup.select("div[class*='detail'], div[class*='info'], "
                           "div[class*='property'], div[class*='feature']"):
        children = [c for c in div.children
                    if hasattr(c, "get_text") and c.get_text(strip=True)]
        if len(children) == 2:
            store(children[0].get_text(" ", strip=True),
                  children[1].get_text(" ", strip=True))

    # JSON blobs in <script> tags 
    for script in soup.find_all("script"):
        txt = script.string or ""
        if len(txt) < 50:
            continue
        # Look for "Label":"Value" patterns near apartment keywords
        pairs = re.findall(r'"([^"]{2,60}?)"\s*:\s*"([^"]{1,120})"', txt)
        for k, v in pairs:
            kl = k.lower()
            if any(tok in kl for tok in [
                "oda", "kat", "m2", "m²", "yaş", "banyo", "ısıtma", "isitma",
                "balkon", "asansör", "asansor", "otopark", "tapu", "yapı",
                "kullanım", "site", "aidat", "fiyat", "emlak", "brüt", "net",
                "eşya", "esya", "krediye", "takas", "kimden"
            ]):
                store(k, v)

    return found


def parse_listing(driver: Driver, url: str, listing_type: str) -> dict:
    record = {
        "listing_url":  url,
        "listing_type": listing_type,
        "scraped_at":   datetime.now().isoformat(),
    }

    if not open_url(driver, url):
        return record

    html = driver.get_page_source()
    if is_blocked(html):
        print("    → blocked — solve CAPTCHA in browser ...")
        input("    Press ENTER when done... ")
        html = driver.get_page_source()

    soup = BeautifulSoup(html, "lxml")

    # price 
    for sel in [
        ".classifiedPrice strong",
        "[class*='price'] strong", "[class*='Price'] strong",
        "[class*='fiyat']", "h3[class*='price']",
        "span[class*='price']", "div[class*='price']",
    ]:
        el = soup.select_one(sel)
        if el:
            txt = el.get_text(" ", strip=True)
            if txt and any(c.isdigit() for c in txt):
                record["price"] = txt
                break

    # title
    for sel in ["h1.classifiedDetailTitle", "h1[class*='title']",
                "h1[class*='Title']", "h1"]:
        el = soup.select_one(sel)
        if el:
            record["title"] = el.get_text(" ", strip=True)
            break

    # location 
    crumbs = soup.select(
        ".classifiedBreadCrumb a, [class*='Breadcrumb'] a, "
        "[class*='breadcrumb'] a, nav a"
    )
    crumbs = [c for c in crumbs if c.get_text(strip=True)]
    if len(crumbs) >= 2:
        record["district"]      = crumbs[-2].get_text(strip=True)
        record["neighbourhood"] = crumbs[-1].get_text(strip=True)
    if crumbs:
        record["address"] = ", ".join(c.get_text(strip=True) for c in crumbs[1:])

    # all key-value fields 
    raw = extract_kv_pairs(soup)

    for raw_key, raw_val in raw.items():
        col = FIELD_MAP.get(raw_key)
        if col:
            record.setdefault(col, raw_val)

    # atore unrecognised fields with extra prefix 
    mapped = set(FIELD_MAP.keys())
    for raw_key, raw_val in raw.items():
        if raw_key not in mapped:
            safe = "extra_" + re.sub(r"[^\w]", "_", raw_key)[:40]
            record.setdefault(safe, raw_val)

    # Description 
    for sel in [".classifiedDescription", "#classifiedDescription",
                "[class*='description']", "[class*='Description']"]:
        el = soup.select_one(sel)
        if el:
            record["description"] = el.get_text(" ", strip=True)[:600]
            break

    return record


def resolve_input(raw: str) -> Path:
    p = Path(raw)
    if p.exists():
        return p
    root = Path(__file__).parent.parent
    for candidate in [root / raw, root / "data" / "raw" / Path(raw).name]:
        if candidate.exists():
            return candidate
    return p


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input",  required=True)
    ap.add_argument("--limit",  type=int, default=200)
    args = ap.parse_args()

    path = resolve_input(args.input)
    if not path.exists():
        raw_dir = Path(__file__).parent.parent / "data" / "raw"
        print(f"[ERROR] File not found: {args.input}")
        print("JSON files in data/raw/:")
        for f in sorted(raw_dir.glob("*.json")):
            print(f"  {f.name}")
        return

    with open(path, encoding="utf-8") as f:
        links = json.load(f)

    if not links:
        print("[ERROR] JSON is empty — run scraper.py first.")
        return

    links = links[:args.limit]
    print(f"\nParsing {len(links)} listings ...\n")

    driver  = make_driver()
    records = []

    
    print("Warming up on sahibinden.com ...")
    try:
        driver.uc_open_with_reconnect(BASE, reconnect_time=6)
        try:
            driver.uc_gui_click_captcha()
        except Exception:
            pass
        time.sleep(3)
    except Exception:
        pass

    try:
        for i, item in enumerate(links, 1):
            url = item.get("listing_url", "")
            lt  = item.get("listing_type", "satilik")
            print(f"  [{i:>3}/{len(links)}] {url[:70]}")

            rec   = parse_listing(driver, url, lt)
            count = sum(1 for k, v in rec.items()
                        if v and str(v).strip() not in ("nan", "None", ""))
            print(f"           → {count} fields extracted")
            records.append(rec)

            if i % 50 == 0:
                ck = OUTPUT_DIR / f"checkpoint_{i}.csv"
                pd.DataFrame(records).to_csv(ck, index=False, encoding="utf-8-sig")
                print(f"    ✓ checkpoint → {ck.name}")

            time.sleep(random.uniform(2.5, 5.0))

    finally:
        driver.quit()

    df    = pd.DataFrame(records)
    stamp = datetime.now().strftime("%Y%m%d_%H%M")
    out   = OUTPUT_DIR / f"raw_listings_{stamp}.csv"
    df.to_csv(out, index=False, encoding="utf-8-sig")

    print(f"\n✓  {len(df)} records  →  {out}")
    print(f"\nColumns extracted ({len(df.columns)} total):")
    non_empty = df.notna().sum()
    print(non_empty[non_empty > 0].sort_values(ascending=False).to_string())
    print(f'\nNext:  python scraper/cleaning.py --input "{out}"')


if __name__ == "__main__":
    main()