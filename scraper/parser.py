"""
parser.py  —  Visit each listing URL and extract all apartment details.
Uses SeleniumBase UC mode to bypass Cloudflare on individual listing pages.

Install once:
    pip install seleniumbase beautifulsoup4 lxml pandas

Usage:
    python scraper/parser.py --input data/raw/listing_links_YYYYMMDD_HHMM.json
    python scraper/parser.py --input data/raw/listing_links_YYYYMMDD_HHMM.json --limit 200
"""

import json, argparse, time, random
from pathlib import Path
from datetime import datetime

from seleniumbase import Driver
from bs4 import BeautifulSoup
import pandas as pd

OUTPUT_DIR = Path(__file__).parent.parent / "data" / "raw"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

BASE = "https://www.sahibinden.com"

FIELD_MAP = {
    "ilan no":                "listing_id",
    "ilan tarihi":            "AdCreationDate",
    "güncelleme tarihi":      "AdUpdateDate",
    "fiyat":                  "price",
    "net m²":                 "HallSquareMeters",
    "brüt m²":                "GrossSquareMeters",
    "oda sayısı":             "NumberOfRooms",
    "bina yaşı":              "BuildingAge",
    "bulunduğu kat":          "FloorLocation",
    "kat sayısı":             "NumberFloorsofBuilding",
    "banyo sayısı":           "NumberOfBathrooms",
    "balkon":                 "Balcony",
    "balkon sayısı":          "NumberOfBalconies",
    "balkon tipi":            "BalconyType",
    "balkon m²":              "BalconySquareMeters",
    "wc m²":                  "WCSquareMeters",
    "ısıtma":                 "HeatingType",
    "mutfak":                 "KitchenType",
    "eşya durumu":            "ItemStatus",
    "asansör":                "Elevator",
    "otopark":                "Parking",
    "site içerisinde":        "InsideTheSite",
    "kullanım durumu":        "UsingStatus",
    "yapı tipi":              "StructureType",
    "yapı durumu":            "BuildStatus",
    "tapu durumu":            "TitleStatus",
    "tapu tipi":              "TitleType",
    "takas":                  "Swap",
    "krediye uygun":          "CreditEligibility",
    "yatırıma uygun":         "EligibilityForInvestment",
    "ipotek durumu":          "MortgageStatus",
    "kira getirisi":          "RentalIncome",
    "aidat":                  "Subscription",
    "video":                  "IsItVideoNavigable?",
}


def make_driver() -> Driver:
    return Driver(uc=True, headed=True)


def get_soup(driver: Driver, url: str) -> BeautifulSoup | None:
    try:
        driver.uc_open_with_reconnect(url, reconnect_time=5)
        try:
            driver.uc_gui_click_captcha()
            time.sleep(2)
        except Exception:
            pass
        time.sleep(random.uniform(2, 3.5))
        return BeautifulSoup(driver.get_page_source(), "lxml")
    except Exception as e:
        print(f"    [WARN] {e}")
        return None


def is_blocked(soup: BeautifulSoup) -> bool:
    text = soup.get_text().lower()
    return any(kw in text for kw in ["just a moment", "checking your browser",
                                      "bağlantınız kontrol", "verify you are human"])


def parse_listing(soup: BeautifulSoup, url: str, listing_type: str) -> dict:
    record = {
        "listing_url":  url,
        "listing_type": listing_type,
        "scraped_at":   datetime.now().isoformat(),
    }

    # ── Price ─────────────────────────────────────────────────────────────────
    for sel in [".classifiedPrice strong", "div.price-container strong",
                "span[class*='price']", "h3[class*='price']",
                "[class*='price'] strong", "[class*='Price']"]:
        el = soup.select_one(sel)
        if el and el.get_text(strip=True):
            record["price"] = el.get_text(strip=True)
            break

    # ── Breadcrumb → location ─────────────────────────────────────────────────
    crumbs = soup.select(".classifiedBreadCrumb a")
    if len(crumbs) >= 2:
        record["district"]      = crumbs[-2].get_text(strip=True)
        record["neighbourhood"] = crumbs[-1].get_text(strip=True)
    if crumbs:
        record["address"] = ", ".join(c.get_text(strip=True) for c in crumbs[1:])

    # ── Characteristics table ─────────────────────────────────────────────────
    rows = soup.select(
        "ul.classifiedInfoList li, "
        "div.classified-properties li, "
        "div[class*='classifiedInfo'] li, "
        "table.classifiedInfoTable tr, "
        "[class*='ClassifiedInfo'] li"
    )
    for row in rows:
        spans = row.select("span")
        if len(spans) >= 2:
            key = spans[0].get_text(strip=True).lower().rstrip(": ")
            val = spans[1].get_text(strip=True)
        elif row.name == "tr":
            cells = row.select("td")
            if len(cells) >= 2:
                key = cells[0].get_text(strip=True).lower().rstrip(": ")
                val = cells[1].get_text(strip=True)
            else:
                continue
        else:
            continue
        col = FIELD_MAP.get(key)
        if col:
            record[col] = val

    # ── Date fallback ─────────────────────────────────────────────────────────
    for li in soup.select("li.classifiedInfoItem, .classified-date-info li"):
        spans = li.select("span")
        if len(spans) >= 2:
            key = spans[0].get_text(strip=True).lower()
            val = spans[1].get_text(strip=True)
            if "ilan tarihi" in key:
                record.setdefault("AdCreationDate", val)
            elif "güncelleme" in key:
                record.setdefault("AdUpdateDate", val)

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
        print("Files in data/raw/:")
        for f in sorted(raw_dir.glob("*.json")):
            print(f"  {f.name}")
        return

    with open(path, encoding="utf-8") as f:
        links = json.load(f)

    if not links:
        print("[ERROR] JSON is empty — run scraper.py first.")
        return

    links = links[:args.limit]
    print(f"\nParsing {len(links)} listings with SeleniumBase UC mode ...")
    print("Chrome will open. Cloudflare challenges will be handled automatically.\n")

    driver  = make_driver()
    records = []

    # Warm up on homepage first
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
            print(f"  [{i:>3}/{len(links)}] {url[:75]}")

            soup = get_soup(driver, url)

            if soup is None:
                print("           → failed to load")
                records.append({"listing_url": url, "listing_type": lt,
                                 "scraped_at": datetime.now().isoformat()})
                continue

            if is_blocked(soup):
                print("           → still blocked, waiting for manual solve ...")
                input("           Solve CAPTCHA in the browser, then press ENTER... ")
                soup = BeautifulSoup(driver.get_page_source(), "lxml")

            rec = parse_listing(soup, url, lt)
            records.append(rec)

            filled = sum(1 for v in rec.values() if v and str(v).strip()
                         and str(v) not in ("nan", "None"))
            print(f"           → {filled} fields extracted")

            # Checkpoint every 50
            if i % 50 == 0:
                ck = OUTPUT_DIR / f"checkpoint_{i}.csv"
                pd.DataFrame(records).to_csv(ck, index=False, encoding="utf-8-sig")
                print(f"    ✓ checkpoint → {ck.name}")

            time.sleep(random.uniform(2.5, 4.5))

    finally:
        driver.quit()

    df    = pd.DataFrame(records)
    stamp = datetime.now().strftime("%Y%m%d_%H%M")
    out   = OUTPUT_DIR / f"raw_listings_{stamp}.csv"
    df.to_csv(out, index=False, encoding="utf-8-sig")

    print(f"\n✓  {len(df)} records saved  →  {out}")
    print("\nFields extracted (non-null counts):")
    non_empty = df.notna().sum()
    print(non_empty[non_empty > 0].to_string())
    print(f'\nNext:  python scraper/cleaning.py --input "{out}"')


if __name__ == "__main__":
    main()