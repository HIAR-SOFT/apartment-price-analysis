"""
scraper.py  —  Collect listing URLs from sahibinden.com
Uses SeleniumBase UC+CDP mode to bypass Cloudflare Turnstile automatically.

Install once:
    pip install seleniumbase beautifulsoup4 lxml pandas

Usage:
    python scraper/scraper.py --pages 10
    python scraper/scraper.py --type kiralik --pages 10
"""

import time, json, random, argparse
from datetime import datetime
from pathlib import Path
from seleniumbase import Driver
from bs4 import BeautifulSoup

OUTPUT_DIR = Path(__file__).parent.parent / "data" / "raw"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

BASE     = "https://www.sahibinden.com"
URLS     = {
    "satilik": BASE + "/satilik-daire/istanbul",
    "kiralik": BASE + "/kiralik-daire/istanbul",
}

ROW_SELECTORS = [
    "tr.searchResultsItem",
    "tr[class*='searchResults']",
    "li[class*='classified']",
    "div[class*='classified-item']",
    "article",
]


def make_driver() -> Driver:
    driver = Driver(uc=True, headed=True)
    return driver


def open_with_cloudflare_bypass(driver: Driver, url: str) -> bool:
    """Open URL and auto-handle Cloudflare Turnstile if it appears."""
    try:
        driver.uc_open_with_reconnect(url, reconnect_time=6)
        # Try auto-clicking Cloudflare checkbox if it shows up
        try:
            driver.uc_gui_click_captcha()
            time.sleep(3)
        except Exception:
            pass  # No captcha, or already passed
        return True
    except Exception as e:
        print(f"  [WARN] Navigation error: {e}")
        return False


def detect_row_selector(driver: Driver) -> str | None:
    for sel in ROW_SELECTORS:
        try:
            els = driver.find_elements("css selector", sel)
            if els:
                print(f"  Detected row selector: '{sel}' ({len(els)} rows)")
                return sel
        except Exception:
            pass
    return None


def scrape_page(driver: Driver, url: str, row_sel: str) -> list[dict]:
    open_with_cloudflare_bypass(driver, url)
    time.sleep(random.uniform(3, 5))

    soup  = BeautifulSoup(driver.get_page_source(), "lxml")
    items = []

    # Try finding listing links directly from HTML
    for a in soup.select("a[href*='/ilan/']"):
        href = a.get("href", "")
        if not href.startswith("http"):
            href = BASE + href
        title = a.get_text(strip=True)
        if href and "/ilan/" in href and href not in [i["listing_url"] for i in items]:
            items.append({
                "listing_url": href,
                "title":       title,
                "scraped_at":  datetime.now().isoformat(),
            })

    return items


def collect(driver: Driver, base_url: str, listing_type: str,
            max_pages: int) -> list[dict]:
    collected  = []
    seen_urls  = set()

    for page in range(max_pages):
        offset = page * 20
        url    = f"{base_url}?pagingOffset={offset}&pagingSize=20"
        print(f"  [{listing_type}] page {page+1}/{max_pages}  →  {url}")

        items = scrape_page(driver, url, "")
        new   = [i for i in items if i["listing_url"] not in seen_urls]

        if not new:
            print("    No new listings — may be blocked or last page.")
            # Give user a chance to fix CAPTCHA manually
            ans = input("    Press ENTER to retry, or type 'skip' to stop this type: ").strip()
            if ans.lower() == "skip":
                break
            items = scrape_page(driver, url, "")
            new   = [i for i in items if i["listing_url"] not in seen_urls]
            if not new:
                break

        for item in new:
            item["listing_type"] = listing_type
            seen_urls.add(item["listing_url"])

        collected.extend(new)
        print(f"    → {len(new)} new  |  total so far: {len(collected)}")
        time.sleep(random.uniform(2, 4))

    return collected


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--type",  default="both", choices=["satilik", "kiralik", "both"])
    ap.add_argument("--pages", type=int, default=10,
                    help="Pages per type (20 listings/page → 10 pages = 200 listings)")
    args = ap.parse_args()

    print("=" * 60)
    print("Sahibinden Scraper — SeleniumBase UC Mode")
    print("Chrome will open. Cloudflare will be handled automatically.")
    print("If a CAPTCHA appears that can't be auto-solved, click it manually.")
    print("=" * 60)

    driver    = make_driver()
    all_links = []

    try:
        # Warm up on homepage
        print("\nOpening sahibinden.com homepage ...")
        open_with_cloudflare_bypass(driver, BASE)
        time.sleep(3)

        types = ["satilik", "kiralik"] if args.type == "both" else [args.type]
        for t in types:
            print(f"\n{'='*40}\nCollecting {t.upper()} listings ...\n{'='*40}")
            all_links += collect(driver, URLS[t], t, args.pages)

    finally:
        driver.quit()

    if not all_links:
        print("\n[!] 0 listings collected.")
        return

    # Deduplicate
    seen = set()
    deduped = []
    for item in all_links:
        if item["listing_url"] not in seen:
            seen.add(item["listing_url"])
            deduped.append(item)

    stamp = datetime.now().strftime("%Y%m%d_%H%M")
    out   = OUTPUT_DIR / f"listing_links_{stamp}.json"
    with open(out, "w", encoding="utf-8") as f:
        json.dump(deduped, f, ensure_ascii=False, indent=2)

    print(f"\n✓  {len(deduped)} unique listing URLs saved  →  {out}")
    print(f'\nNext step:  python scraper/parser.py --input "{out}"')


if __name__ == "__main__":
    main()