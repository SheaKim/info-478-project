import requests
import pandas as pd
from bs4 import BeautifulSoup

url = "https://www.cdc.gov/vaccines-for-children/php/awardees/current-cdc-vaccine-price-list.html"
r = requests.get(url)

soup = BeautifulSoup(r.text, "html.parser")

#
date_div = soup.find("div", class_="dfe-section__content")
date_text = date_div.get_text(strip=True) if date_div else "Date Not Found"

# Get all tables on the page with that class
tables = soup.find_all("table", class_="table table-bordered")

# Check how many tables there are and pick the right one
# print(f"Number of tables found: {len(tables)}")

# For example, if you want the second table (index 1)
target_table = tables[3]  # Change index based on what table you want

# Prepare headers
headers = [
    "Vaccine", "Brandname/ Tradename", "NDC", "Packaging",
    "CDC Cost/ Dose", "Private Sector Cost/ Dose", "Contract End Date",
    "Manufacturer", "Contract Number", "Date"
]

# Process rows
final_data = []
rows = target_table.find_all("tr")

for row in rows:
    # Get all <th> and <td> cells
    th_cells = row.find_all("th")
    td_cells = row.find_all("td")
    
    # We expect 3 <th> (vaccine, brand, ndc), and 6 <td> (other data)
    if len(th_cells) == 3 and len(td_cells) == 6:
        vaccine_name = th_cells[0].get_text(strip=True)
        brand_name = th_cells[1].get_text(strip=True)
        ndc = th_cells[2].get_text(strip=True)
        
        # Get rest of the data
        other_data = [td.get_text(strip=True) for td in td_cells]
        
        # Combine all into a row
        row_data = [vaccine_name, brand_name, ndc] + other_data + [date_text]
        
        # Debug print to check
        print(f"Row Data: {row_data}")
        
        # Add to final dataset
        final_data.append(row_data)

# Build DataFrame and export
df = pd.DataFrame(final_data, columns=headers)
print(df.head())  # Preview first few rows to confirm
df.to_csv("cdc_vaccine_prices_fixed.csv", index=False)
print("Data saved to 'cdc_vaccine_prices_fixed.csv'.")

# # Now, extract headers from that table
# headers = [
#     "Vaccine", "Brandname/ Tradename", "NDC", "Packaging",
#     "CDC Cost/ Dose", "Private Sector Cost/ Dose", "Contract End Date",
#     "Manufacturer", "Contract Number"
# ]

# df = pd.DataFrame(columns=headers)

# #print(titles)
# #print(df)

# rows = target_table.find_all("tr")
# #print(rows)

# for i in rows[0:]:
#     data = i.find_all("td")
#     row = [tr.text for tr in data]
#     row = (row + [''] * len(df.columns))[:len(df.columns)]  # Pad if short, cut if long
#     df.loc[len(df)] = row

# df.to_csv("test_scrape.csv")

# soup = BeautifulSoup(r.text, "html.parser")

# table = soup.find("table", class_ = "table table-bordered")
# headers = table.find_all("th")

# titles = []
# for i in headers:
#     title = i.text
#     titles.append(title)

# print(titles)