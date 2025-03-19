import requests
import pandas as pd
from bs4 import BeautifulSoup

# Archive URL for the page you want
archive_url = "https://archive.cdc.gov/#/details?url=https://www.cdc.gov/vaccines/programs/vfc/awardees/vaccine-management/price-list/2024/2024-09-13.html"

# Send the request to the Wayback Machine API
response = requests.get(archive_url)

# Check the response status
if response.status_code == 200:
    print("Response Content:")
    print(response.text[:1000])  # Print the first 1000 characters of the response to check its structure

    # Parse the HTML to inspect the structure
    soup = BeautifulSoup(response.text, "html.parser")

    # Check if there's a link with the snapshot
    snapshot_url = None
    try:
        # Try to find all 'a' tags and see where the snapshot URL is located
        all_links = soup.find_all("a", href=True)
        
        # Print out all the links found to understand the structure
        for link in all_links:
            print(link["href"])
        
        # After inspecting, we can try to pick out the correct snapshot link
        # (adjust this based on the actual content structure of the response)

        # Example logic: if the snapshot link contains 'archive.org', it's likely the one
        snapshot_url = next((link["href"] for link in all_links if "archive.org" in link["href"]), None)

        if snapshot_url:
            print(f"Found archived page at: {snapshot_url}")
            
            # Now scrape the archived page
            r = requests.get(snapshot_url)
            soup = BeautifulSoup(r.text, "html.parser")

            # Extract date and tables as before
            date_div = soup.find("div", class_="dfe-section__content")
            date_text = date_div.get_text(strip=True) if date_div else "Date Not Found"

            # Get all tables on the page with that class
            tables = soup.find_all("table", class_="table table-bordered")

            # Check if tables are found
            if len(tables) < 4:
                print("Error: Table not found or incorrect index.")
            else:
                # For example, if you want the fourth table (index 3)
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

                        # Add to final dataset
                        final_data.append(row_data)

                # Build DataFrame and export
                df = pd.DataFrame(final_data, columns=headers)
                df.to_csv("cdc_vaccine_prices_test2.csv", index=False)
                print("Data saved to 'cdc_vaccine_prices_test2.csv'.")
        else:
            print("No valid archived version found.")
    except Exception as e:
        print(f"Error extracting the snapshot URL: {e}")
else:
    print(f"Failed to retrieve data. Status code: {response.status_code}")
