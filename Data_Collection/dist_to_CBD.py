from geopy.geocoders import Nominatim
from geopy.distance import distance
import pandas as pd
import time

from loguru import logger

geolocator = Nominatim(user_agent="my_geocoder_app")

df = pd.read_csv('Domain_housing_data_all.csv')

major_cities = {"VIC": geolocator.geocode("Melbourne, Victoria, Australia"),
                "ACT": geolocator.geocode("Canberra, Australian Capital Territory, Australia"),
                "NSW": geolocator.geocode("Sydney, New South Wales, Australia"),
                "QLD": geolocator.geocode("Brisbane, Queensland, Australia"),
                "NT": geolocator.geocode("Darwin, Northern Territory, Australia"),
                "WA": [
                    geolocator.geocode("Perth, Western Australia, Australia"),
                    geolocator.geocode("Broome, Western Australia, Australia")
                    ],
                "SA": geolocator.geocode("Adelaide, South Australia, Australia"),
                "TAS": geolocator.geocode("Hobart, Tasmania, Australia")
}
logger.info(f"Geocode obtained for the major cities\n{major_cities}")

latitudes = [pd.NA] * len(df)
longitudes = [pd.NA] * len(df)
dist_to_cbd = [pd.NA] * len(df)
logger.info("Values initialised, starting search for distances!")
for idx, row in df.iterrows():    
    match row["State"]:
        case "VIC":
            state = "Victoria"
        case "ACT":
            state = "Australian Capital Territory"
        case "NSW":
            state = "New South Wales"
        case "QLD":
            state = "Queensland"
        case "NT":
            state = "Northern Territory"
        case "WA":
            state = "West Australia"
        case "SA":
            state = "South Australia"
        case "TAS":
            state = "Tasmania"
    
    full_addr = ""
    if not pd.isna(row["Address"]):
        full_addr += row["Address"].split('/')[-1]
    
    if not pd.isna(row["Suburb"]):
        full_addr += ", " + row["Suburb"] + ", " + state
    else:
        full_addr = state
    
    try:
        # Get the coordinates for the current address
        cur_addr = geolocator.geocode(full_addr)
        
        # If lat-long not found for this address
        if cur_addr is None:
            # Wait to avoid exceeding API rate limit
            time.sleep(1.1)
            continue
        
        latitudes[idx] = cur_addr.latitude
        longitudes[idx] = cur_addr.longitude
        time.sleep(1.1)
        
        # Since WA is such a large state, find the distance to the closest CBD
        if row["State"] != "WA":
            dist_to_cbd[idx] = distance((cur_addr.latitude, cur_addr.longitude), 
                                        (major_cities[row["State"]].latitude, major_cities[row["State"]].longitude)).km
        else:
            dist_to_cbd[idx] = min(distance((cur_addr.latitude, cur_addr.longitude), (major_cities[row["State"]][0].latitude, major_cities[row["State"]][0].longitude)).km,
                    distance((cur_addr.latitude, cur_addr.longitude), (major_cities[row["State"]][1].latitude, major_cities[row["State"]][1].longitude)).km)
    
        if (idx+1) % 100 == 0 or idx == 0:
            # Print update after every 100 iterations
            logger.info(f"{idx}: full addr: {full_addr},distance: {dist_to_cbd[idx]}")
    except Exception as err:
        logger.error(f"full addr: {full_addr}")
        logger.error(err)
        
# Add column to data frame
df["latitude"] = latitudes
df["longitude"] = longitudes
df["dist_to_cbd"] = dist_to_cbd
logger.info("Longitudes, latitudes, and distances added to dataframe. Saving to file.")

logger.success(f"Distance search complete! {len(df) - df['dist_to_cbd'].isna().sum()}")
file = "Domain_housing_data_all_coordinates.csv"
df.to_csv(file, index=False)
logger.success(f"File saved: {file}")