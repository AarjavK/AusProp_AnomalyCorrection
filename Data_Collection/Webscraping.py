import playwright
from playwright.sync_api import sync_playwright
import playwright.sync_api
from bs4 import BeautifulSoup

import pandas as pd
import time
import re
from datetime import datetime
from os import listdir
from os.path import join

def get_titles_links(pw_page: playwright.sync_api.Page, page1:int,page2:int,state:str, path:str) -> None:
    """Function to get the titles from different page links from Domain.com.au. Saves the html contents into separate
        html files.

    Inputs:
    --------
        [+] pw_page (playwright.sync_api.Page): playwright page which is opened using some browser
        [+] page1 (int): Start page number to search from
        [+] page2 (int): Page number to search up to
        [+] state (str): Australian state to search in - VIC, NSW, ACT, etc.
        [+] path (str): The path in which to store the file

    Returns:
        None
    """
    search = 'https://www.domain.com.au/sold-listings/?excludepricewithheld=1&state='+str(state)+'&page='
    for i in range(page1,page2):
        check=True
        page_num=i
        
        search_url=search + str(page_num)
        while check:
            try:
                print ('Downloading Page ' + str(page_num) + ' for ' + state + '\n' + search_url)
                page.goto(search_url)
                content = pw_page.content()
                print ('Page %d is Done \n' %page_num + str(state) + '-' + str(i))
                file = join(path, str(state) + '-' + str(i) + '.html')
                soup = BeautifulSoup(content, 'html')
                if soup.find_all('img')[0].get('alt') == 'No search results':
                    print('no search result')
                    break
                
                with open(file, "w", encoding='utf-8') as f:
                    f.write(soup.prettify())
                time.sleep(1)
                check=False
            except IOError:
                print ('IOError Occured, trying again...')
                time.sleep(2)

def scrape_page(soup):
    data_dict = {'Date': [],
                'Address': [], 
                 'Suburb': [], 
                 'State': [],
                 'Postcode': [],
                 'Beds': [], 
                 'Baths': [], 
                 'Cars': [], 
                 'Land': [], 
                 'Type': [], 
                 'SaleType': [],
                 'Price': []
                }
    for q in soup.find_all('ul'):
        if q.get('data-testid')=='results':
            for w in q.find_all('li'):
                if w.get('data-testid')[0:8] == 'listing-':
                    date = None # Date sold
                    ad = None # Street address
                    sub = None # Suburb
                    stat = None # State
                    post = None # Postcode
                    bed = None # Number of beds
                    bath = None # Number of bathrooms
                    car = None # Number of car parks
                    lan = None # Land size
                    pty = None # Type of property
                    saletype = None # Type of sale
                    price = None # Sale price
                    for u in w.find_all('p'):
                        #print(u.contents)
                        if u.get('data-testid') == 'listing-card-price':
                            price = u.contents[0].replace("\n", "").replace("$", "").replace(",","").strip()
                    for e in w.find_all('span'):
                        if e.get('class') == ['css-693528']:
                            pty = re.sub(r'[^A-Za-z/]+', '', e.contents[0].replace('\n', '').strip())
                        if e.get('class') == ['css-1nj9ymt']:
                            sale = re.sub(r'[^A-Za-z0-9 ]+', '', e.contents[0].replace('\n', '').strip())
                            saletype = sale[8:-12].upper()
                            date = datetime.strptime(sale[-11:], '%d %b %Y').date()
                        if e.get('data-testid') == "address-line1":
                            ad = e.contents[0].replace("\n","").strip()
                        if e.get('data-testid') == "address-line2":
                            AL2 = e.text.split('\n')
                            sub = AL2[2].strip()
                            stat = AL2[5].strip()
                            post = AL2[8].strip()
                        if e.get('data-testid') == "property-features-text-container":
                            # Temporary value to hold a digit which comes under the property features text
                            # reasoning - the digit comes before the description
                            temp = None
                            if e.contents[0].strip().isdigit():
                                temp = int(e.contents[0].strip())
                            for t in e.find_all('span'):
                                text_contents = t.contents[0].replace('\n','').strip()
                                if t.get('data-testid') == 'property-features-text':
                                    if re.match('Bed[s]?', text_contents):
                                        bed = temp
                                    elif re.match('Bath[s]?', text_contents):
                                        bath = temp
                                        #print(bath)
                                    elif text_contents == 'Parking':
                                        car = temp
                            del temp
                            if re.search('[0-9]+m', e.contents[0]):
                                lan = re.sub(r'[^0-9]+', '', e.contents[0].replace('\n', ''))
                    
                    # Add all values to dictionary
                    data_dict['Date'].append(date)
                    data_dict['Address'].append(ad)
                    data_dict['Suburb'].append(sub)
                    data_dict['State'].append(stat)
                    data_dict['Postcode'].append(post)
                    data_dict['Beds'].append(bed)
                    data_dict['Baths'].append(bath)
                    data_dict['Cars'].append(car)
                    data_dict['Land'].append(lan)
                    data_dict['Type'].append(pty)
                    data_dict['SaleType'].append(saletype)
                    data_dict['Price'].append(price)

    # Convert to dataframe after data is scraped
    # df = pd.DataFrame(data_dict)
    return data_dict

if __name__ == '__main__':
    datagather = False
    states = ['VIC', 'NSW', 'QLD', 'NT', 'TAS', 'ACT', 'WA', 'SA']
    rundate = datetime.strftime(datetime.today(), '%Y%m%d')
    if datagather:
        for state in states:
            with sync_playwright() as p:
                browser = p.chromium.launch() # Launch chromium
                page = browser.new_page()
                get_titles_links(page, 1, 51, state, './HTMLs/')

    # Read the specified page for data
    data_dict = {'Date': [],
                 'Address': [], 
                 'Suburb': [], 
                 'State': [],
                 'Postcode': [],
                 'Beds': [], 
                 'Baths': [], 
                 'Cars': [], 
                 'Land': [], 
                 'Type': [], 
                 'SaleType': [],
                 'Price': []
                }
    # Create an HTMLs folder to store all the pages we download
    path = './HTMLs/'
    html_files = [f for f in listdir('./HTMLs/')]
    for file in html_files:
        fpath = join(path, file)
        print(fpath)
        with open(fpath,"r", encoding='utf-8') as f:
            content = f.read()
            
        soup_page = BeautifulSoup(content, 'html')
        data = scrape_page(soup_page)
        for key in data:
            data_dict[key] += data[key]
    
    df = pd.DataFrame(data_dict)
    df.to_csv(f"Domain_housing_data_{rundate}.csv", index=False)
