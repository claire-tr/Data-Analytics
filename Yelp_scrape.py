from bs4 import BeautifulSoup
import requests
import csv

def get_resturants(num) :
    # From Lisa's references: scrape using tags
    city_code = 'New+York%2C+NY'
    url = 'http://www.yelp.com/search?find_desc=Restaurants&find_loc='+city_code[4]+'&start='+str(num)
    page = requests.get(url)
    soup = BeautifulSoup(page.text,"html.parser")
    rests_info = soup.find_all("li", {"class":"regular-search-result"})
    return rests_info

def main() :
    num = 1
    headers = ['Index','Name','Address','Zip Code','Price','Review Count','Rating']
    with open('yelp_Dallas.csv','w') as f:
        f_csv = csv.writer(f)
        f_csv.writerow(headers)
        for i in range(0,90) : #index for going through the website
            rests_info = get_resturants(i*10)#because there are 10 records per page
            for rest in rests_info:
                #print ("Restaurant #"+str(num))#index for restaurants
                rest_rating = rest.find('div',{'class':'rating-large'}).i['title'][0:3]
                #print ("Restaurant Rating: "+rest_rating)
                rest_name = rest.find('a',{'class':'biz-name'}).text.encode('utf-8')#name of restaunts
                #print("Restaurant Name: "+ rest_name)
                rest_address = rest.find('address').text[14:].replace('\n',' ').encode('utf-8')#address of restaunts
                #print("Restaurant Address: "+ rest_address)
                rest_zip = rest_address.replace(' ','')[-5:].encode('utf-8')#zip code of restaunts
                if not rest_zip.isdigit():
                    rest_zip = 'N/A'
                #print("Restaurant Zip Code: "+ rest_zip)
                try :
                    rest_price = rest.find('span',{'class':'business-attribute price-range'}).text.encode('utf-8')
                    #price of restaunts, some restuants don't have the price attribute, but those are usually '$' restaunts, we record them as '0', but when future counting, we still see them as '$' restaunts.
                    #print("Restaurant Price: "+ rest_price)
                except AttributeError:
                    rest_price = '0'
                    #print("Restaurant Price: 0")
                rest_review_count = rest.find('span',{'class':'review-count rating-qualifier'}).text.encode('utf-8')
                rest_review_count = rest_review_count.replace('\n','')[12:].split(' ')[0]
                rest_info = [num,rest_name, rest_address,rest_zip,rest_price,rest_review_count,rest_rating] #store in a list
                #print rest_info
                f_csv.writerow(rest_info) #write the list into csv file
                num += 1

    return


main()
