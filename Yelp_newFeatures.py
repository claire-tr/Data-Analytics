from operator import itemgetter
import csv

#read data from file and store in list
city = ['DC','NYC','Boston','SanFrancisco','Dallas',
        'LA','Chicago','Houston', 'Philadelphia', 'Phoenix',
        'SanAntonio','SanDiego','SanJose', 'Austin','Jacksonville',
        'Indianapolis','Columbus', 'FortWorth', 'Charlotte','Detroit',
        'ElPaso','Seattle','Denver', 'Memphis','Nashville']
for c in range(0,25):
    Rests = [] #Restaurants' infomation
    rest = [] #temporary list store information for current restaurant
    with open('yelp_'+city[c]+'.csv') as f:
        f_csv = csv.reader(f);
        headers = next(f_csv) #read headers from csv file
        for rest in f_csv:
            Rests.append(rest)

    Rests.sort(key=itemgetter(3))

    headers = ['Zip Code','$','$$','$$$','$$$$','major_restaunt','average_price_in_$_sign','average_rating']

    price = []
    zip = Rests[0][3]
    row =[zip,0,0,0,0]
    rating_total = 0.0
    for r in Rests:
        if(r[3]==zip):
            row[len(r[4])] += 1 #counters for certain price restaunts in a zip code area
            rating_total += float(r[6])
        if(r[3]!=zip):
            row_max = max(row[1:])
            major_index = row.index(row_max) #major restaurants in a zip code area
            row.append(headers[major_index])
            (total,count)=(0.0,0.0); # count for weighted average price in a zip code area
            for i in range(1,5):
                total += row[i]*i
                count += row[i]
            row.append(total/count) #weighted average for price in a zip code area
            row.append(rating_total/count) #average rating within a zip code area
            price.append(row)
            rating_total = 0.0
            # start stats in a new zip code area
            zip = r[3]
            row =[zip,0,0,0,0]
            row[len(r[4])] +=1
            rating_total += float(r[6])
    
    with open('yelp_price_'+city[c]+'.csv','w') as f:
        f_csv = csv.writer(f)
        f_csv.writerow(headers)
        f_csv.writerows(price)

#print price
