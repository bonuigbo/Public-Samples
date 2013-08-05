from shared_modules.data_file_reader import DataFileReader
import csv
import os

results_location = r'C:\Users\BoNu\Desktop\cpsc_485_project_images\data_results'
test_location = r'C:\Users\BoNu\Desktop\cpsc_485_project_images\test_Data'
data_file = os.path.join(results_location, 'scaled_generator.csv')
scaled_results = os.path.join(results_location, "scaled_results.csv")
buyers_business = os.path.join(results_location, "buyers_business.csv")
buyers_country = os.path.join(results_location, "buyers_country .csv")
nonbuyers_business = os.path.join(results_location, "nonbuyers_business.csv")
nonbuyers_country = os.path.join(results_location, "nonbuyers_country.csv")
scaled_business = os.path.join(results_location, "scaled_business.csv")
scaled_country = os.path.join(results_location, "scaled_country.csv")
old_design_data = os.path.join(test_location, "03_design_data.csv")
old_test_data = os.path.join(test_location, "03_test_data.csv")
new_design_data = os.path.join(test_location, "converted_design.csv")
new_test_data = os.path.join(test_location, "converted_test.csv")

class PyDataInterpreter(Object):
    def __init__(self):
        pass

    
    def process(self, data_file):
        self.data_file = data_file

    def set_it_up(self):
        pass
#Data files


headers = ["Class","Country","Type","#Total Packages","Cancelled Orders",\
           "Total Sales For All Items","$Total From Promos","Total Orders Placed","BV"]

reader = DataFileReader()
reader.read_as_csv(data_file)
buyers = {
    "countries": {},
    "businesses": {}
}

nonbuyers = {
    "countries": {},
    "businesses": {}
}

scaled_values = {
    "countries": {},
    "businesses": {}
}
countries = {}
businesses = {}
for row in reader.record_list:
    country = row[1]
    business = row[2]
    if country not in countries.keys():
        countries[country] = 0
    if business not in businesses.keys():
        businesses[business] = 0
    if 'not' in str(row[0]).lower():
        if country not in nonbuyers["countries"].keys():
            nonbuyers["countries"][country] = 0.0
        if business not in nonbuyers["businesses"].keys():
            nonbuyers["businesses"][business] = 0.0
        nonbuyers["businesses"][business] += 1.0
        nonbuyers["countries"][country] += 1.0
    else:
        if country not in buyers["countries"].keys():
            buyers["countries"][country] = 0.0
        if business not in buyers["businesses"].keys():
            buyers["businesses"][business] = 0.0
        buyers["businesses"][business] += 1.0
        buyers["countries"][country] += 1.0      
    countries[country] += 1.0
    businesses[business] += 1.0
         
def set_scaled_values(field):
    for buybusiness, buycount in buyers[field].items():
        normalized_buy = str(buybusiness).strip().lower()
        has_counterpart = False
        for nonbusiness, noncount in nonbuyers[field].items():            
            normalized_non = str(nonbusiness).strip().lower()
            if normalized_non not in scaled_values[field].keys():
                scaled_values[field][normalized_non] = -1.0 * float(noncount)
            if normalized_buy == normalized_non:
                has_counterpart = True
                if buycount > noncount:
                    scaled_values[field][normalized_non] = float(buycount) / float(noncount)
                else:
                    scaled_values[field][normalized_non] = -1.0 * float(noncount)/float(buycount)
        if not has_counterpart:
            scaled_values[field][normalized_buy] = buycount
    
def print_sorted(f_stream, unsorted_dict):
    for teh_item in sorted(unsorted_dict, key = unsorted_dict.get, reverse=True):
        f_stream.write("{0}     -     {1}\n".format(teh_item, unsorted_dict[teh_item]))
        
def print_to_csv(fstream, unsorted_dict):
    print_dict = {}
    for teh_item in sorted(unsorted_dict, key = unsorted_dict.get, reverse=True):
        print_dict[teh_item] = unsorted_dict[teh_item]
    for field, value in print_dict.items():
        fstream.write("\"{0}\",\"{1}\"\n".format(field, value))
        
def generate_scaled_records(data_file, print_file_name):
    reader = DataFileReader()
    reader.read_as_csv(data_file)
    del reader.record_list[0]
    converted_data = []
    for row in reader.record_list:
        converted_row = {}
        count = 0
        for header in headers:
            if count == 1:
                normalized_country = str(row[count]).strip().lower()
                for country, scaled_value in scaled_values['countries'].items():
                    if normalized_country == country:
                        converted_row[header] = str(scaled_value)
            elif count == 2:
                normalized_business = str(row[count]).strip().lower()
                for business, scaled_value in scaled_values['businesses'].items():
                    if normalized_business == business:
                        converted_row[header] = str(scaled_value)
            else:
                converted_row[header] = row[count]
            count += 1
        converted_data.append(converted_row)
    reader.print_file(print_file_name, converted_data, headers)
    
set_scaled_values("businesses")
set_scaled_values("countries")


    
with open(scaled_results, 'wb') as f:
    f.write("Buyers\n")
    f.write("Country,Count\n")
    print_to_csv(f, buyers["countries"])
    f.write("Business,Count\n")
    print_to_csv(f, buyers["businesses"])
    f.write("Not Buyers\n")
    f.write("Country,Count\n")
    print_to_csv(f, nonbuyers["countries"])
    f.write("Business,Count\n")
    print_to_csv(f, nonbuyers["businesses"])
    f.write("Scales\n")
    f.write("Country,Value\n")
    print_to_csv(f, scaled_values["countries"])
    f.write("Business,Value\n")
    print_to_csv(f, scaled_values["businesses"])
    
with open(buyers_business, 'wb') as f:
    f.write("Business,Count\n")
    print_to_csv(f, buyers["businesses"])
        
with open(buyers_country, 'wb') as f:
    f.write("Country,Count\n")
    print_to_csv(f, buyers["countries"])
        
    
with open(nonbuyers_business, 'wb') as f:
    f.write("Business,Count\n")
    print_to_csv(f, nonbuyers["businesses"])    
    
with open(nonbuyers_country, 'wb') as f:
    f.write("Country,Count\n")
    print_to_csv(f, nonbuyers["countries"])   
     
with open(scaled_business, 'wb') as f:
    f.write("Business,Value\n")
    print_to_csv(f, scaled_values["businesses"])   
     
with open(scaled_country, 'wb') as f:
    f.write("Country,Value\n")
    print_to_csv(f, scaled_values["countries"])
    
generate_scaled_records(old_design_data, new_design_data)
generate_scaled_records(old_test_data, new_test_data)
