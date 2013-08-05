'''
2012-10-25

@author: bonuigbo
@summary:
    This is a tool that I've written to help coworkers
    process the bulk of certified mail that we receive.
    This process automates alot of manual work
    that is usually done, although due to the dynamic
    nature of certified mail, there is still more steps
    to do before and after processing
'''


import os
import shutil
from shared_modules.data_file_reader import DataFileReader
from shared_modules.customers import CUSTOMERS
from shared_modules.data_converter import Converter
from shared_modules.pdf_tool import PdfTool

return_address_file = r'D:\file_factory\resources\return_addresses.csv'
customer_dir = r'D:\file_factory\crt_processor\customers'

class CertifiedError(Exception):
    def __init__(self, message):
        self.message = message
    def __str__(self):
        return repr(self.message)
        
class Order(object):
    '''
        Stores relevant information for each order being processed
    '''
    def __init__(self):
        self.raw_records = []
        self.optimal_records = []
        self.data_map = None
        self.order_name = ''
        self.order_path = ''
        self.customer = ''
        self.document_files = []
        self.data_files = []
        self.total_pdf_pages = 0
        self.size_of_each_pdf = 0
        self.merged = True # Merged means the doc file has multiple resident documents


class CertifiedTool(object):
    """
        Handles parsing customer's certified data into 
        an Optimal Format, updating the records on the database,
        and creating print files
    """
    def __init__(self):
        self.converter = Converter()
        self.printer = DataFileReader()
        self.order = None
        self.order_list = []
    
    """
            Main function to call which does all the processings
    """
    def run(self):
        try:
            for customer_folder in os.listdir(customer_dir):
                folder_path = os.path.join(customer_dir, customer_folder)
                for order_folder in os.listdir(folder_path):
                    if "CRT" in order_folder and not "zip" in order_folder:
                        print("Processing {0} for {1}".format(order_folder, customer_folder))
                        order_path = os.path.join(folder_path, order_folder)
                        # Here we map all the relevant information to process each order
                        try:
                            customer = CUSTOMERS[customer_folder]
                        except KeyError:
                            print "No configuration found for %s - %s, moving along" % (customer_folder, order_folder)
                            continue
                        self.order = Order()
                        self.order.order_path = order_path
                        self.order.order_name = order_folder
                        self.order.customer = customer_folder
                        self.order.data_files = self.get_data_files(order_path)
                        self.order.document_files = self.get_document_files(order_path)
                        self.order.data_map = customer["CRT"]["data_map"]
                        
                        # Now we generate the optimal file and upload documents
                        self.read_raw_records()
                        # Downloaded optimal packages already have a data file and
                        # fix the mapping
                        if self.order.customer in "optimal":
                            self.order.optimal_records = self.order.raw_records
                        else:
                            self.convert_records()
                            self.map_cabinet_data()
                        
                        # Now we generate the pdf splits and print them
                        self.split_pdfs()
                        self.print_records()
                        self.print_pdfs()
                        self.order_list.append(self.order)
        except CertifiedError as e:
            print e

    def read_raw_records(self):
        '''
            Grabs the first record from the list of data files, and reads the records in
        '''
        if len(self.order.data_files) == 0:
            raise CertifiedError("Order {0} has no data file".format(self.order.order_name))
        if len(self.order.data_files) > 1:
            print self.order.data_files
            raise CertifiedError("Order {0} has more than one data file".format(self.order.order_name))
        records = []
        data_file_path = self.order.data_files[0]
        data_reader = DataFileReader()
        data_reader.read_file(data_file_path)
        if self.order.data_map["headers"]:
            data_reader.create_dict_list()
        else:
            data_reader.create_num_dict_list()
        for record in data_reader.record_list:
            records.append(record)
        if len(records) < 1:
            raise CertifiedError("Order {0} has no records in its data file".format(self.order.order_name))
        self.order.raw_records = records

    def convert_records(self):
        '''
            Maps the customer data file to the optimal file
        '''
        customer = CUSTOMERS[self.order.customer]
        data_map = customer["CRT"]["data_map"]
        self.order.optimal_records = Converter.convert(self.order.raw_records, data_map)
        for record in self.order.optimal_records:
            record['Customer Slug'] = self.order.customer
            record['Order Number'] = self.order.order_name
        
    def map_cabinet_data(self):
        '''
            Maps the return address, and anything else from cabinets that necessary
        '''
        return_address = self.get_return_address(self.order.customer)
        for count, record in enumerate(self.order.optimal_records):
            self.order.optimal_records[count]["Return Address 1"] = return_address["address_line_1"]
            self.order.optimal_records[count]["Return Address 2"] = return_address["address_line_2"]
            self.order.optimal_records[count]["Return Address 3"] = return_address["address_line_3"]
            self.order.optimal_records[count]["Return Address 4"] = return_address["address_line_4"]
            self.order.optimal_records[count]["Return Address Last"] = return_address["address_last_line"]
        
    def get_return_address(self, customer):
        '''
            Gets the return address, either from the database or the resources file
        '''
        data_reader = DataFileReader()
        data_reader.read_file(return_address_file)
        data_reader.create_dict_list()
        matches = [record for record in data_reader.record_list if customer == record["slug"]]
        if not matches or len(matches) > 1:
            raise CertifiedError( 'Customer %s has %s return address in the csv file' % (customer, len(matches) ))
        return matches[0]
        
    
    def split_pdfs(self):
        '''
            Splits each pdf, assumes the ordering of the pdf matches
            the ordering of the records in the data_file
        '''
        if len(self.order.document_files) == 0:
            raise CertifiedError("Order {0} has no data file".format(self.order.order_name))
        if len(self.order.document_files) > 1:
            print self.order.data_files
            raise CertifiedError("Order {0} has more than one data file".format(self.order.order_name))
        self.order.total_pdf_pages = PdfTool.get_pdf_length(self.order.document_files[0])
        split_path = os.path.join(self.order.order_path, 'documents', 'splits')
        if os.path.exists(split_path):
            shutil.rmtree(split_path)
        self.order.size_of_each_pdf = self.order.total_pdf_pages / len(self.order.optimal_records)
        split_pdfs = PdfTool.split_pdfs(self.order.document_files[0], 
                                        pdf_length = self.order.size_of_each_pdf)
        # rename the pdfs to have the homeowner name, and use that name to map
        for count, record in enumerate(self.order.optimal_records):
            file_dir, file_name = os.path.split(split_pdfs[count])
            new_name = record['Homeowner Name 1'].replace('-','').replace(' ','_').replace(',','')
            new_name = new_name.replace('*','').replace('.','')[0:12] + '_' + file_name
            record["Newsletter Name"] = new_name
            os.rename(split_pdfs[count], os.path.join(file_dir, new_name))
            
        
    def get_data_files(self, order_path):
        '''
                Grabs all readable data files with commonly known
                extensions
        '''
        data_files = []
        data_path = os.path.join(order_path, "data")
        for data_file in os.listdir(data_path):
            if (data_file.lower().endswith("xls") or data_file.lower().endswith("csv") \
                or data_file.lower().endswith("pan")) and "certified_archive" not in data_file:
                data_files.append(os.path.join(data_path, data_file))
        return data_files
        
    def get_document_files(self, order_path):
        doc_files = []
        doc_path = os.path.join(order_path, "documents")
        for doc_file in os.listdir(doc_path):
            if doc_file.lower().endswith("pdf"):
                doc_files.append(os.path.join(doc_path, doc_file))
        return doc_files
    
    def print_records(self):
        '''
            Prints out the csv optimal file and the documents to a custom crt path
        '''
        crt_path = os.path.join(self.order.order_path, 'crt')
        if os.path.exists(crt_path):
            shutil.rmtree(crt_path)
        os.makedirs(crt_path)
        print_dir = os.path.join(crt_path, self.order.order_name + '_optimal_records.csv')
        printer = DataFileReader()
        printer.print_file(print_dir, self.order.optimal_records, 
                           self.order.data_map["output_headers"])
        split_path = os.path.join(self.order.order_path, 'documents', 'splits')
        new_path = os.path.join(self.order.order_path, 'crt', 'splits')
        shutil.move(split_path, new_path)
        
    def print_pdfs(self):
        '''
            Generates inhouse print pdfs
        '''
        address_file = PdfTool.generate_address_file(self.order.optimal_records)
        print_file = PdfTool.merge_address_with_contents(address_file, 
                                                        self.order.document_files[0],
                                                        self.order.size_of_each_pdf,
                                                        merged = self.order.merged)
        inc_amount = 1
        if self.order.size_of_each_pdf > 1:
            inc_amount += 1
        new_file_name = self.order.order_name + '_counts' + str(len(self.order.optimal_records)) \
                                    + '_imp' + str(self.order.size_of_each_pdf + inc_amount) + '.pdf'
        new_path = os.path.join(self.order.order_path, 'production', new_file_name)
        shutil.move(print_file, new_path)
        
if __name__ == "__main__":
    CertifiedTool().run()