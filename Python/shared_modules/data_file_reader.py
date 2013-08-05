import csv
import xlrd

"""
    This module is meant to read any of the various types of data files that
    we may get from our clients, and to convert the file into a list
    of records
"""


class DataFileReader(object):
    
    """
            Initializes the Data File Manager
    """
    def __init__(self):
        self.record_list = []
        self.headers = []

    """
            Determines what type of file is being read, an calls that specific reader,
            creating a list of records
    """
    def read_file(self, raw_file, delimiter = ','):
        if raw_file.lower().endswith( 'csv' ):
            try:
                self.read_as_xls(raw_file)
            except:
                self.read_as_csv(raw_file)
        elif raw_file.lower().endswith('xls'):
            try:
                self.read_as_xls(raw_file)
            except:
                self.read_as_csv(raw_file)
        else:
            self.read_as_text(raw_file, delimiter)        


    """
            Reads the file as a normal csv file
    """
    def read_as_csv(self, file):
        with open(file, 'rb') as file_name:
            records_reader = csv.reader(file_name)
            for row in records_reader:
                self.record_list.append(row)
                    
    """
            Reads the file as an xls file
    """
    def read_as_xls(self, file):
        xls_workbook = xlrd.open_workbook(file)
        xls_sheet = xls_workbook.sheet_by_index(0)
        for row in range(xls_sheet.nrows):
            # Skips any empty rows
            if self.is_empty_row(row, xls_sheet):
                continue
            current_record = []
            # Initialize current record to blank, and start reading in fields
            for col in range(xls_sheet.ncols):
                current_record.append(xls_sheet.cell(row,col).value)
            self.record_list.append(current_record)
                    
    """
            Reads the file as raw text and uses the delimiter to parse
    """
    def read_as_text(self, file, idelimiter):
        with open(file, 'rb') as file_name:
            records_reader = csv.reader(file_name, delimiter = idelimiter)
            for row in records_reader:
                self.record_list.append(row)
                
    """
        Determines if the row is empty
    """
    def is_empty_row(self, row, xls_sheet):
        data = ''
        for col in range(xls_sheet.ncols):
            data += str(xls_sheet.cell(row,col).value)
        if len(data) < 10:
            return True
        return False
        
    """
            If the first row of the records are the headers, uses that to turn each record
            into  a record dictionary
    """    

    def create_dict_list(self):
        self.headers = self.record_list[0]
        del self.record_list[0]
        self.create_dict_list_from(self.headers)
        
    """
            Uses the input headers of the file to turn each record into a dictionary which maps the
            header to the headers value in that record
    """
    
    def create_dict_list_from(self, headers):
        self.headers = headers
        record_dict_list = []
        for record in self.record_list:
            record_dict = self.generate_record_dict(self.headers, record)
            record_dict_list.append(record_dict)
        self.record_list = record_dict_list
    
    """
            Maps string numbers to each row
    """    
    def create_num_dict_list(self):
        self.headers = range(0, len(self.records_list[0]))
        record_dict_list = []
        for record in self.record_list:
            record_dict = self.generate_record_dict(self.headers, record)
            record_dict_list.append(record_dict)
        self.record_list = record_dict_list
        
    """
            Takes a list of strings and associates keys with that string, returning a dictionary
    """
    def generate_record_dict( self, headers, row ):
        record_dictionary = {}
        for field_name, field_value in zip(headers, row ):
            record_dictionary[ field_name ] = field_value
        return record_dictionary
    
    """
            Prints out the list of records into a csv file
    """
    def print_file(self, outputfile, records = [], headers = []):
        with open(outputfile, 'wb') as file:
            if records and headers:
                    file_writer = csv.DictWriter(file, headers)
                    file_writer.writeheader()
                    file_writer.writerows(records)    

    """
            This method determines if the data file has headers, and which row
    """
    def has_headers(self):
        contains_headers = False
        check_count = 0
        search_strings = ['name', 'address', 'city', 'state', 'zip']
        row_count = 0
        for row in self.record_list:
            for field in row:
                for search_string in search_strings:
                    if search_string in str(field).lower():
                        check_count += 1
                        break
            if check_count >= 3:
                contains_headers = True
                break
            row_count += 1
        return contains_headers
    
"""
        For the exceptions this class may throw
"""
class DataFileReaderException(Exception):
    def __init__(self, message):
        self.message = message
        
    def __str__(self):
        return repr(self.message)