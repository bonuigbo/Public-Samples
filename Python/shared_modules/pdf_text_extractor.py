'''
        @author: Brian Onuigbo
        @summary: This module reads text from pdf files,
        does it's best to identify where the mailing address
        is, and creates an address file based on that
        @requires: pdfminer libray, pyPdf library
'''


import os
import csv
import re

from pdfminer.pdfinterp import PDFResourceManager, process_pdf
from pdfminer.converter import TextConverter
from pdfminer.layout import LAParams
from pyPdf import PdfFileReader

address_regex = '[a-zA-Z ]+,?[ ]+[a-zA-Z]{2}[ ]+\d{5}([-][0-9]{4})?'
working_dir = r'D:\file_factory\crt_processor\text_extractor'

class PdfTextExtractor(object):
    '''
            Extracts all the text from pdf files and generates
            addresses
    '''
    def __init__(self, pdf_file):
        self.pdf_file = pdf_file
        self.raw_text = ''
        self.text_per_page_list = []
        self.total_pages = 0
        self.addresses = []
        print pdf_file
        if os.path.exists(pdf_file):
            self.get_total_pages()
            self.extract_text()
            print 'cool'
            self.generate_addresses()
            self.print_address_file()
        
    def get_total_pages(self):
        '''
                Gets total pages in the pdf to read
        '''
        input_pdf_stream = file(self.pdf_file, 'rb')
        input_pdf_reader = PdfFileReader(input_pdf_stream)
        self.total_pages = input_pdf_reader.getNumPages()
        input_pdf_stream.close()
        
    def extract_text_from_page(self, page_no):
        '''
                Extracts all the text from the given page number in the pdf file
        '''
        file_location = os.path.dirname(self.pdf_file)
        temp_file = os.path.join(file_location, 'temp.txt')
        # Setup options for reading the pdf and outputing all contents to text
        rsrcmgr = PDFResourceManager(caching=True)
        outfp = file(temp_file, 'w')
        codec = 'utf-8'
        laparams = LAParams()
        device = TextConverter(rsrcmgr, outfp, codec=codec, laparams=laparams)
        # Set to include all pages in the pdf
        pagenos = set()
        pagenos.update( [page_no])
        #Setup the reader and read
        fp = file(self.pdf_file, 'rb')
        process_pdf(rsrcmgr, device, fp, pagenos, maxpages=self.total_pages, password='',
                    caching=True, check_extractable=True)        
        #read raw data from text
        outfp.close()
        with open(temp_file, 'r') as temp:
            self.raw_text = temp.read()
        # Delete temporary text file        
        os.remove(temp_file)
        fp.close()
        device.close()
                
    def extract_text(self):
        '''
                Extracts all the raw text from a pdf file, and adds the
                raw text page by page to a list
        '''
        list_of_pages = [x for x in range(self.total_pages)]
        for page in list_of_pages:
            self.extract_text_from_page(page)
            self.text_per_page_list.append(self.raw_text)
        
    def generate_addresses(self):
        '''
                Goes through the raw text data for each page in the pdf
                and extracts the mailing address
        '''
        for raw_text in self.text_per_page_list:
            # First, format the text to be a list of lines
            text_lines = self.convert_raw_text_to_lines(raw_text)
            # Next, get the indices of all lines which contain a city, state zip
            indices = []
            for line_num, line in enumerate(text_lines):
                stripped_line = line.strip()
                m = re.match(address_regex, stripped_line)
                if m:
                    indices.append(line_num)
            # Assuming a max of two city, state zip lines, the first is the return address
            # we are interested in the second, which should be the mailing address
            address_index = indices[0]
            #address_index = indices[len(indices)-1]
            # Add previous four address lines, ignore blanks
            current_address = [text_lines[index] for index in range(address_index-3, address_index+1)
                                                    if text_lines[index].strip()]
            # This normalizes it to having two address fields if there is only 1
            if len(current_address) == 3:
                current_address.insert(3,'')
            # This adds an index to the field, but currently only necessary for another script I use
            current_address.insert(0,'')
            current_address.insert(0,len(self.addresses))
            self.addresses.append(current_address)

    def print_address_file(self):
        '''
                Prints all the addresses to a file in a format
                that can be used by the next script
        '''
        file_location = os.path.dirname(self.pdf_file)
        pdf_file = os.path.basename(self.pdf_file)
        new_file = pdf_file.replace(' ','_').replace('.pdf',"_addresses.csv")
        output_file = os.path.join(file_location, new_file)
        with open(output_file, 'wb') as print_file:
            file_writer = csv.writer(print_file)
            file_writer.writerows(self.addresses)    
        
    def convert_raw_text_to_lines(self, raw_text):
        '''
                Identifies the line breaks in the raw text from the pdf,
                and groups the text accordingly. Removes blank lines
        '''
        text_lines = []
        current_line= ""
        for character in raw_text:
            if '\n' in character:
                text_lines.append(current_line.strip())
                current_line = ""
            current_line += character
        text_lines.append(current_line)
        #text_lines = [line for line in text_lines if line.strip()]
        return text_lines
        
if __name__ == "__main__":
    for pdf_file in os.listdir(working_dir):
        print pdf_file
        if pdf_file.endswith('.pdf'):
            extractor = PdfTextExtractor(os.path.join(working_dir, pdf_file))

