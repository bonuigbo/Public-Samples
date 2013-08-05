"""
    @author:bonuigbo
    @summary:Allows for the manipulation of pdf files.
    Mostly for combining many into one, splitting one into
    many, and merging addresses and contents
"""

import os
import shutil
from pyPdf import PdfFileWriter, PdfFileReader
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter

RESOURCE_DIR = r'D:\file_factory\pdf_tool\resources'
BLANK_PDF = r'D:\file_factory\pdf_tool\resources\blank.pdf'
TEMP_PDF = r'D:\file_factory\pdf_tool\resources\temp.pdf'
DRIVER_PDF = r'D:\file_factory\pdf_tool\resources\driver.pdf'
START_DX = 83.5
START_DY = 21
START_DH = 10
START_DMOD = 1

class PdfTool(object):
    def __init__(self):
        pass
    
    @classmethod
    def get_pdf_length(cls, pdf_file):
        '''
            Just gets the number of pages in a pdf file
        '''
        pdf_stream = file(pdf_file, 'rb')
        pdf_reader = PdfFileReader(pdf_stream)
        total_pages = pdf_reader.getNumPages()
        pdf_stream.close()
        return total_pages
                
    @classmethod
    def merge_pdfs(cls, input_pdf_dir, output_dir = '', output_name = 'merged.pdf'):
        '''
            Takes a list of pdf files, merges them, returns the output
            pdf file. Ignores anything that isn't a pdf
        '''
        new_path = os.path.join(input_pdf_dir, "merged")
        if os.path.exists(new_path):
            shutil.rmtree(new_path)
        os.makedirs(new_path)
        pdf_streams = []
        joined_file_name = os.path.join(new_path, output_name)        
        joined_stream = file(joined_file_name, 'wb')
        joined_writer = PdfFileWriter()
        for raw_file in os.listdir(input_pdf_dir):
            if str(raw_file).lower().endswith('pdf'):
                pdf_file = os.path.join(input_pdf_dir, raw_file)
                pdf_stream = file(pdf_file, 'rb')
                pdf_reader = PdfFileReader(pdf_stream)
                for page in pdf_reader.pages:
                    joined_writer.addPage(page)
                pdf_streams.append(pdf_stream)
        joined_writer.write(joined_stream)
        joined_stream.close()
        for stream in pdf_streams:
            stream.close()
        return joined_file_name
        
    @classmethod
    def split_pdfs(cls, original_pdf, counts = None, pdf_length = None):
        '''
            Takes a pdf and splits it, renaming it to a list of
            file names if providd, or integers if not. If counts
            are supplied, the files are split by the counts, else
            they are matched up to the length of the file names
        '''
        if not pdf_length and not counts:
            return None
        split_files = []
        file_path, filen = os.path.split(original_pdf)
        if not os.path.exists(os.path.join(file_path, 'splits')):
            os.makedirs(os.path.join(file_path, 'splits'))
        file_path = os.path.join(file_path, 'splits')
        pdf_stream = file(original_pdf, 'rb')
        pdf_reader = PdfFileReader(pdf_stream)
        total_pages = pdf_reader.getNumPages()
        if pdf_length:
            counts = [pdf_length for i in range(total_pages/pdf_length)]
        current_page = 0
        for i, count in enumerate(counts):       
            pdf_name = str(i) + '_split.pdf'         
            list_of_pages = range(current_page, current_page + count)
            current_page += count                        
            pdf_file_name = os.path.join(file_path, pdf_name)
            split_files.append(pdf_file_name)
            print_stream = file(pdf_file_name, 'wb')
            print_writer = PdfFileWriter()
            for page_index in list_of_pages:
                print_writer.addPage(pdf_reader.getPage(page_index))    
            print_writer.write(print_stream)
            print_stream.close()
        pdf_stream.close()
        return split_files
        
    @classmethod
    def generate_address_file(cls, records):
        '''
            Generates an address file from a dictionary of records. Requires the
            following fields
            Address1, . . . ,Address4, AddressLast, ReturnAddress1, ... ReturnAddress4,
            ReturnAddressLast
        '''
        address_driver_buffer_stream = file(TEMP_PDF, 'wb')
        address_driver_pdf = PdfFileWriter()
        for record in records:
            dX = START_DX
            dY = START_DY
            dH = START_DH
            dMod = START_DMOD
            c = canvas.Canvas(DRIVER_PDF, pagesize=letter, bottomup = 0)
            c.setFont("Helvetica", 9)
            fields = ['1','2','3','4','Last']
            for field in fields:                
                if "Return Address " + field in record and record["Return Address " + field]:
                    c.drawString(dX, dY + (dH * dMod), record["Return Address " + field])
                    dMod += 1
            dMod += 7
            dY = dY + 3
            fields = ['Name 1','Address 1','Address 2',' Address 3','Address 4','Address Last']
            for field in fields:                
                if "Homeowner " + field in record and record["Homeowner " + field]:
                    c.drawString(dX, dY + (dH * dMod), record["Homeowner " + field])
                    dMod += 1
            c.showPage()
            c.save()
            final_pdf = PdfFileReader(file(DRIVER_PDF, 'rb'))
            address_driver_pdf.addPage(final_pdf.getPage(0))
            address_driver_pdf.write(address_driver_buffer_stream)
        address_driver_buffer_stream.close()
        return TEMP_PDF
        
    @classmethod
    def merge_address_with_contents(cls, address_file, contents_file, length_of_each_contents, merged):
        '''
            Takes a single impression address file and merges it with
            a contents file
        '''
        pdf_path = os.path.dirname(address_file)
        blank_stream = file(BLANK_PDF, 'rb')
        blank_reader = PdfFileReader(blank_stream)
        joined_file_name = os.path.join(pdf_path, "merged_print.pdf")
        joined_stream = file(joined_file_name, 'wb')
        joined_writer = PdfFileWriter()
        address_stream = file(address_file, 'rb')
        address_reader = PdfFileReader(address_stream)
        contents_stream = file(contents_file, 'rb')
        contents_reader = PdfFileReader(contents_stream)
        current_contents_page = 0
        for page in address_reader.pages:
            list_of_contents_pages = range(current_contents_page, 
                                           current_contents_page + length_of_each_contents)
            joined_writer.addPage(page)
            if length_of_each_contents == 1:
                    for page_index in list_of_contents_pages:
                        joined_writer.addPage(contents_reader.getPage(page_index))
            else:             
                joined_writer.addPage(blank_reader.getPage(0))
                for page_index in list_of_contents_pages:
                    joined_writer.addPage(contents_reader.getPage(page_index))
                    # If the total number of impressions are odd, add another blank
                    if length_of_each_contents % 2 != 0:
                        joined_writer.addPage(blank_reader.getPage(0))
            if merged:
                current_contents_page += length_of_each_contents
        joined_writer.write(joined_stream)
        joined_stream.close()
        address_stream.close()
        contents_stream.close()
        blank_stream.close()
        return joined_file_name
        
    def add_blank(self, pdf_writer):
        '''
            Adds a blank page to the opened PdfFileWriter
        '''
        blank_pdf = os.path.join(self.location, 'resources', 'blank.pdf')
        blank_stream = file(blank_pdf, 'rb')
        blank_reader = PdfFileReader(blank_stream)
        pdf_writer.addPage(blank_reader.getPage(0))
        blank_stream.close()
        
if __name__ == "__main__":
    records = [
        {"Homeowner Address 1":"BONU","Homeowner Address 2":"7 RanchoCircle","Homeowner Address Last":"Lake Forest, CA 21234",
         "Return Address 1":"BONU2","Return Address 2":"7 Rancho  Circle","Return Address Last":"Lake Forest, CA 44112"},
        {"Homeowner Address 1":"UKNOW","Homeowner Address 2":"7 RanchoCircle","Homeowner Address Last":"Lake Forest, CA 21234",
         "Return Address 1":"THISMAAAN","Return Address 2":"7 Rancho  Circle","Return Address Last":"Lake Forest, CA 44112"},
    ]
    work_dir = r'd:\file_factory\pdf_tool'
    merge_dir = r'D:\file_factory\pdf_tool\merged\merged.pdf'
    counts2 = [5,3,2,1]
    PdfTool.merge_pdfs(work_dir)
    PdfTool.split_pdfs(merge_dir, counts = counts2)
    address_pdf = PdfTool.generate_address_file(records)
    print address_pdf