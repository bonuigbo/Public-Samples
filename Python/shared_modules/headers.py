"""
        @author: Brian Onuigo
        @summary:
        This module just stores a list of the curret
        optimal file headers for gm, blts, certified,
        and assessment orders.
        @todo: Turn this into a script that generates
        these lists by connecting to the database
"""

GM =[
        "Optimal ID",
        "Customer Slug",
        "Order Number",
        "Association Key",
        "Association Name",
        "Account Number",
        "Property Address",
        "Homeowner Name 1",
        "Homeowner Name 2",
        "Homeowner Address 1",
        "Homeowner Address 2",
        "Homeowner Address 3",
        "Homeowner Address 4",
        "Homeowner Address Last",
        "Mailing DPC",
        "Mailing Barcode",
        "Remit Address 1",
        "Remit Address 2",
        "Remit Address 3",
        "Remit Address 4",
        "Remit Address Last",
        "Return Address 1",
        "Return Address 2",
        "Return Address 3",
        "Return Address 4",
        "Return Address Last",
        "Return DPC",
        "Return Barcode",
        "Window Message",
        "Ballot Type",
        "Delivery Information",
        "Job Option",
        "Newsletter Name",
        "Newsletter Option 1",
        "Newsletter Option 2",
        "Free Form Header",
        "Free Form Paragraph 1",
        "Free Form Paragraph 2",
        "Free Form Paragraph 3",
        "Free Form Paragraph 4",
        "Utility 1",
        "Utility 2",
        "Utility 3",
        "Utility 4",
        "Utility 5"
]

JENARK_CRT = [
    "GL Entity",
    "App Num",
    "Building",
    "Unit",
    "Resident",
    "First Name",
    "Last Name",
    "Billing Address 1",
    "Billing Address 2",
    "Bill City",
    "Bill St",
    "Bill Zip",
    "Property",
    "ResidentBalance",
    "Unit_22",
    "CH Code",
    "CH Date",                  
]

PANDORA_CRT = ['pdf_document','homeowner_name','address_1','address_2','city_st_zip']
KC_CRT = ["Unit No","Name","Addres1","Address2","City","State","Zip"]

