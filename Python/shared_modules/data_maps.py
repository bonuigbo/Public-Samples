"""
        @author: bonuigbo
        @summary: This module contains mapping from
        data files of various formats to other data files
        of various formats, as well as special functions
        to assist the converter in geneating the map

        Currently three fields required
        1. The fields being mapped, which is in list format
        2. An integer representing a code to call one of the
        following functions of the data_converter
            0:Converter.identity,
            1:Converter.join,
            2:Converter.join_and_strip_nonalpha,
        3. An optional separator. Defaults to a space if not supplied
"""
import headers

JENARK_CRT_TO_OPTIMAL = {
    "headers":"true",
    "input_headers":headers.JENARK_CRT,
    "output_headers":headers.GM,
    # Header fields in the data with these values will be skipped
    "ignore_fields":{
        "CH Code":"C1"
    },
    "data_map":{
        "Homeowner Name 1" : [    ["First Name", "Last Name"],2    ],
        "Homeowner Address 1" : [    ["Billing Address 1"], 0    ],
        "Homeowner Address 2": [    ["Billing Address 2"], 0    ], 
        "Homeowner Address Last": [    ["Bill City","Bill St", "Bill Zip"], 1    ],
        "Property Address" : [    ["Property"], 0     ], 
        "Account Number" : [    ["App Num"], 0    ],
        "Association Key" : [    ["GL Entity"], 0    ],
    },
}

KW_CRT_TO_OPTIMAL = {
    "headers":"true",
    "input_headers":'',
    "output_headers":headers.GM,                  
    "data_map":{
        "Homeowner Name 1" : [    ["First Name", "Last Name"],2    ],
        "Homeowner Address 1" : [    ["Billing Address 1"], 0    ],
        "Homeowner Address 2": [    ["Billing Address 2"], 0    ], 
        "Homeowner Address Last": [ ["City","St","Zip"], 1    ], 
        "Account Number" : [    ["App Num"], 0    ],
        "Property Address" : [    ["Address"], 0     ], 
        "Association Key" : [    ["GL Entity"], 0    ],
    },
    "duplicate":{
        "Homeowner Address 1": [    ["Address"], 0    ],
        "Homeowner Address 2": "",
        "Homeowner Address Last":"Miami FL 33032",
    },
}

OPTIMAL_IDENTITY = {
    "headers":"true",
    "input_headers":headers.GM,
    "output_headers":headers.GM,                          
    "data_map":{
        "Homeowner Name 1" : [    ["Homeowner Name 1"],0    ],
        "Homeowner Address 1" : [    ["Homeowner Name 1"], 0    ],
        "Homeowner Address 2": [    ["Homeowner Address 1"], 0    ], 
        "Homeowner Address Last": [    ["Homeowner Address Last"], 0    ], 
        "Account Number" : [    ["Account Number"], 0    ],
    },
}