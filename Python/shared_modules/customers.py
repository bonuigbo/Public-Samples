"""
        @author: bonuigbo
        @summary: This python struct contains a
        list of customers by their slug, and currently
        a list of their data maps from their own
        specific data to the optimal format for
        that particular order type

"""

import data_maps

CUSTOMERS = {
    "association-management-services":{
        "CRT":{
            "data_map":data_maps.JENARK_CRT_TO_OPTIMAL,
        },
    },
    "optimal":{
        "CRT":{
            "data_map":data_maps.OPTIMAL_IDENTITY,
        },
    },
    "kw-property-management":{
        "CRT":{
            "data_map":data_maps.KW_CRT_TO_OPTIMAL,
        },
    },
}
