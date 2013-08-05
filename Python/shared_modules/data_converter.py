'''
        @author: bonuigbo
        @summary: Converts data between different formats.
        Works one general way, but is versatile when used
        that way. Depends on a data_map
        @todo: Turn the datamap from a python dictionary
        into a fully functional class
'''
class Options():
    '''
        Represents special parsing options available to the converter
    '''
    IDENTITY = 0
    JOIN = 1
    JOIN_AND_STRIP = 2
        
class Converter():    
    @classmethod
    def convert(cls, raw_records, data_map_struct):
        '''
            Takes in a dictionary of data records, with string
            headers, and maps the fields to a set of new
            records using the new_headers and the data_map
            struct, which also specifies special formatting
            options
        '''
        new_headers = data_map_struct['output_headers']
        data_map = data_map_struct["data_map"]
        converted_records = []
        for record in raw_records:
            #Skip fields indicated in the ignore_fields section
            skip = False            
            if "ignore_fields" in data_map_struct:
                for skip_field, skip_value in data_map_struct["ignore_fields"].items():
                    if record[skip_field] == skip_value:
                        skip = True
            if skip:
                continue
            
            # Initialize all the fields in the new records to blank
            converted_record = {}
            for field in new_headers:
                converted_record[field] = ""
                # Go through all the 
                for new_field, map_struct in data_map.items():
                    # For fields
                    converted_record[new_field] = Converter.map_fields(map_struct, record)
            converted_records.append(converted_record)
            
            # Hack for KW to duplicate fields
            if "duplicate" in data_map_struct:
                dup_map = data_map_struct["duplicate"]
                dup_record = converted_record.copy()
                for new_field, map_struct in dup_map.items():
                    # If direct string, map it
                    if isinstance(map_struct, basestring):
                        dup_record[new_field] = map_struct
                    else:
                        dup_record[new_field] = Converter.map_fields(map_struct, record)
                converted_records.append(dup_record)
        return converted_records
    
    @classmethod 
    def map_fields(cls, map_struct, record):
        '''
                Returns a string specifying how to format
                the given fields being mapped
        '''
        
        # The list of fields being mapped
        old_field_headers = map_struct[0]
        # A list of the actual values from the necessary fields
        data = [str(record[old_header]).strip() for old_header in old_field_headers]
        # The option, a single int, specifying how to map these fields
        option = map_struct[1]
        separator = " "
        if len(map_struct) > 2:
            separator = map_struct[2]
        function_map = {
            0:Converter.identity,
            1:Converter.join,
            2:Converter.join_and_strip_nonalpha,
            4:Converter.join_raw,
         }
        return function_map[option](data, separator)
        
    @classmethod
    def identity(cls, data, separator):
        return data[0]
    
    @classmethod
    def join_and_strip_nonalpha(cls, data, separator):
        '''
            Returns the old values joined by spaces, and with the following
            characters removed:
            _, -, +, ., /,,
            Useful for name fields
        '''
        data = [value.replace("_","").replace("-","") for value in data]
        return separator.join(data)
    
    @classmethod
    def join(cls, data, separator):
        '''
            Basic join of the fields using a specific separator
        '''
        return separator.join(data)
    
    @classmethod
    def join_raw(cls, data, separator):
        '''
            Maps the fields directly in the datamap struct
        '''