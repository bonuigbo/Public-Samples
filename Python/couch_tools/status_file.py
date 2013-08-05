import csv
import os
    
class StatusFile(object):
    '''
        For managing the completion statuses of the various scripts
        format. Pretty much a ghetto database
    '''
    headers = ['script','complete','start_time','run_time','log']
    def __init__(self, log_loc = ''):
        self.log_loc = log_loc
        self.status_doc = os.path.join(log_loc, 'script_status.txt')
        if not os.path.exists(self.status_doc):
            self.create_status_file()
        self.load_file()
    
    def load_file(self):
        self.records = []        
        with open(self.status_doc, 'rb') as file_name:
            records_reader = csv.reader(file_name)
            for (count, row) in enumerate(records_reader):
                if count == 0:
                    continue
                record = self.convert_to_dic(row)
                self.records.append(record)
    
    def convert_to_dic(self, row):
        record = {}
        for index in range(len(StatusFile.headers)):
            record[StatusFile.headers[index]] = row[index]
        return record
                   
    def write_file(self):
        with open(self.status_doc, 'wb') as stat_file:
            file_writer = csv.DictWriter(stat_file, StatusFile.headers)
            #file_writer.writeheader()
            file_writer.writerow(dict(zip(StatusFile.headers, StatusFile.headers)))
            file_writer.writerows(self.records)
            
    def create_status_file(self):
        with open(self.status_doc, 'wb') as stat_file:
            file_writer = csv.DictWriter(stat_file, StatusFile.headers)
            #file_writer.writeheader()
            file_writer.writerow(dict(zip(StatusFile.headers, StatusFile.headers)))
            
    def get_status(self, script_name):
        for record in self.records:
            if record['script'] == script_name:
                return record
        return None
            
    def update(self, script_name, updated_row):
        updated_record = self.convert_to_dic(updated_row)
        for record in self.records:
            if record['script'] == script_name:
                for key in record.keys():
                    record[key] = updated_record[key]
                updated_record = None
                break
        if updated_record:
            self.records.append(updated_record)
    
    