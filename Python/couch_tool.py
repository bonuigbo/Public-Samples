'''
2012-10-25

@author: bonuigbo

Manages the following tasks:

1. Replicate AES and CRT documents from MAKALU to VISTOFTE
    Runs at the first of the month at midnight
2. Once above is done, clears AES impressions and insert sources
    Runs at 22:00 on the first friday of the month
3. Compacts MAKALU and HALLAND, replicates MAKALU to HALLAND
    Runs afte 6:00 on the first sunday of the month
'''

import sys
import couchdb
import calendar
import smtplib

from datetime import datetime, date
from email.mime.text import MIMEText

from couch_tools.settings import SETTINGS
from couch_tools.doc_mover import DocMover
from couch_tools.doc_cleaner import DocCleaner
from couch_tools.doc_manager import DocManager
from couch_tools.status_file import StatusFile

class CouchTool(object):
    '''
        Takes in the script name, runs the script, updates
        the status file
    '''
    def __init__(self):        
        time_list = datetime.today().timetuple()
        self.script = None
        self.status_file = StatusFile(SETTINGS['log_dir'])
        #current_year = time_list[0]
        #current_month = time_list[1]
        current_day = time_list[2]
        current_hour = time_list[3]
        current_weekday = time_list[6]
        #last_day_of_month = calendar.monthrange(current_year, current_month)[1]
        # Currently aspen is 8 hours ahead of local time, so everyting is set ahead by 8
        if current_day == 1 and current_hour >= 7 \
                    and not self.completed('doc_mover'):
            self.script = DocMover(SETTINGS)
        elif 1 <= current_day <= 7 and current_hour >= 6 \
                    and current_weekday == 5 and \
                    self.completed('doc_mover') and not self.completed('doc_cleaner'):
            self.script = DocCleaner(SETTINGS)
        elif 2 <= current_day <= 8 and  current_hour >= 14 \
                    and current_weekday == 6 and \
                    self.completed('doc_cleaner') and not self.completed('doc_manager'):
            self.script = DocManager(SETTINGS)       
        
    def run(self):
            if self.script:
                self.script.run()
                record = [ self.script.name, str( self.script.complete ), \
                          datetime.today().isoformat('-'), self.script.run_time, self.script.log ]
                self.status_file.update(self.script.name, record)
                self.email_devs( self.to_message( record ) )
                if self.completed('doc_manager'):
                    self.reset_statuses()
                self.status_file.write_file()        
    
    def reset_statuses(self):
        for record in self.status_file.records:
            record['complete'] = 'False'
    
    def to_message(self, record):
        headers = ['script','complete','start_time','run_time','log']
        message = ''
        for (count, value) in enumerate(record):
            message += headers[count] + ' : ' + str(value) + '\r\n'
        return message
    
    def completed(self, script):
        '''
            Determines if the script has finished this cycle
        '''
        record = self.status_file.get_status(script)
        if not record or record['complete'] == 'False':
            return False
        return True
    
    def email_devs(self, message):
        msg = MIMEText(message)
        msg['Subject'] = 'Couch Tool Log for ' + date.today().isoformat()
        msg['From'] = 'aspen@optimaloutsource.com'
        msg['To'] = 'developers@optimaloutsource.com'
        
        connection = smtplib.SMTP('localhost',
            25)
        connection.sendmail('aspen@optimaloutsource.com',
            'developers@optimaloutsource.com',
            msg.as_string())
        connection.quit()       
            
def main():
    CouchTool().run()
    
if __name__ == '__main__':
    sys.exit(main())