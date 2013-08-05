import couchdb
import datetime
from datetime import date, timedelta
import time
from script import Script, ScriptError

'''
Created 06-30-2012

@author: bonuigbo
'''

    
class DocCleaner(Script):
    """
            Does the following:
                1. Clear out all Assessment newsletters for the past month
                2. Clear out all uploaded documents older than 90 days
    """
    def __init__(self, SETTINGS):
        super(DocCleaner, self).__init__(SETTINGS)
        self.name = 'doc_cleaner'
        server = SETTINGS['couchdbs']['makalu']
        self.server = couchdb.client.Server(url=server['url'])
        self.db = self.server[server['db']]
        today = date.today()
        doc_cutoff_day = today - timedelta(days=90)
        self.aes_cutoff_tuple = today.year, today.month, 1, 0, 0, 0
        self.doc_cutoff_tuple = doc_cutoff_day.year, doc_cutoff_day.month, \
            doc_cutoff_day.day, 0, 0, 0
        self.deleted_impressions = 0
        self.deleted_sources = 0
        self.deleted_old_docs = 0
        
    def run(self):
        try:
            start_time = time.time()            
            self.clear_aes_docs()
            self.clear_old_docs()
            self.run_time = time.time() - start_time
            self.complete = True            
            self.log = "imps deleted: {0} -- sources deleted: {1} -- "\
                            "old docs deleted: {2} -- run time -- {3}".\
                    format(self.deleted_impressions, self.deleted_sources,\
                           self.deleted_old_docs, self.run_time)        
        except ScriptError as e:            
            self.log = e
    
    def clear_aes_docs(self):
        """
                Impressions and insert sources for the previous month are removed
        """
        try:
            all_impressions = self.db.view('dionysus/impressions')                        
            for row in all_impressions:
                doc = self.db[row.id]
                if doc.get('product', None):
                    if "AES" in doc.get('product', None):
                        if self.should_delete(doc, self.aes_cutoff_tuple):
                            self.deleted_impressions += 1
                            self.db.delete(self.db[doc['_id']])
            all_sources = self.db.view('dionysus/insertsources')
            for row in all_sources:
                doc = self.db[row.id]
                if doc.get('product', None):
                    if "AES" in doc.get('product', None):
                        if self.should_delete(doc, self.aes_cutoff_tuple):
                            self.deleted_sources += 1
                            self.db.delete(self.db[doc['_id']])
        except Exception as e:
            raise ScriptError("Failed to clear AES docs", e)
                            
    def clear_old_docs(self):
        """
                Any doc with a created date older than 90 days will be removed
        """
        try:            
            result = self.db.view('_all_docs')
            for row in result:
                doc = self.db[row.id]
                if doc.get('created_date', None):
                    if self.should_delete(doc, self.doc_cutoff_tuple):
                        self.deleted_old_docs += 1
                        self.db.delete(self.db[doc['_id']])
        except Exception as e:
            raise ScriptError("Failed to clear old docs", e) 
        
    def should_delete(self, doc, cutoff):
        '''
        Determines when a doc should be deleted from the db.
        '''
        cutoff_date = date(*(cutoff[:3]))        
        d = doc.get('created_date', None)
        if not d:
            d = date.max.year, date.max.month, date.max.day
        # Sometimes the docs are uploaded with incorrect date format
        try:
            doc_date = date(*(d[:3]))
        except TypeError:
            self.db.delete(self.db[doc['_id']])
            return True
        return doc_date < cutoff_date