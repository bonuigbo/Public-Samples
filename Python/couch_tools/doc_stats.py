'''
    Random tasks for couch
    
'''
import couchdb
from script import Script, ScriptError
import time
import datetime
from datetime import date, timedelta
from settings import SETTINGS
import os


class DocStats(Script):
    '''
        A multipurpose script. Allows
    '''
    def __init__(self, SETTINGS):
        self.name = 'doc_stats'
        makalu = SETTINGS['couchdbs']['makalu']
        self.makalu_server = couchdb.client.Server(url=makalu['url'])
        self.makalu_db = self.makalu_server[makalu['db']]
        self.complete = False
        self.log = ''
        self.files_moved = 0
        
    def run(self):
        try:
            start_time = time.time()
            #self.get_stats()
            #self.move_docs_from('vistofte', 'makalu', slug = 'association-management-services')
            #self.move_docs_from('vistofte', 'makalu', slug = 'access-management-group')
            #self.copy_doc_from('eiger', 'vistofte', doc_id = 'caaf973c-0be6-4aba-abe1-71f3c3d0497f')
            #self.copy_doc_from('eiger', 'vistofte', doc_id = '581f335b-9be8-4c91-8d2d-68208220a0b3')
            #self.copy_doc_from('eiger', 'vistofte', doc_id = '3ca78fa9-f2df-41cd-ac30-622b2f7609b0')
            #self.download_docs('vistofte', slug = 'access-management-group')
            #self.download_docs('vistofte', slug = 'omega-association-management')
            #self.download_docs('vistofte', slug = 'association-management-services')
            self.get_docs_in_progress('prime-as')
            self.run_time = time.time() - start_time
            self.log = "Elapsed time: {0} secs: files moved {1}".format(self.run_time, self.files_moved)
            self.complete = True
            print self.log
        except ScriptError as e:
            print e
        
    
    def get_docs_in_progress(self, slug):
        '''
            Grabs the id's of the docs that are in progress for
            the customer based on the slug, and prints
            them out
        '''
        strings = []
        source = SETTINGS['couchdbs']['makalu']
        source_server = couchdb.client.Server(url=source['url'])
        source_db = source_server[source['db']]
        all_docs = source_db.view('_all_docs')
        for row in all_docs:
                doc = source_db[row.id]     
                if doc.get('product', None) and 'AES' in doc['product'] and \
                        doc.get('slug', None) and slug in doc['slug'] and \
                        ('InsertSource' in doc['doc_type'] or 'Impression' in doc['doc_type']):
                    print_string = '_id: %s - doc_type: %s : assoc_key: %s ' % (doc['_id'], doc['doc_type'], doc['assoc_key'])
                    strings.append(print_string)
                    if 'InsertSource' in doc['doc_type'] and 'in-progress' in doc['formats']['bw']:
                        strings.append('doc potentially stuck\n')
        output_dir = r'D:\file_factory\doc_stats\docs_in_progress.txt'
        with open(output_dir, 'wb') as writefile:
            for line in strings:
                writefile.write(line + '\r\n')
                
    def get_stats(self):
        '''
                Gets a count of all the document types in 
                the database
        '''
        
        product_list = {}
        all_docs = self.makalu_db.view('_all_docs')               
        for row in all_docs:
            try:
                doc = self.makalu_db[row.id]
            except couchdb.http.ResourceNotFound:
                continue     
            if doc.get('doc_type', None):
                product = doc['doc_type']
                if not product_list.has_key(product):
                    product_list[product] = 0
                product_list[product] += 1
        for product, count in product_list.items():
            print product, count
    
    def download_docs(self, couch, slug = None):
        source = SETTINGS['couchdbs'][couch]
        source_server = couchdb.client.Server(url=source['url'])
        source_db = source_server[source['db']]
        output_dir = r'D:\file_factory\doc_stats'
        output_dir = os.path.join(output_dir, slug)
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
        try:            
            all_insert_data = source_db.view('dionysus/impressions')           
            for row in all_insert_data:
                source_doc = row.value            
                if source_doc.get('product', None):
                    if 'AES' in source_doc['product'] and 'bw' in source_doc['format']:
                        if slug:
                            if slug in source_doc['slug']:
                                if self.should_return(source_doc):                                    
                                    if '_attachments' in source_doc:
                                        file_name = source_doc['assoc_key'] + '_' + str(int(source_doc['sort_order'])/64) + '.tif'
                                        doc_name = source_doc['_attachments'].keys()[0]
                                        attachment = source_db.get_attachment(source_doc['_id'], doc_name)
                                        with open(os.path.join(output_dir, file_name), 'wb') as doc_file:
                                            doc_file.write( attachment.read()) 
                        else:
                                    if '_attachments' in source_doc:
                                        doc_name = source_doc['_attachments'].keys()[0]
                                        attachment = source_db.get_attachment(source_doc['_id'], doc_name)
                                        with open(os.path.join(output_dir, doc_name), 'wb') as doc_file:
                                            doc_file.write( attachment.read())                         
        except Exception as e:
            raise ScriptError("Document moving failed", e)   
        
                
    def move_docs_from(self, source_couch, dest_couch, slug = None):
        '''
                Copies particular docs from one server to another
        '''
        source = SETTINGS['couchdbs'][source_couch]
        dest = SETTINGS['couchdbs'][dest_couch]
        source_server = couchdb.client.Server(url=source['url'])
        source_db = source_server[source['db']]
        dest_server = couchdb.client.Server(url=dest['url'])
        dest_db = dest_server[dest['db']]
        try:            
            all_insert_data = source_db.view('dionysus/impressions')           
            for row in all_insert_data:
                source_doc = row.value            
                if source_doc.get('product', None):
                    if 'AES' in source_doc['product']:
                        if slug:
                            if slug in source_doc['slug']:
                                if self.should_return(source_doc):
                                    dest_doc = self.dup_to_dest(source_doc, dest_db)
                                    self.load_attach(source_doc, dest_doc, source_db, dest_db)
                                    self.files_moved += 1
                        else:
                            if self.should_return(source_doc):
                                dest_doc = self.dup_to_dest(source_doc, dest_db)
                                self.load_attach(source_doc, dest_doc, source_db, dest_db)
                                self.files_moved += 1                           
        except Exception as e:
            raise ScriptError("Document moving failed", e)        
 
    def copy_doc_from(self, source_couch, dest_couch, doc_id = None):
        '''
                Copies particular docs from one server to another
        '''
        source = SETTINGS['couchdbs'][source_couch]
        dest = SETTINGS['couchdbs'][dest_couch]
        source_server = couchdb.client.Server(url=source['url'])
        source_db = source_server[source['db']]
        dest_server = couchdb.client.Server(url=dest['url'])
        dest_db = dest_server[dest['db']]
        try:
            source_doc = source_db[doc_id]
            dest_doc = self.dup_to_dest(source_doc, dest_db)
            if '_attachments' in source_doc.keys():                
                self.load_attach(source_doc, dest_doc, source_db, dest_db)
                      
        except Exception as e:
            raise ScriptError("Document moving failed", e)
        
    def to_dict(self, couch_doc):
        '''
            Converts the couchdb doc to a raw dict
        '''
        dict_doc = {}
        for key, value in couch_doc.items():
            dict_doc[key] = value
        return dict_doc
                            
    def dup_to_dest(self, source_doc, dest_db):
        '''
            Formats and saves the document to VIS
        '''
        conv_doc = self.to_dict(source_doc)
        del conv_doc['_rev']
        if '_attachments' in conv_doc:
            del conv_doc['_attachments']         
        old_doc = dest_db.get(conv_doc['_id'])
        if not old_doc:
            dest_db.save(conv_doc)
        return dest_db.get(conv_doc['_id'])
            
    def load_attach(self, source_doc, dest_doc, source_db, dest_db):
        '''
                Downloads the attachment locally, and then
                reuploads it to VIS
        '''
        if '_attachments' in source_doc:
            doc_name = source_doc['_attachments'].keys()[0]
            doc_type = source_doc['_attachments'][doc_name]['content_type']
            attachment = source_db.get_attachment(source_doc['_id'], doc_name)
            if '_attachments' not in dest_doc:
                dest_db.put_attachment(dest_doc, attachment.read(), filename=doc_name, content_type = doc_type)     
                  
    def should_return(self, doc):
        '''
        Determines when a doc should be deleted from the db.
        '''
        today = date.today()
        month = today.month -1
        year = today.year
        if month < 1:
            month = 12
            year = year - 1
        cutoff = year, month, 1, 0, 0, 0
        
        cutoff_date = date(*(cutoff[:3]))        
        d = doc.get('created_date', None)
        if not d:
            return False
            #d = date.max.year, date.max.month, date.max.day
        # Sometimes the docs are uploaded with incorrect date format
        try:
            doc_date = date(*(d[:3]))
        except TypeError:
            self.db.delete(self.db[doc['_id']])
            return True
        return doc_date > cutoff_date
    
DocStats(SETTINGS).run()