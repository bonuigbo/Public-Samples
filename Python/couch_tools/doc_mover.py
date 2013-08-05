import couchdb
import time
from script import Script, ScriptError

class DocMover(Script):
    """
            Copies AES impressions from MAKALU to VISTOFTE
    """
    def __init__(self, SETTINGS):
        super(DocMover, self).__init__(SETTINGS)
        self.name = 'doc_mover'
        source_server = SETTINGS['couchdbs']['makalu']
        dest_server = SETTINGS['couchdbs']['vistofte']
        self.source_server = couchdb.client.Server(url=source_server['url'])
        self.dest_server = couchdb.client.Server(url=dest_server['url'])
        self.source_db = self.source_server[source_server['db']]
        self.dest_db = self.dest_server[dest_server['db']]
        self.files_moved = 0
    
    def run(self):
        try:            
            start_time = time.time()
            self.copy_from_mak()
            self.run_time = time.time() - start_time
            self.log = "impressions copied: {0} --- elapsed time: {1} secs".\
                    format(self.files_moved, self.run_time)
            self.complete = True
            print self.log
        except ScriptError as e:
            self.log = e
            print e
        
    @classmethod
    def to_dict(self, couch_doc):
        '''
            Converts the couchdb doc to a raw dict
        '''
        dict_doc = {}
        for key, value in couch_doc.items():
            dict_doc[key] = value
        return dict_doc

    def copy_from_mak(self):
        '''
                Goes through all the impressions,
                downloads AES and CRT impressions to
                a local
        '''
        try:            
            all_docs = self.source_db.view('dionysus/docs_by_slug')           
            for row in all_docs:
                mak_doc = row.value            
                if mak_doc.get('product', None):
                    if ( ('AES' in mak_doc['product'] and mak_doc.get('doc_type', None) \
                            and mak_doc['doc_type'] == 'Impression')  \
                            or 'CRT' in mak_doc['product']) \
                            and mak_doc.get('_attachments', None): 
                        vis_doc = self.dup_to_vis(mak_doc)
                        self.load_attach(mak_doc, vis_doc)
                        self.files_moved += 1
        except Exception as e:
            raise ScriptError("Document moving failed", e)
                            
    def dup_to_vis(self, mak_doc):
        '''
            Formats and saves the document to VIS
        '''
        try:
            conv_doc = DocMover.to_dict(mak_doc)
            del conv_doc['_rev']
            del conv_doc['_attachments']         
            old_doc = self.dest_db.get(conv_doc['_id'])
            if not old_doc:
                self.dest_db.save(conv_doc)
            return self.dest_db.get(conv_doc['_id'])
        except Exception as e:
            raise ScriptError("Error with document: %s\nDocument failed to duplicate data\
                                        \nInternal Error: %s" % ( mak_doc["_id"], e))            
            
    def load_attach(self, mak_doc, vis_doc):
        '''
                Downloads the attachment locally, and then
                reuploads it to VIS
        '''
        try:
            doc_name = mak_doc['_attachments'].keys()[0]
            # Doc names with underscores throw a random error, skip them.
            # Usually a certified issue with a script I wrote, so they can be skipped
            if doc_name.startswith("_"):
                return
            doc_type = mak_doc['_attachments'][doc_name]['content_type']
            attachment = self.source_db.get_attachment(mak_doc['_id'], doc_name)
            if '_attachments' not in vis_doc:
                self.dest_db.put_attachment(vis_doc, attachment.read(), filename=doc_name, content_type = doc_type)
        except Exception as e:
            raise ScriptError("Error with document: %s\nDocument failed to load attachment\
                                        \nInternal Error: %s" % ( mak_doc["_id"], e))
            
if __name__ == '__main__':
    from settings import SETTINGS
    DocMover(SETTINGS).run()