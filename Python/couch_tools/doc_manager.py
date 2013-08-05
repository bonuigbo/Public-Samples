'''
    Used for compacting databases and other
    maintainance tasks, currently only
    compacts makalu
    
'''
import couchdb
from script import Script, ScriptError
import time

class DocManager(Script):
    def __init__(self, SETTINGS):
        super(DocManager, self).__init__(SETTINGS)
        self.name = 'doc_manager'
        
    def run(self):
        try:
            start_time = time.time()
            self.compact_db()
            self.run_time = time.time() - start_time
            self.log = "Compaction successful: Elapsed time: %s secs" % self.run_time
            self.complete = True
        except ScriptError as e:
            self.log = e
        
    def compact_db(self, db_name = 'makalu'):
        '''
            Makalu by default
        '''
        try:            
            couch_db = self.settings['couchdbs'][db_name]     
            server = couchdb.client.Server(url=couch_db['url'])
            db = server[couch_db['db']]
            db.compact()
            while db.info()['compact_running']:
                pass          
            for row in db.view('_all_docs', startkey='_design', endkey='_design\ufff0'):
                viewname = row['id'].split('/')[1]            
                db.compact(ddoc=viewname)
        except Exception as e:
            raise ScriptError("%s failed to compact" % db_name, e)
   
if __name__ == '__main__':
    from settings import SETTINGS
    DocManager(SETTINGS).run()