


class Script(object):
    '''
            All the db scripts should have these fields
    '''
    def __init__(self, settings):
        self.name = ''
        self.settings = settings
        self.log = ''
        self.complete = False
        self.run_time = None
        
    def run(self):
        raise NotImplementedError
        
class ScriptError(Exception):
    """
            In case something happens
    """
    def __init__(self, message, base_error = None):
        if base_error:
            self.message = "{0} -- {1} -- {2}".format(message,\
                base_error.__class__, base_error.message)
        else:
            self.message = message
            
    def __str__(self):
        return repr(self.message)