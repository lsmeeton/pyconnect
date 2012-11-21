#!/usr/bin/env python

'''
Written By Lewis Smeeton, 2012.
'''

#--------------------------------------------------------------------#

__metaclass__ = type

class Keywords():
    '''
    Reads keywords from input file dinfo and stores them as a nested 
    dictionary
    '''
    
    def __init__(self, dinfo):

        self.dinfo = dinfo

        self.interactive = self.interactive(),
        
        # Compulsory Keywords
        self.delta = self.delta() 
        self.first = self.first() 
        self.levels = self.levels()
        self.minima = self.minima()
        self.ts = self.ts()
        # Optional Keywords
        self.metric = [],
        self.centregmin = [],
        self.dumpnumbers = [],
        self.dumpsizes = [],
        self.excludeall = [],
        self.connectmin = [],
        self.colourprint = [],
        self.identify = [],
        self.identify_node = [],
        self.identify_node_size = [],
        self.idmin = [],
        self.labelformat = [],
        self.letter = [],
        self.lowest = [],
        self.monotonic = [],
        self.nconnmin = [],
        self.nobarriers = [],
        self.pick = [],
        self.nosplit = [],
        self.trmin = [],
        self.trval = [],
        self.trvalscale = [],
        self.tsthresh = [],
        self.weights = [],
        self.epsilon = [],
        self.halfpage = []

#--------------------------------------------------------------------#


    def ReadDinfo(self,attribute):
        '''
        Finds and returns line starting with 'attribute' in file 
        self.dinfo, otherwise returns None
        '''
        for line in open(self.dinfo,'r'):
            keyword = line.split()
            
            if len(keyword) == 0: continue
            
            if keyword[0].lower() == 'comment': continue
            
            if keyword[0].lower() == attribute.lower():
                
                break

        return keyword

#--------------------------------------------------------------------#

    def interactive(self):
        '''
        Interactive determines whether or not to enter interactive
        mode. If not present in dinfo file, interactive mode will
        not be started. If dinfo mode cannot be found, interactive mode
        will automatically be started.
        '''
        
        keyword = self.ReadDinfo('interactive')
        
        if keyword: #self.interactive = True
            return True

    def delta(self):
        '''
        DELTA <dE>
        Energetic separation of levels in basin analysis
        '''
        
        keyword = self.ReadDinfo('delta')
        if keyword:
            try:
                keyword[1] = float(keyword[1])
            except:
                print "ERROR"
            
            return dict(dE = keyword[1])


    def first(self):
        '''
        FIRST <E1>
        Specifies the energy of the highest level on the energy axis.
        '''

        keyword = self.ReadDinfo('first')
        if keyword:
            try:
                keyword[1] = float(keyword[1])
            except:
                print 'ERROR'
                
            return dict(E1 = keyword[1])

    def levels(self):
        '''
        LEVELS <n>
        The number of levels at which to perform the basin analysis.
        '''
        
        keyword = self.ReadDinfo('levels')
        if keyword:
            try:
                keyword[1] = float(keyword[1])
            except:
                print 'ERROR'
            
            return dict(n = keyword[1])

    def minima(self):
        '''
        MINIMA <data_file>
        Specifies filename for minima info. 
        '''

        keyword = self.ReadDinfo('minima')
        if keyword:
            try:
                keyword[1] = str(keyword[1])
            except:
                print 'ERROR'

            return dict(data_file = keyword[1])


    def ts(self):
        '''
        TS <data_file>
        Specifies filename for transition state info
        '''

        keyword = self.ReadDinfo('ts')
        if keyword:
            try:
                keyword[1] = str(keyword[1])
            except:
                print 'ERROR'

            return dict(data_file = keyword[1])

if __name__ == '__main__':
    kw = Keywords('dinfo')
    print 'and again...'
    print kw.interactive
    print kw.delta
    print kw.levels
    print kw.minima
    print hasattr(kw,'hello')
    print hasattr(kw, 'goodbye')
    getattr(kw, 'interactive')
