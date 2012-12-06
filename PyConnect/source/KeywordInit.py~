#!/usr/bin/env python

import os

__metaclass__ = type

class Keywords(dict):
    '''
    Reads keywords from input file dinfo and stores them as a nested 
    dictionary
    '''

    def __getitem__(self, key):
        return super(Keywords,self).__getitem__(key)

    __getattr__ = dict.__getitem__
    __setattr__ = dict.__setitem__

    def __init__(self, *args):
        super(Keywords, self).__init__(*args)

        # The draw option determines if the the disconnectivity 
        # graph is ready to be drawn
        self.dinfo = 'dinfo'
        self.draw = False

        # Check dinfo file is present
        self.dinfo = self.FileCheck(self.dinfo)

        # If dinfo file exists, read data from it
        if self.dinfo:
        
            # Compulsory Keywords
            self['delta'] = self.DeltaRead() 
            self['first'] = self.FirstRead() 
            self['levels'] = self.LevelsRead()
            self['minima'] = self.MinimaRead()
            self['ts'] = self.TsRead()
            # Optional Keywords
            self['interactive'] = self.interactive()
            self['metric'] = self.MetricRead()
            self['metric3d'] = self.Metric3DRead()
            self['centregmin'] = [],
            self['dumpnumbers'] = [],
            self['dumpsizes'] = [],
            self['excludeall'] = [],
            self['connectmin'] = self.ConnectminRead()
            self['colourprint'] = [],
            self['identify'] = [],
            self['identify_node'] = [],
            self['identify_node_size'] = [],
            self['idmin'] = [],
            self['labelformat'] = [],
            self['letter'] = [],
            self['lowest'] = [],
            self['monotonic'] = [],
            self['nconnmin'] = [],
            self['nobarriers'] = [],
            self['pick'] = [],
            self['nosplit'] = self.NosplitRead()
            self['trmin'] = [],
            self['trval'] = [],
            self['trvalscale'] = [],
            self['tsthresh'] = self.TsthreshRead()
            self['weights'] = [],
            self['epsilon'] = [],
            self['halfpage'] = []


            # Check minima file and TS file
            self.minima['data_file'] = self.FileCheck(
                self.minima['data_file'])
            
            self.ts['data_file'] = self.FileCheck(
                self.ts['data_file'])

            # Check if there is enough information to create a 
            # disconnectivity graph.
            self.DrawAllow()

#--------------------------------------------------------------------#

    def FileCheck(self,file_name):
        '''
        Checks the existence of the file 'file_name'
        '''
        if not os.path.exists(file_name):
            print ('ERROR: Could not find file %s'%file_name)
            return None
        else:
            return file_name


    def DrawAllow(self):
        '''
        Checks if there is enough information to attempt to 
        plot a disconnectivity graph
        '''
    
        if (self.delta and
            self.first and
            self.levels and
            self.minima and
            self.ts):
            
            self.draw = True


    def ReadDinfo(self,attribute):
        '''
        Finds and returns line starting with 'attribute' in file 
        self.dinfo, otherwise returns None
        '''
        for line in open(self.dinfo,'r'):
            keyword = line.split()
            
            if len(keyword) == 0: 
                keyword = None
                continue
            
            elif keyword[0].lower() == 'comment': 
                keyword = None
                continue
            
            elif keyword[0].lower() == attribute.lower():
                print attribute, keyword
                break
            else:
                keyword = None
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

    def DeltaRead(self):
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


    def FirstRead(self):
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

    def LevelsRead(self):
        '''
        LEVELS <n>
        The number of levels at which to perform the basin analysis.
        '''
        
        keyword = self.ReadDinfo('levels')
        if keyword:
            try:
                keyword[1] = int(keyword[1])
            except:
                print 'ERROR'
            
            return dict(n = keyword[1])

    def MinimaRead(self):
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


    def TsRead(self):
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


    def ConnectminRead(self):
        '''
        CONNECTMIN <min>
        If present then the analysis for a connected database is based
        upon minimum number min. If absent then the global minimum is 
        used to judge connectivity.     
        '''
        keyword = self.ReadDinfo('connectmin')
        if keyword:
            try:
                keyword[1] = int(keyword[1])
            except:
                print 'ERROR'

            return dict(connectmin = keyword[1])


    def MetricRead(self):
        '''

        '''
        keyword = self.ReadDinfo('metric')
        print 'keyword',keyword
        if keyword:
            try:
                keyword[2] = str(keyword[2])
            except:
                print 'ERROR'
            return dict(metric_file = keyword[2])

        else: return dict(metric_file = None)
    
    def Metric3DRead(self):
        '''

        '''
        keyword = self.ReadDinfo('metric3d')
        print 'keyword',keyword
        if keyword:
            try:
                keyword[1] = str(keyword[1])
                keyword[2] = str(keyword[2])
            except:
                print 'ERROR'
            return dict(metricx_file = keyword[1], 
                        metricy_file = keyword[2])

        else: return dict(metricx_file = None,
                          metricy_file = None)
    
        

    def TsthreshRead(self):
        '''
        
        '''
        keyword = self.ReadDinfo('maxtsenergy')
        if keyword:
            try:
                keyword[1] = float(keyword[1])
            except:
                print 'ERROR'

            return dict(tsthresh = keyword[1])

    def NosplitRead(self):
        '''
        
        '''
        keyword = self.ReadDinfo('nosplit')
        if keyword:
            print keyword
            return dict(split = False)
        else:
            return dict(split = True)

if __name__ == '__main__':
    #pr = prac()
    kw = Keywords()
    #print kw.delta
    print kw
    print kw['delta']
    if not kw['metric'][0]: print 'letter'
    else: print 'LLeter', kw.metric


    print callable(kw.DeltaRead)
    #print pr['delta']
    #print hasattr(pr, 'delta')
    #print pr
    #print pr.hi
    #print pr.hello()
