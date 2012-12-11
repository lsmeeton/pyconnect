#!/usr/bin/env python

import os
import sys

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
        print 'Reading keyword data from file "%s"'%self.dinfo
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
            self['dumpnumbers'] = self.DumpNumbersRead()
            self['dumpsizes'] = [],
            self['excludeall'] = [],
            self['connectmin'] = self.ConnectminRead()
            self['colourprint'] = [],
            self['identify'] = [],
            self['identify_node'] = [],
            self['identify_node_size'] = [],
            self['idmin'] = self.IdminRead()
            self['labelformat'] = [],
            self['letter'] = [],
            self['lowest'] = [],
            self['monotonic'] = [],
            self['nconnmin'] = [],
            self['nobarriers'] = [],
            self['pick'] = [],
            self['nosplit'] = self.NosplitRead()
            self['trmin'] = self.TrminRead()
            self['trval'] = self.TrvalRead()
            self['trvalscale'] = [],
            self['tsthresh'] = self.TsthreshRead()
            self['weights'] = [],
            self['epsilon'] = [],
            self['halfpage'] = [],
            self['energy_label'] = self.EnergyLabel()
            self['q1'] = self.Q1()
            self['q2'] = self.Q2()
            self['colour_bar_label'] = self.ColourBarLabelRead()
            self['tex'] = self.TexRead()

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
            
            elif (keyword[0].lower() == 'comment'
                  or keyword[0].lower() == '#'): 
                keyword = None
                continue
            
            elif keyword[0].lower() == attribute.lower():
                label_count = line.count('"')
                if label_count == 0: break
                elif label_count == 2: keyword = line.split('"')
                else:
                    sys.exit("Incorrect No. of parantheses for argument %s"
                             %keyword[0].lower())
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

    def IdminRead(self):
        '''
        IDMIN <min>                                                                                                                                   
        Label this minimum on the graph. Repeat to label more than one minimum.  
        '''
        keyword = self.ReadDinfo('idmin')
        idmin_dict = {}
        if keyword:
            for i in keyword[1:]:
                try:
                    idmin_dict[int(i)] = int(i)
                except IndexError:
                   sys.exit('dsfasdfadsfa!!!!')
        
        return dict(Min = idmin_dict)
#        else:
#            return


    def MetricRead(self):
        '''

        '''
        keyword = self.ReadDinfo('metric')
#        print 'keyword',keyword
        if keyword:
            try:
                keyword[2] = str(keyword[2])
            except:
                print 'ERROR'
            return dict(present = True,
                        metric_file = keyword[2])

        else: return dict(present = False)
    
    def Metric3DRead(self):
        '''

        '''
        keyword = self.ReadDinfo('metric3d')
#        print 'keyword',keyword
        if keyword:
            try:
                keyword[1] = str(keyword[1])
                keyword[2] = str(keyword[2])
            except:
                print 'ERROR'
            return dict(present = True,
                        metricx_file = keyword[1], 
                        metricy_file = keyword[2])

        else: return dict(present = None)
    
    def DumpNumbersRead(self):
        '''
        
        '''
        keyword = self.ReadDinfo('dumpnumbers')
        if keyword:
            return dict(dump = True)
        else:
            return dict(dump = False)

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
            return dict(split = False)
        else:
            return dict(split = True)

    def TrminRead(self):
        '''
        TRMIN <n> <max> <file> <file> ...
        Label n different sections of the graph in colour as \
        specified by the minima in each file, one file for each\
        section.  
        Each file is a list of numbers of minima, 
        one per line as for PICK. max is the total number of minima,\
        not the number in the colour files. 
        currently used for array allocation.
        '''
        keyword = self.ReadDinfo('trmin')
        if keyword:
            file_range = len(keyword)
#            print 'file range', file_range
            temp_list = []
            for i in range(3, file_range):
#                print 'TrminRead',i
                #try:
                temp_list.append(str(keyword[i]))
#                print temp_list, keyword
                #else:
                #    keyword[i] = None
                
            return {'trmin_file': temp_list}
        #else:
        return dict(trmin_file = None)
    
    def TrvalRead(self):
        '''
        TRVAL <max> <filename>                                                                                                                        
        Colour the graph according to an order parameter value for each                                                                               
        minimum. The order parameters are read in from the named file, which 
        should contain one line per minimum.  
        The expected range of the order parameters is [0,1] inclusive.                                                         
        max is the total number of minima.                                                                                                            
        Colours are chosen automatically to be evenly distributed with order 
        parameter value along the edges of the RBG colour cube: 
        red -> yellow -> green -> cyan -> blue   
        '''
        keyword = self.ReadDinfo('trval')
        print 'keyword',keyword
        if keyword:
            try:
                keyword[2] = str(keyword[2])
            except:
                print 'ERROR'
#            return dict(present = True,
#                        trval_file = keyword[2])
#
#        else: return dict(present = False)
            return dict(trval_file = keyword[2])

        else: 
            return dict(trval_file = None)
        
    def EnergyLabel(self):
        '''
        
        '''
        keyword = self.ReadDinfo('energy_label')
        if keyword:
            e_l = keyword[1]#''
#            for s in keyword[1:]:
#                e_l += str(s) + ' '
            #print e_l
            return {'label': e_l}
        else: return {'label': None}
        
    def Q1(self):
        '''
        
        '''
        keyword = self.ReadDinfo('q1')
        if keyword:
            q1 = keyword[1]
#            for s in keyword[1:]:
#                q1 += str(s)
            return {'label': q1}
        else: 
            return {'label': None}
        
    def Q2(self):
        '''
        
        '''
        keyword = self.ReadDinfo('q2')
        if keyword:
            q2 = keyword[1]
#            for s in keyword[1:]:
#                q2 += str(s)
            return {'label': q2}
        else:
             return {'label': None}
         
    def ColourBarLabelRead(self):
        '''
        
        '''
        keyword = self.ReadDinfo('colour_bar_label')
        if keyword:
            label = keyword[1]
#            for s in keyword[1:]:
#                q2 += str(s)
            return {'label': label}
        else:
             return {'label': None}
         
         
    def TexRead(self):
        '''
        Turns on latex rendering in matplotlib plots
        '''
        keyword = self.ReadDinfo('tex')
        if keyword:
            return True
        else:
            return False
        
if __name__ == '__main__':
    #pr = prac()
    kw = Keywords()
    #print kw.delta
#    print kw
#    print kw['delta']
#    if not kw['metric'][0]: print 'letter'
#    else: print 'LLeter', kw.metric


#    print callable(kw.DeltaRead)
    #print pr['delta']
    #print hasattr(pr, 'delta')
    #print pr
    #print pr.hi
    #print pr.hello()
