'''
Written By LEWIS SMEETON, 2012
'''

import numpy as np
import string
import os
import sys
#from KeywordDict import Keywords
from KeywordInit import Keywords
#--------------------------------------------------------------------#
__metaclass__ = type

class Disconnect():
    '''
    Disconnect is the superclass of DisconnectPlot. It contains 
    methods to find and read input file 'dinfo', and to read data
    from minima and TS database files.
    '''
    
    def __init__(self, keyword):
        
        # Bind Keywords
        self.kw = keyword

        # Initialising minima_index and ts_index
        # Not necesary for python, but helpful to know what to expect
        # to arrive in a data structure

        #self.InitialiseMin()
        #self.InitialiseTS()
        
        #self.CountMin()
        #self.CountTS()
        #self.RemoveThreshold()
        #self.RemoveUnderConnect()
        #self.RemoveDisjoint()


        #self.InitialiseBasin()
        
#--------------------------------------------------------------------#
         
    def InitialiseMin(self):
        '''

        '''
        self.minima_index = {} 
        self.ReadMinima(self.kw.minima['data_file'])
        
        temp_dict = {}
        
        for i in range(1,self.kw.levels['n'] + 1):
            temp_dict[i] = None

        for indice in self.minima_index['Index']:
            self.minima_index['Index'][indice]['Degree'] = 0
            self.minima_index['Index'][indice]['Colour'] = \
                {'RGB':(0.0,0.0,0.0)}
#{'r': 0.0,
# 'g': 0.0,
                # 'b': 0.0}
            self.minima_index['Index'][indice]['Metric'] = \
                {'x': None,
                 'y': None,
                 'trval': None}
            self.minima_index['Index'][indice]['Identify'] = False
            self.minima_index['Index'][indice]['TS'] = []
            self.minima_index['Index'][indice]['Basin'] = {}
            self.minima_index['Index'][indice]['Basin']['Level']={}
            for i in range(1,self.kw.levels['n'] + 1):
                
                self.minima_index['Index'][indice]['Basin']\
                    ['Level'][i] = None
                
        self.FindGM()
        
#        if self.kw.idmin['Min']:
#            self.IdentifyMin()
        
        if not self.kw.connectmin:
            self.kw.connectmin = self.minima_index['GM']
        
    def InitialiseTS(self):
        '''

        '''
        self.ts_index = {}
        
        self.ReadTS(self.kw.ts['data_file'])
        
        self.ts_index['HighTS'] = []
        self.ts_index['LowTS'] = []
        

    def InitialiseBasin(self):
        '''

        '''

        self.basin_index = {'Level': {},
                            'MaxX':
                                -1.0,
                            'MaxY':
                                -1.0,
                            'MinX':
                                2.0,
                            'MinY':
                                2.0,
                            'MaxTrval':
                                -1.0,
                            'MinTrval':
                                1.0}
        diff = 1.0/(self.kw.levels['n'] - 1)
        for l in range(1, self.kw.levels['n'] + 1):
            
            energy = self.kw.first['E1']+(1 - l)*self.kw.delta['dE']
            
            self.basin_index['Level'][l] = {'Energy':energy,
                                            'No. of Basins':0,
                                            'Basin':{},
                                            'Z':(l - 1)*diff,
                                            'GLZ':
                                                (1-l)*diff + 0.5}
            
        
    
        #self.AssignBasins()

        #self.PruneBasins()
        #
        #self.ReNumberBasins()
        #
        #self.GetParentsAndChildren()
        #
        #self.GetNodeSize()
        

    def ReadMinima(self, file_name):
        '''
        Reads minima index and energy from 'file_name'
        '''
        i = 1
        self.minima_index['Index'] = {}
        for line in open(file_name, 'r'):
            minima_data = line.split()
            
            self.minima_index['Index'][i] =  dict(Energy = float(minima_data[0]))
            
            i += 1

    def ReadTS(self, file_name):
        '''
        Reads TS index, energy and the two minima it links from 
        'file_name'
        '''
        i = 1
        self.ts_index['Index'] = {}
        for line in open(file_name, 'r'):
            TS_data = line.split()
            
            if int(TS_data[3]) == int(TS_data[4]): continue

            self.ts_index['Index'][i] = dict(Energy = float(TS_data[0]), 
                                    Min1 = int(TS_data[3]), 
                                    Min2 = int(TS_data[4]))
            i += 1

#--------------------------------------------------------------------#

    def AssignBasins(self):
        '''
        
        '''
        for level in self.basin_index['Level']:
            energy_threshold = self.EnergyThreshold(level)

            while True:

                change = False
                
                change = self.AssignByTS(level,change,
                                         energy_threshold)
                if not change:
                    break
            
            for indice in self.minima_index['Index']:

                self.AssignByMin(level, indice)
                        
                self.AssignMinToBasin(level, indice)
        
#--------------------------------------------------------------------#

    def EnergyThreshold(self,level):
        '''
        
        '''
        if self.kw.nosplit['split']:
            energy_threshold = (self.basin_index['Level']
                                    [level]['Energy'])
        else: 
            energy_threshold = (self.basin_index['Level']
                                [level]['Energy'] + self.kw.delta['dE'])
        return energy_threshold
#--------------------------------------------------------------------#

    def AssignByTS(self,level, change, energy_threshold):
        '''

        '''
        for indice in self.ts_index['Index']:
            if (self.ts_index['Index'][indice]['Energy'] <
                energy_threshold):
                Basin = None

                Min1 = self.ts_index['Index'][indice]['Min1']
                Min2 = self.ts_index['Index'][indice]['Min2']
                
                Basin1 = self.minima_index['Index'][Min1]\
                    ['Basin']['Level'][level]
                Basin2 = self.minima_index['Index'][Min2]\
                    ['Basin']['Level'][level]
                
                
                if (not Basin1 and not Basin2):
                    change = True

                    self.basin_index['Level'][level]['No. of Basins'] += 1
                    Basin = self.basin_index['Level'][level]['No. of Basins']
                    
                if (Basin1 != Basin2):
                    change = True

                    if (not Basin1):
                        Basin = Basin2
                    elif (not Basin2):
                        Basin = Basin1
                    else:
                        Basin = min(Basin1, Basin2)

                if Basin:
                    self.minima_index['Index'][Min1]['Basin']['Level'][level] = Basin
                    self.minima_index['Index'][Min2]['Basin']['Level'][level] = Basin

        return change
        
    def AssignByMin(self, l, i):
        '''
        
        '''
        if (not self.minima_index['Index'][i]['Basin']
            ['Level'][l] and 
            self.minima_index['Index'][i]
            ['Energy'] < (self.basin_index['Level']
                          [l]['Energy'] +
                          self.kw.delta['dE'])):
            self.basin_index['Level'][l]['No. of Basins'] += 1
            Basin = self.basin_index['Level'][l]['No. of Basins']
            self.minima_index['Index'][i]['Basin']['Level'][l]= Basin
                

    def AssignMinToBasin(self, l, i):
        '''

        '''
        b = self.minima_index['Index'][i]['Basin']['Level'][l]
        if b:
            try:
                self.basin_index['Level'][l]['Basin'][b]\
                    ['Min'].append(i)
            except KeyError:
                self.basin_index['Level'][l]['Basin'][b] = \
                    {'Min':
                         [i],
                     'Size':
                         0,
                     'Children':
                         [],
                     'Parents':
                         None,
                     'Order':
                         None,
                     'RGB':
                         (0.0,0.0,0.0),
                     'MetXTickNo':
                         None,
                     'MetYTickNo':
                         None,
                     'X':
                         None,
                     'Y':
                         None,
                     'Z':
                         None,
                     'MetricX':
                         None,
                     'MetricY':
                         None,
                     'Trval':
                         None,
                     'FirstClmn':
                         None,
                     'LastClmn':
                         None}
                    

    def PruneBasins(self):
        '''
        Prunes basins
        '''
        
        return
    

    def ReNumberBasins(self):
        '''
        Renumbers basins after pruning
        '''
        for l in self.basin_index['Level']:
            num_basin = 0
            basin_dict = {}
            for m in self.minima_index['Index']:
                b = self.minima_index['Index'][m]['Basin']['Level'][l]
                
                if b:
                    try:
                        self.minima_index['Index'][m]['Basin']\
                            ['Level'][l] = basin_dict[b]
                        
                    except KeyError:
                        
                        num_basin += 1
                        basin_dict[b] = num_basin
                                                
                        self.minima_index['Index'][m]['Basin']\
                            ['Level'][l] = basin_dict[b]
            
            temp = {}
            for b in basin_dict:
            
                try:
             
                    temp[b] = self.basin_index['Level'][l]['Basin'][b].copy()
                    del self.basin_index['Level'][l]['Basin'][b]
                except KeyError, e:
             
                    sys.exit('Level: %s Basin: %s'%(l, e))
                
            
            for b in basin_dict:
                n = basin_dict[b]
                self.basin_index['Level'][l]['Basin'][n] = temp[b]
            
            self.basin_index['Level'][l]['No. of Basins'] = num_basin

    def GetParentsAndChildren(self):
        '''
        Find the parent of each node.
        '''
        for m in self.minima_index['Index']:
            for l in self.basin_index['Level']:
                if l == 1: continue
                b = self.minima_index['Index'][m]['Basin']['Level'][l]
                if b:
                    #if l - 1 != 0:
                    
                    p = self.minima_index['Index'][m]['Basin']\
                        ['Level'][l-1]
                    self.basin_index['Level'][l]['Basin'][b]\
                        ['Parents'] = p

                    if l + 1 <= self.kw.levels['n']:
                        c = self.minima_index['Index'][m]['Basin']\
                            ['Level'][l+1]
                        if c:
                            self.basin_index['Level'][l]['Basin'][b]\
                                ['Children'].append(c)
                        else:
                            self.basin_index['Level'][l]['Basin'][b]\
                                ['Children'] = None
                    else:
                        self.basin_index['Level'][l]['Basin'][b]\
                            ['Children'] = None

        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                if (self.basin_index['Level'][l]
                    ['Basin'][b]['Children']):
                    self.basin_index['Level'][l]['Basin'][b]\
                        ['Children'] = \
                        list(set(self.basin_index['Level'][l]\
                                     ['Basin'][b]['Children']))


    def GgetParentsAndChildren(self):
        '''
        Find the parent of each node.
        '''
        for m in self.minima_index['Index']:
            for l in self.basin_index['Level']:
                if l == 1: continue
                b = self.minima_index['Index'][m]['Basin']['Level'][l]
                if b:
                    #if l - 1 != 0:
                    
                    p = self.minima_index['Index'][m]['Basin']\
                        ['Level'][l-1]
                    self.basin_index['Level'][l]['Basin'][b]\
                        ['Parents'] = p

                    #if l + 1 <= self.kw.levels['n']:
                    #    c = self.minima_index['Index'][m]['Basin']\
                    #        ['Level'][l+1]
                    #    if c:
                    #        self.basin_index['Level'][l]['Basin'][b]\
                    #            ['Children'].append(c)
                    #    else:
                    #        self.basin_index['Level'][l]['Basin'][b]\
                    #            ['Children'] = None
                    #else:
                    #    self.basin_index['Level'][l]['Basin'][b]\
                    #        ['Children'] = None

        for l in self.basin_index['Level']:
            if l == self.kw.levels['n']: continue
            for b in self.basin_index['Level'][l+1]['Basin']:
                p = self.basin_index['Level'][l+1]['Basin'][b]\
                    ['Parents']
                self.basin_index['Level'][l]['Basin'][p]['Children']\
                    .append(b)
            for b in self.basin_index['Level'][l]['Basin']:
                if (self.basin_index['Level'][l]
                    ['Basin'][b]['Children']):
                    self.basin_index['Level'][l]['Basin'][b]\
                        ['Children'] = \
                        list(set(self.basin_index['Level'][l]\
                                     ['Basin'][b]['Children']))
        # Set all children in bottom level to None
        #l = self.kw.levels['n']
        #for b in self.basin_index['Level'][l]['Basin']:
        #    self.basin_index['Level'][l]['Basin'][b]\
        #        ['Children'] = None


    def GetNodeSize(self):
        '''
        Gets size of each node
        '''
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                node_size = len(self.basin_index['Level'][l]['Basin'][b]\
                            ['Min'])
                self.basin_index['Level'][l]['Basin'][b]['Size'] = node_size
                    
                if node_size == 1: self.AssignEnergytoChildlessBasin(l,b)
                    
    def AssignEnergytoChildlessBasin(self,l,b):
        '''
        Assigns energy to Basins which have no children, for plotting purposes
        '''
        i = self.basin_index['Level'][l]['Basin'][b]['Min'][0]
        e = self.minima_index['Index'][i]['Energy']
        self.basin_index['Level'][l]['Basin'][b]['Energy'] = e
#-------------------------------------------------------------------#

    def CountMin(self):
        '''
        Counts No. of minima in minima['data_file']
        '''
        self.minima_index['Size'] = len(self.minima_index['Index'])

    def FindGM(self):
        '''
        Finds the global minimum of the database
        '''
        GM = min(self.minima_index['Index'],
                 key = self.minima_index['Index'].get)#['Energy'])
        self.minima_index['GM'] = self.minima_index['Index'][GM]
        self.minima_index['GM']['Index'] = GM
        
    def CountTS(self):
        '''
        Counts No. of TS in ts['data_file']
        '''
        self.ts_index['Size'] = len(self.ts_index['Index'])

    def HighTS(self):
        '''
        Finds highest energy TS
        '''
        TS = max(self.ts_index['Index'],
                 key = self.ts_index['Index'].get)
        
        self.ts_index['HighTS'] = self.ts_index['Index'][TS]
        self.ts_index['HighTS']['Index'] = TS


    def LowTS(self):
        '''
        Finds the lowest energy TS
        '''
        TS = min(self.ts_index['Index'],
                 key = self.ts_index['Index'].get)
        
        self.ts_index['LowTS'] = self.ts_index['Index'][TS]
        self.ts_index['LowTS']['Index'] = TS

            
            
#    def IdentifyMin(self):
#        '''
#        Assigns min which need to be identified from IDMIN keyword
#        '''
#        self.minima_index['Identify'] = self.kw.idmin['Min']
        
#--------------------------------------------------------------------#

    def RemoveThreshold(self):
        '''
        Remove minima and TS above kw.first from min_index and ts_index
        '''
        self.RemoveThresholdMin()
        self.RemoveThresholdTS()

    def RemoveThresholdMin(self):
        '''
        Remove minima above kw.first from min_index and ts_index
        '''
        index = []
        for indice in self.minima_index['Index']:
            if (self.minima_index['Index'][indice]['Energy'] >
                self.kw.first['E1']):
                
                index.append(indice)

        for i in index: 
            del self.minima_index['Index'][i]
        

    def RemoveThresholdTS(self):
        '''
        Remove TS above kw.first from min_index and ts_index
        '''
        index = []
        for indice in self.ts_index['Index']:
            if (self.ts_index['Index'][indice]['Energy'] >
                self.kw.tsthresh['tsthresh']):
                
                index.append(indice)
        
        for i in index:
            del self.ts_index['Index'][i]


    def RemoveUnderConnect(self):
        '''
        Calculates the degree of each minima, and removes those which 
        have degree less than kw.nconnmin 
        '''
        self.kw.nconnmin = 0
            
        conv = False
        
        while not conv:

            index = []
         
            self.CalcDegree()
            
            for indice in self.minima_index['Index']:
                if (self.minima_index['Index'][indice]['Degree'] <= 
                    self.kw.nconnmin):
                    index.append(indice)
                    for i in self.minima_index['Index'][indice]['TS']:
                        del self.ts_index['Index'][i]
            for i in index:
                del self.minima_index['Index'][i]

            if len(index) == 0: conv = True



    def CalcDegree(self):
        '''
        Calculates the degree of each minima, and removes those which
        have degree less than kw.nconnmin
        '''
        for indice in self.minima_index['Index']:
            self.minima_index['Index'][indice]['Degree'] = 0
            self.minima_index['Index'][indice]['TS'] = []

        for indice in self.ts_index['Index']:
            
            x = self.ts_index['Index'][indice]['Min1']
            y = self.ts_index['Index'][indice]['Min2']
            
            self.minima_index['Index'][x]['Degree'] += 1
            self.minima_index['Index'][y]['Degree'] += 1
            self.minima_index['Index'][x]['TS'].append(indice)
            self.minima_index['Index'][y]['TS'].append(indice)

    def RemoveDisjoint(self):
        '''
        Removes minima which lie in disjointed graphs from the global
        minimum.
        '''
        # Initiate temp_dict
        temp_dict = dict(Index = {}, Size = 0)
        for i in self.minima_index['Index']:
            temp_dict['Index'][i] = {'Connect to GM':False, 
                                     'dist': self.minima_index['Size']}
        GM = self.minima_index['GM']['Index']
        temp_dict['Index'][GM] = {'Connect to GM':True, 
                                  'dist': 0}

        conv = True
        x = 1
        
        while conv:
            change = True
        
            for i in self.minima_index['Index']:
                if temp_dict['Index'][i]['Connect to GM']:
                
                    TS = self.minima_index['Index'][i]['TS']

                    for j in TS:
                        Min = self.ts_index['Index'][j]['Min1']
                        if Min == i:
                            Min = self.ts_index['Index'][j]['Min2']
                        temp_dict['Index'][Min]['Connect to GM']=True
                        
                        dist = temp_dict['Index'][i]['dist'] + 1
                        y = 0
                        if (temp_dict['Index'][Min]['dist'] > dist):
                            change = False
                            temp_dict['Index'][Min]['dist'] = dist
        
                            y += 1

            if change: conv = False

        for indice in temp_dict['Index']:
            if not temp_dict['Index'][indice]['Connect to GM']:
                for i in self.minima_index['Index'][indice]['TS']:
                    if self.ts_index['Index'].has_key(i):
                        del self.ts_index['Index'][i]
                del self.minima_index['Index'][indice]
        
        x += 1

    def DumpNumbers(self):
        '''
        If self.kw.dump_numbers['dump'] = True, write which minima belong to 
        which level in file "node_numbers"
        '''
        f = open(r'node_numbers','w')
        for l in self.basin_index['Level']:
            f.write('LEVEL\t%d\n=========\n\n'%l)
            for b in self.basin_index['Level'][l]['Basin']:
                f.write('Node\t%d\n'%b)
                for m in self.basin_index['Level'][l]['Basin'][b]['Min']:
                    f.write('%d\n'%m)
        f.close()
