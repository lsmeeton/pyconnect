'''
Written by Lewis Smeeton, 2012
'''
import numpy as np
import matplotlib.colors as colors
import matplotlib.cm as cm
import os
#from ProxyDisconnect import Disconnect
from Disconnect import Disconnect
import sys

#--------------------------------------------------------------------#
__metaclass__ = type

class DisconnectPlot(Disconnect):
    def __init__(self,*args):
        super(DisconnectPlot, self).__init__(*args)
        '''
        DisconnectPlot 
        '''
        #self.PositionBasins()
        #print hasattr(self,'basin_index')#['Level'])
        
    def PositionBasins(self):
        '''
        Initalise basin locations, and then either extract positions
        from file or calculate.
        '''
        pres = self.kw.metric3d['present']
        if self.kw.metric3d['present']:#self.kw.metric3D['present']: 
            self.GetMetric3DNewStyle()
#            self.GetMetric3D()
        elif self.kw.metric['present']: 
            self.GetMetric2DNewStyle()
#            self.GetMetric2D()
        print self.kw.trval
        if self.kw.trmin['trmin_file']: self.GetTrminColours()
        elif self.kw.trval['trval_file']: self.GetTrvalColoursNewStyle()
        
        self.CalcCoordinates()
        self.ArangeBasins()

    def CalcCoordinates(self):
        '''
        Calculates absolute coordinates of each basin for the relevant
        display media (ie. ps/eps/openGL etc)
        '''
        self.AssignLineHeight()
#        if self.kw.metric3d['present']:#self.kw.metric3D['present']: 
#            self.OpenGLCoordsMetric3D()
#        elif self.kw.metric['present']: 
#            self.OpenGLCoordsMetric2D()
#        self.OpenGLCoordsMetric3D()

    def OpenGLCoordsMetric2D(self):
        '''
        Calculates absolute openGL coordinates
        '''
        # Calc range, r of order param and scale appropriately
        minX = self.basin_index['MinX']
        maxX = self.basin_index['MaxX']
        r = maxX - minX
        # Calc 'order of magnitude' of r
        r_exponent = np.log10(r)
        r_exponent = np.floor(r_exponent)
        minX = np.floor(minX/(10**r_exponent))*(10**r_exponent)
        maxX = np.ceil(maxX/(10**r_exponent))*(10**r_exponent)
        r = maxX - minX
#        print 'r', r, maxX, minX
        self.basin_index['MetXTickNo'] = int(r/(10**r_exponent))
        self.basin_index['MinX'] = minX
        self.basin_index['MaxX'] = maxX
        
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                c = self.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
         
                
                x = (self.basin_index['Level'][l]['Basin']
                     [b]['MetricX'] - minX)/r
                x -= 0.5 #Shifts 0.5 to left
                
                self.basin_index['Level'][l]['Basin'][b]['GLX'] = x
        
                if not c:
                    z = -1.0*self.basin_index['Level'][l]['Basin'][b]\
                        ['Z']
                    z += 0.5
                    self.basin_index['Level'][l]['Basin'][b]['GLZ'] = z
        
    
    def OpenGLCoordsMetric3D(self):
        '''
        Calculates absolute openGL coordinates
        '''
        # Calc range, r of order param and scale appropriately
        minX = self.basin_index['MinX']
        maxX = self.basin_index['MaxX']
        minY = self.basin_index['MinY']
        maxY = self.basin_index['MaxY']
        rx = self.basin_index['MaxX'] - self.basin_index['MinX']
        ry = self.basin_index['MaxY'] - self.basin_index['MinY']
        
        rx_exponent = np.log10(rx)
        rx_exponent = np.floor(rx_exponent)
        minX = np.floor(minX/(10**rx_exponent))*(10**rx_exponent)
        maxX = np.ceil(maxX/(10**rx_exponent))*(10**rx_exponent)
        rx = maxX - minX
        self.basin_index['MetXTickNo'] = int(rx/(10**rx_exponent))
        self.basin_index['MinX'] = minX
        self.basin_index['MaxX'] = maxX

        ry_exponent = np.log10(ry)
        ry_exponent = np.floor(ry_exponent)
        minY = np.floor(minY/(10**ry_exponent))*(10**ry_exponent)
        maxY = np.ceil(maxY/(10**ry_exponent))*(10**ry_exponent)
        ry = maxY - minY
        self.basin_index['MetYTickNo'] = int(ry/(10**ry_exponent))
        self.basin_index['MinY'] = minY
        self.basin_index['MaxY'] = maxY

        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                c = self.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
         
                
                x = (self.basin_index['Level'][l]['Basin']
                     [b]['MetricX'] - minX)/rx
                x -= 0.5 #Shifts 0.5 to left
                
                y = (self.basin_index['Level'][l]['Basin']
                     [b]['MetricY'] - minY)/ry
                y -= 0.5 #Shifts 0.5 to left

                self.basin_index['Level'][l]['Basin'][b]['GLX'] = x
                self.basin_index['Level'][l]['Basin'][b]['GLY'] = y
        
                if not c:
                    z = -1.0*self.basin_index['Level'][l]['Basin'][b]\
                        ['Z']
                    z += 0.5
                    self.basin_index['Level'][l]['Basin'][b]['GLZ'] = z
    

    
    def AssignLineHeight(self):
        '''
        
        '''
        delta = 1.0/(self.kw.levels['n'] - 1)
        
        r = self.basin_index['Level'][1]['Energy'] - \
            self.basin_index['Level'][self.kw.levels['n']]['Energy']
        for l in self.basin_index['Level']:
            if l == 1: continue
            for b in self.basin_index['Level'][l]['Basin']:
                if not (self.basin_index['Level'][l]['Basin'][b]
                        ['Children']):
                    
                    m = self.basin_index['Level'][l]['Basin'][b]\
                        ['Min'][0]
                    min_e = self.minima_index['Index'][m]['Energy']
                    basin_e = self.basin_index['Level'][l-1]['Energy']
                    
                    self.basin_index['Level'][l]['Basin'][b]['Z']\
                        = self.basin_index['Level'][l-1]['Z'] -\
                        ((min_e - basin_e)/r)
                    
    def GetMetric2D(self):
        '''
        Get metric information for each minima from file 'metric_file'
        '''
        # Open File 'metric_file' and read data
        i = 0
        for lines in open(self.kw.metric['metric_file']):
            i += 1
            if (self.minima_index['Index'].has_key(i)):
                x = float(lines.split()[0])
                self.minima_index['Index'][i]['Metric']['x'] = x
                # Find max and min values of x for scaling
                if x > self.basin_index['MaxX']: 
                    self.basin_index['MaxX'] = x
                if x < self.basin_index['MinX']:
                    self.basin_index['MinX'] = x
                
        
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp = []
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp.append(self.minima_index['Index'][m]
                                ['Metric']['x'])
                self.basin_index['Level'][l]['Basin'][b]\
                    ['MetricX'] = np.mean(temp)
                    
    def GetMetric2DNewStyle(self):
        '''
        Get metric information for each minima from file 'metric_file'. This
        function is designed to work with PCA data from new PCA implementation.
        '''
        # Open File 'metric_file' and read data
        f = open(self.kw.metric['metric_file'])
        lines = iter(f)
        lines.next() # Skip the first two lines of the input file
        lines.next() # as it contains header information
        
        for line in lines:
            
            line = line.split()
            m = int(line[0])
            x = float(line[1])

            try: 
                self.minima_index['Index'][m]['Metric']['x'] = x
                if x > self.basin_index['MaxX']: 
                    self.basin_index['MaxX'] = x
                if x < self.basin_index['MinX']:
                    self.basin_index['MinX'] = x
            except KeyError:
                print self.kw.metric['metric_file'] 
                print('Minimum %d in file "%s" but not in file "%s"'
                      %(m,
                        self.kw.metric['metric_file'],
                        self.kw.minima['data_file']))
#                print self.minima_index['Index'][m ]
                sys.exit()

                
        
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp = []
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp.append(self.minima_index['Index'][m]
                                ['Metric']['x'])
                #print temp
                self.basin_index['Level'][l]['Basin'][b]\
                    ['MetricX'] = np.mean(temp)
                    
    def GetMetric3D(self):
        '''

        '''
        # Open File 'metric_file' and read data
        
        i = 0
        for lines in open(self.kw.metric3d['metricx_file']):
            i += 1
            if (self.minima_index['Index'].has_key(i)):
                x = float(lines.split()[0])
                
                self.minima_index['Index'][i]['Metric']['x'] = x
                
                if x > self.basin_index['MaxX']: 
                    self.basin_index['MaxX'] = x
                if x < self.basin_index['MinX']:
                    self.basin_index['MinX'] = x
                
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp_x = []
                
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp_x.append(self.minima_index['Index'][m]
                                  ['Metric']['x'])
                                    
                self.basin_index['Level'][l]['Basin'][b]\
                    ['MetricX'] = np.mean(temp_x)
                        
        # Open File 'metric_file' and read data
        
        i = 0
        for lines in open(self.kw.metric3d['metricy_file']):
            i += 1
            if (self.minima_index['Index'].has_key(i)):
                y = float(lines.split()[0])
                self.minima_index['Index'][i]['Metric']['y'] = y
                    
                if y > self.basin_index['MaxY']: 
                    self.basin_index['MaxY'] = y
                if y < self.basin_index['MinY']:
                    self.basin_index['MinY'] = y

        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp_y = []
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp_y.append(self.minima_index['Index'][m]
                                  ['Metric']['y'])
                    
                self.basin_index['Level'][l]['Basin'][b]\
                    ['MetricY'] = np.mean(temp_y)
        
    def GetMetric3DNewStyle(self):
        '''
        
        '''
        f = open(self.kw.metric3d['metricx_file'])
        lines = iter(f)
        lines.next() # Skip the first two lines of the input file
        lines.next() # as it contains header information
        
        for line in lines:
            
            line = line.split()
            m = int(line[0])
            x = float(line[1])

            try: 
                self.minima_index['Index'][m]['Metric']['x'] = x
                if x > self.basin_index['MaxX']: 
                    self.basin_index['MaxX'] = x
                    
                if x < self.basin_index['MinX']:
                    self.basin_index['MinX'] = x
                    
            except KeyError:
#                print self.kw.metric3d['metricx_file'] 
                print('Minimum %d in file "%s" but not in file "%s"'
                      %(m,
                        self.kw.metric3d['metricx_file'],
                        self.kw.minima['data_file']))
#                print self.minima_index['Index'][m ]
                sys.exit()

            
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp_x = []
                
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp_x.append(self.minima_index['Index'][m]
                                  ['Metric']['x'])
                                    
                self.basin_index['Level'][l]['Basin'][b]\
                    ['MetricX'] = np.mean(temp_x)
                        
        # Open File 'metric_file' and read data
        f = open(self.kw.metric3d['metricy_file'])
        lines = iter(f)
        lines.next() # Skip the first two lines of the input file
        lines.next() # as it contains header information
        
        for line in lines:
            
            line = line.split()
            m = int(line[0])
            y = float(line[1])

            try: 
                self.minima_index['Index'][m]['Metric']['y'] = y
                if y > self.basin_index['MaxY']: 
                    self.basin_index['MaxY'] = y
                if y < self.basin_index['MinY']:
                    self.basin_index['MinY'] = y
            except KeyError:
                print self.kw.metric['metric_file'] 
                print('Minimum %d in file "%s" but not in file "%s"'
                      %(m,
                        self.kw.metric['metric_file'],
                        self.kw.minima['data_file']))
#                print self.minima_index['Index'][m ]
                sys.exit()
                

        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp_y = []
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp_y.append(self.minima_index['Index'][m]
                                  ['Metric']['y'])
                    
                self.basin_index['Level'][l]['Basin'][b]\
                    ['MetricY'] = np.mean(temp_y)
        


    def GetTrminColours(self):
        '''
        Read Trmin colours from file 'trmin_file'
        '''
        # Initialise a colour dictionary ===> so far only have 4
        # colours to chose from
        colour_dict = {1:(1.0,0.0,0.0), #red
                       2:(0.0,1.0,0.0), #blue
                       3:(0.0,0.0,1.0), #green
                       4:(0.5,0.0,0.5)} #purple
        col = 1
        for f in self.kw.trmin['trmin_file']:
            
            
            for lines in open(f):#(trmin_file):
                m = int(lines.split()[0])
                
                if (self.minima_index['Index'].has_key(m)):
                                            
                    self.minima_index['Index'][m]['Colour']['RGB'] = \
                        colour_dict[col]
                

            for l in self.basin_index['Level']:
                for b in self.basin_index['Level'][l]['Basin']:
                    #temp_y = []
                    for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                        self.basin_index['Level'][l]['Basin'][b]\
                            ['RGB'] =\
                            self.minima_index['Index'][m]['Colour']\
                            ['RGB']
            col += 1
            
    def GetTrvalColours(self):
        '''
        Read Trval colours from file 'trval_file'
        '''
        # Chose colour scheme
        self.col_map = cm.get_cmap('brg_r')
        
        i = 0

        for lines in open(self.kw.trval['trval_file']):
            i += 1
            if (self.minima_index['Index'].has_key(i)):
                col = float(lines.split()[0])
                self.minima_index['Index'][i]['Metric']['trval'] = col
                if col == None: sys.exit('None! %d'%i)
                # Find max and min values of x for scaling
                if col > self.basin_index['MaxTrval']: 
                    self.basin_index['MaxTrval'] = col
                if col < self.basin_index['MinTrval']:
                    self.basin_index['MinTrval'] = col
                
        self.norm = colors.Normalize(vmin = self.basin_index['MinTrval'],
                                     vmax = self.basin_index['MaxTrval'])
        
        self.map = cm.ScalarMappable(norm = self.norm, 
                                     cmap = self.col_map)
        
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp = []
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp.append(self.minima_index['Index'][m]
                                ['Metric']['trval'])

                self.basin_index['Level'][l]['Basin'][b]\
                    ['Trval'] = np.mean(temp)
                    
                self.basin_index['Level'][l]['Basin'][b]\
                    ['RGB'] = self.map.to_rgba(self.basin_index['Level']
                                          [l]['Basin'][b]['Trval'])
    def GetTrvalColoursNewStyle(self):
        '''
        Reads Trval values from "New Style" order parameter files
        '''
        self.col_map = cm.get_cmap('brg_r')
        
#        i = 0

        for lines in open(self.kw.trval['trval_file']):
#            i += 1
            lines = lines.split()
            if lines[0] == '#' or len(lines) == 0: continue
            i = int(lines[0])
#            print lines
            if (self.minima_index['Index'].has_key(i)):
                col = float(lines[1])
#                print i, col
                self.minima_index['Index'][i]['Metric']['trval'] = col
                if col == None: sys.exit('None! %d'%i)
                # Find max and min values of x for scaling
                if col > self.basin_index['MaxTrval']: 
                    self.basin_index['MaxTrval'] = col
                if col < self.basin_index['MinTrval']:
                    self.basin_index['MinTrval'] = col
                
        self.norm = colors.Normalize(vmin = self.basin_index['MinTrval'],
                                     vmax = self.basin_index['MaxTrval'])
        
        self.map = cm.ScalarMappable(norm = self.norm, 
                                     cmap = self.col_map)
        
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                temp = []
                for m in self.basin_index['Level'][l]['Basin'][b]\
                    ['Min']:
                    temp.append(self.minima_index['Index'][m]
                                ['Metric']['trval'])
#                print temp
                self.basin_index['Level'][l]['Basin'][b]\
                    ['Trval'] = np.mean(temp)
                    
                self.basin_index['Level'][l]['Basin'][b]\
                    ['RGB'] = self.map.to_rgba(self.basin_index['Level']
                                          [l]['Basin'][b]['Trval'])

    def ArangeBasins(self):
        '''

        '''
        self.CountMin()
        total_clmn = self.minima_index['Size']
        print 'Level 1'
        low_clmn = 1
        for b in self.basin_index['Level'][1]['Basin']:
        
            self.basin_index['Level'][1]['Basin'][b]['FirstClmn']\
                = low_clmn

            high_clmn = low_clmn + self.basin_index['Level'][1]\
                ['Basin'][b]['Size']

            self.basin_index['Level'][1]['Basin'][b]['LastClmn']\
                = high_clmn - 1

            low_clmn = high_clmn
            
            self.OpenGLCoordsDisconnect(1,b)
            
        for l in self.basin_index['Level']:
            if l == 1: continue
        
            print 'Level %d'%l
            for b in self.basin_index['Level'][l-1]['Basin']:
                c = self.basin_index['Level'][l-1]['Basin'][b]\
                    ['Children']
                if c:
                    low_clmn = self.basin_index['Level'][l-1]\
                        ['Basin'][b]['FirstClmn']
                    high_clmn = self.basin_index['Level'][l-1]\
                        ['Basin'][b]['LastClmn']
        
                
                    self.DistributeBasins(l,c,low_clmn,high_clmn)

    def DistributeBasins(self,l,c,low_clmn,high_clmn):
        '''
        If the parent basin has more than 1 clmn, then
        it could have more than 1 child, which must 
        be distributed correctly. This method 
        distributes basins according to their size
        evenly.
        '''
        sgn = True
        # Sort basins into ascending size order
        size_order = self.OrderByBasinSize(l,c)
       
#        size_order = self.Mix(size_order)
        
        for i in size_order:
            if sgn:
                basin_size = self.basin_index['Level']\
                    [l]['Basin'][i]['Size']
                self.basin_index['Level'][l]['Basin'][i]\
                    ['FirstClmn'] = low_clmn
                high_clmn_temp = low_clmn + basin_size
                self.basin_index['Level'][l]['Basin'][i]\
                    ['LastClmn'] = high_clmn_temp - 1 
                low_clmn = high_clmn_temp
                self.OpenGLCoordsDisconnect(l,i)
                sgn = False
                continue
            if not sgn:
                basin_size = self.basin_index['Level']\
                    [l]['Basin'][i]['Size']
                self.basin_index['Level'][l]['Basin'][i]\
                    ['LastClmn'] = high_clmn
                low_clmn_temp = high_clmn - basin_size
                self.basin_index['Level'][l]['Basin'][i]\
                    ['FirstClmn'] = low_clmn_temp + 1 
                high_clmn = low_clmn_temp 
                self.OpenGLCoordsDisconnect(l,i)
                sgn = True
        

    def OpenGLCoordsDisconnect(self, l, b):
        '''

        '''
        first_clmn = self.basin_index['Level'][l]['Basin'][b]\
            ['FirstClmn']
        last_clmn = self.basin_index['Level'][l]['Basin'][b]\
            ['LastClmn']
        
        if (first_clmn != last_clmn):
            self.basin_index['Level'][l]['Basin'][b]['X']\
                = np.mean([first_clmn,last_clmn])\
                /float(self.minima_index['Size']) - 0.5
        
        if (first_clmn == last_clmn):
            c = self.basin_index['Level'][l]['Basin'][b]['Children']
            if c:
            
                self.basin_index['Level'][l]['Basin'][b]['X']\
                    = float(first_clmn)/float(self.minima_index['Size'])\
                    -0.5
            else:
                z = -1.0*self.basin_index['Level'][l]['Basin'][b]\
                ['Z']
                z += 0.5
                self.basin_index['Level'][l]['Basin'][b]['GLZ'] = z
                
                p = self.basin_index['Level'][l]['Basin'][b]['Parents']
                p_first_clmn = self.basin_index['Level'][l-1]\
                    ['Basin'][p]['FirstClmn']
                p_last_clmn = self.basin_index['Level'][l-1]\
                    ['Basin'][p]['LastClmn']
   
                if (p_first_clmn == p_last_clmn):
                    self.basin_index['Level'][l]['Basin'][b]['X']\
                        = float(first_clmn)/\
                        float(self.minima_index['Size']) - 0.5
                if (p_first_clmn != p_last_clmn):
                    delta = 1.0/(self.kw.levels['n'] - 1)
                    level_z = self.basin_index['Level'][l-1]['Z']
                    basin_z = self.basin_index['Level'][l]['Basin']\
                        [b]['Z']
                    p_x = self.basin_index['Level'][l-1]['Basin'][p]\
                        ['X']
                    ratio = abs(basin_z - level_z)/delta
            
                    x_temp = float(first_clmn)/float(
                        self.minima_index['Size'])-0.5
            
                    self.basin_index['Level'][l]['Basin'][b]['X']\
                        = ratio*(x_temp - p_x) + p_x
        



    def OrderByBasinSize(self,l,c):
        '''
        Aranges the basins in list c (at Level l) according 
        to basin size, and returns an ordered list.
        '''
        temp_order_dict = {}
        temp_order = []
        for i in c:
            
            s = self.basin_index['Level'][l]['Basin'][i]['Size']
            
            temp_order_dict[i] = s
        
        temp_order = sorted(temp_order_dict, key=temp_order_dict.
                            get)
        return temp_order

#    def Mix(self, mixlist):
#        '''
#        Mixes up the size order
#        '''
#        n = len(mixlist)
#        copylist = []#mixlist
#        pos = 1
#        sgn = True
#        delta = n - 1
##        print 'mix', mixlist
#        while True:
#            
#            if sgn:
#                x = mixlist.pop(0)
#                copylist.append(x)
#                if len(mixlist) == 0: break
#                sgn = False
#            #    print 'T', x
#            if not sgn:
#                x = mixlist.pop()
#                copylist.append(x)
#                sgn = True
#            #    print 'F', x
#                if len(mixlist) == 0: break
##        print 'COPY', copylist
#        return copylist
    
#==============================================================================    
#===========================Interactive Functions============================== 
#==============================================================================

    def ChangeBasinColour(self, colour_in, colour_out):
        '''
        Changes the colour of a collection of minima defined using the trmin keyword
        '''
        for l in self.basin_index['Level']:
            for b in self.basin_index['Level'][l]['Basin']:
                for m in self.basin_index['Level'][l]['Basin'][b]['Min']:
                    rgb = self.basin_index['Level'][l]['Basin'][b]['RGB']
                    if (rgb == colour_in):
                            self.basin_index['Level'][l]['Basin'][b]['RGB'] = colour_out
                                
    
    def SwapBasinLocation(self,b1,b2,l,p):
        '''
        Swaps the locations of basins b1 and b2 and level l, who share a parent 
        p at level l-1.
        '''
        pcheck = self.basin_index['Level'][l-1]['Basin'][p].has_key()
        p1 = self.basin_index['Level'][l]['Basin'][b1]['Parent']
        p2 = self.basin_index['Level'][l]['Basin'][b2]['Parent']
        if p1 == p and p2 ==p and pcheck:
            f1 = self.basin_index['Level'][l]['Basin'][b1]['FirstClmn']
            l1 = self.basin_index['Level'][l]['Basin'][b1]['LastClmn']
            s1 = self.basin_index['Level'][l]['Basin'][b1]['Size']
            
            f2 = self.basin_index['Level'][l]['Basin'][b2]['FirstClmn']
            l2 = self.basin_index['Level'][l]['Basin'][b2]['LastClmn']
            s2 = self.basin_index['Level'][l]['Basin'][b2]['Size']
            
            