'''
Written by Lewis Smeeton, 2012
'''
import numpy as np
import os
from ProxyDisconnect import Disconnect
from DisconnectPlot import DisconnectPlot as plot

#--------------------------------------------------------------------#
__metaclass__ = type

class DisconnectPlot(plot):
    def __init__(self,*args):
        super(DisconnectPlot, self).__init__(*args)
        '''
        DisconnectPlot 
        '''
        self.PositionBasins()
        
