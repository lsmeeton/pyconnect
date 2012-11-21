'''
Written by Lewis Smeeton, 2012
'''
import numpy as np
import os
from Disconnect import Disconnect


#--------------------------------------------------------------------#
__metaclass__ = type

class DisconnectPlot(Disconnect):
    def __init__(self):
        super(DisconnectPlot, self).__init__()

        if self.draw:
            
            self.Plot()

    def BasinAssignment(self):
         '''
         Does the plotting
         '''
         # Calculate No. of connections for each minima
         self.CalcDegree()

         # Check that the stationary point database is actually
         # connected, and remove minima that lie in disjoint
         # graphs.
         self.RemoveDisjoint()

         # Calculate minimum number of steps of each minimum 
         # from the global minimum
         self.GlobalDistance()

         # Flag transition states to underconnected minima as dead
         # hmmm

         # Pick File (Whatever that is)

         # Reset Transition states to the energy of the higher of
         # the two minima they connect if this option is turned on
         self.ResetTransitions()
         
         # Assign the minima to their basins at each energy
         #self.BasinAssignment()

    def CalcDegree(self):
        '''
        Calculates the number of connections that each minimum for
        and remove minima which have connections less than 
        self.nconnmin
        '''
        return


    def RemoveDisjoint(self):
        return

    def GlobalDistance(self):
        return

    def ResetTransitions(self):
        return

    
