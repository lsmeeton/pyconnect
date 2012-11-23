#!/usr/bin/env python

import os
import sys

__metaclass__ = type

class Keyword():
    '''
    Reads keywords from input file dinfo and stores them as a nested 
    dictionary
    '''
    
    def __init__(self):
        super(Keyword, self).__init__()
        
        # Initialise attributes
        self.input_file = 'pca_inpt'
        self.conv = 10E-7 # Convergence criteria for rotational invariance 
        # procedure. By default 10E-7
        
        self.iterations = 100 # If rotational invariance procedure doesn't 
        # converge within this many iterations, stop calculation.
        # By default 100.
        
        self.min_file = None # Name of file which contains list of 
        # indices of minima to be included in PCA calc.
        
        self.n_atoms = None # No. of atoms in system
        
        self.points = None  # Location of unformatted-FORTRAN (yawn) co-ordinate
                            # file
        
        # basis = 'dihedral'
        self.basis = None
        
        self.beta = None
                
        
        self.PC_coords_dir_name = 'PC_coords'
        self.PC_project_dir_name = 'PC_projections'
        self.PC_variance = 'PC_variance'
           
        self.FileCheck(self.input_file)
        self.InputRead()
        
        self.InfoCheck()
        self.TypeCheck()
        
         
        
        
        
    def FileCheck(self,file_name):
        '''
        Checks the existence of the file 'file_name'
        '''
        if not os.path.exists(file_name):
            sys.exit('ERROR: Could not find file %s'%file_name)

    def InputRead(self):
        '''
        
        '''
#        f = open(self.input_file,'r')
        for line in open(self.input_file,'r'):
            self.LineRead(line)
       
    def LineRead(self, line):
        '''
        Processes the line passed to it from InputRead in self.input_file
        '''
        line = line.split()
        if str(line[0]) == '#' or str(line[0]).lower() == 'comment': return
        elif self.__dict__.has_key(str(line[0])):
            self.__dict__[str(line[0])] = line[1]
            
    def InfoCheck(self):
        '''
        Checks if all required information is present
        '''
        required_dict = self.__dict__.copy()
        missing_list = [] # List of missing keywords
        del required_dict['beta']
        for attr, var in required_dict.iteritems():

            if not var: missing_list.append(attr)
        
        if missing_list:
            print 'WARNING: Following Keywords not found:\n', missing_list
            sys.exit()
    
    def TypeCheck(self):
        '''
        Ensure Attributes are correct type
        '''
        self.conv = float(self.conv)
        self.iterations = int(self.iterations) 
        self.min_file = str(self.min_file)
        self.n_atoms = int(self.n_atoms)
        self.basis = str(self.basis)
        if self.beta: self.beta = float(self.beta)
        self.PC_coords_dir_name = str(self.PC_coords_dir_name)
        self.PC_project_dir_name = str(self.PC_project_dir_name)
        self.PC_variance = str(self.PC_variance)
    
if __name__ == '__main__':
    kw = Keyword()
    for attr, val in kw.__dict__.iteritems():
        print attr, val
        

