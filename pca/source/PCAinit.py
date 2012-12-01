import sys
import os
import numpy as np
import array
import lewismod
import structure_reader
import matplotlib.mlab as mlab

__metaclass__ = type

class PCAinit():

    def __init__(self, keyword):
        
        self.kw = keyword
        
#        self.kw.conv = 10E-7 # Convergence criteria for rotational invariance 
#        # procedure.
#        
#        self.kw.iterations = 100 # If rotational invariance procedure doesn't 
#        # converge within this many iterations, stop calculation.
#        
#        self.kw.min_file = 'minimaindex' # Name of file which contains list of 
#        # indices of minima to be included in PCA calc.
#        
#        self.kw.n_atoms = 31 # No. of atoms in system
#        
#        # basis = 'dihedral'
#        self.kw.basis = 'cartesian'
        
#        self.kw.beta = 0.0
#        self.kw.beta_true = False
#        if self.kw.beta > 0.0: self.kw.beta_true = True
        
        self.config_space = np.array([])
        
        self.min_index = []
        self.n_min = 0
        self.shape = np.array([])
        
        self.kw.PC_coords_dir_name = 'PC_coords'
        self.kw.PC_project_dir_name = 'PC_projections'
        self.kw.PC_variance = 'PC_variance'
#        self.DirectoryCheck()
#        self.kw.basisCheck()
#        self.ShapeCheck()
#        self.ReadMinimaIndex()
#        self.ReadConfigurationSpace()

    #==========================================================================#
    # FUNCTIONS
    
    #--------------------------------------------------------------------------#
    # Functions ensuring input is acceptable 
    
    def DirectoryCheck(self):
        if os.path.isdir(self.kw.PC_coords_dir_name):
            sys.exit('WARNING: directory "%s" already exists - \nAborting calculation'
                     %self.kw.PC_coords_dir_name)
#        try: os.mkdir(self.kw.PC_coords_dir_name)
#        except OSError: sys.exit('WARNING: directory "%s" already exists - \nAborting calculation'%self.kw.PC_coords_dir_name)
        if os.path.isdir(self.kw.PC_project_dir_name):
            sys.exit('WARNING: directory "%s" already exists - \nAborting calculation'
                     %self.kw.PC_project_dir_name)
#        try: os.mkdir(self.kw.PC_project_dir_name)
#        except OSError: sys.exit('WARNING: directory "%s" already exists - \nAborting calculation'%self.kw.PC_project_dir_name)
        
        if os.path.exists(self.kw.PC_variance):
            sys.exit('WARNING: file "%s" already exists - \nAborting calculation'%self.kw.PC_variance)
        
#        lewismod.file_check('points.min')
#        lewismod.file_check(self.kw.points)
        self.kw.FileCheck(self.kw.points)
#        lewismod.file_check('min.data')
#        lewismod.file_check(self.kw.min_file)
        self.kw.FileCheck(self.kw.min_file)
        self.CountLines()
        
    def CountLines(self):
        self.total_min = lewismod.count_lines(self.kw.min_file)
        print 'No. of minima: %i'%self.total_min
        
    def BasisCheck(self):
        if self.kw.basis != 'cartesian':
            if self.kw.basis != 'dihedral':
                raise RuntimeError('basis set to be transformed to must either be "cartesian" or "dihedral"')
        else:
            print 'PCA will be performed on the', self.kw.basis, 'basis set'
        
    def ShapeCheck(self):
        self.shape = np.shape(self.config_space)
        
        if self.shape != (3*self.kw.n_atoms*self.n_min,):
            raise RuntimeError('Warning: config_space has shape', self.shape,
                               ' expected to have shape (3*',self.kw.n_atoms,'*'
                               ,self.n_min,') = ',(3*self.kw.n_atoms*self.n_min,))


    #--------------------------------------------------------------------------#
    
    def ReadConfigurationSpace(self):
        '''
        Reads the structure of the minima specified in "self.kw.min_file" from the
        unformatted FORTRAN file "points.min" and saves them in the array 
        self.config_space
        '''
        print 'Reading %d structures containing %d atoms each'%(self.n_min,
                                                                 self.kw.n_atoms)
        for i in range(self.n_min):
            rec = self.min_index[i]
            progress = (float(i)/float(self.n_min))*100 # Note integer division
            sys.stdout.write('\r')
            sys.stdout.write("%3.1f%% Structures Read"%(progress))
            sys.stdout.flush()
            self.config_space = np.append(self.config_space, 
                                          structure_reader.structure_read(self.kw.points,
                                                                        rec, 
                                                                        self.kw.n_atoms))
        sys.stdout.write('\r')
        sys.stdout.write('All Structures Read  \n')
        print 'Extraction complete'
    
    def ReadMinimaIndex(self):
        '''    
        Reads minima indices from the file "self.kw.min_file" and saves
        them in the list self.min_index
        '''
        
        for line in open(self.kw.min_file,'r'):
            self.min_index.append(int(line))
        self.n_min = len(self.min_index)
        print 'List of indices of minima to be included in PCA calculation read in from file: %s'%(self.kw.min_file)
    
    
    def WritePCAToFile(self, PCA, Q=1, Normal=True):
        projection = PCA[:,Q - 1]
            
        if Normal:
            projection_max = max(projection)
            projection_min = min(projection)
            projection = (projection -
                          float(projection_min))/(float(projection_max)-
                                                  float(projection_min))
    
            projection_print = np.array([-1.0]*self.total_min)
            for i in range(len(self.min_index)):
                projection_print[self.min_index[i] - 1] = projection[i]
            
        
            f = open(r'PCA_order_parameter_normal' + str(Q), 'w')
            
            for i in range(len(projection_print)):
                
                f.write('%1.3f'%projection_print[i] + '\n')
            f.close()
        else:
            f = open(r'PCA_order_parameter' + str(Q), 'w')
            for i in range(len(projection)):
                f.write('%1.3f'%projection[i] + '\t' + '%i'%min_index[i] + '\n')
            f.close()

if __name__ == '__main__':
    pca = PCAinit()
    pca.DirectoryCheck()
    pca.BasisCheck()
    
    pca.ReadMinimaIndex()
    pca.ReadConfigurationSpace()
    pca.ShapeCheck()