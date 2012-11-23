#!/usr/bin/env python
'''
Written by Lewis Smeeton, 2012
'''

import sys
import numpy as np
from PCAinit import PCAinit

__metaclass__ = type

class MyPCAprep(PCAinit):
    def __init__(self,*args):
        super(MyPCAprep, self).__init__(*args)
        return
        
#        if self.beta_true:
#            self.ReadMinimaEnergies()   
#            self.FindGM()
#            self.CalculateWeighting()
#        
#        # Convert config_space to selected basis set
#        if self.basis == 'cartesian':
#            self.ReshapeCartesianIn()
#            self.LstSqrStructureFit()
#            self.ReshapeCartesianOut()
#
#        if self.basis == 'dihedral':
#            print 'Converting to Internal Dihedrals'
#            self.ReshapeDihedral()
#            print 'Done'
    #==========================================================================#
    # FUNCTIONS
    
    # Functions that calculate weighting for PCA analysis
    
    def ReadMinimaEnergies(self):
        '''
        Reads minima energies from file "min.data" and stores them in the list 
        self.min_energies. The index of the list is equivalent to the index of
        the minimum.
        '''
        self.min_energies = [0]
        for line in open('min.data'):
            self.min_energies.append(float(line.split()[0]))

    
    def FindGM(self):
        '''
        Locates the global minimum in the list self.min_energies
        '''
        self.global_min_energy = min(self.min_energies)
        self.global_min_index = self.min_energies.index(self.global_min_energy)

        print 'Global minimum: %d\tEnergy = %2.4f'%(self.global_min_index,
                                                    self.global_min_energy)
       
    def CalculateWeighting(self):
        '''
        Calculates boltzmann weighting of each minima in self.min_index 
        according to their energy, ie.
        weighting of state i with energy E_i, w_i = exp(-1.0*E_i*beta), where beta
        is a parameter used to preferentially weight towards lower energy 
        structures and is analogous to a fictional inverse temperature
        '''
        self.weight_array = np.zeros(self.n_min)
        print 'Structures will be boltzmann weighted according to e^(-beta*dE) where beta =%3.2f and dE is energy of minima relative to the global minimum'%self.kw.beta
        for i in range(self.n_min):
            j = self.min_index[i] # BAD style, but will have to do
            self.weight_array[i] = self.min_energies[j]
            # Calculate weights for each structure, and store in array
    
        self.weight_array = self.weight_array - self.global_min_energy
    
        self.weight_array = np.exp(self.weight_array*self.kw.beta*(-1.0))
    
    #--------------------------------------------------------------------------#
    
    # Functions that reshape config_space array

    def ReshapeCartesianIn(self):
        '''
        Converts config_space from a 1D [n_min*n_atoms*3] array to 
        a 3D [n_min][3][n_atoms] numpy array of cartesian co-ordinates
        '''
        self.config_space = self.config_space.reshape([self.n_min, 
                                                       self.kw.n_atoms, 
                                                       3])
        self.config_space = np.swapaxes(self.config_space,1,2)
        self.shape = np.shape(self.config_space)

    def ReshapeCartesianOut(self):
        '''
        Converts config_space from a 3D [n_min][3][n_atoms] numpy array to a 
        [n_min][3*n_atoms] numpy array
        '''
        self.config_space = np.swapaxes(self.config_space,1,2)
        self.config_space = self.config_space.flatten()
        self.config_space = self.config_space.reshape(self.n_min,3*self.kw.n_atoms)
        self.shape = np.shape(self.config_space)

        
    def ReshapeDihedral(self):
        '''
        Converts config_space from a 1D [n_min*n_atoms*3] array to 
        a 2D [n_min][2*(n_atoms - 3)] numpy array of the sines and cosines of the 
        internal dihedral angles
        '''
        # Initially convert into 3D [n_min][3][n_atoms] numpy array of 
        # cartesian co-ordinates 
        self.ReshapeCartesianIn()
        self.ConvertDihedrals()
        self.shape = np.shape(self.config_space)
        
    #-------------------------------------------------------#
    # Functions that convert from cartesian co-ordinates to internal 
    # dihedral co-ordinates
        
    def ConvertDihedrals(self):
        ''' 
        
        '''
        dihedral_space = np.zeros([self.n_min, 2*(self.kw.n_atoms - 3)])
        
        for i in range(self.n_min):
            for j in range(self.kw.n_atoms - 3):
                
                atoms_vector = self.config_space[i, :,j:j+4]
                
                angles = self.CalcDihedrals(atoms_vector)
                dihedral_space[i, 2*j:2*(j +1)] = np.array(angles)
        self.config_space = dihedral_space
        
    

    def CalcDihedrals(self,atoms_vector):
        '''
        Calculates the dihedral angle for 4 atoms A B C D. Assumes 
        that atoms_vector is a [3][4] 2D narray with the cartesian co-ordinates 
        arranged as an array of 4 coloumn vectors
        '''
     
        # Calculate Difference vectors BA, BC, CB, CD
        BA = atoms_vector[:,0] - atoms_vector[:,1]
        BC = atoms_vector[:,2] - atoms_vector[:,1]
        CB = -BC
        CD = atoms_vector[:,3] - atoms_vector[:,2]
        
            
        # Calculate surface normals, U and V
        W = np.cross(BA,BC)
        V = np.cross(CB,CD) # This may need to be reversed, not sure!
        # Need to think of how to handle co-linear atoms <====== Ask Mark!
            
        if np.linalg.norm(W) < 0.01:
            print 'help!'
        if np.linalg.norm(V) < 0.01:
            print 'help too!'
            
                    
        # The cosine of the dihedral angle can be calculated easily
        cos_theta = (np.dot(W,V) / (np.linalg.norm(W)*np.linalg.norm(V)))
        
        # In order to calculate the sine of the dihedral, sin_theta, must
        # calculate U = V x CB = |CB|^2*CD - (CB.CD)*CB
        U = np.dot(CB,CB)*CD - np.dot(CB,CD)*CB
        
        
        sin_theta = -1.0*(np.dot(W,U) / (np.linalg.norm(W)*np.linalg.norm(U)))
        
        return cos_theta, sin_theta
        
    #-------------------------------------------------------#
    # Functions which remove global translational and rotational degrees of 
    # freedom
    def LstSqrStructureFit(self):
        '''
        Performing McLachlans-best-fit procedure to remove global rotational 
        and translational degrees of freedom, see 
        "Komatsuzaki et al. J. Chem. Phys. 122" 
        
        For method, (and explanation for uninformative variable names), 
        see "McLachlan, J. Mol. Biol, 128, 1979". 
        '''
    
        # Translate each structure so that Centre of Mass is at the origin
        print 'Translating Structure Centroids to Origin'
        self.CentreOfMass()
        print 'Done'
    
        # Calculate initial ensemble average 
        self.ensemble_average = np.mean(self.config_space, axis=0)
        for i in range(self.kw.iterations + 1):
        
            if i == (self.kw.iterations):
                sys.exit('ERROR: Rotational Invariance does not converge, norm of difference vector = %3.5f'%diff_ensemble_average)
            
            # Calculate a 1D [n_min] narray of the residue for each structure
            self.Residue()
        
        #In this loop, calculate and perform the rotation matrix for each 
        # structure in the ensemble relative to the ensemble mean
            for j in range(self.n_min): 

                self.structure = self.config_space[j,:,:]
                # Calculate matrix U <===== Need a more descriptive name
                self.U = np.dot(self.structure,self.ensemble_average.T)
            
                # Calculate SVD of U ========> U   =   H     D     KT
                #                            (3x3)   (3x3) (3x3)  (3x3)
                # H is a matrix composed of three column vectors h_i {i = 1,2,3}
                # KT is the transverse of matrix K, which is also composed of 
                # three column vectors k_i {i = 1,2,3}
                # D is a diagonal matrix , elements of which correspond to the 
                # singular values of U
                # numpy.linalg.svd returns D as a 1D array, i.e., D(i,i) = D[i]
            
                H, D, KT = np.linalg.svd(self.U)

                K = KT.T
                self.H, self.D, self.K = H, D, K
            
                # Calculate rotation matrix and rotate.
                self.Rotate()    

                self.config_space[j,:,:] = self.structure

            # Calculate ensemble average after iteration
            new_ensemble_average = np.mean(self.config_space, axis=0)

            # Calculate norm of difference between ensemble average before and 
            # after iteration
            diff_ensemble_average = np.linalg.norm(new_ensemble_average 
                                                   - self.ensemble_average)

            # If difference is below critical value, exit loop
            if diff_ensemble_average < self.kw.conv:
                print 'Average structure converged, norm of difference between ensemble averages after%i iterations: %3.8f'%(i+1, diff_ensemble_average)
                break
            self.ensemble_average = new_ensemble_average

            print 'Norm of difference between ensemble averages after %i iterations: %3.8f'%(i+1, diff_ensemble_average)
        
        self.config_space = self.config_space - self.ensemble_average

    def CentreOfMass(self):
        '''
        Calculate Centre of Mass for each structure in config_space, and set 
        to zero 
        '''
        for i in range(self.n_min):
            CoM = (1.0/self.kw.n_atoms)*(np.sum(self.config_space[i,:,:], 
                                             axis = 1))
            self.config_space[i,:,:] = (self.config_space[i,:,:] - 
                                        CoM.reshape([3,1]))
            
    def Residue(self):
        '''
        Calculate a 1D [n_min] narray of the residue for each structure
        '''
        self.structure_residue = np.zeros(self.n_min)
        atom_residue = np.zeros(self.kw.n_atoms)
    
        for i in range(self.n_min):
            for j in range(self.kw.n_atoms):
                diff = self.config_space[i,:,j] - self.ensemble_average[:,j]
                atom_residue[j] = np.dot(diff, diff)

        self.structure_residue[i] = 0.5*np.sum(atom_residue)
    
        
    def Rotate(self):
        ''' 
        Calculate rotation matrix according to selection rules, and act on 
        matrix structure 
        '''
    
        if (self.D[2] == 0):
            if (self.D[1] == 0):
                sys.exit('ERROR: All atoms are in a line')
            sys.exit('ERROR: All atoms are in a plane')
    
        # Caclulate determinant of U
        detU = np.linalg.det(self.U)
    
        K = np.matrix(self.K)
        H = np.matrix(self.H)
    
        if (detU > 0):
            rotation = np.dot(K[:,0],H[:,0].T) + np.dot(K[:,1],H[:,1].T) \
            + np.dot(K[:,2],H[:,2].T)
    
        if (detU < 0):
            if (self.D[1] == self.D[2]):
                sys.exit('ERROR: D[1] == D[2]')
            rotation = np.dot(K[:,0],H[:,0].T) + np.dot(K[:,1],H[:,1].T) \
            - np.dot(K[:,2],H[:,2].T)
    
    
        self.structure= np.dot(rotation, self.structure)
