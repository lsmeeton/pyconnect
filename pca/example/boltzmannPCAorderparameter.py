#!/usr/bin/env python

#LEWIS SMEETON, 2012
# Script to generate order parameter based upon first PC calculated using PCA method in matplotlib.
# Includes module 'structure_read', which is created by compiling 'structure_read.f90' using
# "f2py -c --fcompiler=gfortran -m structure_read strucutre_read.f90".

import sys
import os.path
import numpy as np
import array
import lewismod
import structure_read
import matplotlib.mlab as mlab

class MyPCAPrep:
    def __init__(self, config_space, n_atoms, n_min, basis='cartesian', iterations = 100, conv = 10E-7):
                
        self.basis = basis
        self.config_space = config_space
        self.n_atoms = n_atoms
        self.n_min = n_min
        self.iterations = iterations
        self.conv = conv
        
        
        # Check basis set input correctly
        self.BasisCheck()
        # Check config_space is correct size and shape
        self.ShapeCheck()

        # Convert config_space to selected basis set
        if self.basis == 'cartesian':
            self.ReshapeCartesianIn()
            self.LstSqrStructureFit()
            self.ReshapeCartesianOut()

        if self.basis == 'dihedral':
            print 'Converting to Internal Dihedrals'
            self.ReshapeDihedral()
            print 'Done'
    #========================================================#
    # FUNCTIONS
    
    #--------------------------------------------------------#
    # Functions ensuring input is acceptable 
    
    def BasisCheck(self):
        if self.basis != 'cartesian':
            if self.basis != 'dihedral':
                raise RuntimeError('basis set to be transformed to must either be "cartesian" or "dihedral"')
        
        
    def ShapeCheck(self):#, config_space):
        self.shape = np.shape(self.config_space)
        if self.shape != (3*self.n_atoms*self.n_min,):
            raise RuntimeError('Warning: config_space has shape', self.shape,' expected to have shape (3*',self.n_atoms,'*',self.n_min,') = ',(3*self.n_atoms*self.n_min,))

    #--------------------------------------------------------#
    # Functions that reshape config_space array

    def ReshapeCartesianIn(self):
        '''Converts config_space from a 1D [n_min*n_atoms*3] array to 
        a 3D [n_min][3][n_atoms] numpy array of cartesian co-ordinates'''
        self.config_space = self.config_space.reshape([self.n_min, self.n_atoms, 3])
        self.config_space = np.swapaxes(self.config_space,1,2)

    def ReshapeCartesianOut(self):
        '''Converts config_space from a 3D [n_min][3][n_atoms] numpy array to a 
        [n_min][3*n_atoms] numpy array'''
        self.config_space = np.swapaxes(self.config_space,1,2)
        self.config_space = self.config_space.flatten()
        self.config_space = self.config_space.reshape(self.n_min,3*self.n_atoms)

        
    def ReshapeDihedral(self):
        '''Converts config_space from a 1D [n_min*n_atoms*3] array to 
        a 2D [n_min][2*(n_atoms - 3)] numpy array of the sines and cosines of the 
        internal dihedral angles'''
        # Initially convert into 3D [n_min][3][n_atoms] numpy array of cartesian co-ordinates 
        self.ReshapeCartesianIn()
        self.ConvertDihedrals()

        
    #-------------------------------------------------------#
    # Functions that convert from cartesian co-ordinates to internal dihedral co-ordinates
        
    def ConvertDihedrals(self):
        ''' '''
        dihedral_space = np.zeros([self.n_min, 2*(self.n_atoms - 3)])
        
        for i in range(self.n_min):
            for j in range(self.n_atoms - 3):
                
                atoms_vector = self.config_space[i, :,j:j+4]
                angles = self.CalcDihedrals(atoms_vector)
                dihedral_space[i, 2*j:2*(j +1)] = np.array(angles)
        self.config_space = dihedral_space
        
    

    def CalcDihedrals(self,atoms_vector):
        '''Calculates the dihedral angle for 4 atoms A B C D. Assumes that atoms_vector is 
        a [3][4] 2D narray with the cartesian co-ordinates arranged as an array of 4 
        coloumn vectors'''
        
            
        
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
            
        # The dihedral angle, theta, can then simply be calculated
        #theta = np.arccos(np.dot(W,V) / (np.linalg.norm(W)*np.linalg.norm(V)))
            
        # The cosine and sine of the dihedral angle can be calculated easily
        cos_theta = (np.dot(W,V) / (np.linalg.norm(W)*np.linalg.norm(V)))
        sin_theta = (np.dot(W,CD) / (np.linalg.norm(W)*np.linalg.norm(CD)))
        
        return cos_theta, sin_theta
        
    #-------------------------------------------------------#
    # Functions which remove global translational and rotational degress of freedom
    def LstSqrStructureFit(self):
        '''Performing McLachlans-best-fit procedure to remove global rotational and translational degrees of freedom,
        see "Komatsuzaki et al. J. Chem. Phys. 122" 
        
        For method, (and explanation for uninformative variable names), see "McLachlan, J. Mol. Biol, 128, 1979". '''
    
        # Translate each structure so that Centre of Mass is at the origin
        print 'Translating Structure Centroids to Origin'
        self.CentreOfMass()
        print 'Done'
    
        # Calculate initial ensemble average 
        self.ensemble_average = np.mean(self.config_space, axis=0)
        for i in range(self.iterations + 1):
        
            if i == (self.iterations):
                sys.exit('ERROR: Rotational Invariance does not converge, norm of difference vector = %3.5f'%diff_ensemble_average)
            
            # Calculate a 1D [n_min] narray of the residue for each structure
            self.Residue()
        
        #In this loop, calculate and perform the rotation matrix for each structure in the ensemble relative to the ensemble mean
            for j in range(self.n_min): 

                self.structure = self.config_space[j,:,:]
                # Calculate matrix U <===== Need a more descriptive name
                self.U = np.dot(self.structure,self.ensemble_average.T)
            
                # Calculate SVD of U ========> U   =   H     D     KT
                #                            (3x3)   (3x3) (3x3)  (3x3)
                # H is a matrix composed of three column vectors h_i {i = 1,2,3}
                # KT is the transverse of matrix K, which is also composed of three column vectors k_i {i = 1,2,3}
                # D is a diagonal matrix , elements of which correspond to the singular values of U
                # numpy.linalg.svd returns D as a 1D array, i.e., D(i,i) = D[i]
            
                H, D, KT = np.linalg.svd(self.U)

                K = KT.T
                self.H, self.D, self.K = H, D, K
            
                # Calculate rotation matrix and rotate.
                self.Rotate()    

                self.config_space[j,:,:] = self.structure

            # Calculate ensemble average after iteration
            new_ensemble_average = np.mean(self.config_space, axis=0)

            # Calculate norm of difference between ensemble average before and after iteration
            diff_ensemble_average = np.linalg.norm(new_ensemble_average - self.ensemble_average)

            # If difference is below critical value, exit loop
            if diff_ensemble_average < self.conv:
                print 'Average structure converged, norm of difference between ensemble averages after%i iterations: %3.8f'%(i+1, diff_ensemble_average)
                break
            self.ensemble_average = new_ensemble_average

            print 'Norm of difference between ensemble averages after %i iterations: %3.8f'%(i+1, diff_ensemble_average)
        
        self.config_space = self.config_space - self.ensemble_average

    def CentreOfMass(self):
        '''Calculate Centre of Mass for each structure in config_space, and set to zero '''
        for i in range(self.n_min):
            CoM = (1.0/self.n_atoms)*(np.sum(self.config_space[i,:,:], axis = 1))
            self.config_space[i,:,:] = self.config_space[i,:,:] - CoM.reshape([3,1])
            
    def Residue(self):
        '''Calculate a 1D [n_min] narray of the residue for each structure'''
        self.structure_residue = np.zeros(self.n_min)
        atom_residue = np.zeros(self.n_atoms)
    
        for i in range(self.n_min):
            for j in range(self.n_atoms):
                diff = self.config_space[i,:,j] - self.ensemble_average[:,j]
                atom_residue[j] = np.dot(diff, diff)

        self.structure_residue[i] = 0.5*np.sum(atom_residue)
    
        
    def Rotate(self):
        ''' Calculate rotation matrix according to selection rules, and act on matrix structure '''
    
        if (self.D[2] == 0):
            if (self.D[1] == 0):
                sys.exit('ERROR: All atoms are in a line')
            sys.exit('ERROR: All atoms are in a plane')
    
        # Caclulate determinant of U
        detU = np.linalg.det(self.U)
    
        K = np.matrix(self.K)
        H = np.matrix(self.H)
    
        if (detU > 0):
            rotation = np.dot(K[:,0],H[:,0].T) + np.dot(K[:,1],H[:,1].T) + np.dot(K[:,2],H[:,2].T)
    
        if (detU < 0):
            if (self.D[1] == self.D[2]):
                sys.exit('ERROR: D[1] == D[2]')
            rotation = np.dot(K[:,0],H[:,0].T) + np.dot(K[:,1],H[:,1].T) - np.dot(K[:,2],H[:,2].T)
    
    
        self.structure= np.dot(rotation, self.structure)

#===========================================================#
class PCA:
    def __init__(self, a, b):
        """
        compute the SVD of a and store data for PCA.  Use project to
        project the data onto a reduced set of dimensions

        Inputs:

          *a*: a numobservations x numdims array
          
          *b*: array containing the boltzmann weighting of each structure
    
        Attrs:

          *a* a centered unit sigma version of input a

          *c* a centred, weighted version of input a

          *numrows*, *numcols*: the dimensions of a

          *mu* : a numdims array of means of a

          *sigma* : a numdims array of atandard deviation of a

          *fracs* : the proportion of variance of each of the principal components

          *Wt* : the weight vector for projecting a numdims point or array into PCA space

          *Y* : a projected into PCA space


        The factor loadings are in the Wt factor, ie the factor
        loadings for the 1st principal component are given by Wt[0]

        """
        n, m = a.shape
        if n<m:
            raise RuntimeError('we assume data in a is organized with numrows>numcols')

        self.numrows, self.numcols = n, m
        print 'Number of rows: %d, Number of Columns: %d'%(self.numrows,self.numcols)
        
        #self.mu = a.mean(axis=0) #---> This is no longer applicable for a weighted array
        # Calculate weighted mean
        mean = np.zeros(self.numcols)
        
        for i in range(self.numrows):
            mean = mean + b[i]*a[i,:]
            
        mean = mean / sum(b)
        
        self.mu = mean
        #self.sigma = sigma
        #self.sigma = a.std(axis=0)# ---> Neither is this
        std = np.zeros(self.numcols)
        for i in range(self.numrows):
            std = std + b[i]*((a[i,:] - mean)**2) 
        std = np.sqrt(std / sum(b))
        self.sigma = std
        
        a = self.center(a)
        

        # Create the Covariance matrix, cov
        # First, initialise cov
        cov = np.zeros([self.numcols,self.numcols])
        # Then add to cov the vectors in a, multiplied by their corresponding weights from b
        for i in range(self.numrows):
            cov = cov + b[i]*np.dot(np.matrix(a[i,:]).T,np.matrix(a[i,:]))
        # Centre c on its mean, and scale by its standard deviation
        #c = self.center(c)
        self.cov = cov
        
        
        # Calculate the eigenvalues temp_variance, and eigenvalues temp_PCs, of cov
        # Note, temp_* is because vectors are ordered in c differently than they are in the inbuilt PCA solver
        # Therefore, for the sake of consistency with previously written code, one must re-organise temp_PCs
        #
        #  (||||)    (---1st PC---)
        #  (|"21) ==>(---2nd PC---)
        #  (|"PP)    (---"    "---)
        #  (||||)    (---LastPC---)
        #
        # They also point in opposite direction, so we have to multiply by -1.0
        temp_variance, temp_PCs = np.linalg.eigh(cov)
        # Normalise variance
        temp_variance = temp_variance/sum(temp_variance)
        # Order temp_variance from largest ---> smallest
        PC_index_order = np.argsort(temp_variance[::-1]) 
        # Initialise variance vector
        variance = temp_variance[PC_index_order[0]]
        # Initialise PCs vector
        PCs = temp_PCs[:,PC_index_order[0]].reshape(1,self.numcols)
        for i in range( 1, self.numcols):
            
            variance = np.append(variance,temp_variance[PC_index_order[i]])
            PCs = np.vstack((PCs, temp_PCs[:,PC_index_order[i]].reshape(1,self.numcols)))
            
        PCs = -1.0*PCs
        print "eigenvalues calculated"
        
        self.a = a

        U, s, Vh = np.linalg.svd(a, full_matrices=False)
        
        
#Uprime, sprime, Vhprime = np.linalg.svd(b, full_matrices=False)
        #print "Shapes of b, U, s, Vh:", b.shape, U.shape, s.shape, Vh.shape
        #print s, sprime
        Y = np.dot(Vh, a.T).T
        
        X = np.dot(PCs, a.T).T
        #print "Shape of Y: ", X.shape
        #print "Y - X", np.mean(Y - X, axis=0), np.std(Y - X, axis=0), np.sum(Y - X, axis=0)
        
        #print "Vh - PCs", np.mean(Vh - PCs, axis=1).T, np.std(Vh - PCs, axis=1).T, np.sum(Vh - PCs, axis=1).T
        vars = s**2/float(len(s))
        #print "vars - variance", np.mean(vars - variance, axis=0), np.std(vars - variance, axis=0), np.sum(vars - variance, axis=0)
        
        vars = variance
        self.fracs = vars/vars.sum()
        #self.fracs = variance/variance.sum()
        
        #for i in range(self.numcols):
        #    print "self.fracs, variance", self.fracs[i], variance[i]
        
        f = open(r'PCs','w')
        for i in range(self.numcols):
            f.write('%2.3f'%PCs[0,i] + '\t' + '%2.3f\t'%Vh[0,i] + '%2.3f\t'%(Vh[0,i] + PCs[0,i]) + '%2.3f\t%2.3f\t%2.3f\t%2.3f\n'%(self.mu[i],mean[i],self.sigma[i],std[i]))
        f.close()
        
        #self.Wt = Vh
        self.Wt = PCs
        self.Y = X
        #f = open(r'eigenvectors_of_c', 'w')
        #print "shape of Wt, v", self.Wt.shape, v.shape
        #for i in range(self.numcols):
        #    f.write(str(v[i,-1]) + '\t' + str(self.Wt[0,i]) +str(v[i,-2]) + '\t' + str(self.Wt[1,i]) + '\n')
        #f.close()
        
        

    def project(self, x, minfrac=0.):
        'project x onto the principle axes, dropping any axes where fraction of variance<minfrac'
        x = np.asarray(x)

        ndims = len(x.shape)

        if (x.shape[-1]!=self.numcols):
            raise ValueError('Expected an array with dims[-1]==%d'%self.numcols)


        Y = np.dot(self.Wt, self.center(x).T).T
        mask = self.fracs>=minfrac
        if ndims==2:
            Yreduced = Y[:,mask]
        else:
            Yreduced = Y[mask]
        return Yreduced



    def center(self, x):
        'center the data using the mean and sigma from training set a'
        return (x - self.mu)/self.sigma



    @staticmethod
    def _get_colinear():
        c0 = np.array([
            0.19294738,  0.6202667 ,  0.45962655,  0.07608613,  0.135818  ,
            0.83580842,  0.07218851,  0.48318321,  0.84472463,  0.18348462,
            0.81585306,  0.96923926,  0.12835919,  0.35075355,  0.15807861,
            0.837437  ,  0.10824303,  0.1723387 ,  0.43926494,  0.83705486])

        c1 = np.array([
            -1.17705601, -0.513883  , -0.26614584,  0.88067144,  1.00474954,
            -1.1616545 ,  0.0266109 ,  0.38227157,  1.80489433,  0.21472396,
            -1.41920399, -2.08158544, -0.10559009,  1.68999268,  0.34847107,
            -0.4685737 ,  1.23980423, -0.14638744, -0.35907697,  0.22442616])

        c2 = c0 + 2*c1
        c3 = -3*c0 + 4*c1
        a = np.array([c3, c0, c1, c2]).T
        return a

#===========================================================#


def WritePCAToFile(PCA, Q=1, Normal=True):
    projection = PCA[:,Q - 1]
        
    if Normal:
        projection_max = max(projection)
        projection_min = min(projection)
        projection = (projection - float(projection_min))/(float(projection_max) - float(projection_min))

        projection_print = np.array([-1.0]*total_min)
        for i in range(len(min_index)):
            projection_print[min_index[i] - 1] = projection[i]
        
    
        f = open(r'PCA_order_parameter_normal' + str(Q), 'w')
        
        for i in range(len(projection_print)):
            
            f.write('%1.3f'%projection_print[i] + '\n')
        f.close()
    else:
        f = open(r'PCA_order_parameter' + str(Q), 'w')
        for i in range(len(projection)):
            f.write('%1.3f'%projection[i] + '\t' + '%i'%min_index[i] + '\n')
        f.close()


#===========================================================#

if __name__ == '__main__':
    conv = 10E-7 #Convergence criteria for rotational invariance procedure.
    iterations = 100 # If rotational invariance procedure doesn't converge within this many iterations, stop calculation.
    min_file = 'minimaindex' # Name of file which contains list of indices of minima to be included in PCA calc.
    n_atoms = 46 # No. of atoms in system
    #basis = 'dihedral'
    basis = 'cartesian'
    beta = 100.0
    lewismod.file_check('points.min')
    lewismod.file_check('min.data')
    lewismod.file_check(min_file)

    total_min = lewismod.count_lines('min.data')

    print 'No. of minima:%i'%total_min

    print 'PCA will be performed on the', basis, 'basis set'
    


    #read numbers from file into list min_index
    min_index = []
    for line in open(min_file,'r'):
        min_index.append(int(line))
    n_min = len(min_index)
    print 'List of indices of minima to be included in PCA calculation read in from file: %s'%min_file
    
    print 'Structures will be boltzmann weighted according to e^(-beta*dE) where beta =%3.2f and dE is energy of minima relative to the global minimum'%beta
    # read energies from file min.data
    min_energies = [0]
    for line in open('min.data'):
        min_energies.append(float(line.split()[0]))

    global_min_energy = min(min_energies)
    global_min_index = min_energies.index(global_min_energy)

    print 'Global minimum: %d\tEnergy = %2.4f'%(global_min_index, global_min_energy)
    
    weight_array = np.zeros(n_min)
    
    for i in range(n_min):
        j = min_index[i] # BAD style, but will have to do
        weight_array[i] = min_energies[j]
        # Calculare weights for each structure, and store in array
    
    weight_array = weight_array - global_min_energy
    
    weight_array = np.exp(weight_array*beta*(-1.0))
    
    
    
    print 'Extracting structural information for %i minima included in PCA calculation'%len(min_index)
    config_space = np.array([])
    for i in range(n_min):
        rec = min_index[i]
        config_space = np.append(config_space, structure_read.structure_read(rec, n_atoms))
    print 'Extraction complete'    


    
    PCAprep = MyPCAPrep(config_space, n_atoms, n_min, basis)
    config_space = PCAprep.config_space
    
    PCA_results =PCA(config_space, weight_array) 
    
    # Write Principal Components to file
    WritePCAToFile(PCA_results.Y, Q=1,Normal=True)
    WritePCAToFile(PCA_results.Y, Q=1,Normal=False)
    WritePCAToFile(PCA_results.Y, Q=2,Normal=True)
    WritePCAToFile(PCA_results.Y, Q=2,Normal=False)


    # Write fraction of variance of each principal component to file                                                                                                                         
    cum_fracs = np.cumsum(PCA_results.fracs) # Cumalitive sum of the fraction of variance of each PCA coord                                                                                  
    f = open(r'PCA_coords', 'w')
    f.write('No. of Atoms: %i,\t No. of Structures in min.data: %i,\t No. of Structures used to Calculate PCA: %i\n'%(n_atoms, total_min, len(min_index)))
    for i in range(np.shape(config_space)[1]):
        f.write('PCA No. %i,\t mean = %4.6g,\t std dev = %4.6g,\t frac = %1.6g,\t cum. frac = %1.6g\n'%(i+1, PCA_results.mu[i], PCA_results.sigma[i], PCA_results.fracs[i], cum_fracs[i]))
    f.close()

    #print np.shape(PCA_results.Wt)
    # Write factor loadings to file                                                                                                                                                     
    
    if basis == "cartesian":   
        f = open(r'PCA_factorloadings', 'w')
        f.write('No. of Atoms: %i,\t No. of Structures in min.data: %i,\t No. of Structures used to Calculate PCA: %i\n'%(n_atoms, total_min, len(min_index)))
        for i in range(np.shape(config_space)[1]):
            f.write('PCA No. %i\n'%(i+1))
            for j in range(0, np.shape(config_space)[1], 3):
                f.write('% 2.4f\t\t% 2.4f\t\t% 2.4f\n'%(PCA_results.Wt[i,j],PCA_results.Wt[i,j+1],PCA_results.Wt[i,j+2]))
            f.write('\n')
        f.close()
