#!/usr/bin/env python

'''
Simple script which reads three files;
1. format_file: a .pdb file which contatins .pdb specific information required
   effective plotting in VMD.
2. pc_file: a .xyz file which contains the normed PC we are interested in 
   studying.
3. projection_file: a .xyz file which contains the structure we shall project 
   the PC onto.
'''
import numpy as np
import os
import sys

class Keyword():
    '''
    Object which reads keywords from file self.kw_file and stores them as 
    attributes.
    '''
    def __init__(self):
        
        self.input_file = "project_inpt"
        
        self.format_file = None
        self.pc_file = None
        self.base_file = None
        
#        self.base_pc = None # This is the value of the

        self.n_atoms = None
        
        self.pdb_output = None
        
        self.n_steps = None
        self.parm_min = None
        self.parm_max = None
        
        self.FileCheck(self.input_file)
        self.InputRead()
        
        self.InfoCheck()
        self.TypeCheck()
        
    def FileCheck(self, file_name):
        '''
        Checks for the existence of the file "file_name"
        '''
        if not os.path.exists(file_name):
            sys.exit('ERROR: Could not find file "%s"'%file_name)
            
    def InputRead(self):
        '''
        
        '''
        for line in open(self.input_file,'r'):
            self.LineRead(line)
            
    def LineRead(self, line):
        '''
        Processes the line passed to it from InputRead in self.input_file
        '''
        line = line.split()
        if (len(line) == 0
            or str(line[0]) == '#' 
            or str(line[0]).lower() == 'comment'): 
                return
        elif self.__dict__.has_key(str(line[0])):
            self.__dict__[str(line[0])] = line[1]
            
    def InfoCheck(self):
        '''
        Checks if all required information is present
        '''
        required_dict = self.__dict__.copy()
        missing_list = [] # List of missing keywords

        for attr, var in required_dict.iteritems():

            if not var: missing_list.append(attr)
        
        if missing_list:
            print 'WARNING: Following Keywords not found:\n', missing_list
            sys.exit()
            
    def TypeCheck(self):
        '''
        Ensure Attributes are correct type
        '''
        self.n_atoms = int(self.n_atoms)
        self.format_file = str(self.format_file)
        self.pc_file = str(self.pc_file)
        self.base_file = str(self.base_file)
        self.pdb_output = str(self.pdb_output)
        self.n_steps = int(self.n_steps)
        self.parm_min = float(self.parm_min)
        self.parm_max = float(self.parm_max)

class PC_project():
    '''
        
    '''
    def __init__(self,kw):
        self.kw = kw
        
        self.pdb_template = [] # Initialise empty list
        self.pdb = []
        
        # Check if three input files can be found
        self.kw.FileCheck(self.kw.format_file)
        self.kw.FileCheck(self.kw.pc_file)
        self.kw.FileCheck(self.kw.base_file)
        
        # Initialise xyz vectors
        self.pc_xyz = self.Initxyz()
        self.base_xyz = self.Initxyz()
        self.projection_xyz = self.Initxyz()
        
        # Initialise pdb output file name base
        self.pdb_output_file = ""
        
        # Read input files into relevant attributes
        self.PCFileRead()
        self.BaseFileRead()
        self.pdbFileRead()

        
        
    def Initxyz(self):
        '''
        
        '''
        xyz = np.zeros([self.kw.n_atoms, 3])
        
        return xyz
        
    def pdbFileRead(self):
        '''
        Reads .pdb file and saves it as a nested list.
        '''
        for line in open(self.kw.format_file):

            line = self.modsplit(line)#list(str(line).split())
            
            self.pdbLineRead(line)
#            print line, len(line)

    def pdbLineRead(self, line):
        '''
        Analysyses line to check content, and saves in appropriate format to
        self.pdb_template
        '''
#        if line[0] == "ATOM": 
        if line[0].find("ATOM") >= 0:
            line[10:15] = [],[],[]
#            for i in range(5,8): line[i] = []
            self.pdb_template.append(line)

    def modsplit(self,line):
        '''
        A modified version of split() which tokenises line into array but which
        contains the trailing whitespace of each word
        '''
        split_list = []
        split_list.append(line[0])
        for i in line[1:]:
            if i == '\n': break

            if i == ' ':
                if split_list[-1][-1] != ' ': split_list.append(i)
                else: split_list[-1] = split_list[-1] + i
            else: 
                if split_list[-1][-1] == ' ': split_list.append(i)
                else: split_list[-1] = split_list[-1] + i
#        print split_list
        return split_list
    
    def BaseFileRead(self):
        '''
        
        '''
        i = 0
        for line in open(self.kw.base_file):
            line = line.split()
            if len(line)==0 or i == self.kw.n_atoms: break

            try: self.base_xyz[i,:] = self.xyzLineRead(line)
            except IndexError:
                
                sys.exit("i = %d"%i)
            i += 1
            
    def PCFileRead(self):
        '''
        
        '''
        i = 0
        for line in open(self.kw.pc_file):
            line = line.split()
            if i <= 1: 

                i += 1
                continue #First two lines are header info
            if len(line)==0 or i == (self.kw.n_atoms + 2): 

                break

            try: self.pc_xyz[(i-2),:] = self.xyzLineRead(line)
            except IndexError:
                
                sys.exit("i = %d"%i)
            i += 1
        
    def xyzLineRead(self,line):
        '''
        
        '''

        self.xyzLineCheck(line)
        xyz = np.array([])
        for l in line:

            try: xyz = np.append(xyz, float(l))
            except IndexError:

                sys.exit('!!!%s'%type(l))
        return xyz
    
    def xyzLineCheck(self,line):
        '''
        Checks line is in expected format (currently only checks that line 
        contains only 3 numbers).
        '''
        if len(line) != 3:
            sys.exit('Was expecting 3 numbers per line, got %d'
                     %len(line))
        
        
    def Project(self,t):
        '''
        Projects self.pc_xyz onto self.base_xyz by parameter t and saves in 
        self.projection_xyz
        '''
        self.projection_xyz = self.base_xyz + t*self.pc_xyz
        
    def pdbMake(self):
        '''
        Generates a nested list based on self.pdb_template, and then fills in 
        co-ordinate info from self.projection_xyz
        '''
        self.pdb = self.pdb_template[:][:]
        i = 0
        for x, y, z in self.projection_xyz:

            self.pdb[i][10:14] = [x,y,z]
            i += 1
    
    def pdbWrite(self):
        '''
        Writes self.pdb to file "self.pdb_output" 
        '''
        i = 0
        f = open(self.pdb_output_file, 'w')
        for l in self.pdb:
#            print l[10:14]
            for c in l[0:9]:
                f.write(str(c))
            x, y, z = l[10:13]
            f.write('     {: 1.3f}  {: 1.3f}  {: 1.3f}'.format(x,y,z))
            for c in l[14:]:
                f.write(str(c))
            f.write('\n')
        f.close()
    
    def pdbGenerateName(self, base, index):
        '''
        Generates a string self.pdb_file_name which can be used as a filename
        of the form "base" + "_" + "index"
        '''
        self.pdb_output_file = str(base) + "_" + str(index) + ".pdb"
        
        
    def SetProjectionCoord(self):
        '''
        Performs a parameter sweep of self.kw.n_steps between self.kw.parm_min
        and self.kw.parm_max
        '''
        step_size = (self.kw.parm_max - self.kw.parm_min)/self.kw.n_steps
        t = self.kw.parm_min
        for i in range(self.kw.n_steps + 1):
            t += i*step_size
            self.Project(t)
            self.pdbGenerateName(self.kw.pdb_output, i)
            self.pdbMake()
            self.pdbWrite()
        
    def PCSinglePoint(self, structure):
        '''
        Calculates the the value of self.pc_xyz at structure
        '''
#        norm = (self.pc_xyz*self.pc_xyz).sum()
#        single_point = (self.pc_xyz*structure).sum()
#        single_point = single_point/norm
#        return single_point
        structure = np.ndarray.flatten(structure.T,order='C')
        print np.shape(structure), np.shape(np.dot(self.PCs,structure.T))[0]
        print np.dot(self.PCs.T,structure)[0]
        print np.dot(structure,self.PCs)[0]
        
        return np.dot(self.PCs,structure.T)[0]
    
    def ReadMatrix(self):
        '''
        A thrown together function to read co-ordinate matrix for projection
        purposes
        '''
        self.PCs = np.zeros([3*self.kw.n_atoms,3*self.kw.n_atoms])
#        f = open('self.PCsMatrix','r')
        i = 0
        j = 0
        for line in open('self.PCsMatrix','r'):
            line = line.split()
            for n in line:
#                print i, j, np.shape(self.PCs)
                self.PCs[i,j] = float(n)
                j += 1
            j = 0
            i += 1


if __name__ == '__main__':
    kw = Keyword()
    pc = PC_project(kw)
#    print pc.pdb_template
#    print pc.pc_xyz
#    print pc.projection_xyz
#    pc.Project(0.0)
#    pc.pdbMake()
#    pc.pdb_output_file = pc.kw.pdb_output
    pc.SetProjectionCoord()
#    pc.ReadMatrix()
#    a = pc.PCSinglePoint(pc.base_xyz)
#    b = pc.PCSinglePoint(pc.pc_xyz)
#    c = (pc.base_xyz*pc.base_xyz).sum()
#    print a, b, c
#    print np.shape(pc.base_xyz)
#    print pc.PCs[0,1:3]
#    pc.pdbWrite()
#    print 'hello\n',pc.pdb_template
#    print np.shape(pc.pdb)
#    print np.shape(pc.pdb_template)