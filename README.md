# PyConnect: Software for Visualising Energy Landscapes #

Source code: https://github.com/lsmeeton/pyconnect

## Description ##
Software for producing disconnectivity and metric disconnectivity graphs, and for calculating the principal components of chemical systems.


## Installation ##
PyConnect needs the matplotlib and numpy packages.

1. `numpy`:

       numpy is used for numerical work. It also installs f2py which
       is used to compile fortran code into modules callable by python.

2. `matplotlib`:

       matplotlib is a plotting library which is used to plot the disconnectivity and metric disconnectivity graphs.

**Installation Prerequisites**

In Ubuntu

- `sudo apt-get install python-numpy python-matplotlib`

In OpenSUSE, install the following:

`python-numpy`

`python-numpy-devel`

`python-matplotlib`

`python-matplotlib-tk`

`python-matplotlib-wx`

Windows

numpy and matplotlib can be installed using a python package manager such as the enthought canopy package manager https://www.enthought.com/downloads/

**Installation**

Users can install PyConnect using;

    python setup.py install

## Running PyConnect ##
PyConnect can be run using 

    python path/to/pyconnect/directory/pyconnect/disconnect/rundisconnect.py

the directory in which this is run must contain a `dinfo` parameter file, which instructs PyConnect how to build a particular disconnectivity or metric disconnectivity graph.

The `dinfo` file can contain a number of keywords, five of which are compulsory;

1. `MINIMA <data_file>`
        
	Specifies filename for minima info.
	
	File Format: Data organised into coloumns
	
	Line Number: Minima index
	
	First Coloumn: Energy of minima

2. `TS <data_file>`
	
	Specifies filename for transition state info.

	File Format: Data organised into coloumns
	
	Line Number: Transition state index
	
	First Coloumn: Energy of transition state

	Fourth Coloumn:  Index of minimum1

	Fifth Coloumn: Index of minimum2

3. `DELTA <dE>`
	
	Energetic separation of levels in basin analysis.

4. `FIRST <E1>`
	
	Specifies the energy of the highest level on the energy axis.

5. `LEVELS <n>`

	The number of levels at which to perorm the basin analysis.


In order to plot a metric disconnectivity graph the following keywords must be used;

1.  `METRIC <data_file>`
	
	Specifies filename for metric info for producing 2d metric disconnectivity graphs

	File Format: Data organised into two coloumns

	First Coloumn: Minima index

	Second Coloumn: Metric value of minima

	Lines beginning with `#` will be ignored

2. `METRIC3D <data_file1> <data_file2>`
	
	Specifies filenames for metric info for producing 3d metric disconnectivity graphs

	File format: same as for `METRIC` keyword, both files must conform to file format
