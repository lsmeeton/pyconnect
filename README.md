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

`sudo apt-get install python-numpy python-matplotlib`

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

    python path/to/pyconnect/directory/pyconnect/pyconnect/rundisconnect.py

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


## Algorithmic Details ##
For the interested reader, a brief description of how PyConnect builds a disconnectivity graph from a database of minima and transitions states;

1. The minima and transition states contained in the database files are read and saved into memory.
2. The database is then pruned;
	1. Firstly, the minima and transition states which are *not* connected to a particular minimum (by default the global minimum) below the energy threshold `FIRST` are discarded from the database. 
	2. The database is checked to ensure that it is connected and that the stationary points are physically plausible (i.e. the transition state joining a pair of minima has a higher energy than both of them).


3. Once this database has been built, the minima can be organised into *basins*, a *basin* being all the minima which are connected below a particular energy level, and can contain either a single minimum, or a number of minima. The tree graph structure has now been determined, and the graph can be plotted.

4. For metric disconnectivity graphs, plotting the graph is fairly trivial. Nodes are positioned according to the mean value of the metric for each minima they contain. For disconnectivity graphs, positioning the nodes and edges can be a little trickier, as each node and edge must be placed for maximum clarity and to avoid edge-crossing. This is achieved as follows;
	1. The x-axis is divided into "columns", with a column for each minimum contained in the graph.
	2. Starting from the highest energy level and iterating through to the lowest;
		1. Nodes are assigned a column for each minima they contain
		2. Nodes are positioned below their parents, with the largest nodes positioned as close to the centre of their parents, and smaller nodes positioned progressively further out.
5. Once the positions of each node are determined, the graph can be plotted using matplotlib 
