from distutils.core import setup

setup(
    name='PyConnect',
    version='0.1.0',
    author='Lewis Smeeton',
    author_email='lcs137@bham.ac.uk',
    packages=['pyconnect', 
              'pyconnect.test',
              'pca'],
    scripts=['pyconnect/rundisconnect.py',
             'pca/PCArun.py',
             'pca/ConfigSpaceConvert.py']
    #url='http://pypi.python.org/pypi/Pyconnect/',
    #license='LICENSE.txt',
    #description='Useful towel-related stuff.',
    #long_description=open('README.txt').read(),
    #install_requires=[
    #    "Numpy >= 1.1.1",
    #    "MatPlotLib == 0.1.4",
    #    "Mayavi == 1.0"
    #],
)
