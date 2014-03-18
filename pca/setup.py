from distutils.core import setup

setup(
    name='PCA',
    version='0.1.0',
    author='Lewis Smeeton',
    author_email='lcs137@bham.ac.uk',
    packages=['pca'],# 'pca.test'],
    scripts=['PCArun.py','ConfigSpaceConvert.py'],#,'bin/wash-towels.py'],
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
