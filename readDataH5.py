import pandas as pd
from pandas import HDFStore,DataFrame,read_hdf

# Read the .txt file
big_data = pd.read_csv("../../Data/PHD_Matrix.txt",sep=" ",header=None)

big_datat = big_data.T
big_data1 =  big_datat.drop(big_datat.index[187679])# big_datat[:-187680]

# Create (or open) an hdf5 file and opens in append mode
#import PyTables
hdf = HDFStore("/Users/Yujiao/Desktop/PHD_MatrixDist.h5")

# Put the dataset in the storage
hdf.put('matrix', big_data1)

# Read the .h5 file as 'matrix'
matrix = pd.read_hdf('/Users/Yujiao/Desktop/PHD_MatrixDist.h5', 'matrix')

# Check if we can see top rows of matrix
matrix.head()
matrix.shape