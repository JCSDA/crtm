"""
plot.py

Description:
============
Simple python script to plot the singular value
output from the K-Matrix_SVD example.

Copyright 2019 Patrick Stegmann

"""

# Import modules
import numpy as np
import matplotlib.pyplot as plt

## Load data

# Load singular values from file
val = np.loadtxt("SVD_result.txt")
# Load singular vectors
vec = np.loadtxt("Singular_vectors.txt")
# Load corresponding pressure profile
p = np.loadtxt("output_P.txt")

# Plot singular values as a function of pressure
# for channels 4 to 1.
for ii in range(5):
  plt.semilogy(vec[(ii-1),:],p,label=r'$\lambda_{n}$ = %f'%val[ii])

# Annotate plot
plt.gca().invert_yaxis()
plt.xlabel('Singular Vector')
plt.ylabel('Pressure [hPa]')
plt.legend()
plt.show()
