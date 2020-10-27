"""
plot.py

Description:
------------
Simple plotting script to plot the output of KMatrix_test.f90.

Copyright 2019 Patrick Stegmann

"""

# Import modules
import numpy as np
import matplotlib.pyplot as plt

## Load data

# Load the K-Matrix from file
wgt = np.loadtxt("output_K.txt")

# Load the pressure profile from file
p = np.loadtxt("output_P.txt")

## Plotting

# Initialize plot
plt.figure()

# Get the number of channels
n_chan = wgt.shape[0]
print n_chan

# Plot the weighting functions as the columns of the
# loaded K-Matrix
for ii in range(n_chan):
	plt.semilogy(wgt[ii,:],p,label="Channel %d"%ii)
 
 # Annotate plot
plt.title("CRTM ASMU-A METOP-A Weighting functions")
plt.ylabel("Pressure [hPa]")
plt.xlabel(r"Weighting function $\frac{\partial T_b}{\partial T}$")
plt.legend()
plt.gca().invert_yaxis()
plt.show()

