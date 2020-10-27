"""
grip.py

Description:
------------
Python script to plot a bi-spectral mapping from radiance
space (MODIS Ch. 2 & 7) to cloud physical space (IWP and
effective radius).

Copyright 2019 Patrick Stegmann

"""

# Import modules
import numpy as np
import matplotlib.pyplot as plt

# Define function to plot a deformed grid
def plot_grid(ax, gridx,gridy, **kwargs):
    for i in range(gridx.shape[0]):
        plt.plot(gridx[i,:], gridy[i,:], **kwargs)
    for i in range(gridx.shape[1]):
        plt.plot(gridx[:,i], gridy[:,i], **kwargs)

# Load channel radiance data from file
rad_data = np.loadtxt("output.txt")
ch2 = rad_data[:,2].reshape((10,10))
ch7 = rad_data[:,3].reshape((10,10))

# Initialize plot
fig, ax = plt.subplots()

# Plot radiance mapping
plot_grid(ax,ch2, ch7, color="C0")
plt.grid()
plt.minorticks_on()

# Annotate plot
plt.title('Nakajima-King Diagram MODIS Aqua (Snow particle)')
plt.xlabel(r'Ch. 2 Radiance $[mW/(m^2 \cdot sr \cdot cm^{-1})]$')
plt.ylabel(r'Ch. 7 Radiance $[mW/(m^2 \cdot sr \cdot cm^{-1})]$')

# Create annotation arrows
ax.annotate(r'IWP $[kg/m^2]$', xy=(5.8, 1.25), xytext=(2.8, 0.8),
            arrowprops=dict(facecolor='C0', shrink=0.01),
            )

ax.annotate(r'$R_{eff}$ $[\mu m]$', xy=(6.2, 1.3), xytext=(8.2, 2.25),
            arrowprops=dict(facecolor='C0', shrink=0.01),
            )

plt.show()

# EOF
