# Description
This is a collection of tutorial cases for the Community Radiative Transfer Model (CRTM).
There is a separate folder for each tutorial case:
- `K-Matrix_test` : How to compute instrument weighting functions with the CRTM.
- `K-Matrix_SVD` : How to use the CRTM K-Matrix in other applications.
- `NakajimaKing` : A bispectral retrieval example for cloud optical thickness and effective radius.

# Getting Started

## Requirements
This tutorial requires a linked version of the CRTM library and a LAPACK version.
The code has been tested with CRTM REL-2.1.3 to 2.3.0.
A Python installation with NumPy and Matplotlib modules is required for the plotting scripts.

## Getting the Code
You may either download the code or clone the entire repository with the following command:
```shell
git clone https://github.com/StegmannJCSDA/CRTM_tutorial
```

## Compiling the Code
1. Enter the corresponding case folder.
2. Type  `make`.

## Using the Code
A detailed description on how to run each tutorial can be found in the corresponding case folder.

# Developer Notes
If you plan to contribute to the repository, please create a `feature` branch from `develop` or a `bugfix` branch from `hotfix`.

# LICENSE:
Please see the file `LICENSE.txt` for details.

# References
[1] Stegmann, P., E. Liu, and B. Johnson (2019): *Applying the Community Radiative Transfer Model to Remote Sensing and Data Assimilation.* AGU Fall Meeting 2019 (Advances in Remote Sensing Inversion), Boston MA.

[2] Rodgers, C. D. (2000): *Inverse Methods for Atmospheric Sounding: Theory and Practice*. World Scientific Singapore.

[3] Nakajima T., and M. D. King (1990): *Determination of Optical Thickness and Effective Particle Radius of Clouds from Reflected Solar Radiation Measurements. Part I: Theory.* JAS 47(15), 1878-1893.
