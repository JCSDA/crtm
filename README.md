CRTM REL-2.4.0
==============

Preamble
--------

CRTM v2.4.0 alpha release (`REL-2.4.0-alpha`)

Created on October 7, 2020

This is a fully functional release of CRTM v2.4.0.

"Alpha" status indicates that this release has not been fully tested, and some minor work remains.



Contents
========

1. Configuration
  a. The easy (I hope) way
  b. The more flexible way
2. Building the library
3. Testing the library
4. Installing the library
  a. GNU Install
      - Linking to the library
  b. Uninstalling the library
5. Cleaning up
6. Feedback and contact info



Configuration, building, and testing the library
================================================	
JCSDA CRTM v2.4.x Build Instructions

- Development Repository Build
- Note: the development repository build differs from a release build. 
	
The CRTM development directory structure looks like

```
 .
  ├── LICENSE
  ├── NOTES
  ├── README.md
  ├── Set_CRTM_Environment.sh
  ├── configuration/
  ├── documentation/
  ├── fix/
  │   ├── AerosolCoeff/
  │   ├── CloudCoeff/
  │   ├── EmisCoeff/
  │   ├── SpcCoeff/
  │   └── TauCoeff/
  ├── scripts/
  │   ├── idl/
  │   ├── ruby/
  │   └── shell/
  ├── src/
  │   ├── Ancillary/
  │   ├── AntennaCorrection/
  │   ├── AtmAbsorption/
  │   ├── AtmOptics/
  │   ├── AtmScatter/
  │   ├── Atmosphere/
  │   ├── Build/
  │   ├── CRTM_Utility/
  │   ├── ChannelInfo/
  │   ├── Coefficients/
  │   ├── GeometryInfo/
  │   ├── InstrumentInfo/
  │   ├── Interpolation/
  │   ├── NLTE/
  │   ├── Options/
  │   ├── RTSolution/
  │   ├── SensorInfo/
  │   ├── SfcOptics/
  │   ├── Source_Functions/
  │   ├── Statistics/
  │   ├── Surface/
  │   ├── TauProd/
  │   ├── TauRegress/
  │   ├── Test_Utility/
  │   ├── User_Code/
  │   ├── Utility/
  │   ├── Validation/
  │   ├── Zeeman/
  └── test/
      └── Main/
```

Feedback and Contact Information

CRTM SUPPORT EMAIL: crtm-support@googlegroups.com OR visit https://forums.jcsda.org/

```
If you have problems building the library please include the
generated "config.log" file in your email correspondence.
```





