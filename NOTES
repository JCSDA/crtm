Non-answer-changing mods:
o Remove obsolete VERSION_ID settings
o Get around various compiler complaints about
  "may be used uninitialized" and "unused variable"
o Printout precision format changed from e13.6 to e22.15
o ODPS_CoordinateMapping.f90: Compiler bug workaround for ifort 17 for when
    trapping SIGFPE is enabled
o Addition to Options component to skip the profile
o CRTM_CloudCover_Define.f90: Bugfix for e.g. MPAS when n_Layers changes
o NESDIS_MHS_SICEEM_Module.f90: Bugfix for log(negative number)

Answer-changing mods:
o OpenMP, ONLY because OpenMP fixed a bug whereby GeometryInfo used settings from one profile
  in the next
o CRTM_MW_Ice_SfcOptics.f90, CRTM_MW_Snow_SfcOptics.f90: From A. Collard:
  This, together with the changes in the UFO branch crtm_subset_channels2,
  allows channel-subsetting of microwave radiances. This simply ensures that
  the correct channels are passing to the NESDIS Ice and Snow emissivity
  subroutines when only a subset of channels is available.
o CRTM_Surface_Define.f90: Change TOLERANCE from 1.0e-10 to 1.0e-6
  "modified tolerance in CRTM_Surface_Define.f90 to allow ctest to pass"
  