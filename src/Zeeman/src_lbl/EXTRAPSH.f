CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE extrapsh (date, dte1, nmax1, gh1, nmax2,
     1 gh2, nmax, gh)
c ===============================================================
c
c       version 1.01
c
c       extrapolates linearly a spherical harmonic model with a
c       rate-of-change model.
c
c       input:
c           date  - date of resulting model (in decimal year)
c           dte1  - date of base model
c           nmax1 - maximum degree and order of base model
c           gh1   - schmidt quasi-normal internal spherical
c                   harmonic coefficients of base model
c           nmax2 - maximum degree and order of rate-of-change
c                   model
c           gh2   - schmidt quasi-normal internal spherical
c                   harmonic coefficients of rate-of-change model
c
c       output:
c           gh    - coefficients of resulting model
c           nmax  - maximum degree and order of resulting model
c
c       a. zunde
c       usgs, ms 964, box 25046 federal center, denver, co  80225
c
c ===============================================================
        dimension       gh1(*), gh2(*), gh(*)
c ---------------------------------------------------------------
c       the coefficients (gh) of the resulting model, at date
c       date, are computed by linearly extrapolating the coef-
c       ficients of the base model (gh1), at date dte1, using
c       those of the rate-of-change model (gh2), at date dte2. if
c       one model is smaller than the other, the extrapolation is
c       performed with the missing coefficients assumed to be 0.
c ---------------------------------------------------------------
        factor = (date - dte1)
        if (nmax1 .eq. nmax2) then
            k = nmax1 * (nmax1 + 2)
            nmax = nmax1
        else if (nmax1 .gt. nmax2) then
            k = nmax2 * (nmax2 + 2)
            l = nmax1 * (nmax1 + 2)
            do 5 i = k + 1, l
                gh(i) = gh1(i)
   5        continue
            nmax = nmax1
        else
            k = nmax1 * (nmax1 + 2)
            l = nmax2 * (nmax2 + 2)
            do 10 i = k + 1, l
                 gh(i) = factor * gh2(i)
   10       continue
            nmax = nmax2
        endif
        do 15 i = 1, k
            gh(i) = gh1(i) + factor * gh2(i)
   15   continue
        return
        end
