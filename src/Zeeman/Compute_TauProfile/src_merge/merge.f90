PROGRAM merge

  USE Type_Kinds
  
  IMPLICIT NONE

  CHARACTER(256) :: file_list, file_out
  CHARACTER(256), ALLOCATABLE :: file_data(:)
  REAL(fp_kind), ALLOCATABLE  :: z_lev(:,:), p_std(:,:), t_lev(:,:), w_lev(:,:), &
                                 Bfield(:), CBTH(:), angles(:), freqs(:), &
                                 tau(:,:,:,:)
  REAL(fp_kind)  :: angle
  INTEGER        :: n_files, n_profiles, n_layers, n_bfield, n_cbth, n_angles, n_freqs, n_levels
  INTEGER        :: i, ii, k, iprof, ibfield, icbth, ichan, iskip
  WRITE(*, '("Enter the filenames containing the data file names:")')
  READ(*, '(A)')file_list
  WRITE(*, '("Enter the ouput file filename:")')
  READ(*, '(A)')file_out
  
  OPEN(10, FILE=file_list, STATUS='OLD')
  READ(10, *)ichan
  READ(10, *)n_files, n_angles, n_freqs 
  ALLOCATE(file_data(n_files), angles(n_angles), freqs(n_freqs))
  
  READ(10, *)(angles(i), i=1,n_angles)
  READ(10, *)(freqs(i), i=1,n_freqs)
  READ(10, *)(file_data(i), i=1,n_files)
  
  DO ii = 1, n_files
    OPEN(20, FILE=file_data(ii), STATUS='OLD')
    READ(20, *)n_profiles, n_layers, n_bfield, n_cbth
    n_levels = n_layers + 1
    
    IF(ii == 1)THEN
      ALLOCATE(z_lev(n_levels,n_profiles), &
               p_std(n_levels,n_profiles), &
               t_lev(n_levels,n_profiles), &
               w_lev(n_levels,n_profiles), &
               Bfield(N_Bfield), &
               CBTH(N_CBTH), &
               tau(n_layers, n_bfield, n_cbth, n_profiles) )
    END IF
    
    DO iprof = 1, n_profiles
      READ(20, *)iskip, angle
      READ(20, *) (z_lev(k,iprof), p_std(k,iprof), t_lev(k,iprof), w_lev(k,iprof), k = 1, n_levels)
      DO ibfield = 1, N_Bfield
        DO icbth = 1, N_CBTH
          READ(20, *) Bfield(ibfield), CBTH(icbth), ichan
          READ(20, *) (tau(k, ibfield, icbth, iprof), k=1, n_layers)
        END DO
      END DO
    END DO
    IF(ii == 1)THEN
      OPEN(30, FILE=file_out, STATUS='REPLACE')
      WRITE(30, '(i5)')ichan
      WRITE(30, '(6i5)')n_profiles, n_layers, n_bfield, n_cbth, n_angles, n_freqs
      WRITE(30, '(50es16.8)')Bfield
      WRITE(30, '(50es16.8)')CBTH
      WRITE(30, '(50es16.8)')angles
      WRITE(30, '(50es16.8)')freqs
    END IF
    DO iprof = 1, n_profiles
    DO ibfield = 1, N_Bfield
    DO icbth = 1, N_CBTH
      WRITE(30, '(es16.8)')(tau(k, ibfield, icbth, iprof), k=1, n_layers)
    END DO
    END DO
    END DO
      
  END DO
  CLOSE(30)
  
  OPEN(40, FILE='AtmProfile.txt', STATUS='REPLACE')
  WRITE(40, '(2i5)')n_profiles, n_layers
  DO iprof=1, n_profiles
    WRITE(40, '(i5)')iprof
    WRITE(40, '(f9.3, 1x, f12.6, 1x, f10.3, es16.8)')&
           (z_lev(k,iprof), p_std(k,iprof), t_lev(k,iprof), w_lev(k,iprof), k = 1, n_levels)
  END DO
  CLOSE(40)
  
END PROGRAM merge
