 MODULE PredictorSubsetIndex
 
  !--- modules
  use type_kinds
   
  IMPLICIT NONE

  PRIVATE

  PUBLIC reset_search_predSubset  
  PUBLIC Get_subsetIndex_combination
  PUBLIC Get_subsetIndex_stepwise
  PUBLIC Get_subsetIndex_rev_stepwise
  PUBLIC retrieve_bestset_stepwise
  PUBLIC retrieve_bestset_rev_stepwise
  PUBLIC retrieve_bestset_combination
  PUBLIC evaluate_fittingError
  
  integer, parameter      :: MAX_PREDICTORS = 50   
  logical,         save   :: reset = .true.
  integer,         save   :: predictorIdx_best(MAX_PREDICTORS)  
  real(fp_kind),   save   :: MinErr

  real(fp_kind),   save   :: MinErr_array(MAX_PREDICTORS)
  integer,         save   :: best_array(MAX_PREDICTORS, MAX_PREDICTORS)


  real(fp_kind), parameter      :: INFINITE = HUGE(1.0_fp_kind)
   
 CONTAINS

  subroutine reset_search_predSubset()
  
     reset = .true.
     MinErr = INFINITE
    
  end subroutine reset_search_predSubset

  !=============================================================================
  ! Generate a predictor index set using stepwise algorithm:
  !    The step is from 1 to Npredictor_maxUsed out of the total Npredictor_all
  !    predictors. So, Npredictor_maxUsed <= Npredictor_all
  !
  !  Inputs: 
  !    Npredictor_all       - total number of predictors 
  !  In/Outpus:
  !    predictorIdx         - predictor index array
  !                           dimension: Rank-1, size = Npredictor_maxUsed
  !                           As input, it holds alread selected predictor indexes
  !                           As output, it holds update predictor indexes.
  !
  !  Outputs:
  !    Npredictors          - number of currently selected predictors
  !
  !  Return:
  !    endOfsel             - = 1 the selection process is completed 
  !                           = 0 a selection but the process has not reached an end
  !=============================================================================

  function Get_subsetIndex_stepwise(Npredictor_all, &
                                    predictorIdx, &
                                    Npredictors ) result( endOfsel)

    integer, intent(in)     :: Npredictor_all
    integer, intent(inout)  :: predictorIdx(:)
    integer, intent(out)    :: Npredictors
    
    integer                 :: endOfsel        ! 0) find a comb, 1) no more comb

    integer, save           :: index_start
    integer, save           :: predictor_flag(MAX_PREDICTORS)
    integer, save           :: Ipredictor  ! The ith predictor to be selected

    integer ::  predIndex, Npredictor_maxUsed

    Npredictor_maxUsed = SIZE(predictorIdx)
        
    if ( reset ) then                       
 
      Ipredictor = 1
      index_start = 1
      predictor_flag(1:Npredictor_maxUsed) = 1
      reset = .false.

    endif
     
    if(index_start > Npredictor_all)then
    
      MinErr_array(Ipredictor) = MinErr
      MinErr = INFINITE 
      predictorIdx(1:Ipredictor) = predictorIdx_best(1:Ipredictor)

      ! set flag to indicate the predictor has been selected
      
      predictor_flag(predictorIdx_best(Ipredictor)) = 0  
      
      ! selection is completed
      
      if(Ipredictor >= Npredictor_maxUsed)then
    
        endOfsel = 1
        return 
              
      endif

      Ipredictor = Ipredictor + 1
      index_start = 1
                  
    endif
    
    Npredictors = Ipredictor
    
    do predIndex = index_start, Npredictor_all
    
      if(predictor_flag(predIndex) == 0)then
      
        CYCLE
        
      endif
      
      predictorIdx(Ipredictor) = predIndex

      exit
      
    enddo
    
    index_start = predIndex + 1
    
    endOfsel = 0

      
  end function Get_subsetIndex_stepwise          

  !=============================================================================
  ! Generate a predictor index set using reversed stepwise algorithm:
  !    It starts with 1, 2, ..., SIZE(predictorIdx) predictor indexes,
  !    then removes one at a time until the number of indexes reaches 1.
  !    For example, if SIZE(predictorIdx) = 1, the following index sets are
  !    selected after each call of the routine:
  !        1, 2, 3
  !        1, 2
  !        1, 3
  !        2, 3
  !        2      ! if 1 & 3 are the best indexes  
  !
  !  Outpus:
  !    predictorIdx         - predictor index array
  !                           dimension: Rank-1
  !
  !  Outputs:
  !    Npredictors          - number of currently selected predictors
  !
  !  Return:
  !    endOfsel             - = 1 the selection process is completed 
  !                           = 0 a selection but the process has not reached an end
  !=============================================================================

  function Get_subsetIndex_rev_stepwise(predictorIdx, &
                                        Npredictors ) result( endOfsel)

    integer, intent(out)    :: predictorIdx(:)
    integer, intent(out)    :: Npredictors
    
    integer                 :: endOfsel        ! 0) find a comb, 1) no more comb

    integer, save           :: predIndex
    integer, save           :: Npredictors_selection
    integer ::  Npredictor_maxUsed, Npredictors_selected, i, j, k
    
    endOfsel = 0
    Npredictor_maxUsed = SIZE(predictorIdx)
        
    if ( reset ) then                       
 
      do i = 1, Npredictor_maxUsed
        predictorIdx(i) = i
        predictorIdx_best(i) = i
      end do
      
      Npredictors = Npredictor_maxUsed

      Npredictors_selection = Npredictor_maxUsed+1
      predIndex = Npredictor_maxUsed + 2
      reset = .false.

      return
    endif


    if(predIndex > Npredictors_selection)then
    
      Npredictors_selected = Npredictors_selection - 1 
      i = Npredictor_maxUsed - Npredictors_selected + 1   
      MinErr_array(i) = MinErr
      MinErr = INFINITE
      best_array(1:Npredictors_selected, i) = predictorIdx_best(1:Npredictors_selected)

      ! selection is completed     

      if(Npredictors_selected == 1)then  
    
        endOfsel = 1 
        return 
 
      endif    

      ! prepare for the next selection run
      Npredictors_selection = Npredictors_selected
      predIndex = 1
                 
    endif

    k = Npredictor_maxUsed - Npredictors_selection + 1
    j = 0
    do i = 1, Npredictors_selection
      if(i /= predIndex)then
        j = j + 1
        predictorIdx(j) = best_array(i, k)
      endif
    enddo
    Npredictors = Npredictors_selection - 1          
   
    predIndex = predIndex + 1
          
  end function Get_subsetIndex_rev_stepwise          
  
  subroutine retrieve_bestset_stepwise(predictorIdx, Npredictors)     
                                                                      
    integer, intent(out)         :: predictorIdx(:)  
    integer, intent(out)         :: Npredictors  
    
    real(fp_kind) :: MinValue
    integer :: ibest, i, Npredictor_maxUsed
    
    Npredictor_maxUsed = SIZE(predictorIdx)                    
                                                                      
    MinValue = INFINITE                                                 
    ibest = 1                                                         

    do i = 1, Npredictor_maxUsed                                      
                                                                    
      if(MinErr_array(i) < MinValue)then                                
                                                                      
        ibest = i                                                     
        MinValue = MinErr_array(i)
                                                                            
      endif                                                           
                                                                      
    enddo                                                             
                                                                      
    predictorIdx(:) = 0                                               
    predictorIdx(1:ibest) = predictorIdx_best(1:ibest)                
    Npredictors = ibest                                               
                                                                      
  end subroutine retrieve_bestset_stepwise                            
 
   subroutine retrieve_bestset_rev_stepwise(predictorIdx, Npredictors)     
                                                                      
    integer, intent(out)         :: predictorIdx(:)  
    integer, intent(out)         :: Npredictors  
    
    real(fp_kind) :: MinValue
    integer :: ibest, i, Npredictor_maxUsed
    
    Npredictor_maxUsed = SIZE(predictorIdx)                    
                                                                      
    MinValue = INFINITE                                                 
    ibest = 1                                                         

    do i = 1, Npredictor_maxUsed                                      
                                                                    
      if(MinErr_array(i) < MinValue)then                                
                                                                      
        ibest = i                                                     
        MinValue = MinErr_array(i)
                                                                            
      endif                                                           
                                                                      
    enddo                                                             
                                                                      
    predictorIdx(:) = 0                                               
    Npredictors = Npredictor_maxUsed - ibest + 1                                               
    predictorIdx(1:Npredictors) = best_array(1:Npredictors, ibest)                
                                                                      
  end subroutine retrieve_bestset_rev_stepwise                            
                                                                      
     
  !====================================================================
  ! Generate a predictor combination to be verified next
  ! Note that Npredictors should not be changed when the function is
  ! called at different times for a Npredictors combination.
  ! Npredictors out of total Npredictor_all predictors
  !====================================================================

  function Get_subsetIndex_combination( Npredictor_all, Npredictors, predictorIdx ) result( endOfsel)

    !--- interface  

    integer, intent(in)   :: Npredictor_all, Npredictors  
    integer,intent(inout) :: predictorIdx(:)

    integer               :: endOfsel        ! 0) find a comb, 1) no more comb

    !--- local variables

    integer               :: i, j
    
    
    
    if ( reset ) then                       
 
      predictorIdx(1:Npredictors) = (/(i, i=1,Npredictors)/)
      predictorIdx(Npredictors) = Npredictors - 1                                                                 
      reset = .false.
                                                                                           
    endif                                          
     
    !--- find the next combination

    do i = Npredictors, 1, -1

      predictorIdx(i) = predictorIdx(i) + 1

      if( predictorIdx(i) <= Npredictor_all - (Npredictors - i) )then

        if( i < Npredictors )then
          do j = i+1, Npredictors
            predictorIdx(j) = predictorIdx(j-1) + 1
          enddo
        endif
        
        endOfsel = 0
        return

      endif

    enddo

    !--- no more combination

    endOfsel = 1

  end function Get_subsetIndex_combination

  subroutine retrieve_bestset_combination(predictorIdx)               
                                                                      
    integer, intent(out)  :: predictorIdx(:)
    
    integer :: n  

    n = SIZE(predictorIdx)
    predictorIdx = predictorIdx_best(1:n)                                  
                                                                      
  end subroutine retrieve_bestset_combination                         


  subroutine evaluate_fittingError(fittingError, significance, predictorIdx)        
                                                                                
    real(fp_kind), INTENT(IN)  :: fittingError 
    real(fp_kind), INTENT(IN)  :: significance                                       
    INTEGER      , INTENT(IN)  :: predictorIdx(:)                

    integer :: n
   
    n = SIZE(predictorIdx)
                                                                                   
    if ( (fittingError + significance) < MinErr )then                                   
                                                                                
      MinErr = fittingError                                                        
      predictorIdx_best(1:n) = predictorIdx  

    endif                                                                       

  end subroutine evaluate_fittingError                                           

end MODULE PredictorSubsetIndex 
