MODULE NavierStokesRHSWENO

  USE Grid
  USE Roe
  USE MetricOps
  USE EOS
  USE Simple
  USE Special

  IMPLICIT NONE

  INTEGER(KIND=4), PARAMETER :: NSCBC_NOSLIP_ISOTHERMAL     = 6
  INTEGER(KIND=4), PARAMETER :: NSCBC_SLIP_ADIABATIC        = 7
  INTEGER(KIND=4), PARAMETER :: NSCBC_INFLOW_SUPERSONIC     = 8
  INTEGER(KIND=4), PARAMETER :: NSCBC_INFLOW_SUBSONIC       = 9
  INTEGER(KIND=4), PARAMETER :: NSCBC_INFLOW_VELTEMP        = 10
  INTEGER(KIND=4), PARAMETER :: NSCBC_OUTFLOW_NONREFLECTION = 11
  INTEGER(KIND=4), PARAMETER :: NSCBC_AXISYMMETRY           = 12

  INTEGER(KIND=4), PARAMETER :: WENO_CENTRAL        = 1
  INTEGER(KIND=4), PARAMETER :: WENO_LEFTBOUNDARY1  = 4
  INTEGER(KIND=4), PARAMETER :: WENO_LEFTBOUNDARY2  = 8
  INTEGER(KIND=4), PARAMETER :: WENO_RIGHTBOUNDARY1 = 10
  INTEGER(KIND=4), PARAMETER :: WENO_RIGHTBOUNDARY2 = 14
  INTEGER(KIND=4), PARAMETER :: WENO_AXISBOUNDARY1  = 20
  INTEGER(KIND=4), PARAMETER :: WENO_AXISBOUNDARY2  = 26

  INTEGER, parameter :: NS = 3  ! number of stencils
  integer(KIND=4), parameter :: width  = 6  ! total stencil width

  type dissipation_local
    real(kind=8), allocatable :: cv_stencil(:,:)
    real(kind=8), allocatable :: f_stencil(:,:)
    real(kind=8), allocatable :: ws_stencil(:,:)
    real(kind=8), allocatable :: ws_max_local(:)
    real(kind=8), allocatable :: f_stencil_char(:,:)
    real(kind=8), allocatable :: cv_stencil_char(:,:)
    real(kind=8), allocatable :: f_stencil_char_pos(:,:)
    real(kind=8), allocatable :: f_stencil_char_neg(:,:)
    real(kind=8), allocatable :: f_face_1d_char(:)
    real(kind=8), allocatable :: f_face_1d_char_pos(:)
    real(kind=8), allocatable :: f_face_1d_char_neg(:)
    real(kind=8), allocatable :: uL(:)
    real(kind=8), allocatable :: uR(:)
    real(kind=8), allocatable :: Tmat(:,:)
    real(kind=8), allocatable :: Tinv(:,:)
    real(kind=8), allocatable :: Lambda(:,:)
    real(kind=8), allocatable :: dvL(:)
    real(kind=8), allocatable :: dvR(:)
    real(kind=8), allocatable :: uRoe(:)
    real(kind=8), allocatable :: dvRoe(:)
    real(kind=8), allocatable :: dpRoe(:)
    real(kind=8), allocatable :: pointMetricR(:)
    real(kind=8), allocatable :: pointMetricL(:)
    real(kind=8), allocatable :: pointMetric(:)
    real(kind=8), allocatable :: metricUnitVectors(:)
    real(kind=8), allocatable :: metricMags(:)
  end type

  type characteristic_local
    real(kind=8), allocatable :: uL(:)
    real(kind=8), allocatable :: dvL(:)
    real(kind=8), allocatable :: dpL(:)
  end type

  type upwinding_local
    real(kind=8), allocatable :: f_stencil_char(:,:)
    real(kind=8), allocatable :: C(:)
    real(kind=8), allocatable :: IS(:,:)
    real(kind=8), allocatable :: alpha(:,:)
    real(kind=8), allocatable :: w(:,:)
    real(kind=8), allocatable :: tp(:,:)
  end type

CONTAINS

  subroutine allocate_local_types(d_local, c_local, u_local, numEquations, numDim, nAuxVars)
    type(dissipation_local) :: d_local
    type(characteristic_local) :: c_local
    type(upwinding_local) :: u_local

    integer, intent(in) :: numEquations, numDim, nAuxVars

    allocate( d_local%cv_stencil(width,numEquations) )
    allocate( d_local%f_stencil(width,numEquations) )
    allocate( d_local%ws_stencil(width+1,numEquations) )
    allocate( d_local%ws_max_local(numEquations) )
    allocate( d_local%f_stencil_char(width,numEquations) )
    allocate( d_local%cv_stencil_char(width,numEquations) )
    allocate( d_local%f_stencil_char_pos(width,numEquations) )
    allocate( d_local%f_stencil_char_neg(width,numEquations) )
    allocate( d_local%f_face_1d_char(numEquations) )
    allocate( d_local%f_face_1d_char_pos(numEquations) )
    allocate( d_local%f_face_1d_char_neg(numEquations) )
    allocate( d_local%uL(numEquations) )
    allocate( d_local%uR(numEquations) )
    allocate( d_local%Tmat(numEquations,numEquations) )
    allocate( d_local%Tinv(numEquations,numEquations) )
    allocate( d_local%Lambda(numEquations,numEquations) )
    allocate( d_local%dvL(numDim+2) )
    allocate( d_local%dvR(numDim+2) )
    allocate( d_local%uRoe(numDim+2+nAuxVars) )
    allocate( d_local%dvRoe(numDim+2) )
    allocate( d_local%dpRoe(2+nAuxVars) )
    allocate( d_local%pointMetricR(numDim*numDim) )
    allocate( d_local%pointMetricL(numDim*numDim) )
    allocate( d_local%pointMetric(numDim*numDim) )
    allocate( d_local%metricUnitVectors(numDim*numDim) )
    allocate( d_local%metricMags(numDim) )

    allocate( c_local%uL(numDim+2+nAuxVars) )
    allocate( c_local%dvL(numDim+2) )
    allocate( c_local%dpL(2+nAuxVars) )

    call allocate_upwinding_local(u_local, numEquations)
  end subroutine

  subroutine allocate_upwinding_local(u_local, numEquations)
    type(upwinding_local) :: u_local
    integer, intent(in) :: numEquations

    allocate( u_local%f_stencil_char(width,numEquations) )
    allocate( u_local%C(NS) )
    allocate( u_local%IS(NS,numEquations) )
    allocate( u_local%alpha(NS,numEquations) )
    allocate( u_local%w(NS,numEquations) )
    allocate( u_local%tp(NS,numEquations) )
  end subroutine

  subroutine deallocate_local_types(d_local, c_local, u_local)
    type(dissipation_local) :: d_local
    type(characteristic_local) :: c_local
    type(upwinding_local) :: u_local

    deallocate( d_local%cv_stencil )
    deallocate( d_local%f_stencil )
    deallocate( d_local%ws_stencil )
    deallocate( d_local%ws_max_local )
    deallocate( d_local%f_stencil_char )
    deallocate( d_local%cv_stencil_char )
    deallocate( d_local%f_stencil_char_pos )
    deallocate( d_local%f_stencil_char_neg )
    deallocate( d_local%f_face_1d_char )
    deallocate( d_local%f_face_1d_char_pos )
    deallocate( d_local%f_face_1d_char_neg )
    deallocate( d_local%uL )
    deallocate( d_local%uR )
    deallocate( d_local%Tmat )
    deallocate( d_local%Tinv )
    deallocate( d_local%Lambda )
    deallocate( d_local%dvL )
    deallocate( d_local%dvR )
    deallocate( d_local%uRoe )
    deallocate( d_local%dvRoe )
    deallocate( d_local%dpRoe )
    deallocate( d_local%pointMetricR )
    deallocate( d_local%pointMetricL )
    deallocate( d_local%pointMetric )
    deallocate( d_local%metricUnitVectors )
    deallocate( d_local%metricMags )

    deallocate( c_local%uL )
    deallocate( c_local%dvL )
    deallocate( c_local%dpL )

    call deallocate_upwinding_local(u_local)
  end subroutine

  subroutine deallocate_upwinding_local(u_local)
    type(upwinding_local) :: u_local

    deallocate( u_local%f_stencil_char )
    deallocate( u_local%C )
    deallocate( u_local%IS )
    deallocate( u_local%alpha )
    deallocate( u_local%w )
    deallocate( u_local%tp )
  end subroutine

!--------------------------------------------------------------------------------------------------
! 5th order WENO of Jiang and Shu, JCP 1996--------------------------------------------------------
! Freestream preservation technique of Nonomura, Computer & Fluids 2015----------------------------
! Pooya Movahed 2018-------------------------------------------------------------------------------
! See the WENO verification document for details---------------------------------------------------
!--------------------------------------------------------------------------------------------------

          
  subroutine NS_RHS_WENO(                                              &
       numDim,bufferSizes,numPointsBuffer,numScalars,numEquations,dir, &
       sconn,dfluxbuffer, inviscidFluxBuffer, opInterval,              &
       gridMetric,jacobianDeterminant,gridCoordinates,gridType,        &
       rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,                 &
       temperatureBuffer,sosBuffer,scalarBuffer,WENO_alpha,wenoOptions,&
       eosGasInfo,eosNonDimen,eosGasParams)
    
    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim, numscalars, dir, numEquations, gridType, strongShocks
    INTEGER(KIND=8) :: numPointsBuffer, bufferSizes(numDim), eosGasInfo(NUMGASINFOS)
    INTEGER(KIND=4) :: sconn(numPointsBuffer), wenoOptions(2)
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)         :: inviscidFluxBuffer(numEquations*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8)                        :: dfluxbuffer(numEquations*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: jacobianDeterminant(2*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: gridCoordinates(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: sosBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: eosGasParams(NUMGASPARAMS*(numScalars+1))
    REAL(KIND=8),    INTENT(IN)         :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8) :: WENO_alpha

    ! ... local variables
    INTEGER(KIND=4) :: WENO_STRONGSHOCKS
    INTEGER(KIND=4) :: gridAxisymmetric,haveAxis ! axisymmetric
    INTEGER(KIND=4) :: nAuxVars
    INTEGER(KIND=8) :: iloop(2), eosContext, left_edge_point, planesize
    INTEGER(KIND=8) :: is(3), ie(3), is_unique(3), ie_unique(3)
    INTEGER(KIND=8) :: offset1(3), offset2(3)
    INTEGER(KIND=8) :: index_weno(2), Npencil2, N(3), N_unique(3)
    INTEGER(KIND=8) :: Npencil, Npencilghost, Nghostweno(2), Npencils,pencilStart
    INTEGER(KIND=8) :: ir, ic, i, j, iPencil,baseIndex 
    INTEGER(KIND=8), allocatable :: p_map(:), p_map2(:), p_map3(:)
    REAL(KIND=8), allocatable :: flux_face_1d(:,:,:), flux_temp_1d(:,:), smap(:,:)
    INTEGER(KIND=4) :: jconnect

    ! Apply_WENO_JS_Consistent
    integer :: consistentS(numEquations)

    ! Types containing local variables
    type(dissipation_local) :: d_local
    type(characteristic_local) :: c_local
    type(upwinding_local) :: u_local

    nauxVars = numScalars
    gridAxisymmetric = wenoOptions(2)
    WENO_STRONGSHOCKS = wenoOptions(1)
    eosContext = eosGasInfo(1)

    is(:) = 1; ie(:) = 1; N(:) = 1; N_unique(:) = 1
    is_unique(:) = 1; ie_unique(:) = 1
    do i = 1, numDim
      is_unique(i) = opInterval((i-1)*2+1)
      ie_unique(i) = opInterval(i*2)
      ie(i) = bufferSizes(i)
      N_unique(i) = ie_unique(i) - is_unique(i) + 1
      N(i)        = bufferSizes(i)
    enddo 
    left_edge_point = is_unique(1)
    planesize = bufferSizes(1)
    do i = 2, numDim
       left_edge_point = left_edge_point + is_unique(i)*planesize
       planesize = planesize*bufferSizes(i)
    end do

    !> @todo Check with Pavel, sconn(1) will actually be in the halo [mtc]
    haveAxis = 0
    IF(gridAxisymmetric == 1 .AND. dir == 1) THEN
       !       IF(SUM(MERGE(1,0,sconn.eq.WENO_RIGHTBOUNDARY2)) > 0) THEN
       IF(sconn(left_edge_point) == WENO_RIGHTBOUNDARY2) THEN
          !       IF(sconn(is_unique(1)) == WENO_RIGHTBOUNDARY2) .OR.  &
          !            sconn(is_unique(1)) == WENO_AXISBOUNDARY1)  THEN
         haveAxis=1 ! if we're actually sweeping radially in a subdomain on the centerline
       END IF
    ENDIF
    

    ! ... compute the numerical flux at half points in the interior of the domain,
    ! ... where the full stencil can be applied, without considering iblank points
    ! ... add ghost points to each side of the 1D vector
    Nghostweno(1) = is_unique(dir) - is(dir)
    Nghostweno(2) = ie(dir) - ie_unique(dir)
    Nghostweno(1) = MIN(3_8,Nghostweno(1))
    Nghostweno(2) = MIN(3_8,Nghostweno(2))
    Npencil = N_unique(dir)
    Npencilghost = Npencil + Nghostweno(1) + Nghostweno(2)
    pencilStart  = is_unique(dir) - Nghostweno(1)
!    index_weno(1) = MAX(3_8,Nghostweno(1))
    index_weno(1) = 3_8
    IF (haveAxis==1) THEN
       Npencilghost = Npencilghost + 2
    END IF
!    index_weno(2) = Npencilghost - max(3_8,Nghostweno(2))
    index_weno(2) = Npencilghost - 3_8

    ALLOCATE(flux_temp_1d(Npencilghost,numEquations))
    ALLOCATE(p_map(Npencilghost),p_map2(Npencilghost),p_map3(Npencilghost),smap(Npencilghost,2))
    
    select case (dir)
    case (1)
       iloop(1) = 2
       iloop(2) = 3
       offset1(1) = N(1)
       offset2(1) = N(2) * N(1)
       baseIndex = pencilStart
       do i = 1, Npencilghost
          p_map(i) = baseIndex + (i-1)
          if (haveAxis==1) then
             p_map(i) = p_map(i)-2 ! PPP: start two elements earlier in axisymmetric
             smap(i,1) = 1.0 
             smap(i,2) = 1.0 
          end if
          Npencils = 1
          if(numDim > 1) Npencils = N_unique(2)
          if(numDim > 2) Npencils = Npencils * N_unique(3)
       end do
       if (haveAxis==1) then
          p_map(1) = 2
          p_map(2) = 1
          smap(1,1) = -1.0
          smap(2,1) = -1.0
       end if
    case (2)
       iloop(1) = 1
       iloop(2) = 3
       offset1(2) = 1
       offset2(2) = N(2) * N(1)
       baseIndex = (pencilStart-1)*N(1)+1
       do i = 1, Npencilghost
!          p_map(i)  = (pencilStart+(i - 1)) * N(1) + 1
          p_map(i)  = baseIndex + (i-1)*N(1) 
       end do
       Npencils = N_unique(1)
       if(numDim > 2) Npencils = Npencils * N_unique(3)
    case (3)
      iloop(1) = 1
      iloop(2) = 2
      offset1(3) = 1
      offset2(3) = N(1)
      baseIndex = (pencilStart-1)*N(1)*N(2) + 1
      do i = 1, Npencilghost
!        p_map(i) = (pencilStart+(i - 1)) * N(2) * N(1) + 1
         p_map(i) = baseIndex + (i-1)*N(2)*N(1)
      end do
      Npencils = N_unique(1) * N_unique(2)
    end select

    ALLOCATE(flux_face_1d(Npencilghost,numEquations,Npencils))

    dfluxbuffer(:)=0.0_8

    call allocate_local_types(d_local, c_local, u_local, numEquations, numDim, nAuxVars)

    !$omp barrier

    ! @ICE loop=WENOloop
    iPencil = 1
    do ic = is_unique(iloop(2)), ie_unique(iloop(2))
       p_map3(:) = p_map(:) + offset2(dir) * (ic - is(iloop(2)))
       do ir = is_unique(iloop(1)), ie_unique(iloop(1))
          p_map2(:) = p_map3(:) + offset1(dir) * (ir - is(iloop(1)))

          ! ... Compute the consistent part of the flux
          flux_temp_1d(:,:) = 0.0_8
          Call Apply_WENO_JS_Consistent(dir,p_map2,smap,inviscidFluxBuffer,flux_temp_1d,&
               index_weno,numEquations,numPointsBuffer,haveAxis,consistentS)
          flux_face_1d(:,:,iPencil) = flux_temp_1d(:,:)
          iPencil = iPencil + 1
       end do
    end do

!$omp barrier

          ! ... Compute the dissipation part of the flux
    iPencil = 1
    do ic = is_unique(iloop(2)), ie_unique(iloop(2))
       p_map3(:) = p_map(:) + offset2(dir) * (ic - is(iloop(2)))
       do ir = is_unique(iloop(1)), ie_unique(iloop(1))
          p_map2(:) = p_map3(:) + offset1(dir) * (ir - is(iloop(1)))
          flux_temp_1d(:,:) = 0.0_8
          CALL Apply_WENO_JS_Dissipation(dir,p_map2,smap,numDim,numEquations,flux_temp_1d,index_weno, &
               WENO_alpha, nAuxVars, WENO_STRONGSHOCKS, numPointsBuffer,&
               rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,pressureBuffer,temperatureBuffer,sosBuffer,&
               gridMetric,jacobianDeterminant,eosGasInfo,eosNonDimen,eosGasParams,gridType,haveAxis,&
               d_local, c_local, u_local)
          IF(haveAxis .gt. 0) THEN
             flux_temp_1d(index_weno(1),:) = 0.0_8
          ENDIF
          flux_face_1d(:,:,iPencil) = flux_face_1d(:,:,iPencil) + flux_temp_1d(:,:)
          iPencil = iPencil + 1
       end do
    end do
    
!$omp barrier

    iPencil = 1
    do ic = is_unique(iloop(2)), ie_unique(iloop(2))
       p_map3(:) = p_map(:) + offset2(dir) * (ic - is(iloop(2)))
       do ir = is_unique(iloop(1)), ie_unique(iloop(1))
          p_map2(:) = p_map3(:) + offset1(dir) * (ir - is(iloop(1)))
          ! ... Compute the rhs
          do i = index_weno(1)+1, index_weno(2)
             jconnect = sconn(p_map2(i))
             if (haveAxis==1) then
                if (jconnect==WENO_CENTRAL .or.                                                 &
                     (WENO_LEFTBOUNDARY1  < jconnect .and. jconnect < WENO_LEFTBOUNDARY2)  .or. &
                     (WENO_RIGHTBOUNDARY1 < jconnect .and. jconnect < WENO_RIGHTBOUNDARY2) .or. &
                     (WENO_RIGHTBOUNDARY2 < jconnect .and. jconnect < WENO_AXISBOUNDARY1)  .or. &
                     (WENO_AXISBOUNDARY1  < jconnect .and. jconnect < WENO_AXISBOUNDARY2)) then
                   do j = 1, numEquations
                      dfluxbuffer(p_map2(i) + (j - 1) * numPointsBuffer) = &
                           flux_face_1d(i,j,iPencil) - flux_face_1d(i-1,j,iPencil)
                   enddo
                   dfluxbuffer(p_map2(i) + numPointsBuffer) = dfluxbuffer(p_map2(i) + numPointsBuffer) -&
                        pressureBuffer(p_map2(i))/(gridCoordinates(p_map2(i))*jacobianDeterminant(p_map2(i)))
                endif
             else
                if (jconnect==WENO_CENTRAL .or.                                                  &
                     (WENO_LEFTBOUNDARY1  < jconnect .and. jconnect < WENO_LEFTBOUNDARY2)   .or. &
                     (WENO_RIGHTBOUNDARY1 < jconnect .and. jconnect < WENO_RIGHTBOUNDARY2)) then
                   do j = 1, numEquations
                      dfluxbuffer(p_map2(i) + (j - 1) * numPointsBuffer) = flux_face_1d(i,j,iPencil) - flux_face_1d(i-1,j,iPencil)
                   enddo
                   if ((gridAxisymmetric==1).and.(dir==1)) then
                      dfluxbuffer(p_map2(i) + numPointsBuffer) = dfluxbuffer(p_map2(i) + &
                           numPointsBuffer) - pressureBuffer(p_map2(i))/(gridCoordinates(p_map2(i))&
                           *jacobianDeterminant(p_map2(i)))
                   end if
                endif
             endif
          end do
          if (haveAxis==1) then
             if ((sconn(p_map2(index_weno(1)))==WENO_RIGHTBOUNDARY2).or.&
                  (sconn(p_map2(index_weno(1)))==WENO_AXISBOUNDARY1)) then
                do j = 1, numEquations
                   dfluxbuffer(p_map2(index_weno(1)) + (j - 1) * numPointsBuffer) = flux_face_1d(index_weno(1),j,iPencil)
                enddo
                dfluxbuffer(p_map2(index_weno(1)) + numPointsBuffer) = dfluxbuffer(p_map2(index_weno(1)) + &
                     numPointsBuffer) - pressureBuffer(p_map2(index_weno(1)))/&
                     (gridCoordinates(p_map2(index_weno(1)))*jacobianDeterminant(p_map2(index_weno(1)))) 
             end if ! PPP: take care of the first element, for which we need no left flux                    
          endif
          iPencil = iPencil + 1
       end do
    end do!ic
    ! @ICE endloop

    call deallocate_local_types(d_local, c_local, u_local)
    
    deallocate(flux_face_1d,flux_temp_1d,p_map,p_map2,p_map3,smap)
    
    return

  end subroutine NS_RHS_WENO

  subroutine Apply_WENO_JS_Consistent(dir,p_map,smap,inviscidFluxBuffer,f_face_1d, &
                                      index_weno,numEquations,numPointsBuffer,haveAxis,s)

    IMPLICIT NONE


    INTEGER(KIND=4) :: dir, numEquations
    INTEGER(KIND=8) :: index_weno(2), numPointsBuffer
    INTEGER(KIND=8) :: p_map(:)
    INTEGER(KIND=4) :: haveAxis
    REAL(KIND=8)    :: smap(:,:)
    REAL(KIND=8) :: f_face_1d(:,:)
    REAL(KIND=8)    :: inviscidFluxBuffer(numEquations*numPointsBuffer)
    integer :: s(numEquations) ! PPP sign: 1 for anitisymmetric, 2 for symmetric

    !---local variables----
    integer :: i, j, offset

    if (haveAxis==1) then
       s = 2
       s(2) = 1 ! flux of axial velocity is symmetric (2)
       
       do i = index_weno(1), index_weno(2)
          do j = 1, numEquations
             offset = (j-1) * numPointsBuffer
             f_face_1d(i,j) = (inviscidFluxBuffer(p_map(i-2) + offset)*smap(i-2,s(j)) &
                  - 8.0_8 * inviscidFluxBuffer(p_map(i-1) + offset)*smap(i-1,s(j)) + &
                  37.0_8 * inviscidFluxBuffer(p_map(i)+offset)*smap(i,s(j)) &
                  + inviscidFluxBuffer(p_map(i+3) + offset)*smap(i+3,s(j)) -    &
                  8.0_8 * inviscidFluxBuffer(p_map(i+2) + offset)*smap(i+2,s(j)) &
                  + 37.0_8 * inviscidFluxBuffer(p_map(i+1) + offset)*smap(i+1,s(j))) &
                  /60.0_8
          enddo
       end do
    else
       do i = index_weno(1), index_weno(2)
          do j = 1, numEquations
             offset = (j-1) * numPointsBuffer
             f_face_1d(i,j) = (inviscidFluxBuffer(p_map(i-2) + offset) - 8.0_8 * inviscidFluxBuffer(p_map(i-1) + offset) + &
                  37.0_8 * inviscidFluxBuffer(p_map(i)+offset) + inviscidFluxBuffer(p_map(i+3) + offset) -    &
                  8.0_8 * inviscidFluxBuffer(p_map(i+2) + offset) + 37.0_8 * inviscidFluxBuffer(p_map(i+1) + offset)) &
                  /60.0_8
          enddo
       end do
    endif

    return
    
  end subroutine Apply_WENO_JS_Consistent
  
  subroutine Conserved_Variables_Single_Point(u,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,&
       map,smap,numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)
 
    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim, numEquations, nauxVars
    INTEGER(KIND=8) :: numPointsBuffer, map
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(nauxVars*numPointsBuffer)
    REAL(KIND=8)    :: u(numEquations), smap(:)
    INTEGER(KIND=4) :: haveAxis

    !-----local variables-------
    INTEGER(KIND=8) :: i

    u(1) = rhoBuffer(map)
    do i = 1, numDim
       u(1 + i) = rhoVBuffer(map + (i - 1) * numPointsBuffer) 
       if (haveAxis==1) then
          if (i == 1) then
             u(1 + i) = u(1 + i)*smap(1) ! u velocity is antisymmetric
          end if
       end if
    enddo
    u(2 + numDim) = rhoEBuffer(map)
    do i = 1, nauxVars
       u(2 + numDim + i) = scalarBuffer(map + (i - 1) * numPointsBuffer) 
    enddo
    
    return
    
  end subroutine Conserved_Variables_Single_Point
  
  subroutine Frozen_Incviscid_Flux(f_stencil,width,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,& 
       pressureBuffer,map,p_map,smap,numDim,nauxVars,numEquations,numPointsBuffer,MT1_freeze,haveAxis)

    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim, width, numEquations, nauxVars, map
    INTEGER(KIND=8) :: numPointsBuffer
    REAL(KIND=8),    INTENT(IN)         :: MT1_freeze(numDim)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(nauxVars*numPointsBuffer)
    REAL(KIND=8)    :: f_stencil(width,numEquations)
    INTEGER(KIND=8) :: p_map(:)
    REAL(KIND=8)    :: smap(:,:)
    INTEGER(KIND=4) :: haveAxis

    !-----local variables-------
    INTEGER(KIND=8) :: j
    INTEGER(KIND=4) :: m, jl
    REAL(KIND=8)    :: Uhat
!    REAL(KIND=8)    :: XI_TAU_Freeze

!    XI_TAU_Freeze = 0.0_8  !for now change later

    ! ... compute the contravariant velocities
    do j = map - 2, map + 3
      jl = j - (map - 2) + 1
      Uhat = 0.0_8
      do m = 1, numDim
         if ((haveAxis == 1).and.(m==1)) then
            Uhat = Uhat + MT1_freeze(m) * rhoVBuffer(p_map(j)) * smap(j,1)  
         else
            Uhat = Uhat + MT1_freeze(m) * rhoVBuffer(p_map(j)+ (m-1) * numPointsBuffer)
         end if
      enddo
      Uhat = Uhat / rhoBuffer(p_map(j))
!      Uhat = Uhat + XI_TAU_freeze

      ! ... continuity
      f_stencil(jl,1) = rhoBuffer(p_map(j)) * Uhat
      ! ... momentum
      do m =1, numDim
         if ((haveAxis == 1).and.(m==1)) then
            f_stencil(jl,1+m) = rhoVBuffer(p_map(j)) * smap(j,1) * Uhat +&
                 MT1_freeze(m) * pressureBuffer(p_map(j))
         else
            f_stencil(jl,1+m) = rhoVBuffer(p_map(j)+(m-1)*numPointsBuffer) * Uhat + MT1_freeze(m) * pressureBuffer(p_map(j))
         end if
      end do
      ! ... energy
      f_stencil(jl,numDim+2) = (rhoEBuffer(p_map(j)) + pressureBuffer(p_map(j))) * Uhat! - XI_TAU_freeze * pressureBuffer(p_map(j))
      ! ... scalar
      do m = 1, nAuxVars
        f_stencil(jl,numDim + 2 + m) = scalarBuffer(p_map(j) + (m - 1) * numPointsBuffer) * Uhat
      end do 
    end do

    return

  end subroutine Frozen_Incviscid_Flux


  subroutine Characteristic_Velocity(ws_stencil,width,iLeft,iRight,         &
       rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,                        &
       pressureBuffer,temperatureBuffer,sosBuffer,map,p_map,smap,dir,numDim,nauxVars, &
       numEquations,numPointsBuffer,gridMetric,                             &
       jacobianDeterminant,eosInfo,eosNonDimen,eosGasParams,haveAxis,c_local) ! PPP - done
    
    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim, width, dir, map, numEquations, nauxVars, iLeft, iRight
    INTEGER(KIND=8) :: numPointsBuffer,eosInfo(NUMGASINFOS)
    integer(KIND=8) :: p_map(:)
    REAL(KIND=8)    :: smap(:,:)
    REAL(KIND=8)    :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8)    :: gridMetric(numDim*numDim*numPointsBuffer)
    INTEGER(KIND=4) :: haveAxis
    REAL(KIND=8),    INTENT(IN)         :: eosGasParams(NUMGASPARAMS*(nauxVars+1))
    REAL(KIND=8),    INTENT(IN)         :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: sosBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(nauxVars*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT)        :: ws_stencil(width+1,numEquations)

    type(characteristic_local) :: c_local

    !----local variables--------
    INTEGER(KIND=8) :: j, jl, offset, jj, k, eosContext
    REAL(KIND=8)    :: sound_speed, XI_X, XI_Y, XI_Z, ucon
    
    eosContext = eosInfo(1)

    ! ... compute the wave speeds for flux splitting
    do j = map - iLeft, map + IRight

      jl = j - (map - iLeft) +1

      CALL Conserved_Variables_Single_Point(c_local%uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(j), &
           smap(j,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)
      
      c_local%dvL(1) = pressureBuffer(p_map(j))
      c_local%dvL(2) = temperatureBuffer(p_map(j))
      sound_speed = sosBuffer(p_map(j))

      IF(haveAxis == 1) THEN
         c_local%dvL(3) = (rhoVBuffer(p_map(j))/rhoBuffer(p_map(j)))*smap(j,1)
      ELSE
         c_local%dvL(3) = rhoVbuffer(p_map(j))/rhoBuffer(p_map(j))
      ENDIF
      DO K = 2,numDim
         c_local%dvL(k+2) = rhoVbuffer(p_map(j)+(k-1)*numPointsBuffer)/rhoBuffer(p_map(j))
      END DO
      
      !CALL GASDP(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1), &
           !c_local%uL(1),c_local%dvL(1),c_local%dpL(1))
      !CALL GASSOUNDSPEED(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
           !c_local%uL(1),c_local%dvL(1),c_local%dpL(1),sound_speed)
      
      offset = (dir-1) * numDim * numPointsBuffer
      XI_X = gridMetric(offset + p_map(j)) * jacobianDeterminant(p_map(j))
      XI_Y = gridMetric(offset + p_map(j)+ numPointsBuffer) * jacobianDeterminant(p_map(j))
      
      if (haveAxis==1) then
         ucon = rhoVBuffer(p_map(j)) * XI_X * smap(j,1) + &
              rhoVBuffer(p_map(j) + numPointsBuffer) * XI_Y
         
      else
         ucon = rhoVBuffer(p_map(j)) * XI_X + &
              rhoVBuffer(p_map(j) + numPointsBuffer) * XI_Y
      end if
      

      XI_Z = 0.0_8
      if (numDim >= 3) then
        XI_Z = gridMetric(offset + p_map(j)+ 2 * numPointsBuffer) * jacobianDeterminant(p_map(j))
        ucon = ucon + rhoVBuffer(p_map(j) + 2 * numPointsBuffer) * XI_Z
      end if
      ucon = ucon / rhoBuffer(p_map(j))

      ws_stencil(jl,1:numDim) = abs(ucon)
      ws_stencil(jl,numDim+1) = abs(ucon + sound_speed * sqrt(XI_X**2 + XI_Y**2 + XI_Z**2))
      ws_stencil(jl,numDim+2) = abs(ucon - sound_speed * sqrt(XI_X**2 + XI_Y**2 + XI_Z**2))

      do jj = 1, nAuxVars
        ws_stencil(jl,jj+numDim+2) = abs(ucon)
      enddo
   end do
   
   return
   
 end subroutine Characteristic_Velocity
 
 
 
 SUBROUTINE ComputeCharacteristicVelocities(            &
      numDim,numPointsBuffer,bufferSizes,opInterval,    &
      numEquations,nAuxVars,haveAxis,dir,               &
      velocityBuffer,sosBuffer,gridMetric,              &
      jacobianDeterminant,charVel) 
   
   IMPLICIT NONE
   
   INTEGER(KIND=4), INTENT(IN)  :: numDim,dir,numEquations,nauxVars,haveAxis
   INTEGER(KIND=8), INTENT(IN)  :: numPointsBuffer,bufferSizes(numDim)
   INTEGER(KIND=8), INTENT(IN)  :: opInterval(2*numDim)
   REAL(KIND=8),    INTENT(IN)  :: jacobianDeterminant(numPointsBuffer)
   REAL(KIND=8),    INTENT(IN)  :: gridMetric(numDim*numDim*numPointsBuffer)
   REAL(KIND=8),    INTENT(IN)  :: velocityBuffer(numDim*numPointsBuffer)
   REAL(KIND=8),    INTENT(IN)  :: sosBuffer(numPointsBuffer)
   REAL(KIND=8),    INTENT(OUT) :: charVel(3*numPointsBuffer)
   
   !----local variables--------
   INTEGER(KIND=8) :: i,j,k,jl,l0
   INTEGER(KIND=8) :: iStart(3),iEnd(3),N(3)
   INTEGER(KIND=8) :: planeSize,metricOffset 
   INTEGER(KIND=8) :: jBufferOffset,kBufferOffset
   REAL(KIND=8)    :: xi,xiMag,ucon,velo
   
   iStart = 1
   iEnd   = 1
   N      = 0
   planeSize = 0
   
   DO i = 1,numDim
      iStart(i) = opInterval(2*i - 1)
      iEnd(i)   = opInterval(2*i)
      N(i)      = bufferSizes(i)
   END DO
   
   planeSize = N(1)*N(2)
   metricOffset = (dir-1) * numDim * numPointsBuffer
   
   ! ... compute the wave speeds for flux splitting    
   DO K = iStart(3),iEnd(3)
      kBufferOffset = (K-1)*planeSize
      DO J = iStart(2),iEnd(2)
         jBufferOffset = kBufferOffset+(J-1)*N(1)
         DO I = iStart(1),iEnd(1)
            l0 = kBufferOffset + jBufferOffset + I
            
            uCon = 0_8
            xiMag = 0_8
            
            DO jl = 1, numDim
               velo = velocityBuffer(l0 + (jl-1)*numPointsBuffer)
               xi = gridMetric(metricOffset + l0 + (jl-1)*numPointsBuffer)*jacobianDeterminant(l0)
               ucon = ucon + velo*xi
               xiMag = xiMag + xi*xi
            END DO
            
            xiMag = SQRT(xiMag)
            xiMag = xiMag*sosBuffer(l0)
            
            charVel(1) = ABS(ucon)
            charVel(2) = ABS(ucon + xiMag)
            charVel(3) = ABS(ucon - xiMag)
            
         END DO
      END DO
   END DO
   
   RETURN
   
 END SUBROUTINE ComputeCharacteristicVelocities

  subroutine Characteristic_Velocity_Old(ws_stencil,width,iLeft,iRight,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,&
       pressureBuffer,temperatureBuffer,sosBuffer,map,p_map,smap,dir,numDim,nauxVars,numEquations,numPointsBuffer,gridMetric,&
       jacobianDeterminant,eosInfo,eosNonDimen,eosGasParams,haveAxis) ! PPP - done

    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim, width, dir, map, numEquations, nauxVars, iLeft, iRight
    INTEGER(KIND=8) :: numPointsBuffer,eosInfo(NUMGASINFOS)
    integer(KIND=8) :: p_map(:)
    REAL(KIND=8) :: smap(:,:)
    REAL(KIND=8),    INTENT(IN)         :: eosGasParams(NUMGASPARAMS*(nauxVars+1))
    REAL(KIND=8),    INTENT(IN)         :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: sosBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(nauxVars*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT)        :: ws_stencil(width+1,numEquations)
    REAL(KIND=8)    :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8)    :: gridMetric(numDim*numDim*numPointsBuffer)
    INTEGER(KIND=4) :: haveAxis

    !----local variables--------
    INTEGER(KIND=8) :: j, jl, offset, jj,k, eosContext
    REAL(KIND=8)    :: sound_speed, XI_X, XI_Y, XI_Z, ucon
    REAL(KIND=8)    :: uL(numDim+2+nAuxVars),dvL(numDim+2),dpL(2+nAuxVars)
    
    eosContext = eosInfo(1)

    ! ... compute the wave speeds for flux splitting
    do j = map - iLeft, map + IRight
 
       jl = j - (map - iLeft) +1
 
      CALL Conserved_Variables_Single_Point(uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(j), &
            smap(j,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)

      dvL(1) = pressureBuffer(p_map(j))
      dvL(2) = temperatureBuffer(p_map(j))
      sound_speed = sosBuffer(p_map(j))
      do k = 1,numDim
        dvL(k+2) = rhoVbuffer(p_map(j)+(k-1)*numPointsBuffer)/rhoBuffer(p_map(j))
      end do
 
      !CALL GASDP(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1), &
           !uL(1),dvL(1),dpL(1))
      !CALL GASSOUNDSPEED(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
           !uL(1),dvL(1),dpL(1),sound_speed)

      offset = (dir-1) * numDim * numPointsBuffer
      !if (haveAxis==1) then
      !      XI_X = gridMetric(offset + p_map(j)) * jacobianDeterminant(p_map(j)) *&
      !             smap(j,1)
      !else
      XI_X = gridMetric(offset + p_map(j)) * jacobianDeterminant(p_map(j))
      !end if
      XI_Y = gridMetric(offset + p_map(j)+ numPointsBuffer) * jacobianDeterminant(p_map(j))
      
      if (haveAxis==1) then
         ucon = rhoVBuffer(p_map(j)) * XI_X * smap(j,1) + &
              rhoVBuffer(p_map(j) + numPointsBuffer) * XI_Y
         
      else
         ucon = rhoVBuffer(p_map(j)) * XI_X + &
              rhoVBuffer(p_map(j) + numPointsBuffer) * XI_Y
      end if
      
      XI_Z = 0.0_8
      if (numDim >= 3) then
        XI_Z = gridMetric(offset + p_map(j)+ 2 * numPointsBuffer) * jacobianDeterminant(p_map(j))
        ucon = ucon + rhoVBuffer(p_map(j) + 2 * numPointsBuffer) * XI_Z
      end if
      ucon = ucon / rhoBuffer(p_map(j))

      ws_stencil(jl,1:numDim) = abs(ucon)
      ws_stencil(jl,numDim+1) = abs(ucon + sound_speed * sqrt(XI_X**2 + XI_Y**2 + XI_Z**2))
      ws_stencil(jl,numDim+2) = abs(ucon - sound_speed * sqrt(XI_X**2 + XI_Y**2 + XI_Z**2))

      do jj = 1, nAuxVars
        ws_stencil(jl,jj+numDim+2) = abs(ucon)
      enddo
    end do

    return

  end subroutine Characteristic_Velocity_Old ! PPP - done
  
  subroutine Apply_WENO_JS_Dissipation(dir,p_map,smap,numDim,numEquations,f_face_1d,index_weno, &
       WENO_alpha, nAuxVars, WENO_STRONGSHOCKS, numPointsBuffer,                     &
       rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer, pressureBuffer,temperatureBuffer,sosBuffer,  &
       gridMetric,jacobianDeterminant,eosInfo,eosNonDimen,eosGasParams,gridType,haveAxis,&
       d_local, c_local, u_local)
    
    USE SATUtil 

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN) :: numDim, nAuxVars, dir, numEquations, gridType
    REAL(KIND=8),    INTENT(IN) :: smap(:,:)
    INTEGER(KIND=8), INTENT(IN) :: p_map(:)
    INTEGER(KIND=4), INTENT(IN) :: haveAxis    
    INTEGER(KIND=8), INTENT(IN) :: numPointsBuffer, index_weno(2),eosInfo(NUMGASINFOS)
    REAL(kind=8),    INTENT(IN) :: WENO_alpha
    INTEGER(KIND=4), INTENT(IN) :: WENO_STRONGSHOCKS
    REAL(KIND=8),    INTENT(IN)         :: eosGasParams(NUMGASPARAMS*(nauxVars+1))
    REAL(KIND=8),    INTENT(IN)         :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: sosBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(nAuxVars*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT)        :: f_face_1d(:,:)

    type(dissipation_local) :: d_local
    type(characteristic_local) :: c_local
    type(upwinding_local) :: u_local

    !-----local variables--------------
    integer(KIND=4) :: sign_flag, i, j, m, jl, ii, iii, offset, mm
    integer(KIND=4), parameter :: iLeft  = 2
    integer(KIND=4), parameter :: iRight = 3
    INTEGER(KIND=8) :: eosContext
    REAL(KIND=8) :: JAC_freeze
    REAL(KIND=8) :: XI_X, XI_Y, XI_Z, norm(3),xh
    REAL(KIND=8) :: sound_speed
    REAL(KIND=8) :: cRoe, result1, result2

    eosContext = eosInfo(1)

    do i = index_weno(1), index_weno(2)

       ! ... compute the wave speeds for flux splitting
      CALL Characteristic_Velocity(d_local%ws_stencil,width,iLeft,iRight,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,        &
           pressureBuffer,temperatureBuffer,sosBuffer,i,p_map,smap,dir,numDim,nauxVars,numEquations,numPointsBuffer,gridMetric, &
           jacobianDeterminant,eosInfo,eosNonDimen,eosGasParams,haveAxis,c_local)
       
       ! ... compute the wave speeds and eigenvectors for flux splitting-based on Roe-average
       ! ... set up the right and left state
       CALL Conserved_Variables_Single_Point(d_local%uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(i), &
            smap(i,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)

       d_local%dvL(1) = pressureBuffer(p_map(i))
       d_local%dvL(2) = temperatureBuffer(p_map(i))
       if(haveAxis == 1) then
          d_local%dvL(3) = (rhoVbuffer(p_map(i))/rhoBuffer(p_map(i)))*smap(i,1)
       else
          d_local%dvL(3) = rhoVbuffer(p_map(i))/rhoBuffer(p_map(i))
       endif
       do j = 2,numDim
          d_local%dvL(j+2) = rhoVbuffer(p_map(i)+(j-1)*numPointsBuffer)/rhoBuffer(p_map(i))
       end do

       CALL Conserved_Variables_Single_Point(d_local%uR,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(i+1), &
            smap(i+1,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)

       d_local%dvR(1) = pressureBuffer(p_map(i+1))
       d_local%dvR(2) = temperatureBuffer(p_map(i+1))

       if(haveAxis == 1) then
          d_local%dvR(3) = (rhoVbuffer(p_map(i+1))/rhoBuffer(p_map(i+1)))*smap(i+1,1)
       else
          d_local%dvR(3) = rhoVbuffer(p_map(i+1))/rhoBuffer(p_map(i+1))
       endif

       do j = 2,numDim
         d_local%dvR(j+2) = rhoVbuffer(p_map(i+1)+(j-1)*numPointsBuffer)/rhoBuffer(p_map(i+1))
       end do

       ! ... freeze the metrics
       CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,p_map(i),gridType,&
            gridMetric,d_local%pointMetricL)
       CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,p_map(i+1),gridType,&
            gridMetric,d_local%pointMetricR) ! PPP: no need for axi, left of centerline

       JAC_freeze = 0.5_8 * (jacobianDeterminant(p_map(i)) + jacobianDeterminant(p_map(i+1)))
       d_local%pointMetric = 0.5_8 * (d_local%pointMetricL + d_local%pointMetricR)
       XI_X = d_local%pointMetric(1) * JAC_freeze
       XI_Y = d_local%pointMetric(2) * JAC_freeze
       XI_Z = 0.0_8
       if (numDim >= 3) then
          XI_Z = d_local%pointMetric(3) * JAC_freeze
       end if
       norm = (/ XI_X, XI_Y, XI_Z /)
       xh = sqrt((XI_X*XI_X)+(XI_Y*XI_Y)+(XI_Z*XI_Z))

       CALL PointEigenVectors(numDim,d_local%pointMetric,d_local%metricUnitVectors,d_local%metricMags)
       
       CALL PointRoeState2(numDim,nAuxVars,d_local%uL,d_local%dvL,d_local%uR,d_local%dvR,d_local%uRoe)

       d_local%dvRoe(2) = (d_local%dvL(2) + d_local%dvR(2))/2.0_8
       CALL GASEOSROE(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
            d_local%uRoe(1),d_local%dvRoe(1),d_local%dpRoe(1),cRoe)
       CALL EigenMatrices(numDim,nAuxVars,d_local%uRoe,d_local%dvRoe,d_local%dpRoe,cRoe,xh, &
            d_local%metricUnitVectors,d_local%tmat,d_local%tinv,d_local%lambda) 

       do m = 1, numEquations
          d_local%ws_stencil(width+1,m) = abs(d_local%Lambda(m,m))
       end do
       
       do m = 1, numEquations
          d_local%ws_max_local(m) = WENO_alpha * maxval(d_local%ws_stencil(:,m))
       end do
       
       if (WENO_STRONGSHOCKS == 1) d_local%ws_max_local(:) = maxval(d_local%ws_max_local)
       
       ! ... compute the fluxes that have frozen metrics
       !XI_TAU_freeze = 0.0_8 !0.5_8 * (XI_TAU(p_map(i),dir) + XI_TAU(p_map(i+1),dir))
       
       ! ... compute the inviscid fluxes, now the fluxes have the frozen metrics
       CALL Frozen_Incviscid_Flux(d_local%f_stencil,width,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,& 
            pressureBuffer,i,p_map,smap,numDim,nauxVars,numEquations,numPointsBuffer,d_local%pointMetric(1:numDim),haveAxis)
       
       ! ... also freeze the metrics of the conservative variables
       do j = i-2, i+3
          jl = j - (i-2) + 1
          CALL Conserved_Variables_Single_Point(d_local%uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(j), &
               smap(j,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)
          d_local%cv_stencil(jl,:) = d_local%uL(:) / JAC_freeze
       end do
       
       ! ... project the solution and physical flux into the right eigenvector space
       do j = i-2, i+3
          jl = j - (i-2) + 1
          do m = 1, numEquations
             result1 = 0.0_8
             result2 = 0.0_8
             do mm = 1,numEquations
                result1  = result1 + d_local%Tinv(m,mm)*d_local%cv_stencil(jl,mm)
                result2  = result2 + d_local%Tinv(m,mm)*d_local%f_stencil(jl,mm)
             end do
             d_local%cv_stencil_char(jl,m) = result1
             d_local%f_stencil_char(jl,m)  = result2
          end do
       end do

              
       ! ... local Lax-Friedrichs flux splitting
       do m = 1, numEquations
          d_local%f_stencil_char_pos(:,m) = &
            0.5_8 * (d_local%f_stencil_char(:,m) + d_local%ws_max_local(m) * d_local%cv_stencil_char(:,m))
          d_local%f_stencil_char_neg(:,m) = &
            0.5_8 * (d_local%f_stencil_char(:,m) - d_local%ws_max_local(m) * d_local%cv_stencil_char(:,m))
       end do
       
       ! ... apply the WENO_JS reconstruction operator and sum the pos and neg flux
       ! ... the minus sign of the positive upwinding flux is taken care of in Dissipation_Part_By_Upwinding_Flux 
       sign_flag = 1
       Call Dissipation_Part_By_Upwinding_Flux(d_local%f_stencil_char_pos,dir,numDim, &
            numEquations,width,sign_flag,d_local%pointMetric(1:numDim),d_local%f_face_1d_char_pos, u_local)
       
       sign_flag = -1
       Call Dissipation_Part_By_Upwinding_Flux(d_local%f_stencil_char_neg,dir,numDim,numEquations,&
            width,sign_flag,d_local%pointMetric(1:numDim),d_local%f_face_1d_char_neg, u_local)
       
       d_local%f_face_1d_char(:) = d_local%f_face_1d_char_pos(:) + d_local%f_face_1d_char_neg(:) 
       
       ! ... project the flux back
      do m = 1, numEquations
         f_face_1d(i,m) = 0.0_8
         do j = 1, numEquations
           f_face_1d(i,m) = f_face_1d(i,m) + d_local%Tmat(m,j)*d_local%f_face_1d_char(j)
         end do
      end do
    end do

    return

  end subroutine Apply_WENO_JS_Dissipation

  subroutine Apply_WENO_JS_Dissipation_Old(dir,p_map,smap,numDim,numEquations,          &
       f_face_1d,index_weno,WENO_alpha, nAuxVars, WENO_STRONGSHOCKS, numPointsBuffer,   &
       rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer, pressureBuffer, temperatureBuffer, &
       sosBuffer,gridMetric,jacobianDeterminant,eosInfo,eosNonDimen,eosGasParams,gridType,haveAxis)
    
    USE SATUtil 
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN) :: numDim, nAuxVars, dir, numEquations, gridType
    INTEGER(KIND=8) :: p_map(:)
    REAL(KIND=8),    INTENT(IN)    :: smap(:,:)
    INTEGER(KIND=4) :: haveAxis    
    INTEGER(KIND=8) :: numPointsBuffer, index_weno(2),eosInfo(NUMGASINFOS)
    REAL(KIND=8),    INTENT(IN)         :: eosGasParams(NUMGASPARAMS*(nAuxVars+1))
    REAL(KIND=8),    INTENT(IN)         :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: sosBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(nAuxVars*numPointsBuffer)
    REAL(KIND=8)    :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8)    :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(kind=8)          :: WENO_alpha
    INTEGER(KIND=4) :: WENO_STRONGSHOCKS
    REAL(KIND=8), INTENT(OUT) :: f_face_1d(:,:)

    !-----local variables--------------
    integer(KIND=4) :: sign_flag, i, j, m, jl, ii, iii, offset
    integer(KIND=4), parameter :: iLeft  = 2
    integer(KIND=4), parameter :: iRight = 3
    INTEGER(KIND=8) :: eosContext
    REAL(KIND=8) :: cv_stencil(width,numEquations), f_stencil(width,numEquations), ws_stencil(width+1,numEquations)
    REAL(KIND=8) :: ws_max_local(numEquations), UVWhat(width)
    REAL(KIND=8) :: f_stencil_char(width,numEquations), cv_stencil_char(width,numEquations)
    REAL(KIND=8) :: JAC_freeze
    REAL(KIND=8) :: f_stencil_char_pos(width,numEquations), f_stencil_char_neg(width,numEquations)
    REAL(KIND=8) :: f_face_1d_char(numEquations), f_face_1d_char_pos(numEquations), f_face_1d_char_neg(numEquations)
    REAL(KIND=8) :: uL(numEquations), uR(numEquations), Tmat(numEquations,numEquations) 
    REAL(KIND=8) :: Tinv(numEquations,numEquations), Lambda(numEquations,numEquations)
    REAL(KIND=8) :: sound_speed,dvL(numDim+2),dvR(numDim+2)
    REAL(KIND=8) :: XI_X, XI_Y, XI_Z, norm(3),xh
    REAL(KIND=8) :: uRoe(numDim+2+nAuxVars),dvRoe(numDim+2),dpRoe(2+nAuxVars),cRoe
    REAL(KIND=8) :: pointMetricR(numDim*numDim),pointMetricL(numDim*numDim),pointMetric(numDim*numDim)
    REAL(KIND=8) :: metricUnitVectors(numDim*numDim),metricMags(numDim)
    REAL(KIND=8) :: Ivec(numDim+2,numdim+2)

    type(upwinding_local) :: u_local

    eosContext = eosInfo(1)

    call allocate_upwinding_local(u_local, numEquations)

    do i = index_weno(1), index_weno(2)

       ! ... compute the wave speeds for flux splitting
      CALL Characteristic_Velocity_Old(ws_stencil,width,iLeft,iRight,rhoBuffer,rhoVBuffer,    &
           rhoEBuffer,scalarBuffer,pressureBuffer,temperatureBuffer,sosBuffer,i,p_map,smap,dir,numDim,nauxVars,           &
           numEquations,numPointsBuffer,gridMetric,jacobianDeterminant,eosInfo,eosNonDimen,eosGasParams,haveAxis)
      ! ... compute the wave speeds and eigenvectors for flux splitting-based on Roe-average
      ! ... set up the right and left state
       CALL Conserved_Variables_Single_Point(uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(i), &
            smap(i,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)
       CALL Conserved_Variables_Single_Point(uR,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(i+1), &
            smap(i+1,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)

       dvL(1) = pressureBuffer(p_map(i))
       dvL(2) = temperatureBuffer(p_map(i))
       DO j = 1,numDim
          dvL(j+2) = rhoVbuffer(p_map(i)+(j-1)*numPointsBuffer)/rhoBuffer(p_map(i))
       END DO
          
       dvR(1) = pressureBuffer(p_map(i+1))
       dvR(2) = temperatureBuffer(p_map(i+1))
       DO j = 1,numDim
          dvR(j+2) = rhoVbuffer(p_map(i+1)+(j-1)*numPointsBuffer)/rhoBuffer(p_map(i+1))
       END DO
       !       ENDIF

       ! ... freeze the metrics
       CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,p_map(i),gridType,&
            gridMetric,pointMetricL)
       CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,p_map(i+1),gridType,&
            gridMetric,pointMetricR) ! PPP: no need for axi, left of centerline
       JAC_freeze = 0.5_8 * (jacobianDeterminant(p_map(i)) + jacobianDeterminant(p_map(i+1)))
       pointMetric = 0.5_8 * (pointMetricL + pointMetricR)
       XI_X = pointMetric(1) * JAC_freeze
       XI_Y = pointMetric(2) * JAC_freeze
       XI_Z = 0.0_8
       if (numDim >= 3) then
          XI_Z = pointMetric(3) * JAC_freeze
       end if
       norm = (/ XI_X, XI_Y, XI_Z /)
       xh = sqrt((XI_X*XI_X)+(XI_Y*XI_Y)+(XI_Z*XI_Z)) ! PPP: good up to here

       CALL PointEigenVectors(numDim,pointMetric,metricUnitVectors,metricMags)
       
       CALL PointRoeState2(numDim,nAuxVars,uL,dvL,uR,dvR,uRoe)

       dvRoe(2) = (dvL(2) + dvR(2))/2.0_8
       CALL GASEOSROE(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
            uRoe(1),dvRoe(1),dpRoe(1),cRoe)
       CALL EigenMatrices(numDim,nAuxVars,uRoe,dvRoe,dpRoe,cRoe,xh, &
            metricUnitVectors,tmat,tinv,lambda) 

       do m = 1, numEquations
          ws_stencil(width+1,m) = abs(Lambda(m,m))
       end do
       
       do m = 1, numEquations
          ws_max_local(m) = WENO_alpha * maxval(ws_stencil(:,m))
       end do
       
       if (WENO_STRONGSHOCKS == 1) ws_max_local(:) = maxval(ws_max_local)
       
       ! ... compute the fluxes that have frozen metrics
       !XI_TAU_freeze = 0.0_8 !0.5_8 * (XI_TAU(p_map(i),dir) + XI_TAU(p_map(i+1),dir))
       
       ! ... compute the inviscid fluxes, now the fluxes have the frozen metrics
       CALL Frozen_Incviscid_Flux(f_stencil,width,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,& 
            pressureBuffer,i,p_map,smap,numDim,nauxVars,numEquations,numPointsBuffer,pointMetric(1:numDim),haveAxis)
       
       ! ... also freeze the metrics of the conservative variables
       do j = i-2, i+3
          jl = j - (i-2) + 1
          CALL Conserved_Variables_Single_Point(uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(j), &
               smap(j,:),numDim,nauxVars,numEquations,numPointsBuffer,haveAxis)
          cv_stencil(jl,:) = uL(:) / JAC_freeze
       end do
       
       ! ... project the solution and physical flux into the right eigenvector space
       do j = i-2, i+3
          jl = j - (i-2) + 1
          do m = 1, numEquations
             cv_stencil_char(jl,m) = dot_product(Tinv(m,:),cv_stencil(jl,:))
             f_stencil_char(jl,m) = dot_product(Tinv(m,:),f_stencil(jl,:))
          end do
       end do
              
       ! ... local Lax-Friedrichs flux splitting
       do m = 1, numEquations
          f_stencil_char_pos(:,m) = 0.5_8 * (f_stencil_char(:,m) + ws_max_local(m) * cv_stencil_char(:,m))
          f_stencil_char_neg(:,m) = 0.5_8 * (f_stencil_char(:,m) - ws_max_local(m) * cv_stencil_char(:,m))
       end do
       
       ! ... apply the WENO_JS reconstruction operator and sum the pos and neg flux
       ! ... the minus sign of the positive upwinding flux is taken care of in Dissipation_Part_By_Upwinding_Flux 
       sign_flag = 1
       Call Dissipation_Part_By_Upwinding_Flux(f_stencil_char_pos,dir,numDim, &
            numEquations,width,sign_flag,pointMetric(1:numDim),f_face_1d_char_pos, u_local)
       
       sign_flag = -1
       Call Dissipation_Part_By_Upwinding_Flux(f_stencil_char_neg,dir,numDim,numEquations,&
            width,sign_flag,pointMetric(1:numDim),f_face_1d_char_neg, u_local)
       
       f_face_1d_char(:) = f_face_1d_char_pos(:) + f_face_1d_char_neg(:) 
       
       ! ... project the flux back
      do m = 1, numEquations
         f_face_1d(i,m) = 0.0_8
         do j = 1, numEquations
           f_face_1d(i,m) = f_face_1d(i,m) + Tmat(m,j)*f_face_1d_char(j)
         end do
      end do
    end do

    call deallocate_upwinding_local(u_local)

    return

  end subroutine Apply_WENO_JS_Dissipation_Old



  subroutine Dissipation_Part_By_Upwinding_Flux(f_stencil_char_sign,dir,numDim, &
                           numEquations,width,sign_flag,MT1_freeze,f_face_1d_char, u_local)

    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim, numEquations, width
    REAL(KIND=8)    :: f_stencil_char_sign(width,numEquations)
    INTEGER(KIND=4) :: dir, sign_flag
    REAL(KIND=8)    :: MT1_freeze(numDim)
    REAL(KIND=8)    :: f_face_1d_char(numEquations)    

    type(upwinding_local) :: u_local

    !-----local variables-----------------
    INTEGER(KIND=8)    :: m
    INTEGER, parameter :: p = 2
    REAL(KIND=8)       :: eps, alpha_sum

    ! ... flip order of array if in the negative direction
    select case (sign_flag)
    case (1)
      u_local%f_stencil_char(:,:) = f_stencil_char_sign(:,:)
    case (-1)
      do m = 1, width
        u_local%f_stencil_char(m,:) = f_stencil_char_sign(width-m+1,:)
      end do
    end select

    ! ... compute the weights of the stencils
    u_local%C(1) = 0.1_8
    u_local%C(2) = 0.6_8
    u_local%C(3) = 0.3_8

    eps = 0.0_8
    do m = 1, numDim
      eps = eps + MT1_freeze(m)**2
    enddo
    eps = eps * 0.000001_8

    do m = 1, numEquations
      u_local%IS(1,m) = 0.25_8 * (u_local%f_stencil_char(1,m) - &
        4.0_8*u_local%f_stencil_char(2,m) + 3.0_8*u_local%f_stencil_char(3,m))**2 + &
        13.0_8 / 12.0_8 * (u_local%f_stencil_char(1,m) - 2.0_8*u_local%f_stencil_char(2,m) + &
        u_local%f_stencil_char(3,m))**2
      u_local%IS(2,m) = 0.25_8 * (u_local%f_stencil_char(2,m) - &
        u_local%f_stencil_char(4,m))**2 + 13.0_8 / 12.0_8 * (u_local%f_stencil_char(2,m) - &
        2.0_8*u_local%f_stencil_char(3,m) + u_local%f_stencil_char(4,m))**2
      u_local%IS(3,m) = 0.25_8 * (3.0_8*u_local%f_stencil_char(3,m) - &
        4.0_8*u_local%f_stencil_char(4,m) + u_local%f_stencil_char(5,m))**2 + &
        13.0_8 / 12.0_8 * (u_local%f_stencil_char(3,m) - 2.0_8*u_local%f_stencil_char(4,m) + &
        u_local%f_stencil_char(5,m))**2
    end do

    do m = 1, numEquations
      u_local%alpha(1,m) = u_local%C(1) / (u_local%IS(1,m) + eps)**p
      u_local%alpha(2,m) = u_local%C(2) / (u_local%IS(2,m) + eps)**p
      u_local%alpha(3,m) = u_local%C(3) / (u_local%IS(3,m) + eps)**p
    end do

    do m = 1, numEquations
      alpha_sum = u_local%alpha(1,m) + u_local%alpha(2,m) + u_local%alpha(3,m)
      u_local%w(1,m) = u_local%alpha(1,m) / alpha_sum
      u_local%w(2,m) = u_local%alpha(2,m) / alpha_sum
      u_local%w(3,m) = u_local%alpha(3,m) / alpha_sum
    end do

    ! ... compute the dissipation part by positive/negative upwinding flux
    do m = 1, numEquations
      u_local%tp(1,m) = u_local%f_stencil_char(1,m) - 3.0_8 * u_local%f_stencil_char(2,m) &
        + 3.0_8 * u_local%f_stencil_char(3,m) - u_local%f_stencil_char(4,m)
      u_local%tp(2,m) = u_local%f_stencil_char(2,m) - 3.0_8 * u_local%f_stencil_char(3,m) &
        + 3.0_8 * u_local%f_stencil_char(4,m) - u_local%f_stencil_char(5,m)
      u_local%tp(3,m) = u_local%f_stencil_char(3,m) - 3.0_8 * u_local%f_stencil_char(4,m) &
        + 3.0_8 * u_local%f_stencil_char(5,m) - u_local%f_stencil_char(6,m)
      f_face_1d_char(m) = ((20.0_8 * u_local%w(1,m) - 1.0_8) * u_local%tp(1,m) &
        - (10.0_8 * (u_local%w(1,m) + u_local%w(2,m))- 5.0_8) * u_local%tp(2,m) + u_local%tp(3,m)) / 60.0_8
    end do

    return

  end subroutine Dissipation_Part_By_Upwinding_Flux

  ! ... Reduced-order MUSCL-type boundary implementation- added by Pooya Movahed 2018
  subroutine NS_RHS_MUSCL(numDim,bufferSizes,numPointsBuffer,normDir,sconn,opInterval,bufferInterval, &
       gridType,gridMetric,jacobianDeterminant,bcParams,gasParams,numScalars,                         &
       rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,pressureBuffer,temperatureBuffer,sosBuffer,       &
       rhoRHSBuffer,rhoVRHSBuffer, rhoERHSBuffer, scalarRHSBuffer,                                    &
       WENO_alpha,wenoOptions,eosInfo,eosNonDimen,eosGasParams)

    USE SATUtil

    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim,numscalars,normDir,gridType,wenoOptions(2)
    INTEGER(KIND=8) :: numPointsBuffer,bufferSizes(numDim),eosInfo(NUMGASINFOS)
    INTEGER(KIND=8), INTENT(IN)      :: opInterval(2*numDim)     ! patch interval
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim) ! buffer interval
    REAL(KIND=8), INTENT(IN)         :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: bcParams(5),gasParams(5)
    REAL(KIND=8), INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: sosBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN), TARGET :: scalarBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: eosGasParams(NUMGASPARAMS*(numScalars+1))
    REAL(KIND=8),    INTENT(IN)         :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8), INTENT(INOUT)         :: rhoRHSBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(INOUT), TARGET :: rhoVRHSBuffer(numDim*numPointsBuffer)
    REAL(KIND=8), INTENT(INOUT)         :: rhoERHSBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(INOUT), TARGET :: scalarRHSBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8) :: WENO_alpha

    !-----local variables--------------
    INTEGER(KIND=4) :: WENO_STRONGSHOCKS
    INTEGER(KIND=4) :: ppax, haveAxis
    INTEGER(KIND=8) :: lindex,rindex, eosContext
    INTEGER(KIND=8) :: index_weno(2)
    INTEGER(KIND=4) :: i,iLeft,iRight,iStart,sgn,dir,width,j,jl,m,offset,numEquations
    INTEGER(KIND=4) :: ir,ic,N(3),is_unique(3),ie_unique(3),is(3),ie(3)
    INTEGER(KIND=4) :: iloop(2),offset1(3),offset2(3),patch_is(3),patch_ie(3)
    INTEGER(KIND=4), parameter :: Npencil = 4
    INTEGER(KIND=4), parameter :: NpencilW = 6
    INTEGER(KIND=8), parameter :: WENOWIDTH =6
    INTEGER(KIND=8) :: p_map(NpencilW),p_map2(NpencilW),p_map_local(NpencilW),p_map3(NpencilW)
    INTEGER(KIND=4) :: sconn(numPointsBuffer)
    REAL(KIND=8)    :: smap(NpencilW,2)

    REAL(KIND=8) :: ws_max_local(numDim+2+numScalars)
    REAL(KIND=8) :: cv_stencil(NpencilW,numDim+2+numScalars)
    REAL(KIND=8) :: cv_stencil_local(NpencilW,numDim+2+numScalars)
    REAL(KIND=8) :: ws_stencil(NpencilW+1,numDim+2+numScalars)
    REAL(KIND=8) :: f_stencil(NpencilW,numDim+2+numScalars)
    REAL(KIND=8) :: f_stencil_char(NpencilW,numDim+2+numScalars)
    REAL(KIND=8) :: cv_stencil_char(NpencilW,numDim+2+numScalars)
    REAL(KIND=8) :: MT1_freeze(numDim), JAC_freeze, XI_TAU_freeze
    REAL(KIND=8) :: f_face_1d_char(numDim+2+numScalars)
    REAL(KIND=8) :: f_face_1d_char_pos(numDim+2+numScalars)
    REAL(KIND=8) :: f_face_1d_char_neg(numDim+2+numScalars)
    REAL(KIND=8) :: f_stencil_char_pos(NpencilW,numDim+2+numScalars)
    REAL(KIND=8) :: f_stencil_char_neg(NpencilW,numDim+2+numScalars)
    REAL(KIND=8) :: uL(numDim+2+numScalars), uR(numDim+2+numScalars)
    REAL(KIND=8) :: Tmat(numDim+2+numScalars,numDim+2+numScalars)
    REAL(KIND=8) :: Tinv(numDim+2+numScalars,numDim+2+numScalars)
    REAL(KIND=8) :: Lambda(numDim+2+numScalars,numDim+2+numScalars)
    REAL(KIND=8) :: XI_X,XI_Y,XI_Z,norm(3),xh
    REAL(KIND=8) :: f_face_1d(3,numDim+2+numScalars)
    REAL(KIND=8) :: rhoBuffer_local(NpencilW),rhoEBuffer_local(NpencilW)
    REAL(KIND=8) :: gridMetric_local(numDim*numDim*NpencilW),scalarBuffer_local(numScalars*NpencilW)
    REAL(KIND=8) :: rhoVBuffer_local(numDim*NpencilW),jacobianDeterminant_local(NpencilW)
    REAL(KIND=8) :: pressureBuffer_local(NpencilW)
    REAL(KIND=8) :: temperatureBuffer_local(NpencilW)
    REAL(KIND=8) :: sosBuffer_local(NpencilW)
    REAL(KIND=8) :: flux_temp_1d(NpencilW+1,numDim+2+numScalars)

    REAL(KIND=8) :: i1
    REAL(KIND=8) :: dvL(numDim+2),dvR(numDim+2)
    REAL(KIND=8) :: uRoe(numDim+2+numscalars),dvRoe(numDim+2),dpRoe(2+numscalars),cRoe
    REAL(KIND=8) :: pointMetricR(numDim*numDim),pointMetricL(numDim*numDim),pointMetric(numDim*numDim)
    REAL(KIND=8) :: metricMags(numDim),metricUnitVectors(numDim*numDim)
 

    WENO_STRONGSHOCKS = wenoOptions(1)
    ppax = wenoOptions(2)
    haveAxis = 0

    eosContext = eosInfo(1)

    numEquations = numDim+2+numScalars 
    index_weno(:) = 3
    dir = abs(normDir)
    sgn = normDir / dir
    if (sgn==1) then
      do i =1, NpencilW
        p_map_local(i) = i
      enddo
    else
      do i =1, NpencilW
        p_map_local(i) = 7 - i
      enddo
    endif

    if (sgn == 1) then
      iLeft  = 0
      iRight = NpencilW - 1 
      istart = 1
      width  = NpencilW
    else
      iLeft  = 0
      iRight = NpencilW -1
      istart = 1
      width  = NpencilW
    endif

    is(:) = 1; ie(:) = 1; N(:) = 1
    is_unique(:) = 1; ie_unique(:) = 1
    patch_is(:) = 1; patch_ie(:) = 1
    do i = 1, numDim
      patch_is(i) = opInterval((i-1)*2+1)
      patch_ie(i) = opInterval(i*2)
      is_unique(i) = bufferInterval((i-1)*2+1)
      ie_unique(i) = bufferInterval(i*2)
      ie(i) = bufferSizes(i)
      N(i)        = ie(i)
    enddo

    If ((sgn == 1).AND.(patch_is(dir)>ie_unique(dir)).or.(is_unique(dir)-patch_is(dir)>2)) then
      return
    elseif ((sgn == -1).AND.(patch_is(dir)<is_unique(dir).or.(patch_ie(dir)-ie_unique(dir)>2))) then
      return
    endif

    i1 = 1.0_8
    if (is_unique(dir)-patch_is(dir)==2) i1 = 0.0_8
    if (patch_ie(dir)-ie_unique(dir)==2) i1 = 0.0_8
    
    select case (dir)

    case (1)
      iloop(1) = 2
      iloop(2) = 3
      offset1(1) = N(1)
      offset2(1) = N(2) * N(1)
      if (sgn == 1) then
        do i = 1, NpencilW
          p_map2(i) = i + patch_is(dir)-is(dir)
        end do
      elseif (sgn == -1) then
        do i = 1, 2-NpencilW, -1
          p_map2(2-i) = patch_is(dir)-is(dir) + i
        end do
      endif

    case (2)
      iloop(1) = 1
      iloop(2) = 3
      offset1(2) = 1
      offset2(2) = N(2) * N(1)
      if (sgn == 1) then
        do i = 1, NpencilW
          p_map2(i)  = (i-1) * N(1) + 1 + (patch_is(dir)-is(dir)) * N(1)
        end do
      elseif (sgn == -1) then
        do i = 1, 2-NpencilW, -1
          p_map2(2-i)  = (i-1) * N(1) + 1 + (patch_is(dir)-is(dir)) * N(1)
        end do
      endif

    case (3)
      iloop(1) = 1
      iloop(2) = 2
      offset1(3) = 1
      offset2(3) = N(1)
      if (sgn == 1) then
        do i = 1, NpencilW
          p_map2(i) = (i-1) * N(2) * N(1) + 1 + (patch_is(dir)-is(dir)) * N(2) * N(1)
        end do
      elseif (sgn == -1) then
        do i = 1, 2-NpencilW, -1
          p_map2(2-i) = (i-1) * N(2) * N(1) + 1 + (patch_is(dir)-is(dir)) * N(2) * N(1)
        end do
      endif
    end select

    do ic = max(patch_is(iloop(2)), is_unique(iloop(2))), min(patch_ie(iloop(2)), ie_unique(iloop(2)))
      p_map3(:) = p_map2(:) + offset2(dir) * (ic - is(iloop(2)))
      do ir = max(patch_is(iloop(1)), is_unique(iloop(1))), min(patch_ie(iloop(1)), ie_unique(iloop(1)))
        p_map(:) = p_map3(:) + offset1(dir) * (ir - is(iloop(1)))
        ws_stencil = 0.0_8

        ! ... compute the wave speeds for flux splitting
        CALL Characteristic_Velocity_Old(ws_stencil,width,iLeft,iRight,rhoBuffer, &
             rhoVBuffer,rhoEBuffer,scalarBuffer,pressureBuffer,temperatureBuffer,sosBuffer, &
             istart,p_map,smap,dir,numDim,numScalars,numEquations,numPointsBuffer,&
             gridMetric,jacobianDeterminant,eosInfo,eosNonDimen,eosGasParams,haveAxis)
        
        do i = 1, 4 
           CALL Conserved_Variables_Single_Point(uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,p_map(i), &
                smap(i,:),numDim,numScalars,numEquations,numPointsBuffer,haveAxis)
           cv_stencil_Local(i,:) = uL(:)
        end do
        
!--------------Edge 1--------------------------------------------------------
        i = 1

        ! ... compute the wave speeds and eigenvectors for flux splitting-based on Roe-average
        ! ... set up the right and left state
        do j =1, numEquations
          uL(j) = cv_stencil_local(i,j)
          uR(j) = cv_stencil_local(i+1,j)
        enddo

        lindex = p_map(1)
        rindex = p_map(2)
 
        dvL(1) = pressureBuffer(lindex)
        dvL(2) = temperatureBuffer(lindex)
        do j = 1,numDim
           dvL(j+2) = rhoVbuffer(lindex+(j-1)*numPointsBuffer)/rhoBuffer(lindex)
        end do

        dvR(1) = pressureBuffer(rindex)
        dvR(2) = temperatureBuffer(rindex)
        do j = 1,numDim
           dvR(j+2) = rhoVbuffer(rindex+(j-1)*numPointsBuffer)/rhoBuffer(rindex)
        end do
        

        ! ... freeze the metrics
        CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,lindex,gridType,&
              gridMetric,pointMetricL)
        CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,rindex,gridType,&
              gridMetric,pointMetricR)
        JAC_freeze = 0.5_8 * (jacobianDeterminant(lindex) + jacobianDeterminant(rindex))
        pointMetric = 0.5_8 * (pointMetricL + pointMetricR)
        XI_X = pointMetric(1) * JAC_freeze
        XI_Y = pointMetric(2) * JAC_freeze
        XI_Z = 0.0_8
        if (numDim >= 3) then
         XI_Z = pointMetric(3) * JAC_freeze
        end if
        norm = (/ XI_X, XI_Y, XI_Z /)
        xh = sqrt((XI_X*XI_X)+(XI_Y*XI_Y)+(XI_Z*XI_Z))

        CALL PointEigenVectors(numDim,pointMetric,metricUnitVectors,metricMags)

        CALL PointRoeState2(numDim,numscalars,uL,dvL,uR,dvR,uRoe)

        dvRoe(2) = (dvR(2) + dvL(2))/2.0_8
        CALL GASEOSROE(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
                       uRoe(1),dvRoe(1),dpRoe(1),cRoe)

        CALL EigenMatrices(numDim,numscalars,uRoe,dvRoe,dpRoe,cRoe,xh, &
              metricUnitVectors,tmat,tinv,lambda)

        do j = 1, numEquations
    !      ws_stencil(NpencilW+1,j) = dabs(lambda(j,j))
          ws_max_local(j) = WENO_alpha * maxval(ws_stencil(1:4,j))
        end do

        if (WENO_STRONGSHOCKS == 1) ws_max_local(:) = maxval(ws_max_local)

        ! ... calculate the inviscid fluxes
        f_stencil = 0.0_8
        CALL InviscidFlux_Freeze(rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,scalarBuffer,p_map,dir,numDim,numScalars,&
                          numEquations,numPointsBuffer,pointMetric(1:numDim),NpencilW,3,f_stencil)

        do j = 1, 3
          do m = 1, numEquations
            cv_stencil(j,m) = cv_stencil_local(j,m) / JAC_freeze
          enddo
        enddo

        do j = 1, 3
          do m = 1, numEquations
            cv_stencil_char(j,m) = dot_product(Tinv(m,:),cv_stencil(j,:))
            f_stencil_char(j,m)  = dot_product(Tinv(m,:),f_stencil(j,:))
          end do
        end do

        ! ... local Lax-Friedrichs flux splitting
        do m = 1, numEquations
          f_stencil_char_pos(:,m) = 0.5_8 * (f_stencil_char(:,m) + ws_max_local(m) * cv_stencil_char(:,m))
          f_stencil_char_neg(:,m) = 0.5_8 * (f_stencil_char(:,m) - ws_max_local(m) * cv_stencil_char(:,m))
        end do

        ! ...  APPLY MUSCL
        if (sgn ==1) then
          CALL MUSCL_Reconstruction(f_stencil_char_neg,numEquations,NpencilW,1,2,3,-1.0_8,f_face_1d_char_neg)
          f_face_1d_char_pos(:) = f_stencil_char_pos(1,:)
        elseif (sgn == -1) then
          CALL MUSCL_Reconstruction(f_stencil_char_pos,numEquations,NpencilW,1,2,3,-1.0_8,f_face_1d_char_pos)
          f_face_1d_char_neg(:) = f_stencil_char_neg(1,:)
        endif

        f_face_1d_char(:) = f_face_1d_char_pos(:) + f_face_1d_char_neg(:)

        ! ... project the flux back
        do m = 1, numEquations
          f_face_1d(i,m) = dot_product(Tmat(m,:),f_face_1d_char(:))
        end do

!-----------------------------------Edge 2---------------------------------------------------------
        ! ... project the solution and physical flux into the right eigenvector space
        i = 2

        ! ... compute the wave speeds and eigenvectors for flux splitting-based on Roe-average
        ! ... set up the right and left state
        uL(:) = cv_stencil_local(i,:)
        uR(:) = cv_stencil_local(i+1,:)

        lindex = p_map(2)
        rindex = p_map(3)

        dvL(1) = pressureBuffer(lindex)
        dvL(2) = temperatureBuffer(lindex)
        do j = 1,numDim
           dvL(j+2) = rhoVbuffer(lindex+(j-1)*numPointsBuffer)/rhoBuffer(lindex)
        end do

        dvR(1) = pressureBuffer(rindex)
        dvR(2) = temperatureBuffer(rindex)
        do j = 1,numDim
           dvR(j+2) = rhoVbuffer(rindex+(j-1)*numPointsBuffer)/rhoBuffer(rindex)
        end do

        ! ... freeze the metrics
        CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,lindex,gridType,&
            gridMetric,pointMetricL)
        CALL BoundaryPointMetric(numDim,Dir,numPointsBuffer,rindex,gridType,&
            gridMetric,pointMetricR)
        JAC_freeze = 0.5_8 * (jacobianDeterminant(lindex) + jacobianDeterminant(rindex))
        pointMetric = 0.5_8 * (pointMetricL + pointMetricR)
        XI_X = pointMetric(1) * JAC_freeze
        XI_Y = pointMetric(2) * JAC_freeze
        XI_Z = 0.0_8
        if (numDim >= 3) then
           XI_Z = pointMetric(3) * JAC_freeze
        end if
        norm = (/ XI_X, XI_Y, XI_Z /)
        xh = SQRT(XI_X*XI_X+XI_Y*XI_Y+XI_Z*XI_Z)
        CALL PointEigenVectors(numDim,pointMetric,metricUnitVectors,metricMags)

        CALL PointRoeState2(numDim,numscalars,uL,dvL,uR,dvR,uRoe)
        dvRoe(2) = (dvR(2)+dvL(2))/2.0_8
        CALL GASEOSROE(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
                       uRoe(1),dvRoe(1),dpRoe(1),cRoe)

        CALL EigenMatrices(numDim,numscalars,uRoe,dvRoe,dpRoe,cRoe,xh, &
            metricUnitVectors,tmat,tinv,lambda)

        do j = 1, numEquations
          !ws_stencil(NpencilW+1,j) = dabs(lambda(j,j))
          ws_max_local(j) = WENO_alpha * maxval(ws_stencil(1:4,j))
        end do

        ! ... calculate the inviscid fluxes
        f_stencil = 0.0_8
        CALL InviscidFlux_Freeze(rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,scalarBuffer, &
             p_map,dir,numDim,numScalars,numEquations,numPointsBuffer,pointMetric(1:numDim),  &
             NpencilW,4,f_stencil)

        do j = 1, 4
          cv_stencil(j,:) = cv_stencil_local(j,:) / JAC_freeze
        enddo

        do j = 1, 4
          do m = 1, numEquations
            cv_stencil_char(j,m) = dot_product(Tinv(m,:),cv_stencil(j,:))
            f_stencil_char(j,m)  = dot_product(Tinv(m,:),f_stencil(j,:))
          end do
        end do

        ! ... local Lax-Friedrichs flux splitting
        do m = 1, numEquations
          f_stencil_char_pos(:,m) = 0.5_8 * (f_stencil_char(:,m) + ws_max_local(m) * cv_stencil_char(:,m))
          f_stencil_char_neg(:,m) = 0.5_8 * (f_stencil_char(:,m) - ws_max_local(m) * cv_stencil_char(:,m))
        end do

        ! ...   APPLY MUSCL
        If (sgn == 1) then
          CALL MUSCL_Reconstruction(f_stencil_char_neg,numEquations,NpencilW,2,3,4,-1.0_8,f_face_1d_char_neg)
          CALL MUSCL_Reconstruction(f_stencil_char_pos,numEquations,NpencilW,1,2,3,1.0_8,f_face_1d_char_pos)
        elseif (sgn == -1) then
          CALL MUSCL_Reconstruction(f_stencil_char_neg,numEquations,NpencilW,1,2,3,1.0_8,f_face_1d_char_neg)
          CALL MUSCL_Reconstruction(f_stencil_char_pos,numEquations,NpencilW,2,3,4,-1.0_8,f_face_1d_char_pos)
        endif

        f_face_1d_char(:) = f_face_1d_char_pos(:) + f_face_1d_char_neg(:)

        ! ... project the flux back
        do m = 1, numEquations
          f_face_1d(i,m) = dot_product(Tmat(m,:),f_face_1d_char(:))
        end do

!-----------------------------------Edge 3---------------------------------------------------------
        ! ... calculate the flux using WENO
        i = 3

        ! ... Compute the consistent part of the flux
        ! ... calculate the inviscid fluxes
        f_stencil = 0.0_8
        CALL InviscidFlux(rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,scalarBuffer,p_map,dir,numDim,numScalars,&
                          numEquations,numPointsBuffer,gridMetric,NpencilW,f_stencil)

        do j = 1, numEquations
          f_face_1d(i,j) = (f_stencil(i-2,j) - 8.0_8 * f_stencil(i-1,j) + &
                          37.0_8 * f_stencil(i,j) + f_stencil(i+3,j) -    &
                          8.0_8 * f_stencil(i+2,j) + 37.0_8 * f_stencil(i+1,j)) &
                         /60.0_8
        enddo

        ! ... Compute the dissipation part of the flux
        do j = 1, NpencilW
          rhoBuffer_local(j) = rhoBuffer(p_map(j))
          rhoEBuffer_local(j) = rhoEBuffer(p_map(j))
          pressurebuffer_local(j) = pressurebuffer(p_map(j))
          temperaturebuffer_local(j) = temperaturebuffer(p_map(j))
          sosbuffer_local(j) = sosbuffer(p_map(j))
          jacobianDeterminant_local(j) = jacobianDeterminant(p_map(j))
        enddo

        do m = 1, numDim
          do j = 1, NpencilW
            rhoVBuffer_local(j+(m-1)*NpencilW) = rhoVBuffer(p_map(j)+(m-1)*numPointsBuffer)
          enddo
        enddo

        do j = 1, NpencilW
          do m = 1, numDim*numDim
              gridMetric_local(j+(m-1)*NpencilW) = gridMetric(p_map(j)+(m-1)*numPointsBuffer)
          enddo
        enddo

        do m = 1, numScalars
          do j =1, NpencilW
            scalarBuffer_local(j+(m-1)*NpencilW) = scalarBuffer(p_map(j)+(m-1)*numPointsBuffer)
          enddo
        enddo

        flux_temp_1d = 0.0_8

        CALL Apply_WENO_JS_Dissipation_Old(dir,p_map_local,smap,numDim,numEquations,  &
             flux_temp_1d,index_weno,WENO_alpha, numScalars, WENO_STRONGSHOCKS,       &
             WENOWIDTH,rhoBuffer_local,rhoVBuffer_local,rhoEBuffer_local,             &
             scalarBuffer_local,pressureBuffer_local,temperatureBuffer_local,sosBuffer_local, &
             gridMetric_local,jacobianDeterminant_local,eosInfo,eosNonDimen,            &
             eosGasParams,gridType,haveAxis)
        
        f_face_1d(i,:) = f_face_1d(i,:) + flux_temp_1d(3,:)
!----------------------------------------------------------------------------
        ! ... update the RHS
        ! only process points with a boundary stencil
        if ( sconn(p_map(1)) .ne. 1) then
          i = 2
  
          rhoRHSBuffer(p_map(i)) = rhoRHSBuffer(p_map(i)) - jacobianDeterminant(p_map(i)) *&
                                   (f_face_1d(i,1) - f_face_1d(i-1,1)) * dble(sgn) * i1
          do m = 1, numDim
            rhoVRHSBuffer(p_map(i)+(m-1)*numPointsBuffer)= rhoVRHSBuffer(p_map(i)+(m-1)*numPointsBuffer) - &
                         jacobianDeterminant(p_map(i)) * (f_face_1d(i,1+m) - f_face_1d(i-1,1+m)) * dble(sgn) * i1
          enddo
          rhoERHSBuffer(p_map(i)) = rhoERHSBuffer(p_map(i)) - &
              jacobianDeterminant(p_map(i)) *  (f_face_1d(i,numDim+2) - f_face_1d(i-1,numDim+2)) * dble(sgn) * i1

          do m = 1, numScalars
            scalarRHSBuffer(p_map(i)+(m-1)*numPointsBuffer)= scalarRHSBuffer(p_map(i)+(m-1)*numPointsBuffer) - &
              jacobianDeterminant(p_map(i)) * (f_face_1d(i,numDim+2+m) - f_face_1d(i-1,numDim+2+m)) * dble(sgn) * i1
          enddo
!
          i = 3

          rhoRHSBuffer(p_map(i)) = rhoRHSBuffer(p_map(i)) - jacobianDeterminant(p_map(i)) *&
                                 (f_face_1d(i,1) - f_face_1d(i-1,1)) * dble(sgn)
          do m = 1, numDim
            rhoVRHSBuffer(p_map(i)+(m-1)*numPointsBuffer)= rhoVRHSBuffer(p_map(i)+(m-1)*numPointsBuffer) - &
                    jacobianDeterminant(p_map(i)) * (f_face_1d(i,1+m) - f_face_1d(i-1,1+m)) * dble(sgn)
          enddo
          rhoERHSBuffer(p_map(i)) = rhoERHSBuffer(p_map(i)) - &
              jacobianDeterminant(p_map(i)) * (f_face_1d(i,numDim+2)- f_face_1d(i-1,numDim+2)) * dble(sgn)

          do m = 1, numScalars
            scalarRHSBuffer(p_map(i)+(m-1)*numPointsBuffer)= scalarRHSBuffer(p_map(i)+(m-1)*numPointsBuffer) - &
              jacobianDeterminant(p_map(i)) * (f_face_1d(i,numDim+2+m) - f_face_1d(i-1,numDim+2+m)) * dble(sgn)
          enddo
        endif
      end do
    end do

    return

  end subroutine NS_RHS_MUSCL

  subroutine MUSCL_Reconstruction(f_input,numEquations,NpencilW,i1,i2,i3,side,f_output)

    Implicit None
    INTEGER(KIND=4) :: numEquations,NpencilW
    INTEGER(KIND=4) :: i1,i2,i3,i
    REAL(KIND=8) :: side
    REAL(KIND=8) :: slope_a,slope_b,slope_limited
    REAL(KIND=8) :: f_output(numEquations),f_input(NpencilW,numEquations)
    
    ! ...  APPLY MUSCL
    do i = 1, numEquations
      slope_a = (f_input(i3,i) - f_input(i2,i))
      slope_b = (f_input(i2,i) - f_input(i1,i))
      slope_limited = 0.25_8 * (DSIGN(1.0_8,slope_a) + DSIGN(1.0_8,slope_b))&
                                        * DMIN1 (DABS(slope_a),DABS(slope_b))
      f_output(i) = f_input(i2,i) + side * slope_limited
    enddo

    return

  end subroutine MUSCL_Reconstruction

  subroutine InviscidFlux(rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,scalarBuffer,p_map,normdir,numDim,numScalars,&
                          numEquations,numPointsBuffer,gridMetric,Npencil,f_stencil)
    Implicit None
    INTEGER(KIND=4) :: numEquations,normDir,Npencil
    INTEGER(KIND=4) :: numDim, numscalars, gridType
    INTEGER(KIND=8) :: numPointsBuffer, bufferSizes(numDim)
    REAL(KIND=8), INTENT(IN)         :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN), TARGET :: scalarBuffer(numScalars*numPointsBuffer)
    INTEGER(KIND=8), INTENT(IN)      :: p_map(:)
    REAL(KIND=8), INTENT(out) :: f_stencil(Npencil,numEquations)
    !--------------Local variables------------------------
    INTEGER(KIND=8) :: i,j,ii
    REAL(KIND=8) :: UVWhat

    do j = 1, Npencil
      i = p_map(j)
      ! ... contravarient velocity in the normal direction
      UVWhat = 0.0_8
      do ii = 1, numDim
        UVWhat = UVWhat + gridMetric((i+numPointsBuffer*((normDir-1)*numDim+(ii-1)))) * rhoVBuffer(i+numPointsBuffer*(ii-1))
      enddo
      UVWhat = UVWhat / rhoBuffer(i)
 
      ! ... Inviscid fluxes in the normal direction
      ! ... continuity
      f_stencil(j,1) = rhoBuffer(i) * UVWhat
      ! ... momentum
      do ii = 1, numDim
        f_stencil(j,1+ii) = rhoVBuffer(i+(ii-1)*numPointsBuffer) * UVWhat + &
               gridMetric((i+numPointsBuffer*((normDir-1)*numDim+(ii-1)))) * pressureBuffer(i)
      enddo
      ! ... energy
      f_stencil(j,numDim+2) = (rhoEBuffer(i) + pressureBuffer(i)) * UVWhat
      ! ... scalar
      do ii = 1, numScalars
        f_stencil(j,numDim+2+ii) = UVWhat * scalarBuffer(i+numPointsBuffer*(ii-1))
      enddo
    enddo

    return

  end subroutine InviscidFlux

  subroutine InviscidFlux_FREEZE(rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,scalarBuffer, &
       p_map,normdir,numDim,numScalars,                                                       &
       numEquations,numPointsBuffer,MT1,Npencil,width,f_stencil)

    Implicit None

    INTEGER(KIND=4) :: numEquations,normDir,Npencil,width
    INTEGER(KIND=4) :: numDim, numscalars, gridType
    INTEGER(KIND=8) :: numPointsBuffer, bufferSizes(numDim)
    REAL(KIND=8), INTENT(IN)         :: MT1(numDim)
    REAL(KIND=8), INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8), INTENT(IN), TARGET :: scalarBuffer(numScalars*numPointsBuffer)
    INTEGER(KIND=8), INTENT(IN)      :: p_map(:)
    REAL(KIND=8), INTENT(out) :: f_stencil(Npencil,numEquations)
    !--------------Local variables------------------------
    INTEGER(KIND=4) :: i,j,ii
    REAL(KIND=8) :: UVWhat

    do j = 1, width
      i = p_map(j)
      ! ... contravarient velocity in the normal direction
      UVWhat = 0.0_8
      do ii = 1, numDim
        UVWhat = UVWhat + MT1(ii) * rhoVBuffer(i+numPointsBuffer*(ii-1))
      enddo
      UVWhat = UVWhat / rhoBuffer(i)

      ! ... Inviscid fluxes in the normal direction
      ! ... continuity
      f_stencil(j,1) = rhoBuffer(i) * UVWhat
      ! ... momentum
      do ii = 1, numDim
        f_stencil(j,1+ii) = rhoVBuffer(i+(ii-1)*numPointsBuffer) * UVWhat + &
                            MT1(ii) * pressureBuffer(i)
      enddo
      ! ... energy
      f_stencil(j,numDim+2) = (rhoEBuffer(i) + pressureBuffer(i)) * UVWhat
      ! ... scalar
      do ii = 1, numScalars
        f_stencil(j,numDim+2+ii) = UVWhat * scalarBuffer(i+numPointsBuffer*(ii-1))
      enddo
    enddo

    return

  end subroutine InviscidFlux_FREEZE

  subroutine NS_BC_WENO(numDim,bufferSizes,numPointsBuffer,normDirIn,opInterval,bufferInterval, &
       gridType,gridMetric,gridJacobian,MT2,                                                    &
       bcType,bcParams,gasVars,numScalars,                                                      &
       rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,pressureBuffer,temperatureBuffer,sosBuffer, &
       rhoRHSBuffer,rhoVRHSBuffer, rhoERHSBuffer, scalarRHSBuffer,eosInfo,eosNonDimen,eosGasParams)
    
    USE SATUtil

    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim,numscalars,normDirIn,gridType,bcType
    INTEGER(KIND=8) :: numPointsBuffer,bufferSizes(numDim),eosInfo(NUMGASINFOS)
    INTEGER(KIND=8), INTENT(IN)            :: opInterval(2*numDim)     ! patch interval
    INTEGER(KIND=8), INTENT(IN)            :: bufferInterval(2*numDim) ! buffer interval
    REAL(KIND=8),    INTENT(INOUT)            :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: MT2(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)            :: gridJacobian(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: gasVars(6)
    REAL(KIND=8),    INTENT(IN)            :: bcParams(5)
    REAL(KIND=8),    INTENT(IN)            :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN),    TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: sosBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN),    TARGET :: scalarBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: eosGasParams(NUMGASPARAMS*(numScalars+1))
    REAL(KIND=8),    INTENT(IN)            :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoRHSBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: rhoVRHSBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoERHSBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: scalarRHSBuffer(numScalars*numPointsBuffer)


    INTEGER(KIND=4) :: sgn,normDir,numEquations
    INTEGER(KIND=8) :: bufferOffset, m2Offset, m2Offset2,metricOffset
    INTEGER(KIND=8) :: i,j,jl,m,offset
    INTEGER(KIND=8) :: ir,ic,N(3),is_unique(3),ie_unique(3),is(3),ie(3),iDim,iDir
    INTEGER(KIND=8) :: iloop(2),offset1(2),offset2(2),patch_is(3),patch_ie(3)
    INTEGER(KIND=4), parameter :: Npencil = 4

    real(KIND=8) :: Pinv(numDim+2+numScalars,numDim+2+numScalars),Pfor(numDim+2+numScalars,numDim+2+numScalars)
    real(KIND=8) :: cons_inviscid_flux(numDim,numDim+2+numScalars), L_vector(numDim+2+numScalars)
    real(KIND=8) :: new_normal_flux(numDim+2+numScalars)
    integer :: ND, Nc, l0, k, ii, jj, jjj, l1, l2, isign, lp, kk

    real(KIND=8)    :: norm(3), mag_norm, tang(2,3)
    real(KIND=8)    :: mag_tang, constant, del, eta, rel_pos(3), dsign, denom
    integer(KIND=8) :: Np(3)
    real(KIND=8)    :: d(5), wave_amp(5), spd_snd
    real(KIND=8)    :: un, ut(2)
    real(KIND=8)    :: norm_vec(3), tang_vec(2,3)

    real(KIND=8) :: bndry_norm(3), p_inf, u_inf, du_inf, metric_fac, T_inf, dT_inf, theta
    real(KIND=8) :: dsgn
    real(KIND=8) :: V_wall, V_wall_vec(3), V_wall_target, V_wall_target_vec(3), A_wall, drhodn, dp_inf, T_wall
    real(KIND=8) :: Uhat_alpha(3), alpha_x(3,3), alpha_t(3)
    real(KIND=8) :: SPVOL, RHO, UX, UY, UZ, PRESSURE, ENERGY,SPD_SND_INV, KE, ENTHALPY, MACH2
    REAL(KIND=8) :: V_DOT_XI
    REAL(8) :: sndspdref2, invtempref, tempref, gamref, spcGasConst
    REAL(8) :: XI_X, XI_Y, XI_Z, XI_T, bndry_h,sbpBoundaryWeight, xh
    REAL(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, pointJacobian
    REAL(8) :: XIX(4)
    REAL(8) :: alpha,beta
    integer(KIND=4) :: patch_bcType, patch_normDir, nAuxVars

    ! ... calculate one-sided derivatives, Pooya 2017
    real(KIND=8) :: ct4(2,5), ct3(2,4), ct1(1,2), ct2(1,3)
    integer(KIND=8) :: NNp,bb,kp,ip,jp,ppp
    integer(KIND=8) :: map_local(2)
    real(KIND=8) :: dflux_local(2,numDim+2+numScalars), UVWHat_local(2,numDim)
    real(KIND=8) :: flux_local(2,numDim+2+numScalars,numDim) 
    integer(KIND=4) :: metric_type,NDP2
    integer(kind=8) :: iii,lindex
    INTEGER(KIND=8) :: eosContext
    real(KIND=8)::dp_drho,dp_de,b1,b2,b3
    real(KIND=8) :: dp_drhoY(numScalars),Yvars(numScalars),z_i(numScalars)
    real(KIND=8) :: wave_spd(numDim+2+numScalars)
    Real(KIND=8) :: U2, uhat, vhat, what, ja
    REAL(KIND=8) :: dvL(numDim+2)
    REAL(KIND=8) :: dpL(2+numscalars)
    REAL(KIND=8) :: pointMetricR(numDim*numDim),pointMetricL(numDim*numDim),pointMetric(numDim*numDim)
    REAL(KIND=8) :: metricMags(numDim),metricUnitVectors(numDim*numDim)
    REAL(KIND=8) :: Lambda(numDim+2+numScalars,numDim+2+numScalars)
    REAL(KIND=8) :: uL(numDim+2+numScalars)
    ! ... simplicity
    nAuxVars = numScalars
    ND = numDim
    NDP2 = ND + 2 + nAuxVars
    eosContext = eosInfo(1)
    !----------------------------------------------------------------------
    !-----first-order accurate
    NNp=2
    ct1(1,1)=-1.0_8; ct1(1,2)=1.0_8
    !-----second-order accurate
!    NNp=3
!    ct2(1,1)=-1.5_8; ct2(1,2)=2.0_8; ct2(1,3)=-1.5_8

    !---------one sided differences-3d-order accurate-Thompson BC----------
    !---------first derivative--------------------------------------------
    !----Kennedy,Carpenter,Applied numerical mathematics1994---------------
    !NNp=4
    !ct3(1,1)=-11.0_8/6.0_8; ct3(1,2)=3.0_8; ct3(1,3)=-1.5_8; ct3(1,4)=1.0_8/3.0_8

    !ct3(2,1)=-1.0_8/3.0_8; ct3(2,2)=-0.5_8; ct3(2,3)=1.0_8; ct3(2,4)=-1.0_8/6.0_8
    !---------one sided differences-4th-order accurate-Thompson BC--------
    !---------first derivative--------------------------------------------
    !NNp=5
    !ct4(1,1)=-25.0_8/12.0_8; ct4(1,2)=4.0_8; ct4(1,3)=-3.0_8
    !ct4(1,4)=4.0_8/3.0_8; ct4(1,5)=-0.25_8

    !ct4(2,1)=-0.25_8; ct4(2,2)=-5.0_8/6.0_8; ct4(2,3)=1.5_8
    !ct4(2,4)=-0.5_8; ct4(2,5)=1.0_8/12.0_8

    !---------------------------------------------------------------------
    numEquations = numDim+2+numScalars 

    sgn = normDirIn / abs(normDirIn)
    normDir = abs(normDirIn)

    is(:) = 1; ie(:) = 1; N(:) = 1
    patch_is(:) = 1; patch_ie(:) = 1
    is_unique(:) = 1; ie_unique(:) = 1
    do i = 1, numDim
      patch_is(i) = opInterval((i-1)*2+1)
      patch_ie(i) = opInterval(i*2)
      ie(i) = bufferSizes(i)
      N(i)        = ie(i)
    enddo
!-------------also need is_unique & ie_unique--------------------
!    If ((sgn == 1).AND.(patch_is(dir)>ie_unique(dir))) then
!      return
!    elseif ((sgn == -1).AND.(patch_is(dir)<is_unique(dir))) then
!      return
!    endif

    Nc = numPointsBuffer
    N  = 1; Np = 1;
    do j = 1, ND
      N(j)  =  ie(j) -  is(j) + 1
      Np(j) = patch_ie(j) - patch_is(j) + 1
    end do

    kp=0; jp=0; ip=0

    Select case (normdir)
    case (1)
      ip=sgn
    case (2)
      jp=sgn
    case (3)
      kp=sgn
    End select

    ppp = 1

    !MJA
    !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
      !write(*,*) "patch_is(3), patch_ie(3)", patch_is(3), patch_ie(3)
      !write(*,*) "patch_is(2), patch_ie(2)", patch_is(2), patch_ie(2)
      !write(*,*) "patch_is(1), patch_ie(1)", patch_is(1), patch_ie(1)
    !endif
      
    Do k = patch_is(3), patch_ie(3)
      Do j = patch_is(2), patch_ie(2)
        Do i = patch_is(1), patch_ie(1)
               
          l0 = (k- is(3))* N(1)* N(2) + (j- is(2))* N(1) +  i- is(1)+1
          lp = (k-patch_is(3))*Np(1)*Np(2) + (j-patch_is(2))*Np(1) + (i-patch_is(1)+1)
               
          do ii=1,NNp
            map_local(ii)=((k+kp*(ii-1))- is(3))* N(1)* N(2) + &
                ((j+jp*(ii-1))- is(2))* N(1) + (i+ip*(ii-1))- is(1)+1
          enddo
               
          ! ... construct the forward and backward transformation matrices
          ! ... these matrices are used for both NSCBC and SAT
          CALL BoundaryPointMetric(numDim,normDir,numPointsBuffer,map_local(ppp),gridType,&
               gridMetric,pointMetric)
          IF (gridType <= UNIRECT) THEN
            pointJacobian = gridJacobian(1)
          ELSE IF(gridType .ge. RECTILINEAR) THEN
            pointJacobian = gridJacobian(l0)
          ENDIF          

          XI_X = pointMetric(1) * pointJacobian
          XI_Y = pointMetric(2) * pointJacobian
          XI_Z = 0.0_8
          if (numDim >= 3) then
            XI_Z = pointMetric(3) * pointJacobian
          end if
          XI_T = 0.0_8
          ! ... normalized metrics
          xh = sqrt((XI_X*XI_X)+(XI_Y*XI_Y)+(XI_Z*XI_Z))
          bndry_h = 1.0_8 / xh
          XI_X_TILDE = XI_X * bndry_h
          XI_Y_TILDE = XI_Y * bndry_h
          XI_Z_TILDE = XI_Z * bndry_h
          norm = (/ XI_X, XI_Y, XI_Z /)

          CALL PointEigenVectors(numDim,pointMetric,metricUnitVectors,metricMags)

          CALL Conserved_Variables_Single_Point(uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,map_local(ppp), &
               (/1.0_8,1.0_8/),numDim,numScalars,numEquations,numPointsBuffer,0)
 
          lindex = map_local(ppp)
          dvL(1) = pressureBuffer(lindex)
          dvL(2) = temperatureBuffer(lindex)
          SPD_SND = sosBuffer(lindex)
          DO kk = 1,numDim
             dvL(kk+2) = rhoVbuffer(lindex+(kk-1)*numPointsBuffer)/rhoBuffer(lindex)
          END DO
          
          CALL GASDP(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1), &
               uL(1),dvL(1),dpL(1))
          !CALL GASSOUNDSPEED(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
           !uL(1),dvL(1),dpL(1),SPD_SND)
          CALL EigenMatrices(numDim,nAuxVars,uL,dvL,dpL,SPD_SND,xh, &
               metricUnitVectors,Pfor,Pinv,lambda)

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5) then
              !write(*,*) "Pfor*Pinv"
              !write(*,*)  MATMUL(Pfor,Pinv)
            !endif
          !endif
          
          ENERGY = rhoEBuffer(map_local(ppp))

          ! ... primitive variables
          RHO = rhoBuffer(map_local(ppp))
          SPVOL = 1.0_8 / RHO
          UX = SPVOL * rhoVBuffer(map_local(ppp)); UZ = 0.0_8;
          UY = SPVOL * rhoVBuffer(numPointsBuffer+map_local(ppp))
          if (ND == 3) UZ = SPVOL * rhoVBuffer(2*numPointsBuffer+map_local(ppp))
          PRESSURE = pressureBuffer(map_local(ppp))
          SPD_SND_INV = 1.0_8 / SPD_SND
          KE = 0.5_8 * (UX**2 + UY**2 + UZ**2)
          ENTHALPY = SPVOL*(ENERGY + PRESSURE)
          MACH2 = (2.0_8 * KE) * SPD_SND_INV**2
                  
          ! ... wall-normal velocity
          V_DOT_XI = UX * XI_X_TILDE + UY * XI_Y_TILDE + UZ * XI_Z_TILDE
                  
          ! ... Step 1 compute the flux at each grid point, using the metrics and jacobians at these points
                  
          ! ... (1.1) compute the contravariant velocities
          UVWhat_local = 0.0_8
          if (gridType == CURVILINEAR)  then

            do ii = 1, NNp
              bb = map_local(ii)
              do jj = 1, ND
                do jjj = 1, ND
                  UVWhat_local(ii,jj) = UVWhat_local(ii,jj) + gridmetric(bb+numPointsBuffer*((jj-1)*numDim+(jjj-1)))*&
                                        rhoVbuffer(bb+(jjj-1)*numPointsBuffer)
                enddo
                UVWhat_local(ii,jj) = UVWhat_local(ii,jj) / rhoBuffer(bb)
              enddo
            enddo
 
          else if (gridType == RECTILINEAR) then
                     
            do ii = 1, NNp
              bb=map_local(ii)
              DO iDim = 1, ND
                UVWhat_local(ii,iDim) = gridMetric(bb+(iDim-1)*numPointsBuffer)* &
                                rhoVBuffer(bb+(iDim-1)*numPointsBuffer) / rhoBuffer(bb)
              END DO
            END DO

          else ! Cartesian

            do ii = 1, NNp
              bb=map_local(ii)
              DO iDim = 1, ND
                UVWhat_local(ii,iDim) = gridMetric(iDim)* &
                         rhoVBuffer(bb+(iDim-1)*numPointsBuffer) / rhoBuffer(bb)
              END DO
            END DO
                     
          end if ! gridType 
          
          ! ... (1.2) compute the inviscid fluxes
          flux_local = 0.0_8
          do ii = 1, NNp
            bb=map_local(ii)
            do jj = 1, ND
              flux_local(ii,1,jj) = rhoBuffer(bb) * UVWhat_local(ii,jj) !continuity
              flux_local(ii,ND+2,jj) = (rhoEBuffer(bb) + pressureBuffer(bb)) * UVWhat_local(ii,jj) !energy
              do jjj =1, ND
                flux_local(ii,1+jjj,jj) = rhoVBuffer(bb+(jjj-1)*numPointsBuffer) * UVWhat_local(ii,jj)+&
                   pressureBuffer(bb)*gridMetric(bb+numPointsBuffer*((jj-1)*numDim+(jjj-1))) !momentum
              enddo
              do iii = 1, nAuxVars !scalar
                flux_local(ii,ND+2+iii,jj) = scalarBuffer((iii-1)*numPointsBuffer+bb) * UVWhat_local(ii,jj)
              enddo
            enddo
          enddo

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "flux_local"
              !write(*,*) flux_local
            !endif
          !endif
         
          ! ... compute the flux derivatives   
          dflux_local = 0.0_8
          do ii = 1, NDP2
            do bb = 1, NNp
              dflux_local(1,ii)= dflux_local(1,ii) + ct1(1,bb) * sgn * flux_local(bb,ii,normdir)
            enddo
          enddo

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "dflux_local(1,:)"
              !write(*,*) dflux_local(1,:)
            !endif
          !endif
          
          ! ... compute the conservative flux variables
          cons_inviscid_flux(:,:) = 0.0_8

          ! ... one-dimensional fluxes
          cons_inviscid_flux(1,1)      = rhoVBuffer(map_local(ppp))
          cons_inviscid_flux(1,2)      = RHO * UX**2 + PRESSURE
          cons_inviscid_flux(1,ND+2)   = (ENERGY + PRESSURE) * UX

          ! ... two-dimensional fluxes
          cons_inviscid_flux(1,3)    = RHO * UY * UX
          cons_inviscid_flux(2,1)    = rhoVBuffer(map_local(ppp)+numPointsBuffer)
          cons_inviscid_flux(2,2)    = RHO * UX * UY
          cons_inviscid_flux(2,3)    = RHO * UY**2 + PRESSURE
          cons_inviscid_flux(2,ND+2) = (ENERGY + PRESSURE) * UY

          ! ... three-dimensional fluxes
          if (ND == 3) then
            cons_inviscid_flux(1,4)    = RHO * UZ * UX
            cons_inviscid_flux(2,4)    = RHO * UZ * UY
            cons_inviscid_flux(3,1)    = rhoVBuffer(map_local(ppp)+2*numPointsBuffer)
            cons_inviscid_flux(3,2)    = RHO * UX * UZ
            cons_inviscid_flux(3,3)    = RHO * UY * UZ
            cons_inviscid_flux(3,4)    = RHO * UZ * UZ + PRESSURE
            cons_inviscid_flux(3,ND+2) = (ENERGY + PRESSURE) * UZ
          end if
               
          ! ... compute the inviscid fluxes for scalars
          do iii = 1, nAuxVars
            bufferOffset = (iii-1)*numPointsBuffer+map_local(ppp)
            do jj = 1, ND
              cons_inviscid_flux(jj,ND+2+iii) = scalarBuffer(bufferOffset) * &
                   rhoVBuffer(map_local(ppp)+(jj-1)*numPointsBuffer)/rhoBuffer(map_local(ppp))
            end do ! i
          end do ! 

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "cons_inviscid_flux"
              !write(*,*) cons_inviscid_flux
            !endif
          !endif
               
          !SC = 0.0_8

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5 .and. (i .eq. 29 .or. i .eq. 30)) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "cons_inviscid_flux, (x,y,z)"
              !do ii = 1,numEquations
                !write(*,*) cons_inviscid_flux(:,ii)
              !enddo
            !endif
          !endif

          m2Offset = (normDir - 1)*numDim*numPointsBuffer + map_local(ppp)

          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !!if(j .eq. 5 .and. (i .eq. 29 .or. i .eq. 30)) then
            !if(j .eq. 5 ) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "MT2"
              !do jj = 1, ND
                !write(*,*) MT2(m2Offset+(jj-1)*numPointsBuffer)
              !enddo
            !endif
          !endif
               
          ! MT2 is gridMetric2 [ d(MT1[1,j])/d(xi),d(MT1[2,j])/d(eta),d(MT1[3,j])/d(zeta)  
          L_vector = 0.0_8
          do ii = 1,numEquations
            do jj = 1, ND
              L_vector(ii) = L_vector(ii) - cons_inviscid_flux(jj,ii) * MT2(m2Offset+(jj-1)*numPointsBuffer)
            enddo
            L_vector(ii) = gridJacobian(map_local(ppp)) * (L_vector(ii) + dflux_local(1,ii))  
          enddo

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5 .and. (i .eq. 29 .or. i .eq. 30)) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "L_vector"
              !write(*,*) L_vector
            !endif
          !endif

          ! ... scale the L_vector by Pinv
          L_vector = MATMUL(Pinv,L_vector)

          ! ... transform SC with Pinv
!         SC = MATMUL(Pinv,SC)

          ! ... wavespeeds
          wave_spd(1:ND) = UX * XI_X + UY * XI_Y + UZ * XI_Z
          wave_spd(ND+1)   = wave_spd(1) + SPD_SND / bndry_h
          wave_spd(ND+2)   = wave_spd(1) - SPD_SND / bndry_h
          if (nAuxVars > 0) wave_spd(ND+3:ND+2+nAuxVars) = wave_spd(1)
          ! ... apply bc...
          ! ... solid wall first because no normal velocity thru wall
          if ( bcType == NSCBC_SLIP_ADIABATIC .or. bcType == NSCBC_AXISYMMETRY) then

!           A_wall = XYZ_TAU2(map_local(ppp),1) * XI_X_TILDE
!           if (ND >= 2) A_wall = A_wall + XYZ_TAU2(map_local(ppp),2) * XI_Y_TILDE
!           if (ND == 3) A_wall = A_wall + XYZ_TAU2(map_local(ppp),3) * XI_Z_TILDE

            if (sgn > 0) then
              L_vector(ND+1) = L_vector(ND+2) !+ SC(ND+1) - SC(ND+2) ! - 2.0_8 * A_wall
            else
              L_vector(ND+2) = L_vector(ND+1) !- SC(ND+1) + SC(ND+2) ! + 2.0_8 * A_wall
            end if
            L_vector(1:ND) = 0.0_8
            if (nauxvars > 0) L_vector(ND+3:NDP2) = 0.0_8

          else if ( (bcType == NSCBC_INFLOW_VELTEMP )) THEN

            if (sgn > 0) then
              L_vector(ND+1) = L_vector(ND+2) !+ SC(ND+1) - SC(ND+2) !&
!                  - 2.0_8 * (patch_dudt_inflow(lp) * XI_X_TILDE + patch_dvdt_inflow(lp) * XI_Y_TILDE)
            else
              L_vector(ND+2) = L_vector(ND+1) !- SC(ND+1) + SC(ND+2) !&
!                  + 2.0_8 * (patch_dudt_inflow(lp) * XI_X_TILDE + patch_dvdt_inflow(lp) * XI_Y_TILDE)
            end if

            L_vector(1) = + 0.5_8 * RHO**2 * SPD_SND_INV * (L_vector(ND+1)+L_vector(ND+2)) !&
                     !- 0.5_8 * GM1 * RHO * SPD_SND_INV * (SC(ND+1)+SC(ND+2)) + SC(1) !&
!                     + SPD_SND**2 / GM1 * patch_dTdt_inflow(lp)

            L_vector(2) = 0.0_8 !- SC(2)
            if (ND == 3) L_vector(3) = 0.0_8 !- SC(3)

          else if ( bcType == NSCBC_OUTFLOW_NONREFLECTION ) then

            !SC(:) = 0.0_8
            if (sgn > 0 .and. wave_spd(ND+1) >= 0.0_8) then
              L_vector(ND+1) = 0.0_8
              if (wave_spd(1) >= 0.0_8) then
                L_vector(1:ND) = 0.0_8
                do jj = 1, nauxvars
                  L_vector(ND+2+jj) = 0.0_8
                enddo
                if (wave_spd(ND+2) >= 0.0_8) then
                  L_vector(ND+2) = 0.0_8
                endif
              endif
               
            elseif (sgn < 0 .and. wave_spd(ND+2) <= 0.0_8) then
              L_vector(ND+2) = 0.0_8
              if (wave_spd(1) <= 0.0_8) then
                L_vector(1:ND) = 0.0_8
                do jj = 1, nauxvars
                  L_vector(ND+2+jj) = 0.0_8
                enddo
                if (wave_spd(ND+1) <= 0.0_8) then
                  L_vector(ND+1) = 0.0_8
                endif
              endif
            end if

          else if ( bcType == NSCBC_NOSLIP_ISOTHERMAL ) then 
                
            L_vector(1) = 0.0_8
            L_vector(2)  = 0.0_8
            L_vector(ND) = 0.0_8
            !SC(2)  = 0.0_8
            !SC(ND) = 0.0_8
            do jj = 1, nauxvars
              L_vector(ND+2+jj) = 0.0_8
            enddo
            !if (nauxvars > 0) then
              !!L_vector(ND+3:NDP2) = 0.0_8
              !L_vector(ND+3:NDP2) = 0.0_8
              !!SC(ND+3:NDP2) = 0.0_8
            !endif
            if (sgn > 0) then
              L_vector(ND+1) = L_vector(ND+2) !+ SC(ND+1) - SC(ND+2)
            else
              L_vector(ND+2) = L_vector(ND+1) !- SC(ND+1) + SC(ND+2)
            end if    
                
!         else if ( bcType == NSCBC_INFLOW_SUPERSONIC ) Then
                
            ! ... do nothing, everything is specified in NS_BC_Fix_Value
                
          end if ! bcType

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5 .and. (i .eq. 29 .or. i .eq. 30)) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "L_vector"
              !write(*,*) L_vector
              !write(*,*) "Pfor"
              !write(*,*) Pfor
            !endif
          !endif

          ! ... compute the new normal flux
          new_normal_flux(:) = MATMUL(Pfor,L_vector)

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5 .and. (i .eq. 29 .or. i .eq. 30)) then
              !write(*,*) "***** i=",i,"*****"
              !write(*,*) "1. new_normal_flux(1)", new_normal_flux(1)
            !endif
          !endif
!
          new_normal_flux(:) = gridJacobian(map_local(ppp)+numPointsBuffer) * new_normal_flux(:)

          ! MJA
          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5 .and. (i .eq. 29 .or. i .eq. 30)) then
              !write(*,*) "2. new_normal_flux(1)", new_normal_flux(1)
            !endif
          !endif

          do jj = 1, ND
            m2Offset2 = m2Offset + (jj-1)*numPointsBuffer
            new_normal_flux(:) = new_normal_flux(:) + cons_inviscid_flux(jj,:) * MT2(m2Offset2)
          end do

          !if(bcType == NSCBC_NOSLIP_ISOTHERMAL .and. normdir .eq. 3) then
            !if(j .eq. 5 .and. (i .eq. 29 .or. i .eq. 30)) then
              !write(*,*) "3. new_normal_flux(1)", new_normal_flux(1)
              !write(*,*) "gridJacobian(map_local(ppp))", gridJacobian(map_local(ppp))
              !write(*,*) "rhoBuffer(map_local(ppp))", rhoBuffer(map_local(ppp))
              !write(*,*) "rhoRHSBuffer(map_local(ppp))", rhoRHSBuffer(map_local(ppp))
              !write(*,*) "n_n_f*gJ", new_normal_flux(1)*gridJacobian(map_local(ppp))
            !endif
          !endif

          ! ... applies bc to only those nodes with iblank == 1
          rhoRHSBuffer(map_local(ppp)) = rhoRHSBuffer(map_local(ppp)) &
                   - new_normal_flux(1) * gridJacobian(map_local(ppp))
          DO jj = 1,numDim
            bufferOffset = (jj-1)*numPointsBuffer + map_local(ppp)
            rhoVRHSBuffer(bufferOffset) = rhoVRHSBuffer(bufferOffset) &
                   - gridJacobian(map_local(ppp)) * new_normal_flux(jj+1)
          END DO
          rhoERHSBuffer(map_local(ppp)) = rhoERHSBuffer(map_local(ppp)) &
                        - gridJacobian(map_local(ppp)) * new_normal_flux(numDim+2)
          do jj = 1, nauxvars
            bufferOffset = (jj-1)*numPointsBuffer+map_local(ppp)
            scalarRHSBuffer(bufferOffset) = scalarRHSBuffer(bufferOffset) &
                   - gridJacobian(map_local(ppp)) * new_normal_flux(numDim+2+jj)
          enddo
         enddo!i
       enddo!j
    enddo!k

    return
    
  END SUBROUTINE NS_BC_WENO


  subroutine NS_BC_FIXVALUE(numDim,bufferSizes,numPointsBuffer,normDirIn,numPatchPointsOp,patchPointsOp, &
       gridType,gridMetric,gridJacobian,bcType,bcParams,gasVars,numScalars,pressureBuffer,               &
       temperatureBuffer,sosBuffer,eosInfo,eosNonDimen,eosGasParams,rhoTarget,rhoVTarget, rhoETarget, scalarTarget,  &
       rhoBuffer,rhoVBuffer, rhoEBuffer,scalarBuffer)                                                   
    
    USE SATUtil
    
    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim,numScalars,normDirIn,gridType,bcType
    INTEGER(KIND=8) :: numPointsBuffer,bufferSizes(numDim),eosInfo(NUMGASINFOS)
    INTEGER(KIND=8) :: numPatchPointsOp
    INTEGER(KIND=8) :: patchPointsOp(numPatchPointsOp)
    REAL(KIND=8),    INTENT(IN)            :: eosGasParams(NUMGASPARAMS*(numScalars+1))
    REAL(KIND=8),    INTENT(IN)            :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8),    INTENT(IN)            :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: gridJacobian(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: gasVars(6)
    REAL(KIND=8),    INTENT(IN)            :: bcParams(5)
    REAL(KIND=8),    INTENT(IN)            :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: temperatureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: sosBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: rhoTarget(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET    :: rhoVTarget(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)            :: rhoETarget(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET    :: scalarTarget(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: scalarBuffer(numScalars*numPointsBuffer)

    INTEGER(KIND=4) :: sgn,normDir,numEquations,iScalar,iDim, iDir
    INTEGER(KIND=8) :: bufferOffset, m2Offset, m2Offset2,metricOffset
    INTEGER(KIND=8) :: i,j,jl,m,offset, iPoint, pointIndex
    INTEGER(KIND=4) :: tangDir(3,2)
    REAL(KIND=8)    :: norm_vec(3), tang_vec(2,3), fvec(3,2)
    INTEGER(KIND=4) :: handedness(3)
    integer :: ND, Nc, k, ii, jj, jjj, l1, l2, isign, lp, kk
    integer(kind=8) :: l0
    real(KIND=8)    :: norm(3), mag_norm, tang(2,3)
    real(KIND=8)    :: mag_tang, constant, del, eta, rel_pos(3), dsign, denom
    integer(KIND=8) :: Np(3), eosContext
    real(KIND=8)    :: d(5), wave_amp(5), spd_snd
    real(KIND=8)    :: un, ut(2)
    real(KIND=8) :: bndry_norm(3), p_inf, u_inf, du_inf, metric_fac, T_inf, dT_inf, theta
    real(KIND=8) :: dsgn
    real(KIND=8) :: V_wall, V_wall_vec(3), V_wall_target, V_wall_target_vec(3), A_wall, drhodn, dp_inf, T_wall
    real(KIND=8) :: Uhat_alpha(3), alpha_x(3,3), alpha_t(3)
    real(KIND=8) :: SPVOL, RHO, UX, UY, UZ, PRESSURE, ENERGY,SPD_SND_INV, KE, ENTHALPY, MACH2
    REAL(KIND=8) :: V_DOT_XI
    REAL(8) :: sndspdref2, invtempref, tempref, gamref, spcGasConst
    REAL(8) :: XI_X, XI_Y, XI_Z, XI_T, bndry_h,sbpBoundaryWeight, xh
    REAL(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, pointJacobian
    REAL(8) :: XIX(4)
    REAL(8) :: alpha,beta
    REAL(KIND=8) :: uL(numDim+2+numscalars),dvL(numDim+2),dpL(2+numscalars)
    REAL(KIND=8) :: pointMetric(numDim*numDim)
    REAL(KIND=8) :: RE, SC

    eosContext = eosInfo(1)

    RE         = gasVars(1)
    SC         = gasVars(2)

    ! ... simplicity
    ND = numDim
    numEquations = numDim+2+numScalars 

    sgn = normDirIn / abs(normDirIn)
    normDir = abs(normDirIn)

    tangDir(1,:) = (/2, 3/)
    tangDir(2,:) = (/1, 3/)
    tangDir(3,:) = (/1, 2/)
    handedness = (/1, -1, 1/)

    DO iPoint = 1, numPatchPointsOp
       
       pointIndex = patchPointsOp(iPoint)
       l0 = pointIndex + 1 ! assume the points coming in are from C (0-based)

       ! ... construct the forward and backward transformation matrices
       ! ... these matrices are used for both NSCBC and SAT
       CALL BoundaryPointMetric(numDim,normDir,numPointsBuffer,l0,gridType,&
               gridMetric,pointMetric)
       IF (gridType <= UNIRECT) THEN
         pointJacobian = gridJacobian(1)
       ELSE IF(gridType .ge. RECTILINEAR) THEN
         pointJacobian = gridJacobian(l0)
       ENDIF

       XI_X = pointMetric(1) * pointJacobian
       XI_Y = pointMetric(2) * pointJacobian
       XI_Z = 0.0_8
       if (numDim >= 3) then
         XI_Z = pointMetric(3) * pointJacobian
       end if
       XI_T = 0.0_8

       ! ... normalized metrics
       xh = sqrt((XI_X*XI_X)+(XI_Y*XI_Y)+(XI_Z*XI_Z))
       bndry_h = 1.0_8 / xh
       XI_X_TILDE = XI_X * bndry_h
       XI_Y_TILDE = XI_Y * bndry_h
       XI_Z_TILDE = XI_Z * bndry_h

       CALL Conserved_Variables_Single_Point(uL,rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,l0, &
               (/1.0_8,1.0_8/),numDim,numScalars,numEquations,numPointsBuffer,0)

       dvL(1) = pressureBuffer(l0)
       dvL(2) = temperatureBuffer(l0)
       SPD_SND = sosBuffer(l0)
       do j = 1,numDim
          dvL(j+2) = rhoVbuffer(l0+(j-1)*numPointsBuffer)/rhoBuffer(l0)
       end do

      !CALL GASDP(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1), &
           !uL(1),dvL(1),dpL(1))
      !CALL GASSOUNDSPEED(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
           !uL(1),dvL(1),dpL(1),SPD_SND)

       ENERGY = rhoEBuffer(l0)

       ! ... primitive variables
       RHO = rhoBuffer(l0)
       SPVOL = 1.0_8 / RHO
       UX = SPVOL * rhoVBuffer(l0); UZ = 0.0_8;
       UY = SPVOL * rhoVBuffer(numPointsBuffer+l0)
       if (ND == 3) UZ = SPVOL * rhoVBuffer(2*numPointsBuffer+l0)
       PRESSURE = pressureBuffer(l0)
       SPD_SND_INV = 1.0_8 / SPD_SND
       KE = 0.5_8 * (UX**2 + UY**2 + UZ**2)
       ENTHALPY = SPVOL*(ENERGY + PRESSURE)
       MACH2 = (2.0_8 * KE) * SPD_SND_INV**2

       if(  bcType == NSCBC_INFLOW_VELTEMP  .OR. & 
            bcType == NSCBC_INFLOW_SUBSONIC .OR. &
            bcType == NSCBC_INFLOW_SUPERSONIC) THEN

          if ((MACH2 >= 1.0_8) .OR. (bcType == NSCBC_INFLOW_SUPERSONIC)) then
             rhoBuffer(l0)  = rhoTarget(l0)
             do iDim = 1,numDim
                pointIndex = l0 + (iDim-1)*numPointsBuffer
                rhoVBuffer(pointIndex) = rhoVTarget(pointIndex)
             end do
             rhoEBuffer(l0) = rhoETarget(l0)
             do iScalar = 1,numScalars
                pointIndex = l0 + (iScalar-1)*numPointsBuffer
                scalarBuffer(pointIndex) = scalarTarget(pointIndex)
             end do
          else
             ! ... assume density equation is good
             ! ... update momentum
             DO iDim = 1,numDim
                pointIndex = l0 + (iDim-1)*numPointsBuffer
                rhoVBuffer(pointIndex) = rhoBuffer(l0)*rhoVTarget(pointIndex)/rhoTarget(l0)
             END DO
             DO iScalar = 1, numScalars
                pointIndex = l0 + (iScalar-1)*numPointsBuffer
                scalarBuffer(pointIndex) = rhoBuffer(l0)*scalarTarget(pointIndex)/rhoTarget(l0)
             END DO

             IF (bcParams(1) < 0.0_8) THEN
               rhoEBuffer(l0) = rhoETarget(l0) / rhoTarget(l0) * rhoBuffer(l0)
             ELSE
               uL(1) = rhoBuffer(l0)
               do iDim = 1,numDim
                pointIndex = l0 + (iDim-1)*numPointsBuffer
                uL(1+iDim) = rhoVBuffer(pointIndex)
               enddo
               uL(2+numDim) = 0.0_8
               DO iScalar = 1, numScalars
                 pointIndex = l0 + (iScalar-1)*numPointsBuffer
                 uL(2+numDim+iScalar) = scalarBuffer(pointIndex)
               END DO
               CALL GASISOTHERMAL(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
                    bcParams(1),uL(1))
               rhoEBuffer(l0) = uL(2+numDim)
             ENDIF
          end if

       else if ( bcType == NSCBC_SLIP_ADIABATIC .or. bcType == NSCBC_AXISYMMETRY) then

          ! ... compute the normal and tangential velocity vectors
          if (ND == 2) then ! JKim 11/2007
             un    =  XI_X_TILDE * UX + XI_Y_TILDE * UY
             ut(1) = -XI_Y_TILDE * UX + XI_X_TILDE * UY
          else if (ND == 3) then ! JKim 11/2007
             norm_vec(:) = (/XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE/) ! boundary-normal vector in the
             ! direction of increasing xi or
             ! eta or zeta.
             ! Hard-coded for Curvilinear-only !!! [mtc]
             !> @TODO BC FixValue for different metric types [mtc]
             DO iDim = 1,numDim
                pointIndex = l0+(tangDir(normDir,1)-1)*numDim*numPointsBuffer 
                tang_vec(1,iDim) = gridMetric(pointIndex)
             END DO
             tang_vec(1,:) = tang_vec(1,:) * handedness(normDir) ! adjust handed-ness for right-handed-ness
             tang_vec(1,:) = tang_vec(1,:) * pointJacobian ! multiply by the Jacobian
             denom = 1.0_8 / sqrt(dot_product(tang_vec(1,:), tang_vec(1,:)))
             tang_vec(1,:) = tang_vec(1,:) * denom ! normal vector for another direction, say,
             ! (eta_x_tilde, eta_y_tilde, eta_z_tilde)
             call cross_product(norm_vec, tang_vec(1,:), tang_vec(1,:))
             tang_vec(1,:) = tang_vec(1,:) / sqrt(dot_product(tang_vec(1,:), tang_vec(1,:))) ! now it's done ^^

             call cross_product(norm_vec, tang_vec(1,:), tang_vec(2,:))
             tang_vec(2,:) = tang_vec(2,:) / sqrt(dot_product(tang_vec(2,:), tang_vec(2,:))) ! now it's done ^^

!             un    = dot_product((/UX, UY, UZ/), norm_vec(:)  )
!             ut(1) = dot_product((/UX, UY, UZ/), tang_vec(1,:))
!             ut(2) = dot_product((/UX, UY, UZ/), tang_vec(2,:))
          end if ! ND

          ! ... update the wall-normal velocity
          un = 0.0_8
          !                un = XI_X_TILDE * grid%XYZ_TAU(l0,1)
          !                un = un + XI_Y_TILDE * grid%XYZ_TAU(l0,2)

          ! ... return to Cartesian velocities
          if (ND == 2) then ! JKim 11/2007
             UX = XI_X_TILDE * un - XI_Y_TILDE * ut(1)
             UY = XI_Y_TILDE * un + XI_X_TILDE * ut(1)
          else if (ND == 3) then ! JKim 11/2007
             !                  un = un + XI_Z_TILDE * grid%XYZ_TAU(l0,3)
             UX = dot_product((/un, ut(1), ut(2)/), &
                  (/norm_vec(1), tang_vec(1,1), tang_vec(2,1)/))
             UY = dot_product((/un, ut(1), ut(2)/), &
                  (/norm_vec(2), tang_vec(1,2), tang_vec(2,2)/))
             UZ = dot_product((/un, ut(1), ut(2)/), &
                  (/norm_vec(3), tang_vec(1,3), tang_vec(2,3)/))
          end if ! ND
          
          rhoEBuffer(l0) = rhoEBuffer(l0) + 0.5_8 * RHO * (UX**2 - (rhoVBuffer(l0)/RHO)**2)
          rhoEBuffer(l0) = rhoEBuffer(l0) + 0.5_8 * RHO * (UY**2 - (rhoVBuffer(l0+numPointsBuffer)/RHO)**2)
          rhoVBuffer(l0) = RHO * UX
          rhoVBuffer(l0+numPointsBuffer) = RHO * UY
          IF (numDim == 3) THEN
            rhoEBuffer(l0) = rhoEBuffer(l0) + 0.5_8 * RHO * (UZ**2 - (rhoVBuffer(l0+2*numPointsBuffer)/RHO)**2)
             rhoVBuffer(l0+2*numPointsBuffer) = RHO * UZ
          END IF

       else if ( bcType == NSCBC_NOSLIP_ISOTHERMAL) THEN


          ! ... momentum
          !  state%cv(l0,2:ND+1) = grid%XYZ_TAU(l0,1:ND) * state%cv(l0,1)
          DO iDim = 1,numDim
             rhoVBuffer(l0 + (iDim-1)*numPointsBuffer) = 0.0_8
          END DO
          ! ... update the total energy (hrm)
          uL(1) = rhoBuffer(l0)
          do iDim = 1,numDim
            uL(1+iDim) = 0.0_8
          enddo
          uL(2+numDim) = 0.0_8
          DO iScalar = 1, numScalars
            pointIndex = l0 + (iScalar-1)*numPointsBuffer
            uL(2+numDim+iScalar) = scalarBuffer(pointIndex)
          END DO

          if(bcParams(1) < 0) then
            ! ... get the temperature from the target
            uL(1) = rhoTarget(l0)
            do iDim = 1,numDim
              uL(1+iDim) = 0.0_8
            enddo
            uL(2+numDim) = rhoETarget(l0)
            DO iScalar = 1, numScalars
              pointIndex = l0 + (iScalar-1)*numPointsBuffer
              uL(2+numDim+iScalar) = scalarTarget(pointIndex)
            END DO

            CALL GASDV(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uL(1),dvL(1))

            ! ... reset uL to the current state and find an energy to get that temperature
            uL(1) = rhoBuffer(l0)
            do iDim = 1,numDim
              uL(1+iDim) = 0.0_8
            enddo
            uL(2+numDim) = 0.0_8
            DO iScalar = 1, numScalars
              pointIndex = l0 + (iScalar-1)*numPointsBuffer
              uL(2+numDim+iScalar) = scalarBuffer(pointIndex)
            END DO
            CALL GASISOTHERMAL(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
                 dvL(2),uL(1))
            rhoEBuffer(l0) = uL(2+numDim)
          else
            ! use the specified boundary temperature
            CALL GASISOTHERMAL(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),&
                 bcParams(1),uL(1))
            rhoEBuffer(l0) = uL(2+numDim)
          endif

          !write(*,*) "l0", l0
          !if(l0 .eq. 1275) then
            !write(*,*) "rhoBuffer(l0) ", rhoBuffer(l0)
            !write(*,*) "rhoEBuffer(l0) ", rhoEBuffer(l0)
          !endif

          ! on no-slip boundaries, there can be no evolution in the mass fraction if 
          ! diffusion is absent
          ! in this case, maintain Y (as opposed to rhoY)
          if(.not. SC > 0.0_8) then
            DO iScalar = 1, numScalars
              pointIndex = l0 + (iScalar-1)*numPointsBuffer
              scalarBuffer(pointIndex) = scalarTarget(pointIndex)/rhoTarget(l0)*rhoBuffer(l0)
            END DO
          else
          endif
       END IF

    END DO
    
  END subroutine NS_BC_FIXVALUE

  subroutine cross_product(v1,v2,y)

    Implicit None

    Real(KIND=8) :: v1(3), v2(3), y(3)

    y(1) = v1(2)*v2(3) - v1(3)*v2(2)
    y(2) = v1(3)*v2(1) - v1(1)*v2(3)
    y(3) = v1(1)*v2(2) - v1(2)*v2(1)

    return

  end subroutine cross_product

  ! This subroutine adds axisymmetric geometrical source terms to the RHS
  subroutine NS_RHS_Axisymmetric(                                          &
       numDim,bufferSizes,numPointsBuffer,numScalars,                      &
       opInterval,gridCoordinates,                                         &
       rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,scalarBuffer,        &
       rhoRHSBuffer,rhoVRHSBuffer,rhoERHSBuffer,scalarRHSBuffer)

    IMPLICIT NONE

    INTEGER(KIND=4) :: numDim, numscalars
    INTEGER(KIND=8) :: numPointsBuffer, bufferSizes(numDim)
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoRHSBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: rhoVRHSBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoERHSBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: scalarRHSBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN),    TARGET :: gridCoordinates(numDim*numPointsBuffer)
    !-----------Local variables------------------------------------------------
    INTEGER(KIND=8) :: i,ii,k,i1,i2
    REAL(KIND=8), parameter :: rmin=1d-15
    REAL(KIND=8) :: dudr,r

    do i2 = opInterval(3),opInterval(4)
      do i1 = opInterval(1),opInterval(2)
        i = i1 + bufferSizes(1) * (i2-1) 
        r = gridCoordinates(i)
        if (r >= rmin) then
          !-----continuity
          rhoRHSBuffer(i) = rhoRHSBuffer(i) - rhoVBuffer(i) / r
          !-----momentum-r direction
          rhoVRHSBuffer(i) = rhoVRHSBuffer(i) - &
                             rhoVBuffer(i) * rhoVBuffer(i) / rhoBuffer(i) / r
          !-----momentum-z direction
          rhoVRHSBuffer(i+numPointsBuffer) = rhoVRHSBuffer(i+numPointsBuffer) - &
            rhoVBuffer(i) * rhoVBuffer(i+numPointsBuffer) / rhoBuffer(i) / r
          !-----energy equation
          rhoERHSBuffer(i) = rhoERHSBuffer(i) -  &
               (rhoEBuffer(i) + pressureBuffer(i)) * rhoVBuffer(i) / rhoBuffer(i) / r
          !-----scalar
          do k =1, numScalars
            scalarRHSBuffer(i+(k-1)*numPointsBuffer) = scalarRHSBuffer(i+(k-1)*numPointsBuffer) - &
                   scalarBuffer(i+(k-1)*numPointsBuffer) * rhoVBuffer(i) / rhoBuffer(i) / r
          enddo
        endif
      enddo
    enddo
 
    return

  end subroutine NS_RHS_Axisymmetric

  ! This subroutine adds axisymmetric geometrical source terms to the RHS
  subroutine NS_RHS_Axisymmetric_Axis(                                     &
       numDim,bufferInterval,numPointsBuffer,numScalars,normDir,           &
       opInterval,gridMetric,jacobianDeterminant,gridType,gridCoordinates, &
       rhoBuffer,rhoVBuffer,rhoEBuffer,pressureBuffer,scalarBuffer,        &
       rhoRHSBuffer,rhoVRHSBuffer,rhoERHSBuffer,scalarRHSBuffer)

    IMPLICIT NONE

    INTEGER(KIND=4) :: normDir
    INTEGER(KIND=8), INTENT(IN)            :: opInterval(2*numDim)     ! patch interval
    INTEGER(KIND=8), INTENT(IN)            :: bufferInterval(2*numDim) ! buffer interval
    INTEGER(KIND=4) :: numDim, numscalars, numEquations, gridType
    INTEGER(KIND=8) :: numPointsBuffer, bufferSizes(numDim)
    REAL(KIND=8), INTENT(IN)    :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8), INTENT(IN)    :: jacobianDeterminant(2*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoRHSBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: rhoVRHSBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT)         :: rhoERHSBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: scalarRHSBuffer(numScalars*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN),    TARGET :: gridCoordinates(numDim*numPointsBuffer)
    !-----------Local variables------------------------------------------------
    INTEGER(KIND=8) :: i,ii,k,i1,i2
    REAL(KIND=8), parameter :: rmin=1d-15
    REAL(KIND=8) :: dudr,r

    if (gridType==CURVILINEAR) then
      do i2 = opInterval(3),opInterval(4)
        do i1 = opInterval(1),opInterval(2)
          i = i1 + bufferInterval(1) * (i2-1)
          r = gridCoordinates(i)
          if (r < rmin) then
            !Calculate du/dr - second-order central scheme is used to evaluate the derivative
            dudr = 0.0_8
            If (normDir==1) then
              ii = i + 1
              dudr = rhoVBuffer(ii) / rhoBuffer(ii) * gridMetric(i)
            elseif (normDir==1) then
              ii = i + bufferSizes(1)
              dudr = rhoVBuffer(ii) / rhoBuffer(ii) * gridMetric(i+2*numPointsBuffer)
            elseif (normDir==-1) then
              ii = i - bufferSizes(1)
              dudr = -rhoVBuffer(ii) / rhoBuffer(ii) * gridMetric(i+2*numPointsBuffer)
            endif
          
            dudr = dudr * jacobianDeterminant(i)

          !-----continuity
            rhoRHSBuffer(i) = rhoRHSBuffer(i) - rhoBuffer(i) * dudr
          !-----momentum-r direction

          !-----momentum-z direction
            rhoVRHSBuffer(i+numPointsBuffer) = rhoVRHSBuffer(i+numPointsBuffer) - &
                                         rhoVBuffer(i+numPointsBuffer) * dudr
          !-----energy equation
            rhoERHSBuffer(i) = rhoERHSBuffer(i) - (rhoEBuffer(i) + pressureBuffer(i)) * dudr
          !-----scalar
            do k =1, numScalars
              scalarRHSBuffer(i+(k-1)*numPointsBuffer) = scalarRHSBuffer(i+(k-1)*numPointsBuffer) - &
                                    scalarBuffer(i+(k-1)*numPointsBuffer) * dudr
            enddo
          endif
        enddo
      enddo
    elseif (gridType==RECTILINEAR) then
      do i2 = opInterval(3),opInterval(4)
        do i1 = opInterval(1),opInterval(2)
          i = i1 + bufferInterval(1) * (i2-1)
          r = gridCoordinates(i)
          if (r < rmin) then
            !Calculate du/dr - second-order central scheme is used to evaluate the derivative
            ii = i + 1
            dudr = rhoVBuffer(ii) / rhoBuffer(ii) * gridMetric(i) * jacobianDeterminant(i)

          !-----continuity
            rhoRHSBuffer(i) = rhoRHSBuffer(i) - rhoBuffer(i) * dudr
          !-----momentum-r direction

          !-----momentum-z direction
            rhoVRHSBuffer(i+numPointsBuffer) = rhoVRHSBuffer(i+numPointsBuffer) - &
                                         rhoVBuffer(i+numPointsBuffer) * dudr
          !-----energy equation
            rhoERHSBuffer(i) = rhoERHSBuffer(i) - (rhoEBuffer(i) + pressureBuffer(i)) * dudr
          !-----scalar
            do k =1, numScalars
              scalarRHSBuffer(i+(k-1)*numPointsBuffer) = scalarRHSBuffer(i+(k-1)*numPointsBuffer) - &
                                    scalarBuffer(i+(k-1)*numPointsBuffer) * dudr
            enddo
          endif
        enddo
      enddo
    elseif (gridType==UNIRECT) then
      do i2 = opInterval(3),opInterval(4)
        do i1 = opInterval(1),opInterval(2)
          i = i1 + bufferInterval(1) * (i2-1)
          r = gridCoordinates(i)
          if (r < rmin) then
            !Calculate du/dr - second-order central scheme is used to evaluate the derivative
            ii = i + 1
            dudr = rhoVBuffer(ii) / rhoBuffer(ii) * gridMetric(1) * jacobianDeterminant(1)

          !-----continuity
            rhoRHSBuffer(i) = rhoRHSBuffer(i) - rhoBuffer(i) * dudr
          !-----momentum-r direction

          !-----momentum-z direction
            rhoVRHSBuffer(i+numPointsBuffer) = rhoVRHSBuffer(i+numPointsBuffer) - &
                                         rhoVBuffer(i+numPointsBuffer) * dudr
          !-----energy equation
            rhoERHSBuffer(i) = rhoERHSBuffer(i) - (rhoEBuffer(i) + pressureBuffer(i)) * dudr
          !-----scalar
            do k =1, numScalars
              scalarRHSBuffer(i+(k-1)*numPointsBuffer) = scalarRHSBuffer(i+(k-1)*numPointsBuffer) - &
                                    scalarBuffer(i+(k-1)*numPointsBuffer) * dudr
            enddo
          endif
        enddo
      enddo
    endif
 
    return

  end subroutine NS_RHS_Axisymmetric_Axis

END MODULE NavierStokesRHSWENO
