!
!   GPL License Info  {{{
!
!   disconnectionDPS is free software; you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation; either version 2 of the License, or
!   (at your option) any later version.
!
!   disconnectionDPS is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program; if not, write to the Free Software
!   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!   }}}
!
! Program Info  {{{
!
! Program to plot disconnectivity trees.
! See Becker and Karplus JCP 106 1495 (1997).
!
! Adapted to read PATHSAMPLE.2.0 data files.
! keyword NCONNMIN added
!
! Information and options are passed to the program in a file called
! "dinfo", which is keyword driven.
!
! Compulsory Keywords
! -------------------
!
! DELTA <dE>
! Energetic separation of levels in basin analysis.
!
! FIRST <E1>
! Specifies the energy of the highest level on the energy axis.
!
! LEVELS <n>
! The number of levels at which to perform the basin analysis.
!
! MINIMA <file>
! Specifies filename for minima info.
!
! TS <file>
! Specifies filename for transition state info.
!
!
! Optional Keywords
! -----------------
!
! CENTREGMIN
! If this keyword is present, then when a node splits into its daughter
! nodes, the one containing the global minimum is always placed centrally
! (even if other nodes carry more minima). This does not guarantee that
! the global minimum is central in the overall diagram because other
! nodes may push the one containing the global minimum over to one side.
!
! DUMPNUMBERS
! If present, a file called node_numbers is written, listing the minima
! associated with each node in each level. Nodes are listed from left to
! right within each level.
!
! DUMPSIZES
! If present, a file called node_sizes is written, listing how many minima
! are represented by each node in each level. Nodes are listed from left to
! right in each level.
!
! EXCLUDEALL
! Removes all minima from the list of minima to be plotted.  This is to be
! used in conjunction with the PICK command which can be used to specify
! exclusively which minima are to be included.
!
! CONNECTMIN <min>
! If present then the analysis for a connected database is based upon minimum
! number min. If absent then the global minimum is used to judge connectivity.
!
! COLOURPRINT
! For use with TRMIN, if present colour analysis written to 'node_sections'.   
! Not actually required for colour analysis.
!
! IDENTIFY
! If present, the branch ends are labelled with the lowest-energy minimum
! they represent.
!
! IDENTIFY_NODE <max_min>
! If present, the nodes are labelled with the format N1_N2, where N1 is the number of level,
! N2 is the number of the node at that level. The label is only printed if the
! number of minima below that node is smaller than <max_min>. With this info
! you can pick the number of minima correspondig to that node from the node_numbers file,
! produced by using the keyword DUMPNUMBERS... (and then print any branch of the graph separately)
!
! IDENTIFY_NODE_SIZE <max_min2>
! If present, the nodes are labelled with number of minima corresponding to that node. 
! The label is only printed if the number of minima below that node is smaller than <max_min2>
!
! IDMIN <min>
! Label this minimum on the graph. Repeat to label more than one minimum.
!
! LABELFORMAT <fmt>
! Specifies the Fortran format string for the energy level labels. The default
! is F6.1.
!
! LABELSIZE <n>
! Set the size of the fonts in case of the labels (for IDENTIFY, IDENTIFY_NODE ...)
! Default is 10 pt.
!
! LETTER
! If present, the graph is formatted for American letter paper rather than
! European A4.
!
! LOWEST <n>
! If present, only the branches leading to the lowest n minima are drawn. The
! pruning occurs after the basin analysis, so the "discarded" minima can still
! influence the connectivities.
!
! MONOTONIC
! If present, all minima not lying at the bottom of a monotonic sequences are
! not drawn. This tends to reduce the number of branches drastically. If the
! keyword LOWEST is also used, the MONOTONIC sequence analysis is applied after
! the high energy minima have been discarded.
!
! NCONNMIN
! Minima with NCONNMIN connections or fewer are discarded. Default is zero.
!
! NOBARRIERS
! If present, all transition state energies are reset to the energy of the higher
! of the two minima they connect. This transforms the energy landscape to the
! type explored by gmin.
!
! PICK <file>
! Specifies the name of a list of numbers of minima, one per line.  Minima on
! this list are included on the graph.  Minima preceded with a minus sign are
! removed from the graph.  This process is executed after the commands
! MONOTONIC, LOWEST and EXCLUDEALL have been executed, thereby making it
! possible to override them for particular minima.  Examples: 1. To remove
! certain minima from a full plot, just specify PICK and a list of negative
! minima numbers.  2. To include only specific minima, use EXCLUDEALL and
! PICK plus a list of positive minima numbers.  All basin analysis includes
! the full sample and is performed before minima are removed or added back in.
!
! NOSPLIT
! By default, every minimum is indicated by its own branch, which splits off
! from the parent basin even if the minimum and its lowest transition state do
! not straddle an energy level in the basin analysis. This is to avoid it being
! dependent on precisely where the levels are placed (bulk shifting of the levels
! would cause some branch ends to appear or disappear rather than change node
! if this were not the case). The NOSPLIT option turns this feature off, so that
! if two minima are separated by a barrier lower than the level above their own
! energy, they are grouped together. This option should probably never be used.
!
! TRMIN <n> <max> <file> <file> ...
! Label n different sections of the graph in colour as specified by the 
! minima in each file, one file for each section.  
! Each file is a list of numbers of minima, 
! one per line as for PICK. max is the total number of minima, not the number 
! in the colour files. 
! currently used for array allocation.
! Colours are chosen automatically to spread over a rainbow spectrum  
! (from red to purple) in the order the files are specified but colours can 
! be specified individually at both COLOURMARKER in this file. - vkd 
!
! TRVAL <max> <filename>
! Colour the graph according to an order parameter value for each 
! minimum. The order parameters are read in from the named file, which should contain 
! one line per minimum.  The expected range of the order parameters is [0,1] inclusive.
! max is the total number of minima.
! Colours are chosen automatically to be evenly distributed with order parameter value 
! along the edges of the RBG colour cube: red -> yellow -> green -> cyan -> blue
!
! TRVALSCALE <x> <y>
! Currently only for use with TRVAL.
! Creates a colour scalebar displaying the order parameter values for each colour.
! <x> and <y> (in postscript units) are optional and overide the default values. - vkd
!
! TSTHRESH    <threshold> ignore transition states above this threshold.
! MAXTSENERGY <threshold> ignore transition states above this threshold.
! MAXTSBARRIER <threshold> ignore transition states with both barriers above this threshold.
!
! WEIGHTS <file>
! If present, use weights in <file> to scale the horizontal width. The expected 
! format of <file> is 
! bin number  Vmin   Vmax  ln weight

!................................................................................!

!!!
!!! KEYWORDS LIBRARY
!!!
!!! Subroutines: ma readread_line(unit[,logical])
!!!              get_string(string[,logical])
!!!              get_integer(integer[,logical])
!!!              get_dp(dp[,logical])
!!!              get_logical(lgcl[,logical])
!!!              upper_case(string)
!!!
!!! Version 2.0
!!! MM 21.ix.96
!!!
! }}}

! Modules: KEYWORDS VARS PAGE {{{
!MODULE KEYWORDS {{{
MODULE KEYWORDS
   IMPLICIT NONE
   INTEGER, PARAMETER :: MAX_LINES=2, MAX_LENGTH=100
   INTEGER, PARAMETER :: TOT_LENGTH=MAX_LINES*MAX_LENGTH
   INTEGER :: POSITION
   CHARACTER(LEN=TOT_LENGTH) :: INPUT
   SAVE
   CONTAINS
!!
! READ_LINE(INT U, L SUCCESS){{{
   SUBROUTINE READ_LINE(U, SUCCESS)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: U
      LOGICAL, INTENT(INOUT), OPTIONAL :: SUCCESS
      INTEGER :: I, LINES, NEXT, ERR
      LOGICAL :: CONTINUE
      CHARACTER(LEN=7) :: FMT_STRING
!     Generate format string of max_length characters.
      WRITE (FMT_STRING, '(I4)') max_length
      FMT_STRING = '(A' // TRIM(ADJUSTL(fmt_string)) // ')'
      DO
         INPUT=' '
         NEXT = 1
!        Read in a logical line consisting of up to max_lines of input file.
         DO LINES=1, MAX_LINES
            CONTINUE = .FALSE.
            READ (U, FMT_STRING, IOSTAT=ERR) INPUT(NEXT:NEXT+MAX_LENGTH-1)
            IF (ERR == 0) THEN
               IF (PRESENT(SUCCESS)) SUCCESS = .TRUE.
            ELSE
               IF (PRESENT(SUCCESS)) SUCCESS = .FALSE.
               EXIT
            ENDIF
!           Check for continuation symbol (&).
            DO I=NEXT, NEXT+MAX_LENGTH-1
               IF (INPUT(I:I)=='&') THEN
                  CONTINUE = .TRUE.
                  NEXT = I
                  EXIT
               ENDIF
            END DO
            IF (.NOT.CONTINUE) EXIT
         END DO
         IF (ERR /= 0) EXIT
         IF (TRIM(INPUT) /= '') EXIT   ! Only read in next line if this one's empty.
      END DO
      POSITION = 1
   END SUBROUTINE READ_LINE
! }}}
!!
! UPPER_CASE(STRING){{{
   SUBROUTINE UPPER_CASE(STRING)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: STRING
      INTEGER, PARAMETER :: LOWER_TO_UPPER = ICHAR("A")-ICHAR("a")
      INTEGER :: I
      DO I=1, LEN_TRIM(STRING)
         IF (LGE(STRING(I:I), 'a').AND.LLE(string(i:i), 'z')) THEN
            STRING(I:I) = ACHAR(IACHAR(STRING(I:I))+LOWER_TO_UPPER)
         ENDIF
      END DO
   END SUBROUTINE UPPER_CASE
! }}}
!!
! GET_STRING(CH(*) STRING, L SUCCESS){{{
   SUBROUTINE GET_STRING(STRING, SUCCESS)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: STRING
      LOGICAL, INTENT(INOUT), OPTIONAL :: SUCCESS
      CHARACTER(LEN=TOT_LENGTH) :: TEMP
      INTEGER :: OUTCOME
      CALL NEXT_ITEM(TEMP, OUTCOME)
      IF (OUTCOME == 3) THEN
         STRING = TEMP
         IF (PRESENT(SUCCESS)) SUCCESS = .TRUE.
      ELSE
         IF (PRESENT(SUCCESS)) SUCCESS = .FALSE.
      ENDIF
   END SUBROUTINE GET_STRING
! }}}
!!
! GET_INTEGER(INT VALUE, L SUCCESS){{{
   SUBROUTINE GET_INTEGER(VALUE, SUCCESS)
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: VALUE
      LOGICAL, INTENT(INOUT), OPTIONAL :: SUCCESS
      INTEGER :: TEMP, OUTCOME, ERR
      CHARACTER(LEN=TOT_LENGTH) :: ITEM
      CALL NEXT_ITEM(ITEM, OUTCOME)
      READ (UNIT=ITEM, FMT=*, IOSTAT=ERR) TEMP
      IF ((ERR == 0).AND.(OUTCOME==3)) THEN
         VALUE = TEMP
         IF (PRESENT(SUCCESS)) SUCCESS = .TRUE.
      ELSE
         IF (PRESENT(SUCCESS)) SUCCESS = .FALSE.
      ENDIF
   END SUBROUTINE GET_INTEGER
! }}}
!!
! READ_TRFILE(CH(120) FILE_TRACE, INT VALUE, INT(:) :: VALUES){{{
   SUBROUTINE READ_TRFILE(FILE_TRACE, VALUE, VALUES)
      IMPLICIT NONE
      INTEGER :: VALUES(:)
      INTEGER :: ERR, I, N_TRACE, VALUE, NUMOFMIN
      CHARACTER(LEN=120) :: FILE_TRACE 
      
      CALL COUNT_MIN(FILE_TRACE, N_TRACE)
      OPEN (UNIT=22, FILE=TRIM(FILE_TRACE), STATUS='OLD', IOSTAT=ERR)
      IF (ERR /= 0) THEN
      WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(file_trace)
      STOP
      END IF
      DO I=1, N_TRACE
      READ (22, *) NUMOFMIN
      VALUES(NUMOFMIN)=VALUE
      END DO
      CLOSE (22)
      WRITE (6, '(A,I2,A,I6, 3A)')'Colour ',value, ': ', n_trace, ' minima read in from ', TRIM(file_trace), '.'
  
   END SUBROUTINE READ_TRFILE
! }}}
!!
! GET_DP(DP VALUE, SUCCESS) {{{
   SUBROUTINE GET_DP(VALUE, SUCCESS)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT) :: VALUE
      LOGICAL, INTENT(INOUT), OPTIONAL :: SUCCESS
      DOUBLE PRECISION :: TEMP
      INTEGER :: OUTCOME, ERR
      CHARACTER(LEN=TOT_LENGTH) :: ITEM
      CALL NEXT_ITEM(ITEM, OUTCOME)
      READ (UNIT=ITEM, FMT=*, IOSTAT=ERR) TEMP
      IF ((ERR == 0).AND.(OUTCOME==3)) THEN
         VALUE = TEMP
         IF (PRESENT(SUCCESS)) SUCCESS = .TRUE.
      ELSE
         IF (PRESENT(SUCCESS)) SUCCESS = .FALSE.
      ENDIF
   END SUBROUTINE GET_DP
! }}}
!!
! GET_LOGICAL(L LGCL, L SUCCESS){{{
   SUBROUTINE GET_LOGICAL(LGCL, SUCCESS)
      IMPLICIT NONE
      LOGICAL, INTENT(INOUT) :: LGCL
      LOGICAL, INTENT(INOUT), OPTIONAL :: SUCCESS
      INTEGER :: OUTCOME
      CHARACTER(LEN=TOT_LENGTH) :: ITEM
      CALL NEXT_ITEM(ITEM, OUTCOME)
      CALL UPPER_CASE(ITEM)
      IF ((TRIM(ITEM)=='TRUE').OR.(TRIM(item)=='T').OR.(TRIM(item)=='.TRUE.') &
    & .OR.(TRIM(ITEM)=='.T.').OR.(TRIM(item)=='ON')) THEN
         LGCL = .TRUE.
         IF (PRESENT(SUCCESS)) SUCCESS = .TRUE.
      ELSE IF ((TRIM(ITEM)=='FALSE').OR.(TRIM(item)=='F').OR.(TRIM(item)=='.FALSE.') &
    & .OR.(TRIM(ITEM)=='.F.').OR.(TRIM(item)=='OFF')) THEN
         LGCL = .FALSE.
         IF (PRESENT(SUCCESS)) SUCCESS = .TRUE.
      ELSE
         IF (PRESENT(SUCCESS)) SUCCESS = .FALSE.
      ENDIF
   END SUBROUTINE GET_LOGICAL
! }}}
!!
! NEXT_ITEM(CH(:) ITEM, INT OUTCOME){{{
   SUBROUTINE NEXT_ITEM(ITEM, OUTCOME)
      IMPLICIT NONE
      CHARACTER(LEN=TOT_LENGTH), INTENT(OUT) :: ITEM
      INTEGER, INTENT(OUT) :: OUTCOME
!     Values of outcome:
!      1: null string read
!      2: end of line reached with no string
!      3: correctly read a string of at least one character
      INTEGER :: I, J
      ITEM = ''
      OUTCOME=1
!     Check we've not already reached the end of the input string.
      IF (POSITION > TOT_LENGTH) THEN
         OUTCOME = 2
      ELSE
!        Read past leading blanks.
         DO I=POSITION, TOT_LENGTH
            IF (INPUT(I:I) /= ' ') EXIT
         END DO
!        Check that this hasn't brought us to the end of the input string.
         IF (I==TOT_LENGTH+1) THEN
            OUTCOME=2
         ELSE
            POSITION = I
            J = 1
!           Read until the next space or comma.
            DO I=POSITION, TOT_LENGTH
               SELECT CASE(INPUT(I:I))
!              If we've reached a comma, record the position for the next
!              item and exit loop.
               CASE(',')
                  POSITION = I+1
                  EXIT
!              If we've reached a space, check for a comma preceded by some
!              blanks, and record the position for the next item as after the
!              comma if one is found.
               CASE(' ')
                  DO J=POSITION+1, TOT_LENGTH
                     SELECT CASE(INPUT(J:J))
                     CASE(',')
                        POSITION = J+1
                        EXIT
                     CASE (' ')   ! Do nothing.
                     CASE DEFAULT
                        POSITION = J
                        EXIT
                     END SELECT
                  END DO
                  EXIT
!              Any other character is the next character of the item being read.
               CASE DEFAULT
                  ITEM(J:J) = INPUT(I:I)
                  J = J + 1
                  OUTCOME=3
                  POSITION = I+1
               END SELECT
            END DO
         ENDIF
      ENDIF
   END SUBROUTINE NEXT_ITEM
   ! }}}
!!
END MODULE KEYWORDS
! }}}
!................................................................................!
!MODULE VARS {{{
MODULE VARS 

   IMPLICIT NONE

   INTEGER, DIMENSION(:), ALLOCATABLE :: NODES, FIRSTCOL, LASTCOL, COLSPAN, CHILDREN
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: BASIN, BRANCHES, COL_0, ORDER, &
      NODE_SIZE, PARENT, MARKNODE
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: DPMARKNODE
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: BASIN_LOC
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: END_X, END_Y, M
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: LEVELWEIGHTS, CENTRESPAN

   INTEGER :: N_LEVELS, N_MIN, LOWEST, MAX_MIN, MAX_MIN2
   INTEGER :: NCONNMIN=0
   INTEGER :: NMINID=0
   INTEGER :: NMINTR=0
   INTEGER :: CONNECTMIN=0
   INTEGER :: MINRANGE=10000
   INTEGER, ALLOCATABLE :: MINIDS(:)
   INTEGER, ALLOCATABLE :: MINTRS(:)
   DOUBLE PRECISION, ALLOCATABLE :: DPMINTRS(:)
   DOUBLE PRECISION, ALLOCATABLE :: ORDER_PARAMETER(:)
   DOUBLE PRECISION :: DELTA_E, E_HIGH
   DOUBLE PRECISION :: TSTHRESH=1.0D100
   DOUBLE PRECISION :: TSBARTHRESH=1.0D100
   DOUBLE PRECISION :: CSCALE=1.0D0 
   DOUBLE PRECISION :: TRSCALEX=90.0
   DOUBLE PRECISION :: TRSCALEY=50.0
   DOUBLE PRECISION :: OP_LOWER, OP_HIGHER
   CHARACTER(LEN=120) :: FILE_MIN, FILE_PICK, FILE_TS, FILE_WEIGHTS, &    
      FILE_TRACE, FILE_METRIC
   CHARACTER(LEN=10) :: LAB_FMT
   LOGICAL :: BARRIERS, CENTRE_GMIN, DUMP_NUMBERS, DUMP_SIZES, EXCLUDEALL, &
      IDENTIFY, MONOTONIC, SPLIT, WEIGHTS
   LOGICAL :: IDENTIFY_NODE=.FALSE.
   LOGICAL :: IDENTIFY_NODE_SIZE=.FALSE.
   LOGICAL :: IDMINT=.FALSE.
   LOGICAL :: TRMINT=.FALSE.
   LOGICAL :: TRPRINT=.FALSE.
   LOGICAL :: TRVALT=.FALSE.
   LOGICAL :: WEIGHTLOWER=.FALSE.
   LOGICAL :: TRVALSCALET=.FALSE.
   LOGICAL :: MATCHMIN=.FALSE.
   LOGICAL :: METRICT=.FALSE.
   LOGICAL :: SETAXIST=.FALSE.
   LOGICAL :: EPSILONT = .FALSE.
   INTEGER, ALLOCATABLE :: NCONN(:)

END MODULE VARS
! }}}
!................................................................................!
!MODULE PAGE{{{
MODULE PAGE

   IMPLICIT NONE
   INTEGER :: PAGE_X=595,  PAGE_Y=842
   INTEGER :: MARGIN_X=40, MARGIN_Y=50
   INTEGER :: SCALE_WIDTH=40
   INTEGER :: FONT_SIZE=10
   INTEGER :: LABEL_SIZE=10

   

END MODULE PAGE
! }}}
! }}}
!................................................................................!
!PROGRAM DISCONNECTION{{{
PROGRAM DISCONNECTION
   
   USE PAGE
   USE VARS
   IMPLICIT NONE
! Parameters {{{
   TYPE TRANSITION_STATE
      DOUBLE PRECISION :: E
      INTEGER :: N, MIN1, MIN2
   END TYPE TRANSITION_STATE

   CHARACTER(LEN=3) :: PG
   CHARACTER(LEN=8) :: MIN_TRIM, BRANCH_TRIM, BRANCH_TRIM2
   DOUBLE PRECISION :: B_HIGH, B_LOW, ENERGY, FRAC, LPAFS, &
      & X1, X2, X3, X_POS, Y1, Y2, Y3, Y_POS, PADDING, E_GMIN, SPLIT_OPTION
   TYPE(TRANSITION_STATE), DIMENSION(:), ALLOCATABLE :: TS
   INTEGER :: I, J, K, P, Q, S, R, F, C, MM, NCONNMAX, J1, NDEAD, NCYCLE, NUNCONA, NLEFTMIN, NLEFTTS
   INTEGER :: BASIN_NO, ERR, H_PG, MIN1, MIN2, MIN_TEMP, N_BR, N_COLS, &
      & N_NODES, N_TS, PATH, USED, BIG_ONES, BASIN_GMIN, GMIN
   INTEGER, DIMENSION(:), ALLOCATABLE :: CONNECT, END_M, INDX1, SORTED
   LOGICAL :: AGAIN, CHANGED
   INTEGER, ALLOCATABLE :: PLUS(:), MINUS(:), NDISTA(:)
   LOGICAL, ALLOCATABLE :: DEADTS(:)
   INTEGER NDUMMY, NN 
   DOUBLE PRECISION VMIN, VMAX, DUMMY,R2
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: END_E, TSEN
   DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: BRANCH_XY

   INTEGER, ALLOCATABLE :: DJWBASIN(:), NMINGROUP(:), GROUPMAP(:)
   DOUBLE PRECISION ETHRESH
   INTEGER NBASIN, NCOUNT, J2
! }}}
   WRITE (6, '(/, A)') 'Disconnectivity Graphs'
   WRITE (6, '(A, /)') '----------------------'

   CALL READ_OPTIONS

!  Procure minima info.
   CALL COUNT_MIN(FILE_MIN, N_MIN)
   ALLOCATE (M(N_MIN), END_X(N_MIN), END_Y(N_MIN))
   ALLOCATE (END_E(0:N_MIN), END_M(0:N_MIN))
   ALLOCATE (DJWBASIN(N_MIN),NMINGROUP(N_MIN),GROUPMAP(0:N_MIN))
   OPEN (UNIT=20, FILE=TRIM(FILE_MIN), STATUS='OLD', IOSTAT=err)
   IF (ERR /= 0) THEN
      WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(file_min)
      STOP
   END IF
   E_GMIN = HUGE(E_GMIN)
   DO I=1, N_MIN
!     READ (20, *) j, m(i)
!  Standard PATHSAMPLE.2.0 format
      READ (20, *) M(I)
      IF (M(I) < E_GMIN) THEN
         E_GMIN = M(I)
         GMIN = I
      END IF
   END DO
   CLOSE (20)
   WRITE (6, '(I6, 3A)') n_min, ' minima read in from ', TRIM(file_min), '.'
   IF ((LOWEST <= 0).OR.(LOWEST > N_MIN)) LOWEST = N_MIN


!  Procure transition state info.
   CALL COUNT_TS(FILE_TS, N_TS, B_HIGH, B_LOW)
   ALLOCATE (TS(N_TS),DEADTS(N_TS))
   I = 1
   OPEN (UNIT=20, FILE=TRIM(FILE_TS), STATUS='OLD', IOSTAT=err)
   IF (ERR /= 0) THEN
      WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(file_ts)
      STOP
   END IF
   DO
!     READ (20, *) ts(i)%n, ts(i)%e, lpafs, pg, h_pg, ts(i)%min1, ts(i)%min2
      READ (20, *) TS(I)%E, LPAFS, H_PG, TS(I)%MIN1, TS(I)%MIN2
      IF (TS(I)%MIN1 /= TS(I)%MIN2) I=I+1
      IF (I == N_TS+1) EXIT
   END DO
   CLOSE (20)
   WRITE (6, '(I6, 3A)') &
      & N_TS, ' non-degenerate paths read in from ', TRIM(file_ts), '.'
   WRITE (6, '(A, 2(F18.10))') 'Highest and lowest transition states: ', &
      & B_HIGH, B_LOW

   PRINT *,'n_min,n_ts=',n_min,n_ts
   ALLOCATE (NCONN(N_MIN),PLUS(N_TS),MINUS(N_TS),NDISTA(N_MIN),TSEN(N_TS))
   DO I=1,N_TS
      TSEN(I)=TS(I)%E
      PLUS(I)=TS(I)%MIN1
      MINUS(I)=TS(I)%MIN2
   ENDDO
   CALL GETNCONN(N_MIN,N_TS,NCONN,PLUS,MINUS,NCONNMIN,NCONNMAX,.FALSE.,TSEN)
   DEADTS(1:N_TS)=.FALSE.
   IF (NCONNMIN.GE.0) THEN
      NDEAD=0
      DO J1=1,N_MIN
         IF (NCONN(J1).LE.NCONNMIN) THEN
            NDEAD=NDEAD+1
         ENDIF 
      ENDDO
      PRINT '(3(I8,A))',NDEAD,' minima with ',NCONNMIN,' connections or fewer will not be considered'
   ENDIF

! DEBUG
   OPEN (UNIT=20, FILE='minima_degree', STATUS='REPLACE')
   DO I = 1, N_MIN
      IF (NCONN(I).gt.0)   WRITE (20, *) I, NCONN(I)

   ENDDO
   CLOSE(20)

   WRITE(*,*) TSTHRESH, 'TSTHRESH'
   OPEN (UNIT=20, FILE='minima_energy', STATUS='REPLACE')
   DO I = 1, N_MIN
      !IF (NCONN(I).GT.O) WRITE(20, *)
      WRITE(20, *) I, M(I)
   ENDDO
   CLOSE(20)

   OPEN (UNIT=20, FILE='ts_energy', STATUS='REPLACE')
   DO I = 1, N_TS
      IF (TS(I)%MIN1 == TS(I)%MIN2) CYCLE
      WRITE(20, *) I, TS(I)%E, TS(I)%MIN1, TS(I)%MIN2
      !WRITE(20, *) I, M(I)
   ENDDO
   CLOSE(20)
   
   OPEN (UNIT=20, FILE='minima_threshold_energy', STATUS='REPLACE')
   DO I = 1, N_MIN
      IF (M(I) .GT. E_HIGH) CYCLE
      WRITE(20, *) I, M(I)
      !WRITE(20, *) I, M(I)
   ENDDO
   CLOSE(20)

   OPEN (UNIT=20, FILE='ts_threshold_energy', STATUS='REPLACE')
   DO I = 1, N_TS
      IF (TS(I)%MIN1 == TS(I)%MIN2) CYCLE
      IF (TS(i)%E .GT. TSTHRESH) CYCLE
      WRITE(20, *) I, TS(I)%E
      !WRITE(20, *) I, M(I)
   ENDDO
   CLOSE(20)
      

! DEBUG


!
!  Check that the stationary point database is actually connected, and remove
!  minima that lie in disjoint graphs.
!  Calculate minimum number of steps of each minimum from the global minimum.
!
   NDISTA(1:N_MIN)=1000000
   IF (CONNECTMIN.GT.0) THEN
      NDISTA(CONNECTMIN)=0
   ELSE
      NDISTA(GMIN)=0
   ENDIF
   NCYCLE=0
5  CHANGED=.FALSE.
   NCYCLE=NCYCLE+1
   DO J1=1,N_TS
      IF ((NCONN(PLUS(J1)).LE.NCONNMIN).OR.(NCONN(MINUS(J1)).LE.NCONNMIN)) CYCLE
      IF (TS(J1)%E.GT.TSTHRESH) CYCLE
      IF ((TS(J1)%E-M(TS(J1)%MIN1).GT.TSBARTHRESH).AND.(TS(J1)%E-M(TS(J1)%MIN2).GT.TSBARTHRESH)) CYCLE
      IF (NDISTA(MINUS(J1))+1.LT.NDISTA(PLUS(J1))) THEN
         CHANGED=.TRUE.
         NDISTA(PLUS(J1))=NDISTA(MINUS(J1))+1
      ENDIF
      IF (NDISTA(PLUS(J1))+1.LT.NDISTA(MINUS(J1))) THEN
         CHANGED=.TRUE.
         NDISTA(MINUS(J1))=NDISTA(PLUS(J1))+1
      ENDIF
   ENDDO
   IF (CHANGED) GOTO 5
   NUNCONA=0
   NLEFTMIN=0
   DO J1=1,N_MIN
      IF (NDISTA(J1).EQ.1000000) THEN
         NUNCONA=NUNCONA+1
         NCONN(J1)=0
   IF (CONNECTMIN.GT.0) THEN
      NDISTA(CONNECTMIN)=0
   ELSE
      NDISTA(GMIN)=0
   ENDIF
      ELSEIF (NCONN(J1).GT.NCONNMIN) THEN
         NLEFTMIN=NLEFTMIN+1
      ENDIF
   ENDDO
   IF (CONNECTMIN.GT.0) THEN
      PRINT '(3(A,I8))','Steps to minimum ',CONNECTMIN,' converged in ',NCYCLE-1,' cycles; disconnected=',NUNCONA
   ELSE
      PRINT '(2(A,I8))','Steps to global minimum converged in ',NCYCLE-1,' cycles; disconnected=',NUNCONA
   ENDIF

   ! DEBUG
   OPEN (UNIT=20, FILE='minima_disjoint_removed', STATUS='REPLACE')
   DO I = 1, N_MIN
      IF (NCONN(I).GT.NCONNMIN) WRITE(20, *) I
   ENDDO
   CLOSE(20)

   !OPEN (UNIT=20, FILE='ts_disjoint_removed', STATUS='REPLACE')
   !DO I = 1, N_TS
   !   IF (.NOT.DEADTS(I)) WRITE(20, *) I
   !ENDDO
   !CLOSE(20)



   ! DEBUG

!
!  Flag transition states to underconnected minima as DEAD.
!  NCONN only counts non-degenerate rearrangements as connections.
!
   NLEFTTS=0
   DO J1=1,N_TS
      IF ((NCONN(PLUS(J1)).LE.NCONNMIN).OR.(NCONN(MINUS(J1)).LE.NCONNMIN)) DEADTS(J1)=.TRUE.
      IF (TS(J1)%E.GT.TSTHRESH) DEADTS(J1)=.TRUE.
      IF ((TS(J1)%E-M(TS(J1)%MIN1).GT.TSBARTHRESH).AND.(TS(J1)%E-M(TS(J1)%MIN2).GT.TSBARTHRESH)) DEADTS(J1)=.TRUE.
      IF (.NOT.DEADTS(J1)) NLEFTTS=NLEFTTS+1
   ENDDO
   PRINT '(A,2I8)','Number of remaining minima and transition states=',NLEFTMIN,NLEFTTS

   ! DEBUG
   OPEN (UNIT=20, FILE='ts_disjoint_removed', STATUS='REPLACE')
   DO I = 1, N_TS
      IF (.NOT.DEADTS(I)) WRITE(20, *) I
   ENDDO
   CLOSE(20)
   ! DEBUG


   ALLOCATE (NODES(N_LEVELS))
   ALLOCATE (BASIN(N_LEVELS, N_MIN))
   WRITE (6, *)

   ! DEBUG
   OPEN (UNIT=20, FILE='minima_disjoint_removed', STATUS='REPLACE')
   DO I = 1, N_MIN
      IF (NCONN(I).GT.NCONNMIN) WRITE(20, *) I
   ENDDO
   CLOSE(20)


   ! DEBUG


!  Check existence of pick file rather than doing basin analysis and then failing.
   IF (FILE_PICK /= '') THEN
      OPEN (UNIT=20, FILE=TRIM(FILE_PICK), STATUS='OLD', IOSTAT=err)
      IF (ERR /= 0) THEN
         WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(file_pick)
         STOP
      END IF
      CLOSE (20)
   END IF

!  Reset transition state energies to the energy of the higher of the two minima
!  they connect if this option is turned on.
   IF (.NOT.BARRIERS) THEN
      WRITE (6, '(A)') &
      'Resetting transition state energies to that of the higher connected minimum.'
      DO I = 1, N_TS
         TS(I)%E = MAX(M(TS(I)%MIN1), M(TS(I)%MIN2))
      END DO
      WRITE (6, '(A, /)') 'Done.'
   END IF

!  Assign the minima to their basins at each energy.
   WRITE (6, '(A)') 'Assigning minima to basins.'
   IF (SPLIT) THEN
      SPLIT_OPTION = 0.0D0
   ELSE
      SPLIT_OPTION = DELTA_E
   END IF
   BASIN = 0
   DO I = 1, N_LEVELS
     ENERGY = E_HIGH - (I-1)*DELTA_E

     DJWBASIN(1:N_MIN)=0
     NBASIN=0
     ETHRESH=ENERGY+SPLIT_OPTION
     ! DEBUG
     !WRITE(*,*) I, ENERGY, ETHRESH, SPLIT_OPTION
     J2 = 0
     DO 
        j2 = J2 + 1
        CHANGED=.FALSE.
        DO J1=1,N_TS
           IF (DEADTS(J1)) CYCLE
           IF (NCONN(TS(J1)%MIN1).LE.NCONNMIN) CYCLE
           IF (NCONN(TS(J1)%MIN2).LE.NCONNMIN) CYCLE
           IF (TS(J1)%E.LT.ETHRESH) THEN
              IF ((DJWBASIN(TS(J1)%MIN1).EQ.0).AND.(DJWBASIN(TS(J1)%MIN2).EQ.0)) THEN
                 CHANGED=.TRUE.
                 NBASIN=NBASIN+1
                 DJWBASIN(TS(J1)%MIN1)=NBASIN
                 DJWBASIN(TS(J1)%MIN2)=NBASIN
              ELSEIF (DJWBASIN(TS(J1)%MIN1).NE.DJWBASIN(TS(J1)%MIN2)) THEN
                 CHANGED=.TRUE.
                 IF (DJWBASIN(TS(J1)%MIN1).EQ.0) THEN
                    DJWBASIN(TS(J1)%MIN1)=DJWBASIN(TS(J1)%MIN2)
                 ELSEIF (DJWBASIN(TS(J1)%MIN2).EQ.0) THEN
                    DJWBASIN(TS(J1)%MIN2)=DJWBASIN(TS(J1)%MIN1)
                 ELSE
                    DJWBASIN(TS(J1)%MIN1)=MIN(DJWBASIN(TS(J1)%MIN1),DJWBASIN(TS(J1)%MIN2))
                    DJWBASIN(TS(J1)%MIN2)=DJWBASIN(TS(J1)%MIN1)
                 ENDIF
              ENDIF
           ENDIF
        ENDDO
        IF (.NOT.CHANGED) THEN
           !WRITE(*,*) 'J2', J2
           EXIT
        ENDIF
     ENDDO
!
!  No need to remove empty groups and close up gaps. 
!  Disconnected minima should all belong to group 0.
!  Minima that are not connected at this energy level but lie below it
!  and will be connected should be assigned to a non-zero basin. DJW
!
     ! DEBUG
     IF (I.EQ.1)  THEN
        OPEN(UNIT=26, FILE='min_in_basin', STATUS='REPLACE')
        OPEN(UNIT=27, FILE='min_in_basin_num', STATUS='REPLACE')
     ENDIF
     DO J1=1,N_MIN
        IF (NCONN(J1).GT.NCONNMIN) THEN
           WRITE(26,*) J1, DJWBASIN(J1), I
           
        ENDIF
     ENDDO
     WRITE(27,*) I, NBASIN
     IF (I.EQ.N_LEVELS) THEN
        CLOSE(26)
        CLOSE(27)
     ENDIF
   !
     
     ! DEBUG

     NCOUNT=NBASIN
     DO J1=1,N_MIN
        IF ((DJWBASIN(J1).EQ.0).AND.(NCONN(J1).GT.0).AND.(M(J1).LT.ENERGY+DELTA_E)) THEN
           NBASIN=NBASIN+1
           DJWBASIN(J1)=NBASIN
        ENDIF
     ENDDO

     BASIN(I,1:N_MIN)=DJWBASIN(1:N_MIN)
     BASIN_NO=NBASIN+1
     NODES(I)=NBASIN
     
     
     IF (NODES(I) == 1) THEN
        WRITE (6, '(I6, A, F18.10)') 1, ' basin  at energy ', energy
     ELSE
        WRITE (6, '(I6, A, F18.10)') nodes(i), ' basins at energy ', energy
        ! DEBUG
        !WRITE (6, *) nodes(i),i, ' basins at energy ', energy
        ! DEBUG
     ENDIF
   END DO
   WRITE (6, '(A, /)') 'Done.'
   OPEN(UNIT=20, FILE='basin_at_level', STATUS='REPLACE')
   DO I = 1, N_LEVELS
      WRITE(20,*) I, NODES(I)
   ENDDO
   CLOSE(20)

   ! DEBUG

   OPEN(UNIT=20, FILE='min_in_basin', STATUS='REPLACE')
   
   DO I = 1, N_LEVELS
      DO J1=1,N_MIN
         IF (NCONN(J1).GT.NCONNMIN) THEN
            WRITE(20,*) J1, I, BASIN(I,J1)
            
         ENDIF
      ENDDO
   ENDDO
   CLOSE(20)
   ! DEBUG


!
!  Read weights and add them up to correspond to the energies used in the superbasin 
!  analysis.
!
   IF (WEIGHTS) THEN
      OPEN (1,FILE=TRIM(ADJUSTL(FILE_WEIGHTS)),STATUS='OLD')
      ALLOCATE(LEVELWEIGHTS(N_LEVELS))
      LEVELWEIGHTS(1:N_LEVELS)=0.0D0
      DO
         READ(1,*,END=111) NDUMMY, VMIN, VMAX, DUMMY
         ILOOP: DO I = 1, N_LEVELS
            ENERGY = E_HIGH - (I-1)*DELTA_E
            IF (VMAX.LT.ENERGY) THEN
               LEVELWEIGHTS(I)=LEVELWEIGHTS(I)+DUMMY
            ENDIF
         ENDDO ILOOP
      ENDDO
111   CONTINUE
      PRINT '(A)','Calculated weights:'
      DUMMY=1.0D100
      DO I = 1, N_LEVELS
         IF (LEVELWEIGHTS(I).GT.0.0D0) THEN
            LEVELWEIGHTS(I)=LOG(LEVELWEIGHTS(I))
            IF (LEVELWEIGHTS(I).LT.DUMMY) DUMMY=LEVELWEIGHTS(I)
         ENDIF
      ENDDO
      DO I = 1, N_LEVELS
         IF (LEVELWEIGHTS(I).NE.0.0D0) LEVELWEIGHTS(I)=LEVELWEIGHTS(I)-DUMMY+1.0D0
         PRINT '(I8,2G20.10)',i,e_high - (i-1)*delta_e,LEVELWEIGHTS(i)
      ENDDO
   ENDIF

! Measure min. and max. values of order parameter pr


!  Various pruning processes follow.
   WRITE (6, '(A)') 'Pruning minima.'

   BASIN = -BASIN ! changes sign of all components

!  1. Remove nodes that don't lead to the lowest energy minima.
   ALLOCATE (INDX1(N_MIN))
   CALL INDEXX(N_MIN, M, INDX1)
   WRITE(*,*) 'LOWEST', LOWEST
   DO I = 1, LOWEST
      DO J = 1, N_LEVELS
         BASIN(J, INDX1(I)) = ABS(BASIN(J, INDX1(I)))
      END DO
   END DO
   DEALLOCATE (INDX1)

   ! DEBUG
   J2 = 0
   DO I = 1, N_MIN
      DO J = 1, N_LEVELS
         IF (BASIN(J, I).LT.0) J2 = J2 + 1
      ENDDO
   ENDDO
   WRITE(*,*) 'NUMBER OF NEGATIVE BASIN)', J2
   ! DEBUG

!  2. Remove nodes that are not monotonic sequence basin bottoms.
   IF (MONOTONIC) THEN
      DO K = 1, N_TS
         IF (M(TS(K)%MIN1) < M(TS(K)%MIN2)) THEN
            MIN1 = TS(K)%MIN2
         ELSE
            MIN1 = TS(K)%MIN1
         END IF
         DO J = 1, N_LEVELS
            BASIN(J, MIN1) = -ABS(BASIN(J, MIN1))
         END DO
      END DO
   END IF

!  3. Exclude all nodes.
   IF (EXCLUDEALL) THEN
      BASIN = -ABS(BASIN)
   END IF

!  4. Deal with individual exceptions picked by user.
   IF (FILE_PICK /= '') THEN
      OPEN (UNIT=20, FILE=TRIM(FILE_PICK), STATUS='OLD', IOSTAT=err)
      IF (ERR /= 0) THEN
         WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(file_pick)
         STOP
      END IF
      DO
         READ (UNIT=20, FMT=*, IOSTAT=ERR) I
         IF (ERR /= 0) EXIT
         IF (ABS(I)==0 .OR. ABS(I)>N_MIN) THEN
            WRITE (6, '(A,I8,3A,I6)') 'WARNING: Ignoring ', i, ' in ', &
               TRIM(FILE_PICK), '; minima numbers: 1 to', n_min
         ELSE
            IF (I < 0) THEN
               DO J = 1, N_LEVELS
                  BASIN(J, -I) = -ABS(BASIN(J, -I))
               END DO
            ELSE
               DO J = 1, N_LEVELS
                  BASIN(J, I) = ABS(BASIN(J, I))
               END DO
            END IF
         END IF
      END DO
      CLOSE (20)
   END IF

!  5. Remove minima with location order-parameter not within parameter limits
   IF (METRICT) THEN
      ! Remove minima with Q<0 or Q>0.
      DO I = 1, N_MIN
         IF (ORDER_PARAMETER(I).LT.0.0.OR.ORDER_PARAMETER(I).GT.1.0) ORDER_PARAMETER(I) = -HUGE(ORDER_PARAMETER(I)) 
         DO J = 1, N_LEVELS
            IF (ORDER_PARAMETER(I).LT.0.0) BASIN(J,I) = -ABS(BASIN(J,I))
            IF (SETAXIST) THEN
               IF (ORDER_PARAMETER(I).LT.OP_LOWER.OR.ORDER_PARAMETER(I).GT.OP_HIGHER) BASIN(J,I) = -ABS(BASIN(J,I))
            END IF
         END DO
      END DO
   END IF

! Ensure that minima specified by IDMIN in the dinfo file are included in the 
! graph, even if they would be excluded by e.g. LOWEST cireterion.
   DO I = 1, NMINID
      DO J = 1, N_LEVELS
         BASIN(J, MINIDS(I)) = ABS(BASIN(J, MINIDS(I)))
      END DO
   ENDDO

!  Renumber basins consecutively after pruning.
   ALLOCATE (INDX1(0:MAXVAL(NODES)))
   DO I = 1, N_LEVELS
      INDX1 = 0
      K = 0
      DO J = 1, N_MIN
         IF (NCONN(J).LE.NCONNMIN) CYCLE
         P = BASIN(I, J)
         IF (P > 0) THEN
            IF (INDX1(P) == 0) THEN
               K = K + 1
               INDX1(P) = K
            END IF
         ELSE
            BASIN(I, J) = 0
         END IF
      END DO
      NODES(I) = K
      WRITE (6, '(A, I3, A, I6)') 'Nodes remaining at level ', i, ': ', k
      DO J = 1, N_MIN
         IF (NCONN(J).LE.NCONNMIN) CYCLE
         BASIN(I, J) = INDX1(BASIN(I, J))
      END DO
   END DO
   DEALLOCATE (INDX1)

   ! DEBUG

   OPEN(UNIT=20, FILE='min_in_basin_renumber_num', STATUS='REPLACE')
   
   DO I = 1, N_LEVELS
      DO J1=1,N_MIN
         IF (NCONN(J1).GT.NCONNMIN) THEN
            WRITE(20,*) J1, I, BASIN(I,J1)
            
         ENDIF
      ENDDO
   ENDDO
   CLOSE(20)
   ! DEBUG



   WRITE (6, '(A, /)') 'Done.'
   ! DEBUG
   OPEN(UNIT=20, FILE='min_in_basin_renumber', STATUS='REPLACE') 
   DO I = 1, N_LEVELS
      WRITE(20,*) I, NODES(I)
   ENDDO
   CLOSE(20)
   ! DEBUG

!  Work out how many branches sprout from each node.

   WRITE (6, '(A)') 'Working out basin branching numbers.'
   ALLOCATE (BRANCHES(N_LEVELS, MAXVAL(NODES)))
   BRANCHES = 0
   DO I = 1, N_LEVELS-1
      K = 1
      DO J = 1, N_MIN
         IF (NCONN(J).LE.NCONNMIN) CYCLE
         IF (BASIN(I+1, J) == K) THEN
            BRANCHES(I, BASIN(I, J)) = BRANCHES(I, BASIN(I, J)) + 1
            K = K + 1
         ENDIF
      END DO
   END DO
   WRITE (6, '(A, /)') 'Done.'

   ! DEBUG
   OPEN(UNIT=20, FILE='min_in_basin_children', STATUS='REPLACE') 
   DO I = 1, N_LEVELS
      DO J1 = 1, N_MIN
         IF (NCONN(J1).GT.NCONNMIN) THEN
            WRITE(20,*) J1, I, BASIN(I,J1), BRANCHES(I,BASIN(I,J1))
         ENDIF
      ENDDO
   ENDDO
   CLOSE(20)
   ! DEBUG



!  Count the number of minima represented by each node and the total at each level.

   WRITE (6, '(A)') 'Counting node sizes.'
   ALLOCATE (NODE_SIZE(N_LEVELS, MAXVAL(NODES)))
   NODE_SIZE = 0
   DO I = 1, N_LEVELS
      DO J = 1, N_MIN
         IF (NCONN(J).LE.NCONNMIN) CYCLE
         K = BASIN(I, J)
         IF (K > 0) THEN
            NODE_SIZE(I, K) = NODE_SIZE(I, K) + 1
         END IF
      END DO
   END DO
   WRITE (6, '(A, /)') 'Done.'


!  Work out order in which to print the nodes in each level.

   IF (.NOT.METRICT) THEN
      ! DEBUG
      !OPEN(UNIT=21, FILE='children_of_min', STATUS='REPLACE')
      WRITE (6, '(A)') 'Ordering branches.'
      ALLOCATE (ORDER(N_LEVELS, MAXVAL(NODES)))
      ORDER = 0
      WRITE (6, '(A)') 'Level  1'
      DO I = 1, NODES(1)
         ORDER(1, I) = I
      END DO
      DO I = 2, N_LEVELS
         WRITE (6, '(A, I3)') 'Level ', i
         IF (ORDER(I-1, 1) == 0) EXIT
         BASIN_GMIN = BASIN(I, GMIN)
         K = 1
         DO J = 1, NODES(I-1)
            N_BR = BRANCHES(I-1, ORDER(I-1, J))
            IF (N_BR > 0) THEN
               ALLOCATE (CONNECT(N_BR), SORTED(N_BR), INDX1(N_BR))
               CALL CONNECTIONS(I-1, ORDER(I-1, J), CONNECT, N_BR)
               DO P = 1, N_BR
                  SORTED(P) = NODE_SIZE(I, CONNECT(P))
               END DO
               CALL INDEXX(N_BR, DBLE(SORTED), INDX1)
               !WRITE(*,*) 'SORTED', SORTED(INDX1(N_BR)), NODES(I-1), N_BR ! DEBUG
               DO P = N_BR, 1, -1
                  !WRITE(*,*) SORTED(INDX1(P))/SORTED(INDX1(N_BR)), 1.0D0*SORTED(INDX1(P))/SORTED(INDX1(N_BR)) ! DEBUG
                  IF (1.0D0*SORTED(INDX1(P))/SORTED(INDX1(N_BR)) < 0.5D0) EXIT
               END DO
               BIG_ONES = N_BR-P
               !WRITE(*,*) 'BIG_ONES', BIG_ONES! DEBUG
               CALL MIX(N_BR-BIG_ONES, INDX1(1 : N_BR-BIG_ONES))
               CALL MIX(BIG_ONES, INDX1(N_BR-BIG_ONES+1 : N_BR))
               PADDING = 1.0D0*(N_BR - BIG_ONES)/(BIG_ONES + 1)
               DO P=1, N_BR
                  !WRITE(*,*) SORTED(INDX1(P))
               ENDDO
               !write(*,*) 'PADDING', PADDING,NINT(PADDING), BIG_ONES, N_BR
               DO P = 1, NINT(PADDING)
                  ORDER(I, K) = CONNECT(INDX1(P))
                  !WRITE(*,*) 'ORDER', ORDER(I,K)
                  K = K + 1
               END DO
               DO Q = 1, BIG_ONES
                  ORDER(I, K) = CONNECT(INDX1(N_BR+1-Q))
                  !WRITE(*,*) 'MORE ORDER', ORDER(I,K)
                  K = K + 1
                  DO P = NINT(PADDING*Q)+1, NINT(PADDING*(Q+1))
                     ORDER(I, K) = CONNECT(INDX1(P))
                    ! WRITE(*,*) 'MORE MORE ORDER', ORDER(I,K)
                     K = K + 1
                  END DO
               END DO
               IF (CENTRE_GMIN) THEN
                  DO P = K-N_BR, K-1
                     IF (ORDER(I, P) == BASIN_GMIN) THEN
                        Q = K-N_BR + INT(N_BR/2.0D0)
                        S = ORDER(I, Q)
                        ORDER(I, Q) = BASIN_GMIN
                        ORDER(I, P) = S 
                        EXIT
                     END IF
                  END DO
               END IF
               ! DEBUG
               IF (I.EQ.2) THEN
                  OPEN(UNIT=20, FILE='order_of_min', STATUS='REPLACE') 
               ENDIF
               !DO P =1, N_BR
               IF (ORDER(I,J).GT.0) THEN
                  WRITE(20,*) I, J, PADDING, N_BR, BIG_ONES
                  DO P = 1, N_BR
                     WRITE(20,*) connect(INDX1(P)), NODE_SIZE(I,INDX1(P)) 
                  ENDDO
               ENDIF
               IF (I.EQ.N_LEVELS) CLOSE(20)
               ! DEBUG
               DEALLOCATE (CONNECT, SORTED, INDX1)
            ENDIF
         END DO
      END DO
      WRITE (6, '(A, /)') 'Done.'
   END IF
   ! DEBUG
   OPEN(UNIT=20, FILE='order_of_min2', STATUS='REPLACE')
   DO I = 1, N_LEVELS
      DO J = 1,NODES(I)
         IF (ORDER(I,J).GT.0) WRITE(20,*) I, J, ORDER(I,J)
      ENDDO
   ENDDO
   CLOSE(20)
   ! DEBUG

!  Dump the number of minima represented by each node in the order they will appear.

   IF (DUMP_SIZES) THEN
      WRITE (6, '(A)') 'Writing node sizes to "node_sizes".'
      OPEN (UNIT=20, FILE='node_sizes', STATUS='REPLACE')
      DO I = 1, N_LEVELS
         WRITE (20, '(A, I2, A, F20.10)') 'Level: ', i, ' energy ', e_high - (i-1)*delta_e
         DO J = 1, NODES(I)
            IF (METRICT) THEN
               K = J
            ELSE 
               K = ORDER(I, J)
            END IF
            WRITE (20, '(I5, A, I7)') j, ':', node_size(i, k)
         END DO
         WRITE (20, *)
      END DO
      CLOSE (20)
      WRITE (6, '(A, /)') 'Done.'
   END IF


!  Dump the minima numbers at each node if required.
!  This double loop for each level is probably not the most efficient way, but...

   IF (DUMP_NUMBERS) THEN
      WRITE (6, '(A)') &
      'Writing minima numbers associated with nodes to "node_numbers".'
      OPEN (UNIT=20, FILE='node_numbers', STATUS='REPLACE')
      DO I = 1, N_LEVELS
         WRITE (20, '(/, A, I3, /, A)') 'LEVEL ', i, '========='
         DO J = 1, NODES(I)
            WRITE (20, '(/, A, I6)') 'Node ', j
            IF (METRICT) THEN
               K = J
            ELSE 
               K = ORDER(I, J)
            END IF
               DO P = 1, N_MIN
               IF (NCONN(P).LE.NCONNMIN) CYCLE
               IF (BASIN(I, P) == K) WRITE (20, '(I6)') p
            END DO
         END DO
      END DO
      CLOSE (20)
      WRITE (6, '(A, /)') 'Done.'
   END IF

! Colour minima in sections if required.
! For each node, check all minima for which the node is a parent.
! If all minima are contained on one of the lists, the node 
! will be coloured to represent that list.  
! If any minimum is not contained on one of the lists, the node
! is not coloured. 
! If all minima are contained on lists but more than one list 
! is represented, the node will be the colour of the last list
! as specified on the input line in dinfo. 

   IF (TRMINT) THEN
    IF (TRPRINT) THEN
      WRITE (6, '(A)') 'Writing section assignments of nodes for colour to "node_sections".'
      OPEN (UNIT=20, FILE='node_sections', STATUS='REPLACE') 
    ELSE
      WRITE (6, '(A)') 'Finding section assignments of nodes for colour".'
    ENDIF 
!       Marknode records the list to which the node belongs if any.
       ALLOCATE (MARKNODE(N_LEVELS, MAXVAL(NODES))) 
       MARKNODE=-1
       DO I = 1, N_LEVELS
             DO P = 1, N_MIN
               IF (NCONN(P).LE.NCONNMIN) CYCLE
! BASIN(I,P) is the basin to which minimum P belongs at level I.
                 K=BASIN(I,P)
                 IF (K.EQ.0) CYCLE
                 IF (MARKNODE(I,K)==0) CYCLE
                 IF (MINTRS(P)==0.or.MINTRS(P)> MARKNODE(I,K)) MARKNODE(I,K)=MINTRS(P)
! If any minimum is not listed at all the node is not coloured
            END DO 
    IF (TRPRINT) THEN
         WRITE (20, '(/, A, I3, /, A)') 'LEVEL ', i, '========='
           DO J = 1, NODES(I)
              K=ORDER(I,J) 
              IF (METRICT) K = J
              IF (MARKNODE(I,K)/=0) WRITE (20, '(A, I6, A, I6)') 'Node ', j, ' --> Colour ', MARKNODE(I,K)
           ENDDO
        ENDIF
!         END DO
     END DO
     IF (TRPRINT) THEN
        CLOSE(20)
     ENDIF
     WRITE (6, '(A, /)') 'Done.'
  END IF


! JMC Colour minima by value of DPMARKNODE, with input values in the range 0.0d0 to 1.0d0
! For each node, check all minima for which the node is a parent.
! The node will be the colour of the minimum with the highest value 
! of DPMARKNODE

   IF (TRVALT) THEN
      WRITE (6, '(A)') 'Finding section assignments of nodes for colouring.'
      !       Marknode records the list to which the node belongs if any.
      ALLOCATE (DPMARKNODE(N_LEVELS, MAXVAL(NODES))) 
      DPMARKNODE=-2.0D0 ! initialization
      IF (WEIGHTLOWER) DPMARKNODE=HUGE(1.0D0) ! initialization
      DO I = 1, N_LEVELS
         DO P = 1, N_MIN
            IF (NCONN(P).LE.NCONNMIN) CYCLE
            ! BASIN(I,P) is the basin to which minimum P belongs at level I.
            K=BASIN(I,P)
            IF (K.EQ.0) CYCLE
            IF (DPMARKNODE(I,K)==-1.0D0) CYCLE
            IF (.NOT.WEIGHTLOWER) THEN 
               IF (DPMINTRS(P)==-1.0D0.or.DPMINTRS(P)> DPMARKNODE(I,K)) DPMARKNODE(I,K)=DPMINTRS(P)
            ELSE
               IF (DPMINTRS(P)==-1.0D0.or.DPMINTRS(P)< DPMARKNODE(I,K)) DPMARKNODE(I,K)=DPMINTRS(P)
            ENDIF
            ! DPMINTRS(P)=-1.0D0=initial value. If any minimum is not listed at all the node is not coloured
         END DO
         !         END DO
      END DO
      WRITE (6, '(A, /)') 'Done.'
   END IF

   IF (METRICT) THEN
      
      IF (.NOT.SETAXIST) THEN
         OP_HIGHER = -HUGE(OP_HIGHER)
         OP_LOWER = HUGE(OP_LOWER)
      END IF
      
      ALLOCATE (BASIN_LOC(N_LEVELS,MAXVAL(NODES)))
      BASIN_LOC = 0
      DO J = 1, N_LEVELS
         DO I = 1, N_MIN
            K = BASIN(J,I)
            IF (K.EQ.0) CYCLE
            IF (.NOT.SETAXIST) THEN
               IF (ORDER_PARAMETER(I).GT.OP_HIGHER) OP_HIGHER = ORDER_PARAMETER(I)
               IF (ORDER_PARAMETER(I).LT.OP_LOWER) OP_LOWER = ORDER_PARAMETER(I)
               
            END IF
            !WRITE(*,*) 'ORDER_P', ORDER_PARAMETER(I), 'BASIN', J, K
            BASIN_LOC(J,K) = BASIN_LOC(J,K) + ORDER_PARAMETER(I)
         END DO
         !DO K = 1, NODES(J)
         !   BASIN_LOC(J,K) = BASIN_LOC(J,K) / NODE_SIZE(J,K)
         !END DO
      END DO

      ! Scale BASIN_LOC according to the values of OP_HIGHER and OP_LOWER

      DO J = 1, N_LEVELS
         DO K = 1, NODES(J)
            !WRITE(*,*) 'BASIN', BASIN_LOC(J,K), 'BASIN', J, K
            BASIN_LOC(J,K) = BASIN_LOC(J,K) / NODE_SIZE(J,K)
            BASIN_LOC(J,K) = (BASIN_LOC(J,K) - OP_LOWER) / (OP_HIGHER - OP_LOWER)
            
            !WRITE(*,*) 'BASIN NEW', BASIN_LOC(J,K), 'SIZE', NODE_SIZE(J,K)
         END DO
      END DO
         !WRITE(*,*) 'OP_LOWER', OP_LOWER, 'OP_HIGHER', OP_HIGHER
   END IF
   
   
!  Find the parent of each basin.

   WRITE (6, '(A)') 'Finding the parent of each basin.'
   ALLOCATE (PARENT(N_LEVELS, MAXVAL(NODES)))
   DO I = 2, N_LEVELS
      DO J = 1, N_MIN
         IF (NCONN(J).LE.NCONNMIN) CYCLE
         IF (BASIN(I, J) > 0) THEN
            PARENT(I, BASIN(I, J)) = BASIN(I-1, J)
         END IF
      END DO
   END DO
   WRITE (6, '(A, /)') 'Done.'

   ! DEBUG
   OPEN(UNIT=20, FILE='min_in_basin_parent', STATUS='REPLACE') 
   DO I = 1, N_LEVELS
      DO J1 = 1, N_MIN
         IF (NCONN(J1).LE.NCONNMIN) CYCLE
         IF (BASIN(I,J1) .EQ. 0) CYCLE
         WRITE(20,*) J1, I, BASIN(I,J1), PARENT(I,BASIN(I,J1)), BRANCHES(I,BASIN(I,J1))
         !ENDIF
      ENDDO
   ENDDO
   CLOSE(20)
   ! DEBUG



   IF (.NOT.METRICT) THEN
      !  Allocate virtual columns to each node.

      WRITE (6, '(A)') 'Allocating vertical space to nodes.'

      !  1. Number of columns for graph = total number of minima plotted.

      N_COLS = 0
      DO I = 1, NODES(1)
         N_COLS = N_COLS + NODE_SIZE(1, I)
      END DO

      !  2.  Allocate columns to first-row nodes.
      ALLOCATE (COL_0(N_LEVELS, MAXVAL(NODES)))
      J = 0
      DO I = 1, NODES(1)
         COL_0(1, ORDER(1, I)) = J + 1
         J = J + NODE_SIZE(1, ORDER(1, I))
      END DO

      !  3. Allocate to each node in successive levels a fraction of the vertical space
      !     allocated to its parent.
      ALLOCATE (INDX1(MAXVAL(NODES)))
      DO I = 2, N_LEVELS
         INDX1 = COL_0(I-1, :)
         DO J = 1, NODES(I)
            K = ORDER(I, J)
            COL_0(I, K) = INDX1(PARENT(I, K))
            INDX1(PARENT(I, K)) = INDX1(PARENT(I, K)) + NODE_SIZE(I, K)
         END DO
      END DO
      DEALLOCATE (INDX1)

      WRITE (6, '(A, /)') 'Done.'


      !  Find first and last occupied column in each level

      WRITE (6, '(A)') 'Finding left and right edges of the graph at each level'
      ALLOCATE (FIRSTCOL(N_LEVELS), LASTCOL(N_LEVELS), COLSPAN(N_LEVELS))
      ALLOCATE (CENTRESPAN(N_LEVELS))
      DO I = 1, N_LEVELS
         FIRSTCOL = N_COLS
         LASTCOL = 1
         DO J = 1, NODES(I)
            IF (COL_0(I,J) < FIRSTCOL(I)) FIRSTCOL(I) = COL_0(I,J)
            IF (COL_0(I,J)+NODE_SIZE(I,J) > LASTCOL(I)) LASTCOL(I) = COL_0(I,J)+NODE_SIZE(I,J)
         END DO
         COLSPAN(I) = LASTCOL(I) - FIRSTCOL(I)
         CENTRESPAN(I) = (LASTCOL(I) + FIRSTCOL(I) - 1.0D0) / (2.0D0 * N_COLS)
      END DO
      WRITE (6, '(A, /)') 'Done.'

      IF (IDENTIFY_NODE.OR.IDENTIFY_NODE_SIZE) THEN
         PRINT '(A,2I8)','dimensions of BRANCH_XY are ',SUM(NODES),MAXVAL(NODES)
         ALLOCATE(BRANCH_XY(SUM(NODES),MAXVAL(NODES),4))
      ENDIF
      ALLOCATE(CHILDREN(n_min))
      NN = 1
   END IF
!  Generate the PostScript file.
   
   WRITE (6, '(A)') 'Writing tree.ps.'
   END_X = 0.0D0
   END_Y = 0.0D0
   OPEN (UNIT=20, FILE='tree.ps', STATUS='REPLACE')
   IF (METRICT) THEN
      IF (EPSILONT) THEN
         CALL EPSILON_METRIC_HEADER
      ELSE
         CALL METRIC_HEADER
      END IF
   ELSE 
      IF (EPSILONT) THEN
         CALL EPSILON_HEADER
      ELSE
         CALL HEADER
      END IF
   END IF
   DO I = 2, N_LEVELS
      CALL ENDPOINTS(I, END_E, END_M)
      WRITE (20, '(/, A, I2, A, I2, /)') '% Level: ', i-1, ' to ', i
      IF (NODES(I-1) == 0) EXIT
      ENERGY = E_HIGH - (I-2)*DELTA_E
      Y1 = Y_POS(1.0D0*(I-2)/(N_LEVELS-1))
      Y2 = Y_POS(1.0D0*(I-1)/(N_LEVELS-1))
      ! DEBUG
      WRITE(*,*) I,1.0D0*(I-2)/(N_LEVELS-1),1.0D0*(I-1)/(N_LEVELS-1),ENERGY
      ! DEBUG
      DO J = NODES(I), 1, -1!1, NODES(I)
         
         IF (METRICT) THEN
            !WRITE(*,*) 'I', I, 'J', J, 'K', K
            K = J
            X2 = BASIN_LOC(I,K)
            X1 = BASIN_LOC(I -1,PARENT(I,K))
            !WRITE(*,*) 'X1', X1, 'X2', X2
         ELSE
            K = ORDER(I, J)
            X1 = (COL_0(I-1, PARENT(I,K)) + NODE_SIZE(I-1, PARENT(I,K))/2.0 - 1.0) / N_COLS
            X2 = (COL_0(I, K) + NODE_SIZE(I, K)/2.0 - 1.0) / N_COLS
         END IF
         IF (WEIGHTS) THEN
            X1 = (X1-CENTRESPAN(I-1)) * (LEVELWEIGHTS(I-1)/LEVELWEIGHTS(1)) &
                 * (1.0D0*COLSPAN(1)/COLSPAN(I-1)) + 0.5
            X2 = (X2-CENTRESPAN(I)) * (LEVELWEIGHTS(I)/LEVELWEIGHTS(1)) &
                 * (1.0D0*COLSPAN(1)/COLSPAN(I)) + 0.5
         ENDIF
         !        PRINT '(A,2I6,4F15.5)','i,j,x1,x2,x_pos1,x_pos2: ',i,j,x1,x2,x_pos(x1),x_pos(x2)
         X1 = X_POS(X1)
         X2 = X_POS(X2)

!        IF ( BRANCHES(I,K) > 1) THEN
         IF ( ALLOCATED(BRANCH_XY) ) THEN
            PRINT '(A,2I8)','I,K=',I,K
            PRINT '(A,G20.10)','branch',BRANCH_XY(I,K,1)
            BRANCH_XY(I,K,1) = X1
            BRANCH_XY(I,K,2) = Y1
            BRANCH_XY(I,K,3) = X2
            BRANCH_XY(I,K,4) = Y2
         ENDIF

         NN = NN + 1
         
         IF (BRANCHES(I,K).EQ.0) THEN
         !IF (BRANCHES(I,K) == 0) THEN

            FRAC = (ENERGY-END_E(K))/DELTA_E
            Y3 = FRAC*(Y2-Y1) + Y1
            IF (METRICT) THEN
               X3 = X2
            ELSE
               X3 = FRAC*(X2-X1) + X1
            END IF
            IF (Y3 < Y2) Y3=Y2
            END_X(END_M(K)) = X3
            END_Y(END_M(K)) = Y3
            IF (TRMINT) THEN ! otherwise MARKNODE is not allocated
               IF (MARKNODE(I,K).GT.0) THEN
                  
! COLOURMARKER - This is where the colours are specified for minima.
! To choose your own colours, comment the following section and use the 
! IF (MARKNODE(I,K) ==#) statements below, where # is the section number.
! Colours should be the same for the nodes below.

!                 R2=(MARKNODE(I,K)/FLOAT(NMINTR))*10      
!                 IF (R2.LE.1.AND.R2.GT.0)  WRITE (20, '(A,F6.4,A)') '1 0 ',(1-R2)/2,'  setrgbcolor'
!                 IF (R2.LE.3.AND.R2.GT.1)  WRITE (20, '(A,F6.4,A)') '1 ',(R2-1)/2,' 0  setrgbcolor' 
!                 IF (R2.LE.5.AND.R2.GT.3)  WRITE (20, '(F6.4,A)') (5-R2)/2,' 1 0  setrgbcolor'
!                 IF (R2.LE.7.AND.R2.GT.5)  WRITE (20, '(A,F6.4,A)') '0 1 ',(R2-5)/2  ,'  setrgbcolor'
!                 IF (R2.LE.9.AND.R2.GT.7)  WRITE (20, '(A,F6.4,A)') '0 ',(9-R2)/2 ,' 1  setrgbcolor'
!                 IF (R2.LE.10.AND.R2.GT.9)  WRITE (20, '(F6.4,A)') (R2-9)/2 ,' 0 1  setrgbcolor'
                  IF (MARKNODE(I,K) ==1)  WRITE (20, '(A)') '1 0 0  setrgbcolor'
                  IF (MARKNODE(I,K) ==2)  WRITE (20, '(A)') '0 1 0 setrgbcolor'
                  IF (MARKNODE(I,K) ==3)  WRITE (20, '(A)') '0 0 1 setrgbcolor'
                  IF (MARKNODE(I,K) ==4)  WRITE (20, '(A)') '1 0 1 setrgbcolor'  ! magenta
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x3, y3, ' ls'
                  WRITE (20, '(A)') '0 0 0 setrgbcolor'
               ELSE
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x3, y3, ' ls'
               ENDIF
            ELSE IF (TRVALT) THEN ! otherwise DPMARKNODE is not allocated
               IF (DPMARKNODE(I,K).GE.0.0D0) THEN
                  R2=DPMARKNODE(I,K)*3.0D0*CSCALE
                  IF (R2.GT.3.0D0) THEN
                     WRITE(6,*) 'WARNING: value of DPMARKNODE is ',DPMARKNODE(I,K),' for ',i,k
                     R2=3.0D0
                  END IF
                  IF (R2.LT.1.0d0.AND.R2.GE.0.0d0)  WRITE (20, '(2F7.4,A)') (2.67d0-R2)/2.67D0 ,R2/2.0D0, ' 0  setrgbcolor'
                  IF (R2.LT.1.5d0.AND.R2.GE.1.0d0)  WRITE (20, '(F7.4,A)')  0.625D0-(R2-1.0D0)*1.25D0, ' 0.50  0  setrgbcolor' 
                  IF (R2.LT.2.0d0.AND.R2.GE.1.5d0)  WRITE (20, '(A,F7.4,A)') '0 0.5 ',0.625D0+(R2-2.0d0)*1.25D0,'  setrgbcolor'
                  IF (R2.LE.3.0d0.AND.R2.GE.2.0d0)  WRITE (20, '(A,2F7.4,A)') '0 ',(3.0d0-R2)/2.0D0, ((2.0D0-R2)-1.67d0)/(-2.67D0), '  setrgbcolor'
!                  IF (R2.LE.1.AND.R2.GE.0)  WRITE (20, '(A,F6.4,A)') '1 0 ',(1-R2)/2,'  setrgbcolor'
!                  IF (R2.LE.3.AND.R2.GT.1)  WRITE (20, '(A,F6.4,A)') '1 ',(R2-1)/2,' 0  setrgbcolor' 
!                  IF (R2.LE.5.AND.R2.GT.3)  WRITE (20, '(F6.4,A)') (5-R2)/2,' 1 0  setrgbcolor'
!                  IF (R2.LE.7.AND.R2.GT.5)  WRITE (20, '(A,F6.4,A)') '0 1 ',(R2-5)/2  ,'  setrgbcolor'
!                  IF (R2.LE.9.AND.R2.GT.7)  WRITE (20, '(A,F6.4,A)') '0 ',(9-R2)/2 ,' 1  setrgbcolor'
!                  IF (R2.LE.10.AND.R2.GT.9)  WRITE (20, '(F6.4,A)') (R2-9)/2 ,' 0 1  setrgbcolor'
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x3, y3, ' ls'
                  WRITE (20, '(A)') '0 0 0 setrgbcolor'
               ELSE
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x3, y3, ' ls'
               ENDIF
            ELSE
               WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x3, y3, ' ls'
            ENDIF
         ELSE
            
            IF (TRMINT) THEN
               IF (MARKNODE(I,K).GT.0) THEN

! COLOURMARKER - This is where the colours are specified for nodes.
! To choose your own colours, comment the following section and use the 
! IF (MARKNODE(I,K) ==#) statements below, where # is the section number.
! Colours should be the same for the minima above.
   
!                  R2=(MARKNODE(I,K)/FLOAT(NMINTR))*10
!                  IF (R2.LE.1.AND.R2.GT.0)  WRITE (20, '(A,F6.4,A)') '1 0 ',(1-R2)/2,'  setrgbcolor'
!                  IF (R2.LE.3.AND.R2.GT.1)  WRITE (20, '(A,F6.4,A)') '1 ',(R2-1)/2,' 0  setrgbcolor' 
!                  IF (R2.LE.5.AND.R2.GT.3)  WRITE (20, '(F6.4,A)') (5-R2)/2,' 1 0  setrgbcolor'
!                  IF (R2.LE.7.AND.R2.GT.5)  WRITE (20, '(A,F6.4,A)') '0 1 ',(R2-5)/2  ,'  setrgbcolor'
!                  IF (R2.LE.9.AND.R2.GT.7)  WRITE (20, '(A,F6.4,A)') '0 ',(9-R2)/2 ,' 1  setrgbcolor'
!                  IF (R2.LE.10.AND.R2.GT.9)  WRITE (20, '(F6.4,A)') (R2-9)/2 ,' 0 1  setrgbcolor'
                  IF (MARKNODE(I,K) ==1)  WRITE (20, '(A)') '1 0 0  setrgbcolor'
                  IF (MARKNODE(I,K) ==2)  WRITE (20, '(A)') '0 1 0 setrgbcolor'
                  IF (MARKNODE(I,K) ==3)  WRITE (20, '(A)') '0 0 1 setrgbcolor'
                  IF (MARKNODE(I,K) ==4)  WRITE (20, '(A)') '1 0 1 setrgbcolor'  ! magenta
               
!                 IF (MARKNODE(I,K) ==1)  WRITE (20, '(A)') '1 0 0  setrgbcolor'
!                 IF (MARKNODE(I,K) ==2)  WRITE (20, '(A)') '1 0.5 0 setrgbcolor'
!                 IF (MARKNODE(I,K) ==3)  WRITE (20, '(A)') '1 1 0 setrgbcolor'
!                 IF (MARKNODE(I,K) ==4)  WRITE (20, '(A)') '0.5 1 0 setrgbcolor'
!                 IF (MARKNODE(I,K) ==5)  WRITE (20, '(A)') '0 1 0 setrgbcolor'
!                 IF (MARKNODE(I,K) ==6)  WRITE (20, '(A)') '0 1 0.5 setrgbcolor'
!                 IF (MARKNODE(I,K) ==7)  WRITE (20, '(A)') '0 1 1 setrgbcolor'
!                 IF (MARKNODE(I,K) ==8)  WRITE (20, '(A)') '0 0.5 1 setrgbcolor'
!                 IF (MARKNODE(I,K) ==9)  WRITE (20, '(A)') '0 0 1 setrgbcolor'
!                 IF (MARKNODE(I,K) ==10)  WRITE (20, '(A)') '0.5 0 1 setrgbcolor'
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y2, ' ls'
                  WRITE (20, '(A)') '0 0 0 setrgbcolor'
               ELSE
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y2, ' ls'
               ENDIF
            ELSE IF (TRVALT) THEN
               IF (DPMARKNODE(I,K).GE.0.0D0) THEN
                  R2=DPMARKNODE(I,K)*3.0D0*CSCALE
                  IF (R2.GT.3.0D0) THEN
                     WRITE(6,*) 'WARNING: value of DPMARKNODE is ',DPMARKNODE(I,K),' for ',i,k
                     R2=3.0D0
                  END IF
                  IF (R2.LT.1.0d0.AND.R2.GE.0.0d0)  WRITE (20, '(2F7.4,A)') (2.67d0-R2)/2.67D0 ,R2/2.0D0, ' 0  setrgbcolor'
                  IF (R2.LT.1.5d0.AND.R2.GE.1.0d0)  WRITE (20, '(F7.4,A)')  0.625D0-(R2-1.0D0)*1.25D0, ' 0.50  0  setrgbcolor' 
                  IF (R2.LT.2.0d0.AND.R2.GE.1.5d0)  WRITE (20, '(A,F7.4,A)') '0 0.5 ',0.625D0+(R2-2.0d0)*1.25D0,'  setrgbcolor'
                  IF (R2.LE.3.0d0.AND.R2.GE.2.0d0)  WRITE (20, '(A,2F7.4,A)') '0 ',(3.0d0-R2)/2.0D0, ((2.0D0-R2)-1.67d0)/(-2.67D0), '  setrgbcolor'
!                  IF (R2.LT.1.0d0.AND.R2.GE.0.0d0)  WRITE (20, '(A,F6.4,A)') '1 ',R2,' 0  setrgbcolor'
!                  IF (R2.LT.2.0d0.AND.R2.GE.1.0d0)  WRITE (20, '(F6.4,A)')  2.0d0-R2,' 1 0  setrgbcolor' 
!                  IF (R2.LT.3.0d0.AND.R2.GE.2.0d0)  WRITE (20, '(A,F6.4,A)') '0 1 ',R2-2.0d0,'  setrgbcolor'
!                  IF (R2.LE.4.0d0.AND.R2.GE.3.0d0)  WRITE (20, '(A,F6.4,A)') '0 ',4.0d0-R2,' 1  setrgbcolor'
!                  IF (R2.LE.1.AND.R2.GE.0)  WRITE (20, '(A,F6.4,A)') '1 0 ',(1-R2)/2,'  setrgbcolor'
!                  IF (R2.LE.3.AND.R2.GT.1)  WRITE (20, '(A,F6.4,A)') '1 ',(R2-1)/2,' 0  setrgbcolor' 
!                  IF (R2.LE.5.AND.R2.GT.3)  WRITE (20, '(F6.4,A)') (5-R2)/2,' 1 0  setrgbcolor'
!                  IF (R2.LE.7.AND.R2.GT.5)  WRITE (20, '(A,F6.4,A)') '0 1 ',(R2-5)/2  ,'  setrgbcolor'
!                  IF (R2.LE.9.AND.R2.GT.7)  WRITE (20, '(A,F6.4,A)') '0 ',(9-R2)/2 ,' 1  setrgbcolor'
!                  IF (R2.LE.10.AND.R2.GT.9)  WRITE (20, '(F6.4,A)') (R2-9)/2 ,' 0 1  setrgbcolor'
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y2, ' ls'
                  WRITE (20, '(A)') '0 0 0 setrgbcolor'
               ELSE
                  WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y2, ' ls'
               ENDIF
            ELSE
               WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y2, ' ls'
            ENDIF
         ENDIF
1234  ENDDO
   ENDDO
   write(*,*) nn

   IF (IDENTIFY) THEN
      WRITE (20, '(A)') '1 0 0 setrgbcolor'
      WRITE (20, '(A, I2, A)') &
         & '/Times-Roman findfont ', LABEL_SIZE, ' scalefont setfont'
      DO I = 1, N_MIN
         IF (NCONN(I).LE.NCONNMIN) CYCLE
         IF ( (END_X(I) > 0.0D0).AND.(END_Y(I) > 0.0D0) ) THEN
            WRITE (MIN_TRIM, '(I8)') I
            WRITE (20, '(2F7.2, 3A)') END_X(I), END_Y(I), &
               & ' mt (', TRIM(ADJUSTL(MIN_TRIM)), ') show'
         END IF
      END DO
      WRITE (20, '(A)') '0 0 0 setrgbcolor'
   END IF


   IF (IDENTIFY_NODE) THEN
      nn = 1
      WRITE (20, '(A)') '0.5 0 1 setrgbcolor' ! in dark blue
      WRITE (20, '(A, I2, A)') &
               &'/Times-Roman findfont ', LABEL_SIZE, ' scalefont setfont'
      DO I = 1, N_LEVELS
         DO J = 1, NODES(I)
            K = ORDER(I,J)
            !IF ( node_size(i,order(i,j)) > 1) THEN
            IF ( BRANCHES(I,ORDER(I,J)) > 1) THEN
               IF ( (BRANCH_XY(I,K,3) > 0.0D0).AND.(BRANCH_XY(I,K,4) > 0.0D0)  &
                     & .AND. NODE_SIZE(I,ORDER(I,J)) > MAX_MIN ) THEN
                  WRITE (BRANCH_TRIM, '(I8)') I
                  WRITE (BRANCH_TRIM2, '(I8)') J
                  WRITE (20, '(2F7.2, 3A)') BRANCH_XY(I,K,3), BRANCH_XY(I,K,4), &
                     & ' mt (', TRIM(ADJUSTL(BRANCH_TRIM))//'_'//TRIM(ADJUSTL(BRANCH_TRIM2)), ') show'
               END IF
            END IF
            NN = NN+1
         END DO
      END DO
      WRITE (20, '(A)') '0 0 0 setrgbcolor'
   END IF

   IF (IDENTIFY_NODE_SIZE) THEN
      WRITE (20, '(A)') '0 0.7 0 setrgbcolor' ! in green
      WRITE (20, '(A, I2, A)') '/Times-Roman findfont ', LABEL_SIZE, ' scalefont setfont'
      DO I = 1, N_LEVELS
         DO J = 1, NODES(I)
            K = ORDER(I,J)
            IF ( NODE_SIZE(I,ORDER(I,J)) > 1) THEN
               IF ( (BRANCH_XY(I,K,3) > 0.0D0).AND.(BRANCH_XY(I,K,4) > 0.0D0) &
                    & .AND. NODE_SIZE(I,ORDER(I,J)) > MAX_MIN2 ) THEN
                  WRITE (BRANCH_TRIM, '(I8)') NODE_SIZE(I,ORDER(I,J))
                  WRITE (20, '(2F7.2, 3A)') BRANCH_XY(I,K,3), BRANCH_XY(I,K,4), &
                     & ' mt (', TRIM(ADJUSTL(BRANCH_TRIM)), ') show'
               END IF 
            END IF
         END DO
      END DO
      WRITE (20, '(A)') '0 0 0 setrgbcolor'
   END IF


   IF (IDMINT) THEN
      WRITE (20, '(A)') '1 0 0 setrgbcolor'
      WRITE (20, '(A, I2, A)') &
         & '/Times-Roman findfont ', LABEL_SIZE, ' scalefont setfont'
      DO I = 1, NMINID
         IF ( (END_X(MINIDS(I)) > 0.0D0).AND.(END_Y(MINIDS(I)) > 0.0D0) ) THEN
            WRITE (MIN_TRIM, '(I8)') MINIDS(I)
           WRITE (20, '(2F7.2, 3A)') end_x(MINIDS(I)), end_y(MINIDS(I)), &
               & ' mt (', TRIM(ADJUSTL(min_trim)), ') show'
            WRITE (20, '(2F7.2, A,2F7.2,A)') end_x(MINIDS(I)), end_y(MINIDS(I)), &
               & ' mt ',  end_x(MINIDS(I)), end_y(MINIDS(I))-10.0,' ls '
         END IF
      END DO
      WRITE (20, '(A)') '0 0 0 setrgbcolor'
   END IF
   
   IF (TRVALSCALET) THEN
     WRITE (20, '((A, /),(2F10.5, A, /), 80(A, /), A)') '0 0 0 setrgbcolor', &
     & TRSCALEX, TRSCALEY, ' translate % origin to bottom right', &
     & '0.7 swidth 0.0 swidth moveto', &
     & '1.0 swidth 0.0 swidth lineto', &
     & 'stroke', &
     & 'Step setlinewidth', &
     & '/s 1 swidth def', &
     & '1.0 0.0 0.0 StartRGB', &
     & '0.625 0.5 0.0 EndRGB', &
     & '0 Step s {', &
     & 'StepRGB', &
     & 'newpath', &
     & '0.0 swidth exch moveto % position left hand side', &
     & '1.0 swidth 0 rlineto % draw a line', &
     & 'stroke', &
     & '} for', &
     & '%', &
     & '0.0 swidth 1.0 swidth translate % origin to bottom right', &
     & 'Step setlinewidth', &
     & '/s 0.5 swidth def', &
     & '0.625 0.5 0.0 StartRGB', &
     & '0.0 0.5 0.0 EndRGB', &
     & 'stack', &
     & '0 Step s {', &
     & 'StepRGB', &
     & 'newpath', &
     & '0.0 swidth exch moveto % position left hand side', &
     & '1.0 swidth 0 rlineto % draw a line', &
     & 'stroke', &
     & '} for', &
     & '%', &
     & '0.0 swidth 0.5 swidth translate % origin to bottom right', &
     & 'Step setlinewidth', &
     & '/s 0.5 swidth def', &
     & '0.0 0.5 0.0 StartRGB', &
     & '0.0 0.5 0.625 EndRGB', &
     & 'stack', &
     & '0 Step s {', &
     & 'StepRGB', &
     & 'newpath', &
     & '0.0 swidth exch moveto % position left hand side', &
     & '1.0 swidth 0 rlineto % draw a line', &
     & 'stroke', &
     & '} for', &
     & '%', &
     & '0.0 swidth 0.5 swidth translate % origin to bottom right', &
     & 'Step setlinewidth', &
     & '/s 1 swidth def', &
     & '0.0 0.5 0.625 StartRGB', &
     & '0.0 0.0 1.0 EndRGB', &
     & 'stack', &
     & '0 Step s {', &
     & 'StepRGB', &
     & 'newpath', &
     & '0.0 swidth exch moveto % position left hand side', &
     & '1.0 swidth 0 rlineto % draw a line', &
     & 'stroke', &
     & '} for', &
     & '%', & 
     & '%Draw Outline to Box', &
     & '1.0 setlinewidth', &
     & '0.0 0.0 0.0 setrgbcolor', &
     & '0.0 swidth -2.0 swidth translate % origin to bottom right', &
     & '1 swidth 3 swidth moveto', &
     & '1 swidth 0 swidth lineto', &
     & '0 swidth 0 swidth lineto', &
     & '0 swidth 3 swidth lineto', &
     & 'closepath', &
     & 'stroke', &
     & '0.7 swidth 0.75 swidth moveto', &
     & '1.0 swidth 0.75 swidth lineto', &
     & 'stroke', &
     & '0.7 swidth 1.5 swidth moveto', &
     & '1.0 swidth 1.5 swidth lineto', &
     & 'stroke', &
     & '0.7 swidth 2.25 swidth moveto', &
     & '1.0 swidth 2.25 swidth lineto', &
     & 'stroke', &
     & '%Numbers for scale', &
     & '/Times-Roman findfont 12 scalefont setfont', &
     & '0.0 swidth 3.40 swidth moveto', &
     & '(Order Parameter) show', &
     & '/Times-Roman findfont 10 scalefont setfont'
      WRITE (20, '(4((A, /), (A2, F7.2, A, /)), (A, /), (A1, F7.2, A)) ') &
     & '1.2 swidth -0.04 swidth moveto', &
     & '( ', 0.00/CSCALE, ' ) show', &
     & '1.2 swidth 0.71 swidth moveto', &
     & '( ', 0.25/CSCALE, ' ) show', &
     & '1.2 swidth 1.46 swidth moveto', &
     & '( ', 0.50/CSCALE, ' ) show', &
     & '1.2 swidth 2.21 swidth moveto', &
     & '( ', 0.75/CSCALE, ' ) show', &
     & '1.2 swidth 2.96 swidth moveto', &
     & '( ', 1.00/CSCALE, ' ) show'
   END IF

   WRITE (20, '(/, A)') 'showpage'
   CLOSE (20)
   WRITE (6, '(A, /)') 'Done.'
   DEALLOCATE(DJWBASIN,TSEN)

!   CALL SYSTEM('ps2pdf tree.ps tree.pdf')

END PROGRAM DISCONNECTION
! }}}
!................................................................................!
! Subroutines {{{
! READ_OPTIONS{{{
SUBROUTINE READ_OPTIONS
!> \name READ_OPTIONS
!> \brief Read options from the file dinfo.
   USE KEYWORDS
   USE PAGE
   USE VARS
   IMPLICIT NONE
   CHARACTER(LEN=50) :: KEYWORD
   INTEGER ERR, NDUMMY, I4
   INTEGER, ALLOCATABLE :: SAVEID(:)
   LOGICAL :: SUCCESS

   BARRIERS = .TRUE.
   CENTRE_GMIN = .FALSE.
   DELTA_E = 0.0D0
   DUMP_NUMBERS = .FALSE.
   DUMP_SIZES = .FALSE.
   E_HIGH = 0.0D0
   EXCLUDEALL = .FALSE.
   LOWEST = 0
   LAB_FMT = 'F8.2'
   MONOTONIC = .FALSE.
   N_LEVELS = 0
   FILE_PICK = ''
   FILE_TRACE= ''  
   SPLIT = .TRUE.

    
   OPEN (UNIT=20, FILE='dinfo', STATUS='OLD', IOSTAT=err)
   IF (ERR /= 0) THEN
      WRITE (6, '(/,A,/)') 'ERROR: Could not open dinfo file'
      STOP
   END IF
   ALLOCATE(MINIDS(1))
   DO
      CALL READ_LINE(20, SUCCESS)
      IF (.NOT.SUCCESS) EXIT
      CALL GET_STRING(KEYWORD)
      CALL UPPER_CASE(KEYWORD)
      IF (KEYWORD(1:1) == '#') keyword='#'
      SELECT CASE(TRIM(KEYWORD))
      CASE ('#')
!        Comment; do nothing.
      CASE ('COMMENT')
!        Comment; do nothing.
      CASE ('CENTREGMIN')
         CENTRE_GMIN = .TRUE.
      CASE ('COLOUROUTPUT')
         TRPRINT=.TRUE.
      CASE ('DELTA')
         CALL GET_DP(DELTA_E)
      CASE ('DUMPNUMBERS')
         DUMP_NUMBERS = .TRUE.
      CASE ('DUMPSIZES')
         DUMP_SIZES = .TRUE.
      CASE ('EXCLUDEALL')
         EXCLUDEALL = .TRUE.
      CASE ('FIRST')
         CALL GET_DP(E_HIGH)
      CASE ('IDENTIFY')
         IDENTIFY = .TRUE.
      CASE ('IDENTIFY_NODE')
         IDENTIFY_NODE = .TRUE.
         CALL GET_INTEGER(MAX_MIN)
      CASE ('IDENTIFY_NODE_SIZE')
         IDENTIFY_NODE_SIZE = .TRUE.
         CALL GET_INTEGER(MAX_MIN2)
      CASE ('CONNECTMIN')
         CALL GET_INTEGER(CONNECTMIN)
      CASE ('TSTHRESH')
         CALL GET_DP(TSTHRESH)
      CASE ('MAXTSENERGY')
         CALL GET_DP(TSTHRESH)
      CASE ('MAXTSBARRIER')
         CALL GET_DP(TSBARTHRESH)
      CASE ('IDMIN')
         IDMINT = .TRUE.
         CALL GET_INTEGER(NDUMMY)
         ALLOCATE(SAVEID(NMINID))
         SAVEID(1:NMINID)=MINIDS(1:NMINID)
         DEALLOCATE(MINIDS)
         ALLOCATE(MINIDS(1:NMINID+1))
         MINIDS(1:NMINID)=SAVEID(1:NMINID)
         MINIDS(NMINID+1)=NDUMMY
         NMINID=NMINID+1
         DEALLOCATE(SAVEID)
   
      CASE ('LABELFORMAT')
         CALL GET_STRING(LAB_FMT)
      CASE ('LABELSIZE')
         CALL GET_INTEGER(LABEL_SIZE)
      CASE ('LETTER')
         PAGE_X = 612
         PAGE_Y = 792
      CASE ('LEVELS')
         CALL GET_INTEGER(N_LEVELS)
      CASE ('LOWEST')
         CALL GET_INTEGER(LOWEST)
      CASE ('MINIMA')
         CALL GET_STRING(FILE_MIN)
      CASE ('MONOTONIC')
         MONOTONIC = .TRUE.
      CASE ('NCONNMIN')
         CALL GET_INTEGER(NCONNMIN)
      CASE ('NOBARRIERS')
         BARRIERS = .FALSE.
      CASE ('NOSPLIT')
         SPLIT = .FALSE.
      CASE ('PICK')
         CALL GET_STRING(FILE_PICK)
      CASE ('TRMIN')
         TRMINT = .TRUE.
         CALL GET_INTEGER(NDUMMY)
         CALL GET_INTEGER(MINRANGE)
         IF (NDUMMY.eq.1) THEN
         WRITE (6, '(I6, A)') ndummy, ' set of minima will be traced in colour'
         ELSE
         WRITE (6, '(I6, A)') ndummy, ' sets of minima will be traced in colour'
         ENDIF
         ALLOCATE(MINTRS(1:MINRANGE)) !insert No. of minima in largest minima file
         MINTRS=0
         DO I4=1, NDUMMY 
         CALL GET_STRING(FILE_TRACE)
         CALL READ_TRFILE(FILE_TRACE, I4, MINTRS)  
         ENDDO
         NMINTR=NDUMMY 
      CASE ('TRVAL')
         TRVALT = .TRUE.
         CALL GET_INTEGER(MINRANGE)
         WRITE (6, '(A)') 'Minima will be traced in colour according to values of an order parameter'
         ALLOCATE(DPMINTRS(1:MINRANGE))
         DPMINTRS=-1.0D0 ! new initialization, because 0.0D0 is a legitimate value 
         CALL GET_STRING(FILE_TRACE)
         CALL GET_DP(CSCALE)
         CALL GET_LOGICAL(WEIGHTLOWER)
         OPEN (UNIT=22, FILE=TRIM(FILE_TRACE), STATUS='OLD', IOSTAT=ERR)
         IF (ERR /= 0) THEN
         WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(file_trace)
         STOP
         END IF
         DO I4=1, MINRANGE
            READ(22, *, IOSTAT=ERR) DPMINTRS(I4)
            IF (ERR /= 0) EXIT
         ENDDO
         CLOSE(22)
      CASE ('TRVALSCALE')
         TRVALSCALET = .TRUE.
         CALL GET_DP(TRSCALEX)
         CALL GET_DP(TRSCALEY)
      CASE ('TS')
         CALL GET_STRING(FILE_TS)
      CASE ('WEIGHTS')
         CALL GET_STRING(FILE_WEIGHTS)
         WEIGHTS=.TRUE.
      CASE ('METRIC')
         METRICT = .TRUE.
         CALL GET_INTEGER(MINRANGE)
         WRITE (6, '(A)') 'Minima will be located according to values of an order parameter'
         ALLOCATE(ORDER_PARAMETER(1:MINRANGE))
         ORDER_PARAMETER = -1.0D0
         CALL GET_STRING(FILE_METRIC)
         OPEN (UNIT=22, FILE=TRIM(FILE_METRIC), STATUS='OLD', IOSTAT=ERR)
         IF (ERR /= 0) THEN
         WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(file_trace)
         STOP
         END IF
         DO I4=1, MINRANGE
            READ(22, *, IOSTAT=ERR) ORDER_PARAMETER(I4)
            IF (ERR /= 0) EXIT
         ENDDO
         CLOSE(22)
      CASE ('SETAXIS')
         SETAXIST = .TRUE.
         CALL GET_DP(OP_LOWER)
         CALL GET_DP(OP_HIGHER)
      CASE ('HALFPAGE')
         PAGE_Y = 421
      CASE ('A5')
         PAGE_X = 420
         PAGE_Y = 595
      CASE ('LANDSCAPE')
         PAGE_X = 842
         PAGE_Y = 595
      CASE ('EPSILON')
         EPSILONT = .TRUE.
      CASE DEFAULT
         WRITE (6, '(2A)') 'Keyword not recognised in dinfo: ', TRIM(keyword)
         STOP
      END SELECT
   END DO
   CLOSE (20)

   IF (.NOT. ALLOCATED(MINTRS)) ALLOCATE(MINTRS(1))

   IF (DELTA_E <= 0.0D0) THEN
      WRITE (6, '(A)') 'Energy gap must be specified using DELTA.'
      STOP
   ENDIF

!   IF (E_HIGH == 0.0D0) THEN
!      WRITE (6, '(A)') 'First energy level must by specified using FIRST.'
!      STOP
!   ENDIF

   IF (N_LEVELS <= 0) THEN
      WRITE (6, '(A)') 'Number of levels must be specified using LEVELS.'
      STOP
   ENDIF

END SUBROUTINE READ_OPTIONS
! }}}
!................................................................................!
! COUNT_MIN(FN, N){{{
SUBROUTINE COUNT_MIN(FN, N)
!> \name COUNT_MIN
!> \brief Count the number of minima in the list.
   IMPLICIT NONE
 
   CHARACTER(LEN=100), INTENT(IN) :: FN
   INTEGER, INTENT(OUT) :: N
   DOUBLE PRECISION DUMMY
 
   INTEGER :: ERR, J
 
   N = 0
   OPEN (UNIT=23, FILE=TRIM(FN), STATUS='OLD', IOSTAT=err)
   IF (ERR /= 0) THEN
      WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(fn)
      STOP
   END IF
   DO
      READ (UNIT=23, FMT=*, IOSTAT=ERR) DUMMY
      IF (ERR /= 0) EXIT
      N = N + 1
   END DO
   CLOSE (23)
 
END SUBROUTINE COUNT_MIN
! }}}
!................................................................................!
! COUNT_TS(FN, N_TS, B_HIGH, B_LOW){{{
SUBROUTINE COUNT_TS(FN, N_TS, B_HIGH, B_LOW)
!> \name COUNT_TS 
!> Count the number of transition states for NON-DEGENERATE rearrangements.
!> Also returned are the highest and lowest transition state energies.

   IMPLICIT NONE

   CHARACTER(LEN=100), INTENT(IN) :: FN
   INTEGER, INTENT(OUT) :: N_TS
   DOUBLE PRECISION :: B_HIGH, B_LOW

   CHARACTER(LEN=3) :: PG
   DOUBLE PRECISION :: E, LPAFS
   INTEGER :: ERR, I, H, MIN1, MIN2

   N_TS = 0
   B_HIGH = -HUGE(B_HIGH)
   B_LOW = HUGE(B_HIGH)
   OPEN (UNIT=20, FILE=TRIM(FN), STATUS='OLD', IOSTAT=err)
   IF (ERR /= 0) THEN
      WRITE (6, '(/,2A,/)') 'ERROR: Could not open ', TRIM(fn)
      STOP
   END IF
   DO
!     READ (UNIT=20, FMT=*, IOSTAT=err) i, e, lpafs, pg, h, min1, min2
! standard PATHSAMPLE.2.0 format
      READ (UNIT=20, FMT=*, IOSTAT=ERR) E, LPAFS, H, MIN1, MIN2
      IF (ERR /= 0) EXIT
      IF (MIN1 /= MIN2) N_TS=N_TS+1
      IF (E > B_HIGH) B_HIGH=E
      IF (E < B_LOW) B_LOW=E
   END DO
   CLOSE (20)

END SUBROUTINE COUNT_TS
! }}}
!................................................................................!
! CONNECTIONS(INT :: LEVEL, NODE, INT(N_BR) :: CONNECT, INT N_BR){{{
SUBROUTINE CONNECTIONS(LEVEL, NODE, CONNECT, N_BR)
!> \name CONNECTIONS
!> Subroutine to make a list (CONNECT) of the basins in level LEVEL+1 that are
!> connected to basin NODE in level LEVEL.

   USE VARS
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: LEVEL, NODE, N_BR
   INTEGER, DIMENSION(N_BR), INTENT(OUT) :: CONNECT

   INTEGER :: I, J, LAST

   LAST = 0
   J = 1
   CONNECT = 0
   DO I = 1, N_MIN
      IF (NCONN(I).LE.NCONNMIN) CYCLE
      IF ((BASIN(LEVEL, I) == NODE).AND.(BASIN(LEVEL+1, I) > LAST)) THEN
         CONNECT(J) = BASIN(LEVEL+1, I)
         LAST = CONNECT(J)
         J = J + 1
      ENDIF
   END DO

END SUBROUTINE CONNECTIONS
!}}}
!................................................................................!
! HEADER{{{
SUBROUTINE HEADER
!> \name HEADER
!> \brief Write preamble for PostScript file and draw energy scale.

   USE PAGE
   USE VARS
   IMPLICIT NONE

   DOUBLE PRECISION :: X1, X2, Y1, Y2, X_POS, Y_POS
   DOUBLE PRECISION, PARAMETER :: TICK_WIDTH=3.0D0
   INTEGER :: I

!  Definitions.
   WRITE (20, '(4(A, /), A, I4, (I4, /), 17(A, /), A)') '%!PS-Adobe-3.0 EPSF-3.0', &
      & '%% Creator: disconnection.f90', &
      & '%% Minima: ' // TRIM(file_min), &
      & '%% Transition states: ' // TRIM(file_ts), &
      & '%%BoundingBox: 0 0', PAGE_X, PAGE_Y, &
      & '/ls {lineto stroke} def', &
      & '/mt {moveto} def', &
      & '0.1 setlinewidth', &
      & '/swidth { 30 mul } def', &
      & '/dpi { 30 div } def', &
      & '/Rez 1000 dpi def % calc lines per point for fill', &
      & '/Step 1 Rez div def', &
      & '/StartRGB { /B1 exch def /S1 exch def /H1 exch def } def', &
      & '/EndRGB { /B2 exch def /S2 exch def /H2 exch def', &
      & '          /Steps s Step div def', &
      & '          /Hstep H2 H1 sub Steps div def', &
      & '          /Sstep S2 S1 sub Steps div def', &
      & '          /Bstep B2 B1 sub Steps div def', &
      & '        } def', &
      & '/StepRGB { /H1 H1 Hstep add def', &
      & '           /S1 S1 Sstep add def', &
      & '           /B1 B1 Bstep add def', &
      & '           H1 S1 B1 setrgbcolor', &
      & '         } def' 



!  Vertical energy axis.
   X1 = MARGIN_X + SCALE_WIDTH - 10.0D0
   Y1 = Y_POS(0.0D0)
   Y2 = Y_POS(1.0D0)
   WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x1, y2, ' ls'


!  Ticks on axis.
   X2 = X1 + TICK_WIDTH/2.0D0
   X1 = X1 - TICK_WIDTH/2.0D0
   DO I=1, N_LEVELS
      Y1 = Y_POS(1.0D0*(I-1)/(N_LEVELS-1))
      WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y1, ' ls'
   END DO

!  Tick labels.
   WRITE (20, '(A, I2, A)') &
      & '/Times-Roman findfont ', font_size, ' scalefont setfont'
   DO I=1, N_LEVELS
      X1 = 15.0
      Y1 = Y_POS(1.0D0*(N_LEVELS-I)/(N_LEVELS-1)) - 5.0D0
      WRITE (20, '(2F7.2, A, '//TRIM(lab_fmt)//', A)') &
!     WRITE (20, '(2F7.2, A, F8.3, A)') &
         & X1, Y1, ' mt (', e_high - (n_levels-i)*delta_e, ') show'
   END DO

!  Landmark for manipulate.f90.
   WRITE (20, '(/, A, /)') '% Manipulate landmark.'

END SUBROUTINE HEADER
!}}}
!................................................................................!
! HEADER{{{
SUBROUTINE EPSILON_HEADER
!> \name HEADER
!> \brief Write preamble for PostScript file and draw energy scale.

   USE PAGE
   USE VARS
   IMPLICIT NONE

   DOUBLE PRECISION :: X1, X2, Y1, Y2, X_POS, Y_POS
   DOUBLE PRECISION, PARAMETER :: TICK_WIDTH=3.0D0
   INTEGER :: I

!  Definitions.
   WRITE (20, '(4(A, /), A, I4, (I4, /), 17(A, /), A)') '%!PS-Adobe-3.0 EPSF-3.0', &
      & '%% Creator: disconnection.f90', &
      & '%% Minima: ' // TRIM(file_min), &
      & '%% Transition states: ' // TRIM(file_ts), &
      & '%%BoundingBox: 0 0', PAGE_X, PAGE_Y, &
      & '/ls {lineto stroke} def', &
      & '/mt {moveto} def', &
      & '0.1 setlinewidth', &
      & '/swidth { 30 mul } def', &
      & '/dpi { 30 div } def', &
      & '/Rez 1000 dpi def % calc lines per point for fill', &
      & '/Step 1 Rez div def', &
      & '/StartRGB { /B1 exch def /S1 exch def /H1 exch def } def', &
      & '/EndRGB { /B2 exch def /S2 exch def /H2 exch def', &
      & '          /Steps s Step div def', &
      & '          /Hstep H2 H1 sub Steps div def', &
      & '          /Sstep S2 S1 sub Steps div def', &
      & '          /Bstep B2 B1 sub Steps div def', &
      & '        } def', &
      & '/StepRGB { /H1 H1 Hstep add def', &
      & '           /S1 S1 Sstep add def', &
      & '           /B1 B1 Bstep add def', &
      & '           H1 S1 B1 setrgbcolor', &
      & '         } def' 



!  Vertical energy axis.
   X1 = MARGIN_X + SCALE_WIDTH - 10.0D0
   Y1 = Y_POS(1.0D0*(N_LEVELS-2)/(N_LEVELS-1))
   Y2 = Y_POS(1.0D0)
   WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x1, y2, ' ls'


!  Ticks on axis.
   X2 = X1 + TICK_WIDTH/2.0D0
   X1 = X1 - TICK_WIDTH/2.0D0
   DO I=N_LEVELS - 1, N_LEVELS
      Y1 = Y_POS(1.0D0*(I-1)/(N_LEVELS-1))
      WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y1, ' ls'
   END DO

!  Tick labels.
   WRITE (20, '(A, I2, A)') &
      & '/Symbol findfont ', font_size + 6, ' scalefont setfont'
   
   X1 = MARGIN_X + SCALE_WIDTH
   Y1 = Y_POS(1.0D0*(N_LEVELS - 2)/(N_LEVELS-1)) - 15.0D0
   WRITE (20, '(2F7.2, A, '//TRIM(lab_fmt)//', A)') &
        !     WRITE (20, '(2F7.2, A, F8.3, A)') &
        & X1, Y1, ' mt ( e ) show'
   

!  Landmark for manipulate.f90.
   WRITE (20, '(/, A, /)') '% Manipulate landmark.'

END SUBROUTINE EPSILON_HEADER
!}}}

!................................................................................!
SUBROUTINE METRIC_HEADER
!> \name METRIC_HEADER
!> \brief Write preamble for PostScript file and draw energy scale and order-parameter scale.

   USE PAGE
   USE VARS
   IMPLICIT NONE

   DOUBLE PRECISION :: X1, X2, Y1, Y2, X_POS, Y_POS, RANGE, FIRST_X_TICK, DELTA_X_TICK, R, Q, P
   DOUBLE PRECISION, PARAMETER :: TICK_WIDTH=3.0D0
   INTEGER :: I, J

!  Definitions.
   WRITE (20, '(4(A, /), A, I4, (I4, /), 17(A, /), A)') '%!PS-Adobe-3.0 EPSF-3.0', &
      & '%% Creator: disconnection.f90', &
      & '%% Minima: ' // TRIM(file_min), &
      & '%% Transition states: ' // TRIM(file_ts), &
      & '%%BoundingBox: 0 0', PAGE_X, PAGE_Y, &
      & '/ls {lineto stroke} def', &
      & '/mt {moveto} def', &
      & '0.1 setlinewidth', &
      & '/swidth { 30 mul } def', &
      & '/dpi { 30 div } def', &
      & '/Rez 1000 dpi def % calc lines per point for fill', &
      & '/Step 1 Rez div def', &
      & '/StartRGB { /B1 exch def /S1 exch def /H1 exch def } def', &
      & '/EndRGB { /B2 exch def /S2 exch def /H2 exch def', &
      & '          /Steps s Step div def', &
      & '          /Hstep H2 H1 sub Steps div def', &
      & '          /Sstep S2 S1 sub Steps div def', &
      & '          /Bstep B2 B1 sub Steps div def', &
      & '        } def', &
      & '/StepRGB { /H1 H1 Hstep add def', &
      & '           /S1 S1 Sstep add def', &
      & '           /B1 B1 Bstep add def', &
      & '           H1 S1 B1 setrgbcolor', &
      & '         } def' 



!  Vertical energy axis.
   X1 = MARGIN_X + SCALE_WIDTH - 10.0D0
   Y1 = Y_POS(0.0D0)
   Y2 = Y_POS(1.0D0)
   WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x1, y2, ' ls'

!  Horizontal order parameter axis
   X1 = X_POS(0.0D0)
   X2 = X_POS(1.0D0)
   WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y2, ' mt ', x2, y2, ' ls'
   
!  Ticks on energy axis.
   X1 = MARGIN_X + SCALE_WIDTH - 10.0D0
   X2 = X1 + TICK_WIDTH/2.0D0
   X1 = X1 - TICK_WIDTH/2.0D0
   DO I=1, N_LEVELS
      Y1 = Y_POS(1.0D0*(I-1)/(N_LEVELS-1))
      WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y1, ' ls'
   END DO

   RANGE = OP_HIGHER - OP_LOWER
   ! Determine order of magnitude of range of the order-parameter axis.
   P = AINT(LOG10(RANGE) - 1.0)
   IF ((LOG10(RANGE) - P).LT.0.5) P = P -1.0
   ! Determine location of first tick on order-parameter_axis
   FIRST_X_TICK = AINT((OP_LOWER/((10.0**P))) + 1.0)*((10.0**P))
   
   Y2 = Y_POS(1.0D0) + TICK_WIDTH/2.0D0
   Y1 = Y_POS(1.0D0) - TICK_WIDTH/2.0D0
   I = 0
   DO
     IF (FIRST_X_TICK + I*(10**P).GT.OP_HIGHER) EXIT
     X1 = X_POS(((FIRST_X_TICK + (10.0**P)*I) - OP_LOWER)/(RANGE))
     WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x1, y2, ' ls'
     I = I + 1
  END DO

  ! Tick labels
  Y1 = Y_POS(1.0D0) - 15.0
  I = 0
  WRITE (20, '(A, I2, A)') &
       & '/Times-Roman findfont ', font_size + 2, ' scalefont setfont'
  DO
     IF (FIRST_X_TICK + I*(10.0**P).GT.OP_HIGHER) EXIT
     X1 = X_POS(((FIRST_X_TICK + (10.0**P)*I) - OP_LOWER)/(RANGE)) - 20.0D0
     WRITE (20, '(2F7.2, A, '//TRIM(lab_fmt)//', A)') &
          & X1, Y1, ' mt (', FIRST_X_TICK + (10.0**P)*I, ') show'
     I = I + 1
  ENDDO
!

  WRITE (20, '(A, I2, A)') &
       & '/Times-Roman findfont ', font_size, ' scalefont setfont'
  DO I=1, N_LEVELS
     X1 = 15.0
     Y1 = Y_POS(1.0D0*(N_LEVELS-I)/(N_LEVELS-1)) - 5.0D0
     WRITE (20, '(2F7.2, A, '//TRIM(lab_fmt)//', A)') &
          !WRITE (20, '(2F7.2, A, F8.3, A)') &
          & X1, Y1, ' mt (', e_high - (n_levels-i)*delta_e, ') show'
  END DO

!  Landmark for manipulate.f90.
   WRITE (20, '(/, A, /)') '% Manipulate landmark.'

END SUBROUTINE METRIC_HEADER
!}}}
!................................................................................!
!................................................................................!
SUBROUTINE EPSILON_METRIC_HEADER
!> \name METRIC_HEADER
!> \brief Write preamble for PostScript file and draw energy scale and order-parameter scale.

   USE PAGE
   USE VARS
   IMPLICIT NONE

   DOUBLE PRECISION :: X1, X2, Y1, Y2, X_POS, Y_POS, RANGE, FIRST_X_TICK, DELTA_X_TICK, R, Q, P
   DOUBLE PRECISION, PARAMETER :: TICK_WIDTH=3.0D0
   INTEGER :: I, J

!  Definitions.
   WRITE (20, '(4(A, /), A, I4, (I4, /), 17(A, /), A)') '%!PS-Adobe-3.0 EPSF-3.0', &
      & '%% Creator: disconnection.f90', &
      & '%% Minima: ' // TRIM(file_min), &
      & '%% Transition states: ' // TRIM(file_ts), &
      & '%%BoundingBox: 0 0', PAGE_X, PAGE_Y, &
      & '/ls {lineto stroke} def', &
      & '/mt {moveto} def', &
      & '0.1 setlinewidth', &
      & '/swidth { 30 mul } def', &
      & '/dpi { 30 div } def', &
      & '/Rez 1000 dpi def % calc lines per point for fill', &
      & '/Step 1 Rez div def', &
      & '/StartRGB { /B1 exch def /S1 exch def /H1 exch def } def', &
      & '/EndRGB { /B2 exch def /S2 exch def /H2 exch def', &
      & '          /Steps s Step div def', &
      & '          /Hstep H2 H1 sub Steps div def', &
      & '          /Sstep S2 S1 sub Steps div def', &
      & '          /Bstep B2 B1 sub Steps div def', &
      & '        } def', &
      & '/StepRGB { /H1 H1 Hstep add def', &
      & '           /S1 S1 Sstep add def', &
      & '           /B1 B1 Bstep add def', &
      & '           H1 S1 B1 setrgbcolor', &
      & '         } def' 


!  Vertical energy axis.
   X1 = MARGIN_X + SCALE_WIDTH - 10.0D0
   Y1 = Y_POS(1.0D0*(N_LEVELS-2)/(N_LEVELS-1))
   Y2 = Y_POS(1.0D0)
   WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x1, y2, ' ls'


!  Ticks on axis.
   X2 = X1 + TICK_WIDTH/2.0D0
   X1 = X1 - TICK_WIDTH/2.0D0
   DO I=N_LEVELS - 1, N_LEVELS
      Y1 = Y_POS(1.0D0*(I-1)/(N_LEVELS-1))
      WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y1, ' ls'
   END DO

!  Tick labels.
   WRITE (20, '(A, I2, A)') &
      & '/Symbol findfont ', font_size + 6, ' scalefont setfont'
   
   X1 = MARGIN_X + SCALE_WIDTH
   Y1 = Y_POS(1.0D0*(N_LEVELS - 2)/(N_LEVELS-1)) - 15.0D0
   WRITE (20, '(2F7.2, A, '//TRIM(lab_fmt)//', A)') &
        !     WRITE (20, '(2F7.2, A, F8.3, A)') &
        & X1, Y1, ' mt ( e ) show'

!  Vertical energy axis.
!   X1 = MARGIN_X + SCALE_WIDTH - 10.0D0
!   Y1 = Y_POS(0.0D0)
!   Y2 = Y_POS(1.0D0)
!   WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x1, y2, ' ls'

!  Horizontal order parameter axis
   X1 = X_POS(0.0D0)
   X2 = X_POS(1.0D0)
   Y1 = Y_POS(1.0D0)
   WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y2, ' mt ', x2, y2, ' ls'
   
!  Ticks on energy axis.
!   X1 = MARGIN_X + SCALE_WIDTH - 10.0D0
!   X2 = X1 + TICK_WIDTH/2.0D0
!   X1 = X1 - TICK_WIDTH/2.0D0
!   DO I=1, N_LEVELS
!      Y1 = Y_POS(1.0D0*(I-1)/(N_LEVELS-1))
!      WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x2, y1, ' ls'
!   END DO

   RANGE = OP_HIGHER - OP_LOWER
   ! Determine order of magnitude of range of the order-parameter axis.
   P = AINT(LOG10(RANGE) - 1.0)
   IF ((LOG10(RANGE) - P).LT.0.5) P = P -1.0
   ! Determine location of first tick on order-parameter_axis
   FIRST_X_TICK = AINT((OP_LOWER/((10.0**P))) + 1.0)*((10.0**P))
   
   Y2 = Y_POS(1.0D0) + TICK_WIDTH/2.0D0
   Y1 = Y_POS(1.0D0) - TICK_WIDTH/2.0D0
   I = 0
   DO
     IF (FIRST_X_TICK + I*(10**P).GT.OP_HIGHER) EXIT
     X1 = X_POS(((FIRST_X_TICK + (10.0**P)*I) - OP_LOWER)/(RANGE))
     WRITE (20, '(2F7.2, A, 2F7.2, A)') x1, y1, ' mt ', x1, y2, ' ls'
     I = I + 1
  END DO

  ! Tick labels
  Y1 = Y_POS(1.0D0) - 15.0
  I = 0
  WRITE (20, '(A, I2, A)') &
       & '/Times-Roman findfont ', font_size + 2, ' scalefont setfont'
  DO
     IF (FIRST_X_TICK + I*(10.0**P).GT.OP_HIGHER) EXIT
     X1 = X_POS(((FIRST_X_TICK + (10.0**P)*I) - OP_LOWER)/(RANGE)) - 20.0D0
     WRITE (20, '(2F7.2, A, '//TRIM(lab_fmt)//', A)') &
          & X1, Y1, ' mt (', FIRST_X_TICK + (10.0**P)*I, ') show'
     I = I + 1
  ENDDO
!

!  WRITE (20, '(A, I2, A)') &
!       & '/Times-Roman findfont ', font_size, ' scalefont setfont'
!  DO I=1, N_LEVELS
!     X1 = 15.0
!     Y1 = Y_POS(1.0D0*(N_LEVELS-I)/(N_LEVELS-1)) - 5.0D0
!     WRITE (20, '(2F7.2, A, '//TRIM(lab_fmt)//', A)') &
!          !WRITE (20, '(2F7.2, A, F8.3, A)') &
!          & X1, Y1, ' mt (', e_high - (n_levels-i)*delta_e, ') show'
!  END DO

!  Landmark for manipulate.f90.
   WRITE (20, '(/, A, /)') '% Manipulate landmark.'

END SUBROUTINE EPSILON_METRIC_HEADER
!}}}
!................................................................................!

!DP X_POS(DP X) {{{
FUNCTION X_POS(X)
!> \name X_POS
!> \brief Convert coordinates on (0,1) to PostScript units on paper.

   USE PAGE
   IMPLICIT NONE
   DOUBLE PRECISION :: X_POS
   DOUBLE PRECISION, INTENT(IN) :: X

   X_POS = X * (PAGE_X - 2*MARGIN_X - SCALE_WIDTH) + MARGIN_X + SCALE_WIDTH

END FUNCTION X_POS
! }}}

! DP Y_POS(DP Y){{{
FUNCTION Y_POS(Y)

   USE PAGE
   IMPLICIT NONE
   DOUBLE PRECISION :: Y_POS
   DOUBLE PRECISION, INTENT(IN) :: Y
 
   Y_POS = PAGE_Y - (Y * (PAGE_Y - 2*MARGIN_Y) + MARGIN_Y)
 
END FUNCTION Y_POS
! }}}
!................................................................................!
! ENDPOINTS(INT L, DP(0:N_MIN) END_E, INT(0:N_MIN) END_M){{{
SUBROUTINE ENDPOINTS(L, END_E, END_M)

   USE VARS
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: L
   DOUBLE PRECISION, DIMENSION(0:N_MIN), INTENT(OUT) :: END_E
   INTEGER, DIMENSION(0:N_MIN), INTENT(OUT) :: END_M
 
   INTEGER :: A
 
   END_E = HUGE(END_E)
 
   DO A = 1, N_MIN
      IF (NCONN(A).LE.NCONNMIN) CYCLE
      IF ( M(A) < END_E(BASIN(L, A)) ) THEN
         END_E(BASIN(L, A)) = M(A)
         END_M(BASIN(L, A)) = A
      END IF
   END DO

END SUBROUTINE ENDPOINTS
!}}}
!................................................................................!
! MIX(INT N, INT(N) MIXLIST){{{
SUBROUTINE MIX(N, MIXLIST)

   IMPLICIT NONE

   INTEGER :: N
   INTEGER, DIMENSION(N) :: MIXLIST

   INTEGER :: SGN, DELTA, POS, I
   INTEGER, DIMENSION(N) :: COPYLIST
   LOGICAL :: BIG

   COPYLIST = MIXLIST
   POS = 1
   SGN = 1
   DELTA = N-1
   BIG = .TRUE.

   DO I = 1, N
      MIXLIST(I) = COPYLIST(POS)
      IF (BIG) THEN
         POS = POS + SGN*DELTA
         DELTA = DELTA - 2
         SGN = -SGN
         BIG = .FALSE.
      ELSE
         POS = POS + SGN
         BIG = .TRUE.
      END IF
   END DO

END SUBROUTINE MIX
! }}}
!................................................................................!
!INDEXX(INT N, DP(N) ARR, INT(N) INDX){{{
      SUBROUTINE INDEXX(N,ARR,INDX)
!> \name INDEXX
!> Unmodified Numerical Recipes routine to return a sorted index indx of
!> length n for the array arr such that arr(inx(i)) are in ascending order
!> for i=1, 2, ... n.
      INTEGER N,INDX(N),M,NSTACK
      DOUBLE PRECISION ARR(N)
      PARAMETER (M=7,NSTACK=50)
      INTEGER I,INDXT,IR,ITEMP,J,JSTACK,K,L,ISTACK(NSTACK)
      DOUBLE PRECISION A
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      JSTACK=0
      L=1
      IR=N
1     IF(IR-L.LT.M)THEN
        DO 13 J=L+1,IR
          INDXT=INDX(J)
          A=ARR(INDXT)
          DO 12 I=J-1,1,-1
            IF(ARR(INDX(I)).LE.A)GOTO 2
            INDX(I+1)=INDX(I)
12        CONTINUE
          I=0
2         INDX(I+1)=INDXT
13      CONTINUE
        IF(JSTACK.EQ.0)RETURN
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        K=(L+IR)/2
        ITEMP=INDX(K)
        INDX(K)=INDX(L+1)
        INDX(L+1)=ITEMP
        IF(ARR(INDX(L+1)).GT.ARR(INDX(IR)))THEN
          ITEMP=INDX(L+1)
          INDX(L+1)=INDX(IR)
          INDX(IR)=ITEMP
        ENDIF
        IF(ARR(INDX(L)).GT.ARR(INDX(IR)))THEN
          ITEMP=INDX(L)
          INDX(L)=INDX(IR)
          INDX(IR)=ITEMP
        ENDIF
        IF(ARR(INDX(L+1)).GT.ARR(INDX(L)))THEN
          ITEMP=INDX(L+1)
          INDX(L+1)=INDX(L)
          INDX(L)=ITEMP
        ENDIF
        I=L+1
        J=IR
        INDXT=INDX(L)
        A=ARR(INDXT)
3       CONTINUE
          I=I+1
        IF(ARR(INDX(I)).LT.A)GOTO 3
4       CONTINUE
          J=J-1
        IF(ARR(INDX(J)).GT.A)GOTO 4
        IF(J.LT.I)GOTO 5
        ITEMP=INDX(I)
        INDX(I)=INDX(J)
        INDX(J)=ITEMP
        GOTO 3
5       INDX(L)=INDX(J)
        INDX(J)=INDXT
        JSTACK=JSTACK+2
        IF(JSTACK.GT.NSTACK) THEN
          PRINT '(A)', 'WARNING NSTACK too small in indexx'
        ENDIF
        IF(IR-I+1.GE.J-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I
          IR=J-1
        ELSE
          ISTACK(JSTACK)=J-1
          ISTACK(JSTACK-1)=L
          L=I
        ENDIF
      ENDIF
      GOTO 1
      END
!  (C) Copr. 1986-92 Numerical Recipes Software 1(-V%'2150)-3.
!}}}

! GETNCONN(NMIN,NTS,NCONN,PLUS,MINUS,NCONNMIN,NCONNMAX,DEBUG){{{
      SUBROUTINE GETNCONN(NMIN,NTS,NCONN,PLUS,MINUS,NCONNMIN,NCONNMAX,DEBUG,TSEN)
!> \name GETNCONN
!>  Subroutine GETNCONN sets up array NCONN containing the number of  
!>  connections for each minimum after pruning according to the value of
!>  NCONNMIN.
!     USE COMMON, ONLY: NMIN,NTS,NCONN,PLUS,MINUS,NCONNMIN,NCONNMAX,MAXMIN,DEBUG
      USE VARS, ONLY : M, TSTHRESH, TSBARTHRESH
      IMPLICIT NONE
      INTEGER NMIN
      INTEGER J1, PNCONNECTED, NCONNECTED, NZERO, JMAX, NCONN(NMIN), NTS, NCONNMIN, NCONNMAX, &
     &        PLUS(NTS), MINUS(NTS)
      DOUBLE PRECISION TSEN(NTS)
      LOGICAL CONNECTED(NMIN), DEBUG 
!  
!  Record the number of connections for each minimum in NCONN.
!  NCONN is the number of connections to minima with more
!  than NCONNMIN connections.
!
      NCONNECTED=0
      DO J1=1,NMIN
         CONNECTED(J1)=.TRUE.
      ENDDO
11    DO J1=1,NMIN
         NCONN(J1)=0
      ENDDO
      PNCONNECTED=NCONNECTED
      DO J1=1,NTS
         IF (TSEN(J1).GT.TSTHRESH) CYCLE
         IF ((TSEN(J1)-M(PLUS(J1)).GT.TSBARTHRESH).AND.(TSEN(J1)-M(MINUS(J1)).GT.TSBARTHRESH)) CYCLE
         IF (PLUS(J1).NE.MINUS(J1)) THEN
            IF (CONNECTED(MINUS(J1))) NCONN(PLUS(J1))=NCONN(PLUS(J1))+1
            IF (CONNECTED(PLUS(J1)))  NCONN(MINUS(J1))=NCONN(MINUS(J1))+1
         ENDIF 
      ENDDO
      NCONNECTED=0
      DO J1=1,NMIN
         CONNECTED(J1)=.FALSE.
         IF (NCONN(J1).GT.NCONNMIN) THEN
            CONNECTED(J1)=.TRUE.
            NCONNECTED=NCONNECTED+1
         ENDIF
      ENDDO
      IF (DEBUG) PRINT*,'getnconn> NCONNECTED,PNCONNECTED=',NCONNECTED,PNCONNECTED
      IF (NCONNECTED.NE.PNCONNECTED) GOTO 11

      DO J1=1,NMIN
         IF (DEBUG) WRITE(*,'(A,I6,A,I6)') 'getnconn> number of connections for minimum ',J1,' is ',NCONN(J1)
      ENDDO 

      NCONNMAX=NCONN(1)
      NZERO=0
      IF (NCONN(1).EQ.0) NZERO=1
      JMAX=1
      DO J1=2,NMIN
         IF (NCONN(J1).EQ.0) NZERO=NZERO+1
         IF (NCONN(J1).GT.NCONNMAX) THEN
            NCONNMAX=NCONN(J1)
            JMAX=J1
         ENDIF
      ENDDO
!     WRITE(*,'(4(A,I6))') 'getnconn> max connections: ',NCONNMAX,' for min ',JMAX,' # of zeros=',NZERO, &
!    &                     ' after removing minima with < ',NCONNMIN+1

      RETURN
      END
!}}}
! }}}
