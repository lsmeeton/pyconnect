!................................................................................
!   GPL License Info  {{{
!
!   manipulate is free software; you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation; either version 2 of the License, or
!   (at your option) any later version.
!
!   manipulate is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program; if not, write to the Free Software
!   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!   }}}
!................................................................................
! Program Info:         {{{
!
! Program for editing disconnectivity graphs produced by disconnection.f90.
! The user types in commands on the terminal, and the postscript file is
! rewritten after each command.  [Commands are not case sensitive.]
!
! To get started, make a tree.ps file with disconnection.f90.  Run ghostview
! in the background, and start manipulate.f90.  Type "READ" to load the graph.
! Now use the coordinates of the mouse pointer in ghostview to identify the
! approximate coordinates x and y of the node you would like to move.  Manipulate
! will find the closest node to the point you specify and can perform the
! following operations.  Nodes are only moved horizontally.
!
! ALIGN x y
! Align the node vertically with its parent in the next level up.
!
! JOINUP x y
! Removes the parent of a node and connects the node to its grandparent
! instead.  Only useful for nodes with no sisters.
!
! MOVEBY x y dx
! Moves the node by a horizontal amount dx.
!
! MOVETO x y x'
! Moves the node to horizontal coordinate x'.
!
! PIVOT x y x' y'
! Moves the x coordinate of a node to x' and pivots all lines connected
! below about their intersection with the line y = y'. The value of y'
! should be specified precisely as one of the levels of the graph.
!
! PSQUEEZE x y f
! Recursively compresses all nodes below the one specified by a fixed
! factor f about the x coordinate of the node.
!
! QUIT
! Leave the program
!
! RALIGN x y
! Recursive align.  Same as ALIGN but translates all nodes connected below
! the one moved by the same amount.
!
! RMOVEBY x y dx
! Recursive moveby.  Same as MOVEBY but translates all nodes connected below
! by the same amount.
!
! RMOVETO x y x'
! Recursive moveto.  Same as MOVETO but translates all nodes below by the
! same amount.
!
! RSQUEEZE x0 x1 y f
! Scales all x coordinates in the range x0 to x1 on the level at y about the
! midpoint of the range by a factor f, and recursively translates nodes below
! by the same amount.
!
! SQUEEZE x0 x1 y f
! Same as RSQUEEZE but does not recursively translate the nodes below.
!
! UNDO
! Undoes the last command by reading the previous state from disk.
!
! UPALIGN x y
! Vertically aligns the parent of the specified node.
!
!................................................................................

!!!
!!! KEYWORDS LIBRARY
!!!
!!! Subroutines: read_line(unit[,logical])
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
!................................................................................
! Modules: keywords tree {{{
!MODULE keywords {{{
MODULE keywords
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_lines=2, max_length=100
   INTEGER, PARAMETER :: tot_length=max_lines*max_length
   INTEGER :: position
   CHARACTER(LEN=tot_length) :: input
   SAVE
   CONTAINS
!!
! read_line upper_case get_string get_integer get_dp get_logical next_item {{{

   !read_line(INTEGER u, LOGICAL success) {{{
   SUBROUTINE read_line(u, success)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: u
      LOGICAL, INTENT(INOUT), OPTIONAL :: success
      INTEGER :: i, lines, next, err
      LOGICAL :: continue
      CHARACTER(LEN=7) :: fmt_string
!     Generate format string of max_length characters.
      WRITE (fmt_string, '(I4)') max_length
      fmt_string = '(A' // TRIM(ADJUSTL(fmt_string)) // ')'
      DO
         input=' '
         next = 1
!        Read in a logical line consisting of up to max_lines of input file.
         DO lines=1, max_lines
            continue = .FALSE.
            READ (u, fmt_string, IOSTAT=err) input(next:next+max_length-1)
            IF (err == 0) THEN
               IF (PRESENT(success)) success = .TRUE.
            ELSE
               IF (PRESENT(success)) success = .FALSE.
               EXIT
            ENDIF
!           Check for continuation symbol (&).
            DO i=next, next+max_length-1
               IF (input(i:i)=='&') THEN
                  continue = .TRUE.
                  next = i
                  EXIT
               ENDIF
            END DO
            IF (.NOT.continue) EXIT
         END DO
         IF (err /= 0) EXIT
         IF (TRIM(input) /= '') EXIT   ! Only read in next line if this one's empty.
      END DO
      position = 1
   END SUBROUTINE read_line
!!
! }}}
!upper_case(string) {{{
  SUBROUTINE upper_case(string)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: string
      INTEGER, PARAMETER :: lower_to_upper = ICHAR("A")-ICHAR("a")
      INTEGER :: i
      DO i=1, LEN_TRIM(string)
         IF (LGE(string(i:i), 'a').AND.LLE(string(i:i), 'z')) THEN
            string(i:i) = ACHAR(IACHAR(string(i:i))+lower_to_upper)
         ENDIF
      END DO
   END SUBROUTINE upper_case
!!
! }}}
!get_string(string, success) {{{
   SUBROUTINE get_string(string, success)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: string
      LOGICAL, INTENT(INOUT), OPTIONAL :: success
      CHARACTER(LEN=tot_length) :: temp
      INTEGER :: outcome
      CALL next_item(temp, outcome)
      IF (outcome == 3) THEN
         string = temp
         IF (PRESENT(success)) success = .TRUE.
      ELSE
         IF (PRESENT(success)) success = .FALSE.
      ENDIF
   END SUBROUTINE get_string
!!
! }}}
!get_integer(value, success) {{{
   SUBROUTINE get_integer(value, success) 
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: value
      LOGICAL, INTENT(INOUT), OPTIONAL :: success
      INTEGER :: temp, outcome, err
      CHARACTER(LEN=tot_length) :: item
      CALL next_item(item, outcome)
      READ (UNIT=item, FMT=*, IOSTAT=err) temp
      IF ((err == 0).AND.(outcome==3)) THEN
         value = temp
         IF (PRESENT(success)) success = .TRUE.
      ELSE
         IF (PRESENT(success)) success = .FALSE.
      ENDIF
   END SUBROUTINE get_integer
!!
! }}}
!get_dp(DP value, success) {{{
   SUBROUTINE get_dp(value, success)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT) :: value
      LOGICAL, INTENT(INOUT), OPTIONAL :: success
      DOUBLE PRECISION :: temp
      INTEGER :: outcome, err
      CHARACTER(LEN=tot_length) :: item
      CALL next_item(item, outcome)
      READ (UNIT=item, FMT=*, IOSTAT=err) temp
      IF ((err == 0).AND.(outcome==3)) THEN
         value = temp
         IF (PRESENT(success)) success = .TRUE.
      ELSE
         IF (PRESENT(success)) success = .FALSE.
      ENDIF
   END SUBROUTINE get_dp
!!
! }}}
! get_logical(L lgcl, L success) {{{
   SUBROUTINE get_logical(lgcl, success)
      IMPLICIT NONE
      LOGICAL, INTENT(INOUT) :: lgcl
      LOGICAL, INTENT(INOUT), OPTIONAL :: success
      INTEGER :: outcome
      CHARACTER(LEN=tot_length) :: item
      CALL next_item(item, outcome)
      CALL upper_case(item)
      IF ((TRIM(item)=='TRUE').OR.(TRIM(item)=='T').OR.(TRIM(item)=='.TRUE.') &
    & .OR.(TRIM(item)=='.T.').OR.(TRIM(item)=='ON')) THEN
         lgcl = .TRUE.
         IF (PRESENT(success)) success = .TRUE.
      ELSE IF ((TRIM(item)=='FALSE').OR.(TRIM(item)=='F').OR.(TRIM(item)=='.FALSE.') &
    & .OR.(TRIM(item)=='.F.').OR.(TRIM(item)=='OFF')) THEN
         lgcl = .FALSE.
         IF (PRESENT(success)) success = .TRUE.
      ELSE
         IF (PRESENT(success)) success = .FALSE.
      ENDIF
   END SUBROUTINE get_logical
!!
! }}}
! next_item(CH(tot_length) item, INTEGER outcome) {{{
   SUBROUTINE next_item(item, outcome)
      IMPLICIT NONE
      CHARACTER(LEN=tot_length), INTENT(OUT) :: item
      INTEGER, INTENT(OUT) :: outcome
!     Values of outcome:
!      1: null string read
!      2: end of line reached with no string
!      3: correctly read a string of at least one character
      INTEGER :: i, j
      item = ''
      outcome=1
!     Check we've not already reached the end of the input string.
      IF (position > tot_length) THEN
         outcome = 2
      ELSE
!        Read past leading blanks.
         DO i=position, tot_length
            IF (input(i:i) /= ' ') EXIT
         END DO
!        Check that this hasn't brought us to the end of the input string.
         IF (i==tot_length+1) THEN
            outcome=2
         ELSE
            position = i
            j = 1
!           Read until the next space or comma.
            DO i=position, tot_length
               SELECT CASE(input(i:i))
!              If we've reached a comma, record the position for the next
!              item and exit loop.
               CASE(',')
                  position = i+1
                  EXIT
!              If we've reached a space, check for a comma preceded by some
!              blanks, and record the position for the next item as after the
!              comma if one is found.
               CASE(' ')
                  DO j=position+1, tot_length
                     SELECT CASE(input(j:j))
                     CASE(',')
                        position = j+1
                        EXIT
                     CASE (' ')   ! Do nothing.
                     CASE DEFAULT
                        position = j
                        EXIT
                     END SELECT
                  END DO
                  EXIT
!              Any other character is the next character of the item being read.
               CASE DEFAULT
                  item(j:j) = input(i:i)
                  j = j + 1
                  outcome=3
                  position = i+1
               END SELECT
            END DO
         ENDIF
      ENDIF
   END SUBROUTINE next_item
!!
! }}}
! }}}
END MODULE keywords
!}}}
!................................................................................!
!MODULE tree{{{
MODULE tree

   CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE :: postscript

   INTEGER :: nlines, first, nlist

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: list_x, list_y
   DOUBLE PRECISION, PARAMETER :: tol = 1.0D-6

END MODULE tree
! }}}
! }}}
!................................................................................!
!PROGRAM manipulate{{{
PROGRAM manipulate

   USE keywords
   USE tree
   IMPLICIT NONE

   CHARACTER(LEN=20) :: command
   DOUBLE PRECISION :: change, x, y, xp, yp, x1, x2, y1, y2, f, c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: hlist
   INTEGER :: i, j, k, m, nh
   LOGICAL :: success
! program body {{{
   DO
      CALL read_line(5, success)
      IF (.NOT.success) EXIT
      command = ''
      CALL get_string(command)
      CALL upper_case(command)
      SELECT CASE (TRIM(command))

      CASE ('ALIGN')
!        Aligns a node with the one it is connected to above.
!        Specify: x, y
         CALL get_dp(x)
         CALL get_dp(y)
         CALL find(x, y)
         CALL above(x, y, xp, yp)
         IF (xp < 0.0D0) THEN
            WRITE (6, '(A)') 'Not connected from above.'
         ELSE
            CALL write_tree('tree.ps.bak')
            CALL move(x, y, xp, y)
            CALL write_tree('tree.ps')
         END IF

      CASE ('JOINUP')
!        Removes the node above the one specified, and joins
!        it directly to the one above that.
!        Specify: x, y
         CALL get_dp(x)
         CALL get_dp(y)
         CALL find(x, y)
         CALL above(x, y, xp, yp)
         IF (xp < 0.0D0) THEN
            WRITE (6, '(A)') 'Not connected from above.'
         ELSE
            CALL write_tree('tree.ps.bak')
            CALL rmline(xp, yp, x, y)
            CALL move(xp, yp, x, y)
            CALL write_tree('tree.ps')
         END IF

      CASE ('MOVEBY')
!        Moves the x coordinate of a node by a specified amount.
!        Specify: x, y, delta(x)
         CALL get_dp(x)
         CALL get_dp(y)
         CALL get_dp(change)
         CALL find(x, y)
         CALL write_tree('tree.ps.bak')
         CALL move(x, y, x+change, y)
         CALL write_tree('tree.ps')

      CASE ('MOVETO')
!        Moves the x coordinate of a node to a specified place.
!        Specify: x, y, new x
         CALL get_dp(x)
         CALL get_dp(y)
         CALL get_dp(xp)
         CALL find(x, y)
         CALL write_tree('tree.ps.bak')
         CALL move(x, y, xp, y)
         CALL write_tree('tree.ps')

      CASE ('PIVOT')
!        Moves the x coordinate of a node to a specified place and
!        pivots all lines connected below about their intersection with
!        a line y = y'. The value of y' should be specified precisely.
!        Specify: x, y, new x, y'
         CALL get_dp(x1)
         CALL get_dp(y1)
         CALL get_dp(x2)
         CALL get_dp(y2)
         CALL find(x1, y1)
         CALL write_tree('tree.ps.bak')
         CALL directly_below(x1, y1)
         DO i = 1, nlist
            IF (ABS(list_y(i) - y2) > tol) THEN
               xp = (x2-x1)*(1.0D0 - ((list_y(i)-y1)/(y2-y1))) + list_x(i)
               CALL move(list_x(i), list_y(i), xp, list_y(i))
            END IF
         END DO
         CALL move(x1, y1, x2, y1)
         CALL write_tree('tree.ps')

      CASE ('PSQUEEZE')
!        Recursively compresses all nodes below the one specified by
!        a fixed factor about the x coordinate of the node.
!        Specify: x, y, factor
         CALL get_dp(x)
         CALL get_dp(y)
         CALL get_dp(f)
         CALL find(x, y)
         CALL below(x, y)
         CALL write_tree('tree.ps.bak')
         DO i = 1, nlist
            change = (x-list_x(i))*(1.0D0-f)
            CALL move(list_x(i), list_y(i), list_x(i)+change, list_y(i))
         END DO
         CALL write_tree('tree.ps')

      CASE ('QUIT')
         EXIT

      CASE ('RALIGN')
!        Aligns a node with the node above it and moves all nodes
!        connected below it by the same amount.
!        Specify: x, y
         CALL get_dp(x)
         CALL get_dp(y)
         CALL find(x, y)
         CALL above(x, y, xp, yp)
         CALL below(x, y)
         IF (xp < 0.0D0) THEN
            WRITE (6, '(A)') 'Not connected from above.'
         ELSE
            CALL write_tree('tree.ps.bak')
            DO i = 1, nlist
               CALL move(list_x(i), list_y(i), list_x(i)+xp-x, list_y(i))
            END DO
            CALL write_tree('tree.ps')
         END IF

      CASE ('READ')
         CALL read_tree('tree.ps')

      CASE ('RMOVEBY')
!        Moves a node and those below it by the same amount.
!        Specify: x, y, delta(x)
         CALL get_dp(x)
         CALL get_dp(y)
         CALL get_dp(change)
         CALL find(x, y)
         CALL below(x, y)
         CALL write_tree('tree.ps.bak')
         DO i = 1, nlist
            CALL move(list_x(i), list_y(i), list_x(i)+change, list_y(i))
         END DO
         CALL write_tree('tree.ps')

      CASE ('RMOVETO')
!        Moves a node to a specified point and moves all nodes below
!        it by the same amount.
!        Specify: x, y, new x
         CALL get_dp(x)
         CALL get_dp(y)
         CALL get_dp(xp)
         CALL find(x, y)
         CALL below(x, y)
         CALL write_tree('tree.ps.bak')
         DO i = 1, nlist
            CALL move(list_x(i), list_y(i), list_x(i)+xp-x, list_y(i))
         END DO
         CALL write_tree('tree.ps')

      CASE ('RSQUEEZE')
!        Scales the x coordinate of a range of points about the
!        centre point of the range by a constant factor, and moves
!        all points connected below by the same amount.
!        Specify: left x, right x, y, factor.
         CALL get_dp(x1)
         CALL get_dp(x2)
         CALL get_dp(y1)
         CALL get_dp(f)
         CALL find(x1, y1)
         y2 = y1
         CALL find(x2, y2)
         IF (ABS(y1-y2) > tol) THEN
            WRITE (6, '(A)') 'Nodes must lie on one level.'
         ELSE
            CALL write_tree('tree.ps.bak')
            CALL between(x1, x2, y1)
            ALLOCATE(hlist(nlist))
            hlist = list_x(1:nlist)
            nh = nlist
            c = (x1 + x2)/2.0D0
            k = (nlist+1)/2
            m = 1
            DO i = 1, nh
               change = (f-1.0D0)*(hlist(k)-c)
               CALL below(hlist(k), y1)
               DO j = 1, nlist
                  CALL move(list_x(j), list_y(j), list_x(j)+change, list_y(j))
               END DO
               CALL move(hlist(k), y1, f*(hlist(k)-c) + c, y1)
               k = k + m
               m = SIGN(ABS(m)+1, -m)
            END DO
            DEALLOCATE(hlist)
         END IF
         CALL write_tree('tree.ps')

      CASE ('SQUEEZE')
!        Scales the x coordinate of a range of points about the
!        centre point of the range by a constant factor.
!        Specify: left x, right x, y, factor.
         CALL get_dp(x1)
         CALL get_dp(x2)
         CALL get_dp(y1)
         CALL get_dp(f)
         CALL find(x1, y1)
         y2 = y1
         CALL find(x2, y2)
         IF (ABS(y1-y2) > tol) THEN
            WRITE (6, '(A)') 'Nodes must lie on one level.'
         ELSE
            CALL write_tree('tree.ps.bak')
            CALL between(x1, x2, y1)
            c = (x1 + x2)/2.0D0
            j = (nlist+1)/2
            k = 1
            DO i = 1, nlist
               CALL move(list_x(j), list_y(j), f*(list_x(j)-c) + c, list_y(j))
               j = j + k
               k = SIGN(ABS(k)+1, -k)
            END DO
         END IF
         CALL write_tree('tree.ps')

      CASE ('UNDO')
         CALL read_tree('tree.ps.bak')
         CALL write_tree('tree.ps')

      CASE ('UPALIGN')
!        Aligns the x coordinate of the node above the one specified.
!        Specify: x, y
         CALL get_dp(x)
         CALL get_dp(y)
         CALL find(x, y)
         CALL above(x, y, xp, yp)
         IF (xp < 0.0D0) THEN
            WRITE (6, '(A)') 'Not connected from above.'
         ELSE
            CALL write_tree('tree.ps.bak')
            CALL move(xp, yp, x, yp)
            CALL write_tree('tree.ps')
         END IF

      CASE ('WRITE')
         CALL write_tree('tree.ps')

      CASE DEFAULT
         WRITE (6, '(/, A, /)') 'Command not recognised.'

      END SELECT
   END DO
! }}}
END PROGRAM manipulate
!}}}
!................................................................................!
!Subroutines {{{
!................................................................................!
!read_tree(CH(*) fn) {{{
SUBROUTINE read_tree(fn)

   USE tree
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: fn

   CHARACTER(LEN=1) :: ch
   INTEGER :: err, i
! subroutine body  {{{

   IF (ALLOCATED(postscript)) THEN
      DEALLOCATE(postscript)
      DEALLOCATE(list_x, list_y)
   END IF

   OPEN (UNIT=20, FILE=fn, STATUS='OLD', IOSTAT=err)
   IF (err /= 0) THEN
      WRITE (6, '(/, 2A, /)') 'Could not read ', fn
   ELSE
      nlines = 0
      DO
         READ (UNIT=20, FMT='(A1)', IOSTAT=err) ch
         IF (err /= 0) EXIT
         nlines = nlines + 1
      END DO

      ALLOCATE (postscript(nlines))
      ALLOCATE (list_x(nlines), list_y(nlines))

      REWIND (20)

      DO i = 1, nlines
         READ (20, '(A50)') postscript(i)
         IF (VERIFY('Manipulate', postscript(i)) == 0) first=i+1
      END DO
!     WRITE (6, '(/, 2A, /)') fn, ' read.'

      CLOSE (20)
   END IF
! }}}
END SUBROUTINE read_tree
! }}}
!................................................................................!
!write_tree(CH(*) fn) {{{
SUBROUTINE write_tree(fn)

   USE tree
   IMPLICIT NONE

   CHARACTER(LEN=*), INTENT(IN) :: fn

   INTEGER :: i

! {{{

   IF (.NOT.ALLOCATED(postscript)) THEN
      WRITE (6, '(/, A, /)') 'No tree in memory.'
   ELSE
      OPEN (UNIT=20, FILE=fn, STATUS='REPLACE')
      DO i = 1, nlines
         WRITE (20, '(A)') TRIM(postscript(i))
      END DO
      CLOSE (20)
!     WRITE (6, '(/, 2A, /)') fn, ' written.'
   ENDIF
! }}}
END SUBROUTINE write_tree
! }}}
!................................................................................!
! move(DP :: x, y, xp, yp) {{{
SUBROUTINE move(x, y, xp, yp)

   USE tree
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN) :: x, y, xp, yp

   CHARACTER(LEN=2) :: code
   DOUBLE PRECISION :: x1, y1, x2, y2
   INTEGER :: i, err
! subroutine body {{{
   DO i = first, nlines
      READ (postscript(i), FMT=*, IOSTAT=err) x1, y1, code, x2, y2
      IF (err == 0) THEN
         IF ((ABS(x1-x) < tol).AND.(ABS(y1-y) < tol)) THEN
            WRITE (postscript(i)(1:14), '(2(" ",F6.2))') xp, yp
         END IF
         IF ((ABS(x2-x) < tol).AND.(ABS(y2-y) < tol)) THEN
            WRITE (postscript(i)(19:32), '(2(" ",F6.2))') xp, yp
         END IF
      END IF
   END DO

   WRITE (6, '(2(A, 2F7.2))') 'Moved ', x, y, ' to ', xp, yp
! }}}
END SUBROUTINE move
! }}}
!................................................................................!
! find(DP :: x, y) {{{
SUBROUTINE find(x, y)

   USE tree
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(INOUT) :: x, y

   INTEGER :: err, i
   CHARACTER(LEN=2) :: code
   DOUBLE PRECISION :: x1, y1, x2, y2, x_close, y_close, sep, smallest
! Subroutine body {{{
   smallest = HUGE(smallest)
   DO i = first, nlines
      READ (postscript(i), FMT=*, IOSTAT=err) x1, y1, code, x2, y2
      IF (err == 0) THEN
         sep = (x1-x)**2 + (y1-y)**2
         IF (sep < smallest) THEN
            smallest = sep
            x_close = x1
            y_close = y1
         END IF
         sep = (x2-x)**2 + (y2-y)**2
         IF (sep < smallest) THEN
            smallest = sep
            x_close = x2
            y_close = y2
         END IF
      END IF
   END DO

   x = x_close
   y = y_close
! }}}
END SUBROUTINE find
! }}}
!................................................................................!
! below(DP :: x, y) {{{
SUBROUTINE below(x, y)

   USE tree
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN) :: x, y

   CHARACTER(LEN=2) :: code
   DOUBLE PRECISION :: x1, y1, x2, y2
   INTEGER :: i, j, err, pos
   LOGICAL :: again
! Subroutine body {{{
   list_x(1) = x
   list_y(1) = y
   nlist = 1

   pos = 1
   DO
      again = .FALSE.
      DO i = first, nlines
         READ (postscript(i), FMT=*, IOSTAT=err) x1, y1, code, x2, y2
         IF (err == 0) THEN
            IF ((ABS(x1-list_x(pos)) < tol).AND.(ABS(y1-list_y(pos)) < tol)) THEN
               DO j = 1, nlist
                  IF ((ABS(list_x(j)-x2) < tol).AND.(ABS(list_y(j)-y2) < tol)) EXIT
               END DO
               IF (j == nlist + 1) THEN
                  nlist = j
                  list_x(j) = x2
                  list_y(j) = y2
                  again = .TRUE.
               END IF
            END IF
         ENDIF
      END DO
      pos = pos + 1
      IF (pos > nlist) EXIT
   END DO
! }}}
END SUBROUTINE below
! }}}
!................................................................................!
! above(DP :: x, y, xp, yp) {{{
SUBROUTINE above(x, y, xp, yp)

   USE tree
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN) :: x, y
   DOUBLE PRECISION, INTENT(OUT) :: xp, yp

   CHARACTER(LEN=2) :: code
   DOUBLE PRECISION :: x1, y1, x2, y2
   INTEGER :: i, err, pos
! subroutine body {{{
   xp = -1.0D0
   DO i = first, nlines
         READ (postscript(i), FMT=*, IOSTAT=err) x1, y1, code, x2, y2
         IF (err == 0) THEN
            IF ((ABS(x2-x) < tol).AND.(ABS(y2-y) < tol)) THEN
               xp = x1
               yp = y1
               EXIT
            END IF
         END IF
   END DO
! }}}
END SUBROUTINE above
! }}}
!................................................................................!
!between(DP :: x_left, x_right, y) {{{
SUBROUTINE between(x_left, x_right, y)

   USE tree
   IMPLICIT NONE

   DOUBLE PRECISION, INTENT(IN) :: x_left, x_right, y

   CHARACTER(LEN=2) :: code
   DOUBLE PRECISION :: x1, x2, y1, y2
   INTEGER :: i, err
! subroutine body  {{{
   nlist = 0
   DO i = first, nlines
      READ (postscript(i), FMT=*, IOSTAT=err) x1, y1, code, x2, y2
      IF (err == 0) THEN
         IF ((ABS(y1-y) < tol).AND.(x1-x_left >= 0.0D0).AND.(x_right-x1 >= 0.0D0)) THEN
            nlist = nlist + 1
            list_x(nlist) = x1
            list_y(nlist) = y1
         END IF
      END IF
   END DO
! }}}
END SUBROUTINE between
! }}}
!................................................................................!
!rmline(DP :: x, y, xp, yp) {{{
SUBROUTINE rmline(x, y, xp, yp)

   USE tree
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN) :: x, y, xp, yp

   CHARACTER(LEN=2) :: code
   DOUBLE PRECISION :: x1, y1, x2, y2
   INTEGER :: i, err, pos
! subroutine body {{{
   DO i = first, nlines
      READ (postscript(i), FMT=*, IOSTAT=err) x1, y1, code, x2, y2
      IF (err == 0) THEN
         IF ((ABS(x-x1) < tol).AND.(ABS(y-y1) < tol).AND. &
             (ABS(xp-x2) < tol).AND.(ABS(yp-y2) < tol)) THEN
            postscript(i:nlines) = CSHIFT(postscript(i:nlines), 1)
            postscript(nlines) = ''
            WRITE (6, '(2(A, 2F7.2))') 'Removed line ', x, y, ' to ', xp, yp
         END IF
      END IF
   END DO
! }}}
END SUBROUTINE rmline
! }}}
!................................................................................!
! directly_below(DP :: x, y) {{{
SUBROUTINE directly_below(x, y)

   USE tree
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN) :: x, y

   CHARACTER(LEN=2) :: code
   DOUBLE PRECISION :: x1, y1, x2, y2
   INTEGER :: i, j, err
! subroutine body {{{
   nlist = 0

   DO i = first, nlines
      READ (postscript(i), FMT=*, IOSTAT=err) x1, y1, code, x2, y2
      IF (err == 0) THEN
         IF ((ABS(x1-x) < tol).AND.(ABS(y1-y) < tol)) THEN
            nlist = nlist + 1
            list_x(nlist) = x2
            list_y(nlist) = y2
         END IF
      ENDIF
   END DO
! }}}
END SUBROUTINE directly_below
! }}}
!}}}
