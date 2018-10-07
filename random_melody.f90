!**********************************************************************
! Program generates random melodies for guitar sight reading practice. 
! Output is ascii file to be converted to midi with A.P. Shelby's 
! asc2mid.c code that I found online.
! 
! The midi can then be imported to Tuxguitar (without tab showing) to
! play back and read to practice sight reading.
! 
! Note: This version has no rests or non-quarter notes. Only meant to
!       practice finding the right note fast enough to keep up.
! 
! Note: Guitar is a transposing instrument. I.e. C4 is played as C3.
! 
! Inputs: Controlled in the F90 source (PARAMETER definitions at top)
!**********************************************************************
PROGRAM random_melody
  IMPLICIT NONE
  ! Parameters
  INTEGER, PARAMETER :: N_notes = 64 ! total notes (1/4 notes) in piece
  INTEGER            :: key = 0      ! 0=Ab, 4=C, 11=G, -1=random
  INTEGER, PARAMETER :: note_min=0   ! 0=Low E on guitar
  INTEGER, PARAMETER :: note_max=29  ! 47=23rd fret high E (Eb/D# note)
  REAL(8), PARAMETER :: free = 0.00   ! fraction of notes free to be off key
  REAL(8), PARAMETER :: adjacent=0.6 ! fraction of notes adjacent to prior
  REAL(8), PARAMETER :: ascending=0.8! fraction ascending if adjacent
  REAL(8), PARAMETER :: free_adj=0.1 ! Chromatically adjacent (maybe off key)
  LOGICAL, PARAMETER :: opencl= .true. ! Open and close on lowest root note
  ! Other variables used in the process of melody generation
  INTEGER :: N_in_key_notes            ! Total number of in key notes to draw from
  INTEGER, ALLOCATABLE :: in_key_notes(:) ! indexes of notes in key to use
  INTEGER :: total_notes = note_max-note_min+1 ! Total chromatic notes availible
  TYPE char4
    CHARACTER(4) :: pitch
  END TYPE char4
  TYPE (char4) notes(0:47)
  INTEGER :: notes_in_song(N_notes)
  INTEGER :: i,j,k, note_i_am
  REAL(8) :: rand
  LOGICAL :: inkey
  INTEGER :: lowest_root_note, notes_below_root, notes_above_root!, total_notes
  INTEGER :: ba=0, cr=-1

  CALL seedling() !prime the random number generator with the system clock
  CALL fill_notes_array()
  CALL fill_in_key_notes_array()
  !CALL fill_total_notes_array()

  !Select notes to be in the song
  DO i = 1, N_notes
    IF(i==1 .AND. opencl)       THEN; notes_in_song(i)=lowest_root_note; CYCLE; END IF
    IF(i==N_notes .AND. opencl) THEN; notes_in_song(i)=lowest_root_note; EXIT; END IF
    CALL RANDOM_NUMBER(rand)
    IF( rand < free) THEN;                      ! Note is totally free
      CALL RANDOM_NUMBER(rand)
      note_i_am  =  FLOOR(rand*total_notes) + note_min
      notes_in_song(i) = note_i_am
      CYCLE ! Next note in DO-loop
    END IF
    CALL RANDOM_NUMBER(rand)
    IF( rand < adjacent .AND. i>1 .AND. &
        notes_in_song(i-1)> note_min+3 .AND. &
        notes_in_song(i-1)< note_max-3) THEN;  ! Note is adjacent to last but in key
      CALL RANDOM_NUMBER(rand)
      IF ( rand < ascending) THEN
        note_i_am  =  notes_in_song(i-1) + 1 
        inkey = .FALSE.
        DO j=1,N_in_key_notes
          IF( note_i_am == in_key_notes(j) ) inkey = .TRUE.
          IF(i==3) WRITE(*,*) note_i_am , in_key_notes(j), inkey
          IF( note_i_am == in_key_notes(j) ) WRITE(*,*) i , note_i_am , in_key_notes(j),notes( note_i_am )%pitch
        END DO
        IF(inkey) THEN; notes_in_song(i) = note_i_am; CYCLE; END IF
        note_i_am  =  notes_in_song(i-1) + 2 
        notes_in_song(i) = note_i_am        
      ELSE
        note_i_am  =  notes_in_song(i-1) - 1
        inkey = .FALSE.
        DO j=1,N_in_key_notes
          IF( note_i_am == in_key_notes(j) ) inkey = .TRUE.
          IF( note_i_am == in_key_notes(j) ) WRITE(*,*) i , note_i_am , in_key_notes(j),notes( note_i_am )%pitch
        END DO
        IF(inkey) THEN; notes_in_song(i) = note_i_am; CYCLE; END IF
        note_i_am  =  notes_in_song(i-1) - 2 
        notes_in_song(i) = note_i_am 
      END IF
      CYCLE ! Next note in DO-loop
    END IF
    CALL RANDOM_NUMBER(rand);
    IF (rand < free_adj .AND. i>1 .AND. &
        notes_in_song(i-1)> note_min+3 .AND. &
        notes_in_song(i-1)< note_max-3) THEN;   ! Adjacent chromatic
      CALL RANDOM_NUMBER(rand)
      IF ( rand < ascending) THEN
        note_i_am  =  notes_in_song(i-1) + 1
        notes_in_song(i) = note_i_am        
      ELSE
        note_i_am  =  notes_in_song(i-1) - 1
        notes_in_song(i) = note_i_am 
      END IF
      CYCLE ! Next note in DO-loop
    END IF
    ! Only option left is a random in-key note in the specified range
    CALL RANDOM_NUMBER(rand)
    note_i_am = in_key_notes( FLOOR(rand*N_in_key_notes) + 1 )
    notes_in_song(i) = note_i_am
  END DO

  WRITE(*,*) " The stochastic melody sent to the MIDI file is:"
  DO k=1, N_notes
    WRITE(*,'(I3,X,A4,X)',ADVANCE="NO") k,notes( notes_in_song(k) )%pitch//" "
  END DO
  WRITE(*,*)

  ba=0; cr=-1
  OPEN( UNIT = 10, FILE="out.txt")
  WRITE(10,'(A45)') "# Random melody file made with random_melody.f90"
  WRITE(10,'(A30)') "format=1 tracks=2 division=960"
  DO i = 1, n_notes
    IF(cr==-1) WRITE(10,*)
    cr = cr + 1
    IF(cr==0) ba=ba+1
    !WRITE(10,'(A2,I4,X7,A2,I4,X7,A23,A4,A13)') "BA", ba,   "CR", cr, "TR 1 CH  1 NT  ", &
    !                                            notes(notes_in_song(i))%pitch , "  1   voff=95"
    WRITE(10,'(A6,I3,A14,I1,A24,A4,A13)') "BA  ", INT(ba),   "   CR         ", INT(cr), " TR 1 CH  1 NT  ", &
                                                 notes(notes_in_song(i))%pitch , "  1   voff=95"
    IF(cr==3) cr=-1

  END DO
  CLOSE(10)

STOP
CONTAINS

  SUBROUTINE fill_notes_array()
    IMPLICIT NONE
    notes(0:5)%pitch   =(/ "E-- ","F-- ","F#--", "G-- ","G#--","A-- " /)
    notes(6:11)%pitch  =(/ "A#--","B-- ","C-  ", "C#- ","D-  ","D#- " /)
    notes(12:17)%pitch =(/ "E-  ","F-  ","F#- ", "G-  ","G#- ","A-  " /)
    notes(18:23)%pitch =(/ "A#- ","B-  ","C   ", "C#  ","D   ","D#  " /)
    notes(24:29)%pitch =(/ "E   ","F   ","F#  ", "G   ","G#  ","A   " /)
    notes(30:35)%pitch =(/ "A#  ","B   ","C'  ", "C#' ","D'  ","D#' " /)
    notes(36:41)%pitch =(/ "E'  ","F'  ","F#' ", "G'  ","G#' ","A'  " /)
    notes(42:47)%pitch =(/ "A#' ","B'  ","C'' ", "C#''","D'' ","D#''" /)
  END SUBROUTINE

  SUBROUTINE fill_in_key_notes_array()
!    IMPLICIT NONE
!    INTEGER :: lowest_root_note, notes_below_root, notes_above_root, total_notes
    REAL(8) :: rand
    INTEGER :: i, tally = 0, j, k
    IF(key==-1) CALL RANDOM_NUMBER(rand);
    IF(key==-1) key = FLOOR(rand*12)
    WRITE(*,*)
    ! Could compress the following in 2 lines which set the lowest root not in range
    IF(key==0)  lowest_root_note = 4 
    IF(key==1)  lowest_root_note = 5
    IF(key==2)  lowest_root_note = 6
    IF(key==3)  lowest_root_note = 7
    IF(key==4)  lowest_root_note = 8
    IF(key==5)  lowest_root_note = 9
    IF(key==6)  lowest_root_note = 10
    IF(key==7)  lowest_root_note = 11
    IF(key==8)  lowest_root_note = 0
    IF(key==9)  lowest_root_note = 1
    IF(key==10) lowest_root_note = 2
    IF(key==11) lowest_root_note = 3
    IF(lowest_root_note < note_min) lowest_root_note = lowest_root_note + 12
    IF(lowest_root_note < note_min) lowest_root_note = lowest_root_note + 12
    IF(lowest_root_note < note_min) lowest_root_note = lowest_root_note + 12
    IF(lowest_root_note < note_min) lowest_root_note = lowest_root_note + 12
    IF(lowest_root_note < note_min) THEN; WRITE(*,*) "No root in range"; STOP; END IF
    ! Tally notes below lowest root included in composition
    i = lowest_root_note ; tally = 0
    i = i-1 ; IF(i >= note_min) tally = tally + 1
    i = i-2 ; IF(i >= note_min) tally = tally + 1
    i = i-2 ; IF(i >= note_min) tally = tally + 1
    i = i-2 ; IF(i >= note_min) tally = tally + 1
    i = i-1 ; IF(i >= note_min) tally = tally + 1
    i = i-2 ; IF(i >= note_min) tally = tally + 1
    i = i-2 ; IF(i >= note_min) tally = tally + 1
    notes_below_root = tally
    ! Tally notes ABOVE lowest root included in composition
    i = lowest_root_note ; tally = 0
    DO j = 1 , 4
          i = i+2 ; IF(i <= note_max) tally = tally + 1
          i = i+2 ; IF(i <= note_max) tally = tally + 1
          i = i+1 ; IF(i <= note_max) tally = tally + 1
          i = i+2 ; IF(i <= note_max) tally = tally + 1
          i = i+2 ; IF(i <= note_max) tally = tally + 1
          i = i+2 ; IF(i <= note_max) tally = tally + 1
          i = i+1 ; IF(i <= note_max) tally = tally + 1
    END DO
    notes_above_root = tally
    ! Fill in the indexes of all in-key-notes to draw from
    total_notes = notes_below_root + 1 + notes_above_root
    N_in_key_notes = total_notes
    ALLOCATE( in_key_notes(N_in_key_notes) )
    in_key_notes( notes_below_root + 1 ) = lowest_root_note
    j= notes_below_root; i = lowest_root_note
    i = i-1 ; IF(i >= note_min) in_key_notes(j)=i; j=j-1
    i = i-2 ; IF(i >= note_min) in_key_notes(j)=i; j=j-1
    i = i-2 ; IF(i >= note_min) in_key_notes(j)=i; j=j-1
    i = i-2 ; IF(i >= note_min) in_key_notes(j)=i; j=j-1
    i = i-1 ; IF(i >= note_min) in_key_notes(j)=i; j=j-1
    i = i-2 ; IF(i >= note_min) in_key_notes(j)=i; j=j-1
    i = i-2 ; IF(i >= note_min) in_key_notes(j)=i; j=j-1

    j= notes_below_root + 2; i = lowest_root_note
    DO k = 1 , 4
          i = i+2 ; IF(i <= note_max) in_key_notes(j)=i; j = j+1
          i = i+2 ; IF(i <= note_max) in_key_notes(j)=i; j = j+1
          i = i+1 ; IF(i <= note_max) in_key_notes(j)=i; j = j+1
          i = i+2 ; IF(i <= note_max) in_key_notes(j)=i; j = j+1
          i = i+2 ; IF(i <= note_max) in_key_notes(j)=i; j = j+1
          i = i+2 ; IF(i <= note_max) in_key_notes(j)=i; j = j+1
          i = i+1 ; IF(i <= note_max) in_key_notes(j)=i; j = j+1
    END DO

    WRITE(*,*) " Constructing melody in the key with lowest root note of "//notes(lowest_root_note)%pitch
    WRITE(*,*) " Notes below lowest root included:",  notes_below_root
    WRITE(*,*) " Notes above lowest root included:",  notes_above_root
    WRITE(*,*) N_in_key_notes, "in-key-notes (of 48 total on 23 fret guitar) are:"
    DO k=1, N_in_key_notes
      WRITE(*,'(I5)',ADVANCE="NO") in_key_notes(k)
    END DO
    WRITE(*,*)
    DO k=1, N_in_key_notes
      WRITE(*,'(A4,X)',ADVANCE="NO") notes( in_key_notes(k) )%pitch//" "
    END DO
    WRITE(*,*)

  END SUBROUTINE

END PROGRAM

! From https://gcc.gnu.org/onlinedocs/gcc-4.7.1/gfortran/RANDOM_005fSEED.html
SUBROUTINE seedling() !Feeds the intrinsic random number generator w/ seed
  IMPLICIT NONE
  INTEGER :: i,n,clock
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed
  CALL RANDOM_SEED(size=n)
  ALLOCATE(seed(n))
  CALL SYSTEM_CLOCK(count=clock)
  seed = clock + 37 * (/ (i-1, i = 1, n) /)
! seed = 44  ! Uncomment to get same random melody each time for given inputs
  CALL RANDOM_SEED(PUT = seed)
  DEALLOCATE(seed)
END SUBROUTINE seedling
