;  $Id$
;-
; Copyright (C) 1999-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

; COMMON FXFILTER
;  * Defines mapping between normal IDL I/O and FILTER I/O
;  FILTERFLAG - for each LUN, = 1*FILTERED + 2*PIPE + 4*DISK_STORE
;                               FILTERED - use FXG library? 1=yes 0=no
;                               PIPE     - is a pipe?       1=yes 0=no
;                               DISK_STORE - backing store on disk=1 mem=0
;  SEEK_CMD   - for each LUN, procedure to execute when performing POINT_LUN
;  READ_CMD   - for each LUN, procedure to execute when performing READU
;  WRITE_CMD  - for each LUN, procedure to execute when performing WRITEU     
;  CLOSE_CMD  - for each LUN, procedure to execute when performing CLOSE
;

        COMMON FXFILTER,FILTERFLAG,SEEK_CMD,READ_CMD,WRITE_CMD,CLOSE_CMD
        IF N_ELEMENTS(FILTERFLAG) EQ 0 THEN BEGIN
            FILTERFLAG = INTARR(256)
            SEEK_CMD   = STRARR(256)
            READ_CMD   = STRARR(256)
            WRITE_CMD  = STRARR(256)
            CLOSE_CMD  = STRARR(256)
        ENDIF
        FXFILTER_MAX_LUN = 256

; COMMON FXFILTER_CONFIG
;  * Defines general configuration of the filter package
;  * Can be manipulated by FXMAKEMAP
;  SCRATCH_DIR - directory where cache files are stored   (string)
;  BUFFER_MAX  - maximum buffer size (in bytes) of a pipe read (long)
;  BUFFER_GRAN - buffer size granularity (in bytes), should be a large
;                power of 2, probably >= 512 (long)
;  RM_COMMAND  - unix command to use to remove a file (string)
;  CACHE_MAX   - maximum in-memory cache of a filtered file
;
        COMMON FXFILTER_CONFIG, SCRATCH_DIR, BUFFER_MAX, BUFFER_GRAN, $
          RM_COMMAND, MEMCACHE_MAX
        IF N_ELEMENTS(SCRATCH_DIR) EQ 0 THEN BEGIN
            SCRATCH_DIR = GETENV('IDL_TMPDIR')
            BUFFER_GRAN = 4096L
            BUFFER_MAX  = 8L*BUFFER_GRAN
            RM_COMMAND  = '/bin/rm'
            MEMCACHE_MAX   = 10L*1024L*1024L
        ENDIF

; COMMON FXFILTER_FILTERS
;  * Defines mapping between suffixes and commands used to read them.
;  * Can be manipulated by FXMAKEMAP
;  FILTERS - an array of pairs.  The first of the pair gives the 
;            filename suffix to be mapped (without leading '.'), and
;            the second of the pair gives command to be executed when
;            the suffix is encountered.  The command should be in the
;            form of an IDL format statement which transfers the
;            filename into the command.
;
        COMMON FXFILTER_FILTERS, FILTERS
        IF N_ELEMENTS(FILTERS) EQ 0 THEN BEGIN
            ;            SUFF     FORMAT_COMMAND     FLAGS
            FILTERS = [ $
                        [ 'gz', '("gzip -dc ",A0)', 'COMPRESS' ], $
                        [ 'Z',  '("zcat ",A0)',     ''         ]  $
                      ]
        ENDIF
