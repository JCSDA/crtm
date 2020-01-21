/*
** Purpose: Determine 1) Is this a Fortran sequential binary file?
**                    2) What is the endianness and RCW size?
** Usage: $0 input_file
*/

#include <sys/types.h>  /* lseek */
#include <sys/stat.h>   /* open */
#include <fcntl.h>      /* open */
#include <unistd.h>     /* lseek, read */
#include <stdio.h>
#include <stdint.h>     /* uint32_t, uint64_t */

extern int IsLittleEndian (void);
extern void printresults (int);

int main (int argc, char **argv)
{
  char *fn;  // input file name
  int fd;    // file descriptor
  extern int check32 (int, char *, int);
  extern int check64 (int, char *, int);

  if (argc != 2) {
    printf ("Usage: %s input_file\n", argv[0]);
    return -1;
  }

  fn = argv[1];
  if ((fd = open (fn, O_RDONLY)) == -1) {
    printf ("Failed to open %s for reading\n", fn);
    return -1;
  }

  /*
  ** Check combinations of 32- and 64-bit RCW, native endian and byte-swapped
  */
  if (check32 (fd, fn, 0) < 0) {
    if (check32 (fd, fn, 1) < 0) {
      if (check64 (fd, fn, 0) < 0) {
	if (check64 (fd, fn, 1) < 0) {
	  printf ("All tests failed: %s is probably not a Fortran sequential binary file\n", fn);
	  return -1;
	}
      }
    }
  }
  return 0;
}
  
/*
** check32: See if the 1st record of fn is Fortran sequential binary with 4-byte RCW
**
**   Input args
**     fd:         file descriptor
**     swap_bytes: whether to swap bytes or not
**
**   Return value: 0 on success (4-byte RCW found at start and end of record)
**                 -1 on failure
*/
int check32 (int fd, char *fn, int swap_bytes)
{
  uint32_t rcw[2];  // beginning and ending 4-byte RCW
  extern void bswap4 (uint32_t *);

  /* Should always be able to seek to beginning of file */
  if (lseek (fd, (off_t) 0, SEEK_SET) < 0)
    return -1;

  if (read (fd, &rcw[0], 4) != 4) {
    return -1;
  }
  
  if (swap_bytes)
    bswap4 (&rcw[0]);

  if (lseek (fd, (off_t) (rcw[0]+4), SEEK_SET) < 0) {
    /* Reposition failed: either swapping is wrong or rcw size is wrong */
    return -1;
  }

  /* Now check that a ending rcw matching beginning rcw is found */
  if (read (fd, &rcw[1], 4) != 4) {
    return -1;
  }

  if (swap_bytes)
    bswap4 (&rcw[1]);

  if (rcw[0] == rcw[1]) {
    printf ("%s is likely a Fortran sequential binary file.\n", fn);
    printf ("It has 4-byte record-control words. The first record is %d bytes long\n", rcw[0]);
    printresults (swap_bytes);
    return 0;
  } else {
    /* Ending rcw to match starting rcw not found */
    return -1;
  }
}

/*
** check64: See if the 1st record of fn is Fortran sequential binary with 8-byte RCW
**
**   Input args
**     fd:         file descriptor
**     swap_bytes: whether to swap bytes or not
**
**   Return value: 0 on success (8-byte RCW found at start and end of record)
**                 -1 on failure
*/
int check64 (int fd, char *fn, int swap_bytes)
{
  uint64_t rcw[2];  // beginning and ending 8-byte RCW
  extern void bswap8 (uint64_t *);

  /* Should always be able to seek to beginning of file */
  if (lseek (fd, (off_t) 0, SEEK_SET) < 0)
    return -1;

  if (read (fd, &rcw[0], 8) != 8) {
    return -1;
  }

  if (swap_bytes) 
    bswap8 (&rcw[0]);

  if (lseek (fd, (off_t) (rcw[0]+8), SEEK_SET) < 0) {
    /* Reposition failed: either swapping is wrong or rcw size is wrong */
    return -1;
  }

  /* Now check that a ending rcw matching beginning rcw is found */
  if (read (fd, &rcw[1], 8) != 8) {
    return -1;
  }

  if (swap_bytes)
    bswap8 (&rcw[1]);

  if (rcw[0] == rcw[1]) {
    printf ("%s is likely a Fortran sequential binary file.\n", fn);
    printf ("It has 8-byte record-control words. The first record is %d bytes long\n", rcw[0]);
    printresults (swap_bytes);
    return 0;
  } else {
    /* Ending rcw to match starting rcw not found */
    return -1;
  }
}

/*
** bswap4: swap the bytes of a 4-byte RCW
*/
void bswap4 (uint32_t *rcw)
{
  typedef union {
    uint32_t intvar;
    struct bytes {
      unsigned char c1;
      unsigned char c2;
      unsigned char c3;
      unsigned char c4;
    } both;
  } intbytes;

  intbytes v1, v2;

  /* copy the input */
  v1.intvar = *rcw;

  /* swap the bytes */
  v2.both.c1 = v1.both.c4;
  v2.both.c2 = v1.both.c3;
  v2.both.c3 = v1.both.c2;
  v2.both.c4 = v1.both.c1;

  /* copy the output */
  *rcw = v2.intvar;
}

/*
** bswap8: swap the bytes of an 8-byte RCW
*/
void bswap8 (uint64_t *rcw)
{
  typedef union {
    uint64_t intvar;
    struct bytes {
      unsigned char c1;
      unsigned char c2;
      unsigned char c3;
      unsigned char c4;
      unsigned char c5;
      unsigned char c6;
      unsigned char c7;
      unsigned char c8;
    } both;
  } intbytes;

  intbytes v1, v2;

  /* copy the input */
  v1.intvar = *rcw;

  /* swap the bytes */
  v2.both.c1 = v1.both.c8;
  v2.both.c2 = v1.both.c7;
  v2.both.c3 = v1.both.c6;
  v2.both.c4 = v1.both.c5;
  v2.both.c5 = v1.both.c4;
  v2.both.c6 = v1.both.c3;
  v2.both.c7 = v1.both.c2;
  v2.both.c8 = v1.both.c1;

  /* copy the output */
  *rcw = v2.intvar;
}

/*
** IsLittleEndian: Determine if this is a little endian machine or not
*/
int IsLittleEndian () 
{
  int i = 1;
  
  return (int)*((unsigned char *)&i) == 1;
}

/*
** printresults: print results of testing input file.
*/
void printresults (int swap_bytes)
{
  if (swap_bytes) {
    if (IsLittleEndian ()) {
      printf ("Endianness is BIG: Differs from native so special compiler flags ARE required\n");
    } else {
      printf ("Endianness is LITTLE: Differs from native so special compiler flags ARE required\n");
    }
  } else {
    if (IsLittleEndian ()) {
      printf ("Endianness is LITTLE: Matches native so no special compiler flags required.\n");
    } else {
      printf ("Endianness is BIG: Matches native so no special compiler flags required.\n");
    }
  }
}
