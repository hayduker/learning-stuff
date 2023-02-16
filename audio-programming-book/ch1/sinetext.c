#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifndef M_PI
  #define M_PI (3.141592654)
#endif

enum { ARG_NAME, ARG_NSAMPS, ARG_FREQ, ARG_SR, ARG_NARGS };

int main(int argc, char* argv[])
{

  if (argc != ARG_NARGS)
  {
    fprintf(stderr, "Usage: sintext nsamps freq srate\n");
    return 1;
  }

  double samp;  
  int nsamps   = atoi(argv[ARG_NSAMPS]);
  double freq  = atof(argv[ARG_FREQ]);
  double srate = atof(argv[ARG_SR]);
  double twopi = 2.0 * M_PI;
  double angleincr = twopi * freq / srate;

  for (int i = 0; i < nsamps; i++)
  {
    samp = sin(angleincr * i);
    fprintf(stdout, "%.8lf\n", samp);
  }

  return 0;
}
