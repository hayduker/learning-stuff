/* expdecay.c */
/* implement formula x[t] = a * exp(-k/T) */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(int argc, char* argv[])
{
  if (argc != 4)
  {
    printf("usage: expdecay dur T steps\n");
    return 1;
  }

  double dur = atof(argv[1]);
  double T = atof(argv[2]);
  int nsteps = atoi(argv[3]);

  double k = dur/nsteps;
  double a = exp(-k/T);
  double x = 1.0;
  double step = 0.0;
  for (int i = 0; i < nsteps; i++)
  {
    printf("%.4lf\t%0.8lf\n", step, x);
    x *= a;
    step += k;
  }

  return 0;
}
