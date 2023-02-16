#include <stdio.h>
#include <stdlib.h>

typedef struct breakpoint
{
  double time;
  double value;
} BREAKPOINT;


BREAKPOINT maxpoint(const BREAKPOINT* points, long npoints)
{
  BREAKPOINT point;
  point.time  = points[0].time;
  point.value = points[0].value;

  for (int i = 0; i < npoints; i++)
  {
    if (point.value < points[i].value)
    {
      point.time  = points[i].time;
      point.value = points[i].value;
    }
  }

  return point;
}

BREAKPOINT* get_breakpoints(FILE* fp, long* psize)
{
  int got;
  long npoints = 0, size = 64;
  double lasttime = 0.0;
  char line[80];
 
  if (fp == NULL) return NULL;

  BREAKPOINT* points = (BREAKPOINT*) malloc(sizeof(BREAKPOINT) * size);

  if (points == NULL) return NULL;
 
  while (fgets(line, 80, fp))
  {
    got = sscanf(line, "%lf%lf", &points[npoints].time, &points[npoints].value);

    if (got < 0) continue; /* empty line */

    if (got == 0)
    {
      printf("Line %ld has non-numeric data\n",npoints+1);
      break;
    }

    if (got == 1)
    {
      printf("Incomplete breakpoint found at point %ld\n", npoints+1);
      break;
    }

    if (points[npoints].time < lasttime)
    {
      printf("data error at point %ld: time not increasing\n",
      npoints+1);
      break;
    }

    lasttime = points[npoints].time;

    if (++npoints == size)
    {
      BREAKPOINT* tmp;
      size += npoints;
      tmp = (BREAKPOINT*) realloc(points, sizeof(BREAKPOINT) *size);
      if (tmp == NULL) /* too bad! */
      {
        /* have to release the memory, and return NULL to caller */
        npoints = 0;
        free(points);
        points = NULL;
        break;
      }
      points = tmp;
    }
  }

  if (npoints) *psize = npoints;
  return points;
}

int main(int argc, char* argv[])
{
  printf("breakdur: find duration of breakpoint file\n");
  if (argc < 2)
  {
    printf("usage: breakdur infile.txt \n");
    return 0;
  }

  FILE* fp = fopen(argv[1], "r");
  if (fp == NULL) return 0;

  long size = 0;
  BREAKPOINT* points = get_breakpoints(fp, &size);

  if (points == NULL)
  {
    printf("No breakpoints read.\n");
    fclose(fp);
    return 1;
  }

  if (size < 2)
  {
    printf("Error: at least two breakpoints required\n");
    free(points);
    fclose(fp);
    return 1;
  }

  if (points[0].time != 0.0)
  {
    printf("Error in breakpoint data: first time must be 0.0\n");
    free(points);
    fclose(fp);
    return 1;
  }

  printf("read %ld breakpoints\n", size);

  double dur = points[size-1].time;
  printf("duration: %f seconds\n", dur);

  BREAKPOINT point = maxpoint(points, size);
  printf("maximum value: %f at %f secs\n", point.value, point.time);

  free(points);
  fclose(fp);
  return 0;
}
