/* nscale.c. Display E.T. frequencies for an N-note octave, from a given MIDI note */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(int argc, char* argv[])
{
  // Read command-line input
  if (argc != 3)
  { 
    printf("usage: nscale notes midinote\n");
    return 1;
  }

  int notes = atoi(argv[1]);
  if (notes < 1 || notes > 24)
  {
    printf("Error: notes must be in range 1 - 24, inclusive.\n");
    return 1;
  }

  int midinote = atoi(argv[2]);
  if (midinote < 0 || midinote > 127)
  {
    printf("Error: bad MIDI note value: %s\n", argv[1]);
    return 1;
  }

  // Use 12-tone E.T. math to find frequency of lowest MIDI note
  double semitone_ratio = pow(2, 1/12.0);
  double c5 = 220.0 * pow(semitone_ratio, 3);
  double c0 = c5 * pow(0.5, 5);

  double ratio = pow(2, 1.0/notes);
  double frequencies[notes];
  double frequency = c0 * pow(semitone_ratio, midinote);

  for (int i = 0; i < notes; i++)
  {
    frequencies[i] = frequency;
    frequency *= ratio;
    printf("%d: %f\n", i, frequencies[i]);
  }

  return 0;
}
