/* Calculate the frequency of a MIDI Note number */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>


double midi2freq(int midinote)
{
  // Frequency scale factor for semitone
  double semitone_ratio = pow(2, 1/12.0);

  // Frequency of Middle C, 3 semitones about A220
  double c5 = 220.0 * pow(semitone_ratio, 3);

  // Frequency of MIDI Note 0
  double c0 = c5 * pow(0.5, 5);

  // Resulting frequency
  double frequency = c0 * pow(semitone_ratio, midinote);

  printf("MIDI Note %d has frequency %f\n", midinote, frequency);
  return frequency;
}


int freq2midi(double frequency)
{
  // Frequency scale factor for semitone
  double semitone_ratio = pow(2, 1/12.0);

  // Frequency of Middle C, 3 semitones about A220
  double c5 = 220.0 * pow(semitone_ratio, 3);

  // Frequency of MIDI Note 0
  double c0 = c5 * pow(0.5, 5);

  // "Exact", double result for MIDI Note
  double midinote_frac = log(frequency / c0) / log(semitone_ratio);

  // Round midinote_frac to the nearest whole number
  int midinote = (int) (midinote_frac + 0.5);

  printf("The nearest MIDI note to the frequency %f is %d\n", frequency, midinote);
  return midinote;
}


int main(int argc, char* argv[])
{
  if (argc != 2)
  {
    printf("cpsmidi : converts MIDI note to frequency.\n");
    printf("usage: cpsmidi MIDInote\n");
    printf(" range: 0 <= MIDInote <= 127\n");
    return 1;
  }

  int midinote = atoi(argv[1]);
  if (midinote < 0 || midinote > 127)
  {
    printf("Bad MIDI note value: %s\n", argv[1]);
    return 1;
  }

  double frequency = midi2freq(midinote);
  printf("Frequency of MIDI note %d is %f Hz\n", midinote, frequency);

  return 0;
}
