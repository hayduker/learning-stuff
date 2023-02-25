#pragma once

#include <JuceHeader.h>


class SineOscillator
{
public:
    SineOscillator();
    void setFrequency(float frequency, float sampleRate);
    forcedinline float getNextSample() noexcept;

private:
    forcedinline void updateAngle() noexcept;
    float currentAngle;
    float angleDelta;
};


class WavetableOscillator
{
public:
    WavetableOscillator(const juce::AudioSampleBuffer& wavetableToUse);
    void setFrequency(float frequency, float sampleRate);
    forcedinline float getNextSample() noexcept;

private:
    const juce::AudioSampleBuffer& wavetable;
    const int tableSize;
    float currentIndex;
    float tableDelta;
};


class MainComponent : public juce::AudioAppComponent,
                      public juce::Timer
{
public:
    MainComponent();
    ~MainComponent() override;

    void prepareToPlay(int samplesPerBlockExpected, double sampleRate) override;
    void getNextAudioBlock(const juce::AudioSourceChannelInfo& bufferToFill) override;
    void releaseResources() override;
 
    void paint(juce::Graphics& g) override;
    void resized() override;

    void timerCallback() override;

    void createWavetable();

private:
    juce::Label cpuUsageLabel;
    juce::Label cpuUsageText;

    float level;
//    juce::OwnedArray<SineOscillator> oscillators;
    juce::OwnedArray<WavetableOscillator> oscillators;

    const unsigned int tableSize = 1 << 7;
    juce::AudioSampleBuffer sineTable;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
