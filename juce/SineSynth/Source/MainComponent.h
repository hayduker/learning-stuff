#pragma once

#include <JuceHeader.h>

class MainComponent  : public juce::AudioAppComponent
{
public:
    MainComponent();
    ~MainComponent() override;

    void prepareToPlay (int samplesPerBlockExpected, double sampleRate) override;
    void getNextAudioBlock (const juce::AudioSourceChannelInfo& bufferToFill) override;
    void releaseResources() override;

    void paint (juce::Graphics& g) override;
    void resized() override;

private:
    void updateAngleDelta();

    double currentSampleRate;
    double currentAngle;
    double angleDelta;

    juce::Slider frequencySlider;
    double currentFrequency;
    double targetFrequency;

    juce::Slider levelSlider;
    double currentLevel;
    double targetLevel;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
