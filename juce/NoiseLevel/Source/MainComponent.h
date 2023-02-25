#pragma once

#include <JuceHeader.h>

class DecibelSlider : public juce::Slider
{
public:
    DecibelSlider();

    double getValueFromText(const juce::String& text) override;
    juce::String getTextFromValue(double value) override;

private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (DecibelSlider)
};

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
    juce::Random random;

    DecibelSlider decibelSlider;
    juce::Label   decibelLabel;

    juce::Slider levelSlider;
    juce::Label  levelLabel;

    float level;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};