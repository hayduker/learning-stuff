#pragma once

#include <JuceHeader.h>

class MainComponent : public juce::AudioAppComponent
{
public:
    MainComponent();
    ~MainComponent() override;

    void prepareToPlay(int samplesPerBlockExpected, double sampleRate) override;
    void getNextAudioBlock(const juce::AudioSourceChannelInfo& bufferToFill) override;
    void releaseResources() override;

    void paint(juce::Graphics& g) override;
    void resized() override;

private:
    enum LooperState
    {
        Recording,
        Playing,
        Overdubbing,
        Stopped
    };

    LooperState state;

    void recordButtonClicked();
    void clearButtonClicked();

    juce::TextButton recordButton;
    juce::TextButton clearButton;

    juce::AudioBuffer<float> loopBuffer;
    int position = 0;
    int loopLength = 0;
    bool firstSampleAfterRecording = false;
    bool firstSampleAfterOverdubbing = false;

    int i = 0;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
