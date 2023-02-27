#pragma once

#include <JuceHeader.h>

class MainComponent  : public juce::Component,
                       private juce::Timer
{
public:
    MainComponent();
    ~MainComponent() override;

    void paint(juce::Graphics& g) override;
    void resized() override;

private:
    void timerCallback() override;
    void addMessageToList(const juce::MidiMessage& message);
    static juce::String getMidiMessageDescription (const juce::MidiMessage& m);

    void addMessageToBuffer(const juce::MidiMessage& message);
    void setNoteNumber(int notNumber);
    void logMessage(const juce::String& m);

    juce::TextButton bassDrumButton;
    juce::TextButton snareDrumButton;
    juce::TextButton closedHiHatButton;
    juce::TextButton openHiHatButton;

    juce::Label volumeLabel;
    juce::Slider volumeSlider;

    juce::TextEditor midiMessagesBox;

    int midiChannel;
    double startTime;

    juce::MidiBuffer midiBuffer;
    double sampleRate;
    int previousSampleNumber;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
