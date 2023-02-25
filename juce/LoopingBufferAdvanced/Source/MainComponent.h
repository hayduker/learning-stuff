#pragma once

#include <JuceHeader.h>
#include "ReferenceCountedBuffer.h"

class MainComponent  : public juce::AudioAppComponent,
                       private juce::Thread
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
    void run() override;
    void checkForBuffersToFree();

    void openButtonClicked();
    void clearButtonClicked();

    juce::TextButton openButton;
    juce::TextButton clearButton;

    juce::SpinLock mutex;
    juce::ReferenceCountedArray<ReferenceCountedBuffer> buffers;
    ReferenceCountedBuffer::Ptr currentBuffer;

    std::unique_ptr<juce::FileChooser> chooser;
    juce::AudioFormatManager formatManager;

    juce::CriticalSection pathMutex;
    juce::String chosenPath;

    int count = 0;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};