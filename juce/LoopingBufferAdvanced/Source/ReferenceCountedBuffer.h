#pragma once

#include <JuceHeader.h>

class ReferenceCountedBuffer : public juce::ReferenceCountedObject
{
public:
    typedef juce::ReferenceCountedObjectPtr<ReferenceCountedBuffer> Ptr;

    ReferenceCountedBuffer(const juce::String& nameToUse,
                           int numChannels,
                           int numSamples);

    ~ReferenceCountedBuffer();

    juce::AudioSampleBuffer* getAudioSampleBuffer();

    int position = 0;

    juce::String getName()
    {
        return name;
    }

private:
    juce::String name;
    juce::AudioSampleBuffer buffer;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ReferenceCountedBuffer)
};