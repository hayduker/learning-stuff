#pragma once

#include <JuceHeader.h>


struct SineWaveSound : public juce::SynthesiserSound
{
    SineWaveSound() {}
    bool appliesToNote(int)    override { return true; }
    bool appliesToChannel(int) override { return true; }
};


struct SineWaveVoice : public juce::SynthesiserVoice
{
    SineWaveVoice(const juce::AudioSampleBuffer& wavetableToUse) :
        wavetable(wavetableToUse),
        tableSize(wavetable.getNumSamples())
    {
        jassert(wavetable.getNumChannels() == 1);
    }

    bool canPlaySound (juce::SynthesiserSound* sound) override
    {
        return dynamic_cast<SineWaveSound*> (sound) != nullptr;
    }

    void startNote (int midiNoteNumber, float velocity,
                    juce::SynthesiserSound*, int currentPitchWheelPosition) override
    {
        currentIndex = 0.0f;
        level = velocity * 0.15;
        tailOff = 0.0;

        auto hz = juce::MidiMessage::getMidiNoteInHertz(midiNoteNumber);
        setFrequency(hz, getSampleRate());
    }

    void setSustain(double newSustain)
    {
        sustain = newSustain;
    }

    void stopNote (float /*velocity*/, bool allowTailOff) override
    {
        if (allowTailOff)
        {
            if (tailOff == 0.0)
                tailOff = 1.0;
        }
        else
        {
            clearCurrentNote();
            tableDelta = 0.0;
        }
    }

    void pitchWheelMoved (int) override      {}
    void controllerMoved (int, int) override {}

    void setFrequency(float frequency, float sampleRate)
    {
        auto tableSizeOverSampleRate = (float)tableSize / sampleRate;
        tableDelta = frequency * tableSizeOverSampleRate;
    }

    forcedinline float getNextSample() noexcept
    {
        auto tableSize = wavetable.getNumSamples();

        auto idx0 = (unsigned int)currentIndex;
        auto idx1 = idx0 + 1;

        auto frac = currentIndex - (float)idx0;
        auto* table = wavetable.getReadPointer(0);
        auto sample0 = table[idx0];
        auto sample1 = table[idx1];

        auto currentSample = sample0 + frac * (sample1 - sample0);

        if ((currentIndex += tableDelta) > (float)tableSize)
            currentIndex -= (float)tableSize;

        return currentSample;
    }

    void renderNextBlock(juce::AudioSampleBuffer& outputBuffer, int startSample, int numSamples) override
    {
        if (tableDelta != 0.0)
        {
            if (tailOff > 0.0)
            {
                while (--numSamples >= 0)
                {
                    auto sample = (float)(getNextSample() * level * tailOff);

                    for (auto i = outputBuffer.getNumChannels(); --i >= 0;)
                        outputBuffer.addSample(i, startSample, sample);

                    ++startSample;

                    tailOff *= sustain;

                    if (tailOff <= 0.005)
                    {
                        clearCurrentNote();

                        tableDelta = 0.0;
                        break;
                    }
                }
            }
            else
            {
                while (--numSamples >= 0)
                {
                    auto sample = (float)(getNextSample() * level);

                    for (auto i = outputBuffer.getNumChannels(); --i >= 0;)
                        outputBuffer.addSample(i, startSample, sample);

                    ++startSample;
                }
            }
        }
    }

private:
    const juce::AudioSampleBuffer& wavetable;
    const int tableSize;
    float currentIndex = 0.0f, tableDelta = 0.0f;
    double level = 0.0, tailOff = 0.0, sustain = 0.99;
};


class SynthAudioSource : public juce::AudioSource
{
public:
    SynthAudioSource(juce::MidiKeyboardState& keyState);
    void setUsingSineWiveSound();
    void prepareToPlay(int samplesPerBlockExpected, double sampleRate) override;
    void releaseResources() override;
    void getNextAudioBlock(const juce::AudioSourceChannelInfo& bufferToFill) override;
    juce::MidiMessageCollector* getMidiCollector();
    void setSustain(double sustain);

private:
    void createWavetable();

    juce::MidiMessageCollector midiCollector;
    juce::MidiKeyboardState& keyboardState;
    juce::Synthesiser synth;
    const unsigned int tableSize = 1 << 7;
    juce::AudioSampleBuffer wavetable;
    std::vector<SineWaveVoice*> voices;
    int numVoices;
};


class MainComponent  : public juce::AudioAppComponent,
                       private juce::Timer
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
    void timerCallback() override;
    void setMidiInput(int index);

    juce::Label cpuUsageLabel;
    juce::Label cpuUsageText;
    bool keyboardGrabbed = false;

    juce::MidiKeyboardState keyboardState;
    SynthAudioSource synthAudioSource;
    juce::MidiKeyboardComponent keyboardComponent;

    juce::ComboBox midiInputList;
    juce::Label midiInputListLabel;
    int lastInputIndex;

    juce::Slider sustainSlider;
    juce::Label sustainLabel;
    double sustainLevel;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
