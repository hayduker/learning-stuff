#include "MainComponent.h"


MPEDemoSynthVoice::MPEDemoSynthVoice() :
    phase(0.0),
    phaseDelta(0.0),
    tailOff(0.0)
{}

void MPEDemoSynthVoice::noteStarted()
{
    jassert(currentlyPlayingNote.isValid());
    jassert(currentlyPlayingNote.keyState == juce::MPENote::keyDown ||
            currentlyPlayingNote.keyState == juce::MPENote::keyDownAndSustained);

    double clampedPressure = 0.5 + currentlyPlayingNote.pressure.asUnsignedFloat() * 0.5;
    double clampedTimbre   = 0.5 * currentlyPlayingNote.timbre.asUnsignedFloat();

    level    .setTargetValue(clampedPressure);
    frequency.setTargetValue(currentlyPlayingNote.getFrequencyInHertz());
    timbre   .setTargetValue(clampedTimbre);

    phase = 0.0;
    auto cyclesPerSample = frequency.getNextValue() / currentSampleRate;
    phaseDelta = juce::MathConstants<double>::twoPi * cyclesPerSample;

    tailOff = 0.0;
}

void MPEDemoSynthVoice::noteStopped(bool allowTailOff)
{
    jassert(currentlyPlayingNote.keyState == juce::MPENote::off);

    if (allowTailOff)
    {
        if (tailOff == 0.0) tailOff = 1.0;
    }
    else
    {
        clearCurrentNote();
        phaseDelta = 0.0;
    }
}

void MPEDemoSynthVoice::notePressureChanged()
{
    double clampedPressure = 0.5 + currentlyPlayingNote.pressure.asUnsignedFloat() * 0.5;
    level.setTargetValue(clampedPressure);
}

void MPEDemoSynthVoice::notePitchbendChanged()
{
    frequency.setTargetValue(currentlyPlayingNote.getFrequencyInHertz());
}

void MPEDemoSynthVoice::noteTimbreChanged()
{
    double clampedTimbre = 0.5 * currentlyPlayingNote.timbre.asUnsignedFloat();
    timbre.setTargetValue(clampedTimbre);
}

void MPEDemoSynthVoice::noteKeyStateChanged() {}

void MPEDemoSynthVoice::setCurrentSampleRate(double newRate)
{
    if (currentSampleRate != newRate)
    {
        noteStopped (false);
        currentSampleRate = newRate;

        level    .reset (currentSampleRate, smoothingLengthInSeconds);
        timbre   .reset (currentSampleRate, smoothingLengthInSeconds);
        frequency.reset (currentSampleRate, smoothingLengthInSeconds);
    }
}

void MPEDemoSynthVoice::renderNextBlock(juce::AudioBuffer<float>& outputBuffer,
                                        int startSample,
                                        int numSamples)
{
    if (phaseDelta != 0.0)
    {
        if (tailOff > 0.0)
        {
            while (--numSamples >= 0)
            {
                auto currentSample = getNextSample() * (float) tailOff;

                for (auto i = outputBuffer.getNumChannels(); --i >= 0;)
                    outputBuffer.addSample (i, startSample, currentSample);

                ++startSample;

                tailOff *= 0.99;

                if (tailOff <= 0.005)
                {
                    clearCurrentNote();

                    phaseDelta = 0.0;
                    break;
                }
            }
        }
        else
        {
            while (--numSamples >= 0)
            {
                auto currentSample = getNextSample();

                for (auto i = outputBuffer.getNumChannels(); --i >= 0;)
                    outputBuffer.addSample (i, startSample, currentSample);

                ++startSample;
            }
        }
    }
}

float MPEDemoSynthVoice::getNextSample() noexcept
{
    auto levelDb = (level.getNextValue() - 1.0) * maxLevelDb;
    auto amplitude = std::pow(10.0f, 0.05f * levelDb) * maxLevel;

    // timbre is used to blend between a sine and a square.
    auto f1 = std::sin (phase);
    auto f2 = std::copysign (1.0, f1);
    auto a2 = timbre.getNextValue() / 2.0;
    auto a1 = 1.0 - a2;

    auto nextSample = float (amplitude * ((a1 * f1) + (a2 * f2)));

    auto cyclesPerSample = frequency.getNextValue() / currentSampleRate;
    phaseDelta = 2.0 * juce::MathConstants<double>::pi * cyclesPerSample;
    phase = std::fmod (phase + phaseDelta, 2.0 * juce::MathConstants<double>::pi);

    return nextSample;
}





MainComponent::MainComponent() :
    audioSetupComp(audioDeviceManager, 0, 0, 0, 256, true, true, true, false)
{
    audioDeviceManager.initialise(0, 2, nullptr, true, {}, nullptr);
    audioDeviceManager.addMidiInputDeviceCallback({}, this);
    audioDeviceManager.addAudioCallback(this);

    addAndMakeVisible(audioSetupComp);
    addAndMakeVisible(visualizerViewport);

    visualizerViewport.setScrollBarsShown(false, true);
    visualizerViewport.setViewedComponent(&visualizerComp, false);
    visualizerViewport.setViewPositionProportionately(0.5, 0.0);

    visualizerInstrument.addListener(&visualizerComp);

    for (auto i = 0; i < 15; ++i)
        synth.addVoice(new MPEDemoSynthVoice());

    synth.enableLegacyMode(12);
    synth.setVoiceStealingEnabled(false);

    visualizerInstrument.enableLegacyMode(24);

    setSize(650, 560);
}

MainComponent::~MainComponent()
{
    audioDeviceManager.removeMidiInputDeviceCallback({}, this);
    audioDeviceManager.removeAudioCallback(this);
}

void MainComponent::resized()
{
    midiInputList    .setBounds(200, 10, getWidth() - 210, 20);

    auto visualizerCompWidth = 2800;
    auto visualizerCompHeight = 300;

    auto r = getLocalBounds();

    visualizerViewport.setBounds (r.removeFromBottom (visualizerCompHeight));
    visualizerComp.setBounds ({ visualizerCompWidth,
                                visualizerViewport.getHeight() - visualizerViewport.getScrollBarThickness() });

    audioSetupComp.setBounds (r);
}


void MainComponent::handleIncomingMidiMessage(juce::MidiInput* source,
                                              const juce::MidiMessage& message)
{
    visualizerInstrument.processNextMidiEvent(message);
    midiCollector.addMessageToQueue(message);
}

void MainComponent::audioDeviceAboutToStart(juce::AudioIODevice* device)
{
    auto sampleRate = device->getCurrentSampleRate();
    midiCollector.reset(sampleRate);
    synth.setCurrentPlaybackSampleRate(sampleRate);
}

void MainComponent::audioDeviceIOCallbackWithContext(const float* const* inputChannelData,
                                                     int numInputchannels,
                                                     float* const* outputChannelData,
                                                     int numOutputChannels,
                                                     int numSamples,
                                                     const juce::AudioIODeviceCallbackContext& context)
{
    juce::AudioBuffer<float> buffer (outputChannelData, numOutputChannels, numSamples);
    buffer.clear();

    juce::MidiBuffer incomingMidi;
    midiCollector.removeNextBlockOfMessages (incomingMidi, numSamples);
    synth.renderNextBlock (buffer, incomingMidi, 0, numSamples);
}