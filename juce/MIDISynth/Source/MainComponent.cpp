#include "MainComponent.h"


SynthAudioSource::SynthAudioSource(juce::MidiKeyboardState& keyState) :
    keyboardState(keyState),
    numVoices(8)
{
    createWavetable();

    for (auto i = 0; i < numVoices; i++)
    {
        auto voice = new SineWaveVoice(wavetable);
        synth.addVoice(voice);
        voices.push_back(voice);
    }

    synth.addSound(new SineWaveSound());
}

void SynthAudioSource::setUsingSineWiveSound()
{
    synth.clearSounds();
}

void SynthAudioSource::prepareToPlay(int /*samplesPerBlockExpected*/, double sampleRate)
{
    synth.setCurrentPlaybackSampleRate(sampleRate);
    midiCollector.reset(sampleRate);
}

void SynthAudioSource::releaseResources() {}

void SynthAudioSource::getNextAudioBlock(const juce::AudioSourceChannelInfo& bufferToFill)
{
    bufferToFill.clearActiveBufferRegion();

    juce::MidiBuffer incomingMidi;
    midiCollector.removeNextBlockOfMessages(incomingMidi, bufferToFill.numSamples);
    keyboardState.processNextMidiBuffer(incomingMidi, bufferToFill.startSample,
                                        bufferToFill.numSamples, true);

    synth.renderNextBlock(*bufferToFill.buffer, incomingMidi,
                          bufferToFill.startSample, bufferToFill.numSamples);
}

juce::MidiMessageCollector* SynthAudioSource::getMidiCollector()
{
    return &midiCollector;
}

void SynthAudioSource::setSustain(double sustain)
{
    for (auto voice : voices) voice->setSustain(sustain);
}

void SynthAudioSource::createWavetable()
{
    wavetable.setSize(1, (int)tableSize + 1);
    wavetable.clear();

    auto* samples = wavetable.getWritePointer(0);

    int harmonics[] = { 1 };
    float harmonicWeights[] = { 1.0f };

    jassert (juce::numElementsInArray (harmonics) == juce::numElementsInArray(harmonicWeights));

    for (auto harmonic = 0; harmonic < juce::numElementsInArray(harmonics); ++harmonic)
    {
        auto angleDelta = juce::MathConstants<double>::twoPi / (double)(tableSize - 1) * harmonics[harmonic];
        auto currentAngle = 0.0;

        for (unsigned int i = 0; i < tableSize; ++i)
        {
            auto sample = std::sin(currentAngle);
            samples[i] += (float)sample * harmonicWeights[harmonic];
            currentAngle += angleDelta;
        }
    }

    samples[tableSize] = samples[0];
}


MainComponent::MainComponent() :
    synthAudioSource(keyboardState),
    keyboardComponent(keyboardState, juce::MidiKeyboardComponent::horizontalKeyboard),
    lastInputIndex(0),
    sustainLevel(0.99)
{
    addAndMakeVisible(midiInputListLabel);
    midiInputListLabel.setText("MIDI Input:", juce::dontSendNotification);
    midiInputListLabel.attachToComponent(&midiInputList, true);

    addAndMakeVisible(midiInputList);
    midiInputList.setTextWhenNoChoicesAvailable("No MIDI Inputs Enabled");

    auto midiInputs = juce::MidiInput::getAvailableDevices();
    juce::StringArray midiInputNames;
    for (auto input : midiInputs)
        midiInputNames.add(input.name);

    midiInputList.addItemList(midiInputNames, 1);
    midiInputList.onChange = [this] { setMidiInput(midiInputList.getSelectedItemIndex()); };

    for (auto input : midiInputs)
    {
        if (deviceManager.isMidiInputDeviceEnabled(input.identifier))
        {
            setMidiInput(midiInputs.indexOf(input));
            break;
        }
    }

    if (midiInputList.getSelectedId() == 0)
        setMidiInput(0);

    addAndMakeVisible(keyboardComponent);

    addAndMakeVisible(sustainLabel);
    sustainLabel.setText("Sustain:", juce::dontSendNotification);
    sustainLabel.attachToComponent(&sustainSlider, true);

    addAndMakeVisible(sustainSlider);
    sustainSlider.setTextBoxStyle(juce::Slider::TextBoxRight, true, 80, 20);
    sustainSlider.setRange(0.9, 0.99995);
    sustainSlider.setSkewFactorFromMidPoint(0.9999);
    sustainSlider.setValue(sustainLevel, juce::dontSendNotification);
    sustainSlider.onValueChange = [this]
    {
        synthAudioSource.setSustain(sustainSlider.getValue());
    };

    cpuUsageLabel.setText ("CPU Usage", juce::dontSendNotification);
    cpuUsageText.setJustificationType (juce::Justification::right);
    addAndMakeVisible (cpuUsageLabel);
    addAndMakeVisible (cpuUsageText);


    setAudioChannels(0, 2);
    
    setSize(600, 190);
    startTimer(400);
}

MainComponent::~MainComponent()
{
    shutdownAudio();
}

void MainComponent::prepareToPlay (int samplesPerBlockExpected, double sampleRate)
{
    synthAudioSource.prepareToPlay(samplesPerBlockExpected, sampleRate);
}

void MainComponent::getNextAudioBlock (const juce::AudioSourceChannelInfo& bufferToFill)
{
    synthAudioSource.getNextAudioBlock(bufferToFill);
}

void MainComponent::releaseResources()
{
    synthAudioSource.releaseResources();
}

void MainComponent::paint (juce::Graphics& g)
{
    g.fillAll (getLookAndFeel().findColour (juce::ResizableWindow::backgroundColourId));
}

void MainComponent::resized()
{
    midiInputList    .setBounds(200, 10, getWidth() - 210, 20);
    keyboardComponent.setBounds(10,  40, getWidth() - 20,  getHeight() - 110);
    sustainSlider    .setBounds(90, getHeight() - 60, getWidth() - 100, 20);
    cpuUsageLabel.setBounds (10, getHeight() - 30, getWidth() - 20, 20);
    cpuUsageText .setBounds (10, getHeight() - 30, getWidth() - 20, 20);
}

void MainComponent::timerCallback()
{
    if (!keyboardGrabbed)
    {
        keyboardComponent.grabKeyboardFocus();
        keyboardGrabbed = true;
        startTimer(50);
    }
    
    auto cpu = deviceManager.getCpuUsage() * 100;
    cpuUsageText.setText(juce::String(cpu, 6) + " %", juce::dontSendNotification);
}

void MainComponent::setMidiInput(int index)
{
    auto list = juce::MidiInput::getAvailableDevices();

    deviceManager.removeMidiInputDeviceCallback(list[lastInputIndex].identifier,
                                                synthAudioSource.getMidiCollector());

    auto newInput = list[index];

    if (!deviceManager.isMidiInputDeviceEnabled(newInput.identifier))
        deviceManager.setMidiInputDeviceEnabled(newInput.identifier, true);

    deviceManager.addMidiInputDeviceCallback(newInput.identifier,
                                             synthAudioSource.getMidiCollector());
    midiInputList.setSelectedId(index + 1, juce::dontSendNotification);

    lastInputIndex = index;
}
