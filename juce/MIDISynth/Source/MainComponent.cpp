#include "MainComponent.h"


SynthAudioSource::SynthAudioSource(juce::MidiKeyboardState& keyState) :
    keyboardState(keyState),
    numVoices(4)
{
    for (auto i = 0; i < numVoices; i++)
    {
        auto voice = new SineWaveVoice();
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
    keyboardComponent.setBounds(10,  40, getWidth() - 20,  getHeight() - 80);
    sustainSlider    .setBounds(90, getHeight() - 30, getWidth() - 100, 20);
}

void MainComponent::timerCallback()
{
    keyboardComponent.grabKeyboardFocus();
    stopTimer();
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