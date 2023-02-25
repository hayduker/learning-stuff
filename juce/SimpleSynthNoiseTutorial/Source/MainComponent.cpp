#include "MainComponent.h"

MainComponent::MainComponent()
{
    setSize (800, 600);
    setAudioChannels (0, 2);
}

MainComponent::~MainComponent()
{
    shutdownAudio();
}

void MainComponent::prepareToPlay (int samplesPerBlockExpected, double sampleRate)
{
    juce::String message;
    message << "Preparing to play audio...\n";
    message << " samplesPerBlockExpected = " << samplesPerBlockExpected << "\n";
    message << " sampleRate = " << sampleRate;
    juce::Logger::getCurrentLogger()->writeToLog (message);
}

void MainComponent::getNextAudioBlock (const juce::AudioSourceChannelInfo& bufferToFill)
{
    int numChannels = bufferToFill.buffer->getNumChannels();
    int startSample = bufferToFill.startSample;
    int numSamples  = bufferToFill.numSamples;

    for (int channel = 0; channel < numChannels; channel++)
    {
        auto* buffer = bufferToFill.buffer->getWritePointer(channel, startSample);
        for (int sample = 0; sample < numSamples; sample++)
        {
            buffer[sample] = random.nextFloat() * 0.25f - 0.125f;
        }
    }
}

void MainComponent::releaseResources() 
{
    juce::Logger::getCurrentLogger()->writeToLog ("Releasing audio resources");
}

void MainComponent::paint (juce::Graphics& g)
{
    g.fillAll (getLookAndFeel().findColour (juce::ResizableWindow::backgroundColourId));
}

void MainComponent::resized() {}
