#include "MainComponent.h"

DecibelSlider::DecibelSlider() {}

double DecibelSlider::getValueFromText(const juce::String& text)
{
    auto minusInfinitydB = -100.0;
    auto decibelText = text.upToFirstOccurrenceOf("dB", false, false).trim();
    return decibelText.equalsIgnoreCase("-INF") ? minusInfinitydB
                                                : decibelText.getDoubleValue();
}

juce::String DecibelSlider::getTextFromValue(double value)
{
    return juce::Decibels::toString(value);
}

MainComponent::MainComponent() : level(0)
{
    level = 0;

    levelSlider.setRange(0.0, 1.0);
    levelSlider.setTextBoxStyle(juce::Slider::TextBoxRight, false, 100, 20);
    levelSlider.onValueChange = [this]
    {
        level = (float)levelSlider.getValue();
        decibelSlider.setValue(juce::Decibels::gainToDecibels(level));
    };
    levelSlider.setValue(level);

    levelLabel.setText("Noise Level", juce::dontSendNotification);

    decibelSlider.setRange(-100, 0);
    decibelSlider.setTextBoxStyle(juce::Slider::TextBoxRight, false, 100, 20);
    decibelSlider.onValueChange = [this]
    {
        level = juce::Decibels::decibelsToGain((float)decibelSlider.getValue());
        levelSlider.setValue(level);
    };
    decibelSlider.setValue(juce::Decibels::gainToDecibels(level));

    decibelLabel.setText("Noise Level in dB", juce::dontSendNotification);

    addAndMakeVisible(levelSlider);
    addAndMakeVisible(levelLabel);
    addAndMakeVisible(decibelSlider);
    addAndMakeVisible(decibelLabel);

    setSize (600, 100);
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
    auto currentLevel = level;
    auto levelScale = currentLevel * 2.0f;

    int numChannels = bufferToFill.buffer->getNumChannels();
    int startSample = bufferToFill.startSample;
    int numSamples  = bufferToFill.numSamples;

    for (int channel = 0; channel < numChannels; channel++)
    {
        auto* buffer = bufferToFill.buffer->getWritePointer(channel, startSample);
        for (int sample = 0; sample < numSamples; sample++)
        {
            buffer[sample] = random.nextFloat() * levelScale - currentLevel;
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

void MainComponent::resized()
{
    levelLabel .setBounds(10, 10, 90, 20);
    levelSlider.setBounds(100, 10, getWidth() - 110, 20);
    decibelLabel .setBounds(10, 50, 90, 20);
    decibelSlider.setBounds(100, 50, getWidth() - 110, 20);
}
