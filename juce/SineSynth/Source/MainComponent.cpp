#include "MainComponent.h"

MainComponent::MainComponent() :
    currentSampleRate(0.0),
    currentAngle(0.0),
    angleDelta(0.0),
    currentFrequency(500.0),
    targetFrequency(500.0),
    currentLevel(0.125),
    targetLevel(0.125)
{
    frequencySlider.setRange(50.0, 5000.0);
    frequencySlider.setSkewFactorFromMidPoint(500.0);
    frequencySlider.setTextBoxStyle(juce::Slider::TextBoxRight, true, 100, 20);
    frequencySlider.onValueChange = [this]
    {
        targetFrequency = frequencySlider.getValue();
    };
    frequencySlider.setValue(currentFrequency, juce::dontSendNotification);
    addAndMakeVisible(&frequencySlider);

    levelSlider.setRange(0.0, 0.25, 0.001);
    levelSlider.setTextBoxStyle(juce::Slider::TextBoxRight, true, 100, 20);
    levelSlider.onValueChange = [this]
    {
        targetLevel = (double)levelSlider.getValue();
    };
    levelSlider.setValue(currentLevel, juce::dontSendNotification);
    addAndMakeVisible(&levelSlider);
    
    setSize (600, 100);
    setAudioChannels (0, 2);
}

MainComponent::~MainComponent()
{
    shutdownAudio();
}

void MainComponent::prepareToPlay (int samplesPerBlockExpected, double sampleRate)
{
    currentSampleRate = sampleRate;
    updateAngleDelta();
}

void MainComponent::getNextAudioBlock (const juce::AudioSourceChannelInfo& bufferToFill)
{
    auto* leftBuffer  = bufferToFill.buffer->getWritePointer (0, bufferToFill.startSample);
    auto* rightBuffer = bufferToFill.buffer->getWritePointer (1, bufferToFill.startSample);

    auto localTargetFrequency = targetFrequency;
    if (localTargetFrequency != currentFrequency)
    {
        auto frequencyIncrement = (localTargetFrequency - currentFrequency) / bufferToFill.numSamples;

        for (auto sample = 0; sample < bufferToFill.numSamples; ++sample)
        {
            auto currentSample = (float) std::sin (currentAngle);
            currentFrequency += frequencyIncrement;
            updateAngleDelta();
            currentAngle += angleDelta;
            leftBuffer[sample]  = currentSample;
            rightBuffer[sample] = currentSample;
        }

        currentFrequency = localTargetFrequency;
    }
    else
    {
        for (auto sample = 0; sample < bufferToFill.numSamples; ++sample)
        {
            auto currentSample = (float) std::sin (currentAngle);
            currentAngle += angleDelta;
            leftBuffer[sample]  = currentSample;
            rightBuffer[sample] = currentSample;
        }
    }

    auto localTargetLevel = targetLevel;
    bufferToFill.buffer->applyGainRamp(bufferToFill.startSample, bufferToFill.numSamples, currentLevel, localTargetLevel);
    currentLevel = localTargetLevel;
}

void MainComponent::releaseResources() {}

void MainComponent::paint (juce::Graphics& g)
{
    g.fillAll (getLookAndFeel().findColour (juce::ResizableWindow::backgroundColourId));
}

void MainComponent::resized()
{
    frequencySlider.setBounds(10, 10, getWidth() - 20, 20);
    levelSlider.setBounds(10, 50, getWidth() - 20, 20);
}

void MainComponent::updateAngleDelta()
{
    auto cyclesPerSample = frequencySlider.getValue() / currentSampleRate;
    angleDelta = cyclesPerSample * 2.0 * juce::MathConstants<double>::pi;
}