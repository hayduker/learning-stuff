#include "MainComponent.h"


SineOscillator::SineOscillator() : currentAngle(0.0f), angleDelta(0.0f) {}

void SineOscillator::setFrequency(float frequency, float sampleRate)
{
    auto cyclesPerSample = frequency / sampleRate;
    angleDelta = cyclesPerSample * juce::MathConstants<float>::twoPi;
}

forcedinline float SineOscillator::getNextSample() noexcept
{
    auto currentSample = std::sin(currentAngle);
    updateAngle();
    return currentSample;
}

forcedinline void SineOscillator::updateAngle() noexcept
{
    currentAngle += angleDelta;
    if (currentAngle >= juce::MathConstants<float>::twoPi)
        currentAngle -= juce::MathConstants<float>::twoPi;
}


WavetableOscillator::WavetableOscillator(const juce::AudioSampleBuffer& wavetableToUse) : 
    wavetable(wavetableToUse),
    tableSize(wavetable.getNumSamples() - 1),
    currentIndex(0.0f),
    tableDelta(0.0f)
{
    jassert(wavetable.getNumChannels() == 1);
}

void WavetableOscillator::setFrequency(float frequency, float sampleRate)
{
    auto tableSizeOverSampleRate = (float)tableSize / sampleRate;
    tableDelta = frequency * tableSizeOverSampleRate;
}

forcedinline float WavetableOscillator::getNextSample() noexcept
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



MainComponent::MainComponent()
{
    cpuUsageLabel.setText ("CPU Usage", juce::dontSendNotification);
    cpuUsageText.setJustificationType (juce::Justification::right);
    addAndMakeVisible (cpuUsageLabel);
    addAndMakeVisible (cpuUsageText);

    createWavetable();

    setSize (400, 200);
    setAudioChannels (0, 2);
    startTimer (50);
}

MainComponent::~MainComponent()
{
    shutdownAudio();
}

void MainComponent::prepareToPlay (int, double sampleRate)
{
    auto numberOfOscillators = 1;

    for (auto i = 0; i < numberOfOscillators; ++i)
    {
        auto* oscillator = new WavetableOscillator (sineTable);

        auto midiNote = juce::Random::getSystemRandom().nextDouble() * 36.0 + 48.0;
        auto frequency = 440.0 * pow (2.0, (midiNote - 69.0) / 12.0);

        oscillator->setFrequency ((float) frequency, (float) sampleRate);
        oscillators.add (oscillator);
    }

    level = 0.25f / (float) numberOfOscillators;
}

void MainComponent::getNextAudioBlock (const juce::AudioSourceChannelInfo& bufferToFill)
{
    auto* leftBuffer  = bufferToFill.buffer->getWritePointer (0, bufferToFill.startSample);
    auto* rightBuffer = bufferToFill.buffer->getWritePointer (1, bufferToFill.startSample);

    bufferToFill.clearActiveBufferRegion();

    for (auto oscillatorIndex = 0; oscillatorIndex < oscillators.size(); ++oscillatorIndex)
    {
        auto* oscillator = oscillators.getUnchecked (oscillatorIndex);

        for (auto sample = 0; sample < bufferToFill.numSamples; ++sample)
        {
            auto levelSample = oscillator->getNextSample() * level;

            leftBuffer[sample]  += levelSample;
            rightBuffer[sample] += levelSample;
        }
    }
}

void MainComponent::releaseResources() {}

void MainComponent::paint (juce::Graphics& g)
{
    g.fillAll (getLookAndFeel().findColour (juce::ResizableWindow::backgroundColourId));
}

void MainComponent::resized()
{
    cpuUsageLabel.setBounds (10, 10, getWidth() - 20, 20);
    cpuUsageText .setBounds (10, 10, getWidth() - 20, 20);
}

void MainComponent::timerCallback()
{
    auto cpu = deviceManager.getCpuUsage() * 100;
    cpuUsageText.setText(juce::String(cpu, 6) + " %", juce::dontSendNotification);
}

void MainComponent::createWavetable()
{
    sineTable.setSize (1, (int) tableSize + 1);
    sineTable.clear();

    auto* samples = sineTable.getWritePointer (0);

    int harmonics[] = { 1, 3, 5, 6, 7, 9, 13, 15 };
    float harmonicWeights[] = { 0.5f, 0.1f, 0.05f, 0.125f, 0.09f, 0.005f, 0.002f, 0.001f };

    jassert (juce::numElementsInArray (harmonics) == juce::numElementsInArray (harmonicWeights));

    for (auto harmonic = 0; harmonic < juce::numElementsInArray (harmonics); ++harmonic)
    {
        auto angleDelta = juce::MathConstants<double>::twoPi / (double) (tableSize - 1) * harmonics[harmonic];
        auto currentAngle = 0.0;

        for (unsigned int i = 0; i < tableSize; ++i)
        {
            auto sample = std::sin (currentAngle);
            samples[i] += (float) sample * harmonicWeights[harmonic];
            currentAngle += angleDelta;
        }
    }

    samples[tableSize] = samples[0];
}