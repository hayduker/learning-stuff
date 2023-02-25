#include "MainComponent.h"

MainComponent::MainComponent() : juce::Thread("Background thread")
{
    addAndMakeVisible (openButton);
    openButton.setButtonText ("Open...");
    openButton.onClick = [this] { openButtonClicked(); };

    addAndMakeVisible (clearButton);
    clearButton.setButtonText ("Clear");
    clearButton.onClick = [this] { clearButtonClicked(); };

    setSize (300, 200);

    formatManager.registerBasicFormats();
    setAudioChannels(0,2);

    DBG("MainComponent constructor called");

    startThread();
}

MainComponent::~MainComponent()
{
    stopThread (4000);
    shutdownAudio();
}

void MainComponent::prepareToPlay (int samplesPerBlockExpected, double sampleRate)
{
    juce::Logger::getCurrentLogger()->writeToLog("prepareToPlay() triggered.");
}

void MainComponent::getNextAudioBlock (const juce::AudioSourceChannelInfo& bufferToFill)
{
    auto retainedCurrentBuffer = [&]() -> ReferenceCountedBuffer::Ptr
    {
        const juce::SpinLock::ScopedTryLockType lock (mutex);

        if (lock.isLocked())
            return currentBuffer;

        return nullptr;
    }();

    if (retainedCurrentBuffer == nullptr)
    {
        bufferToFill.clearActiveBufferRegion();
        return;
    }

    auto* currentAudioSampleBuffer = retainedCurrentBuffer->getAudioSampleBuffer();
    auto position = retainedCurrentBuffer->position;

    auto numInputChannels = currentAudioSampleBuffer->getNumChannels();
    auto numOutputChannels = bufferToFill.buffer->getNumChannels();

    auto outputSamplesRemaining = bufferToFill.numSamples;
    auto outputSamplesOffset = 0;

    while (outputSamplesRemaining > 0)
    {
        auto bufferSamplesRemaining = currentAudioSampleBuffer->getNumSamples() - position;
        auto samplesThisTime = juce::jmin (outputSamplesRemaining, bufferSamplesRemaining);

        for (auto channel = 0; channel < numOutputChannels; ++channel)
        {
            bufferToFill.buffer->copyFrom (channel,
                                            bufferToFill.startSample + outputSamplesOffset,
                                            *currentAudioSampleBuffer,
                                            channel % numInputChannels,
                                            position,
                                            samplesThisTime);
        }

        outputSamplesRemaining -= samplesThisTime;
        outputSamplesOffset += samplesThisTime;
        position += samplesThisTime;

        if (position == currentAudioSampleBuffer->getNumSamples())
            position = 0;
    }

    retainedCurrentBuffer->position = position;
}

void MainComponent::releaseResources()
{
    const juce::SpinLock::ScopedLockType lock(mutex);
    currentBuffer = nullptr;
}

void MainComponent::paint (juce::Graphics& g)
{
    g.fillAll (getLookAndFeel().findColour (juce::ResizableWindow::backgroundColourId));
}

void MainComponent::resized()
{
    openButton .setBounds (10, 10, getWidth() - 20, 20);
    clearButton.setBounds (10, 40, getWidth() - 20, 20);
}

void MainComponent::run()
{
    DBG("Thread run() method called");
    while (!threadShouldExit())
    {
        DBG("  Thread loop iteration");
        checkForPathToOpen();
        checkForBuffersToFree();
        wait(4000);
    }
}

void MainComponent::checkForPathToOpen()
{
    DBG("    Checking for path to open");

    juce::String pathToOpen;

    {
        const juce::ScopedLock lock (pathMutex);
        pathToOpen.swapWith (chosenPath);
    }

    if (pathToOpen.isNotEmpty())
    {
        juce::File file (pathToOpen);
        std::unique_ptr<juce::AudioFormatReader> reader (formatManager.createReaderFor (file));

        if (reader.get() != nullptr)
        {
            auto duration = (float) reader->lengthInSamples / reader->sampleRate;

            if (duration > 2) return;

            ReferenceCountedBuffer::Ptr newBuffer = new ReferenceCountedBuffer (file.getFileName(),
                                                                                (int) reader->numChannels,
                                                                                (int) reader->lengthInSamples);

            reader->read (newBuffer->getAudioSampleBuffer(), 0, (int) reader->lengthInSamples, 0, true, true);

            {
                const juce::SpinLock::ScopedLockType lock (mutex);
                currentBuffer = newBuffer;
            }

            buffers.add (newBuffer);
        }
    }
}

void MainComponent::checkForBuffersToFree()
{
    DBG("    Checking " + juce::String(buffers.size()) + " buffers to free");
    for (auto i = buffers.size(); --i >= 0;)
    {
        ReferenceCountedBuffer::Ptr buffer(buffers.getUnchecked(i));
        DBG("      Buffer " + juce::String(i) + " named " + buffer->getName() + " has refcount " + juce::String(buffer->getReferenceCount()));
        if (buffer->getReferenceCount() == 2) buffers.remove(i);
    }
}

void MainComponent::openButtonClicked()
{
    chooser = std::make_unique<juce::FileChooser> ("Select a Wave file to play...", juce::File{}, "*.wav");
    auto chooserFlags = juce::FileBrowserComponent::openMode
                      | juce::FileBrowserComponent::canSelectFiles;
    
    chooser->launchAsync (chooserFlags, [this] (const juce::FileChooser& fc)
    {
        auto file = fc.getResult();

        if (file == juce::File{}) return;

        auto path = file.getFullPathName();

        {
            const juce::ScopedLock lock (pathMutex);
            chosenPath.swapWith (path);
        }

        notify();
    });
}

void MainComponent::clearButtonClicked()
{
    const juce::SpinLock::ScopedLockType lock(mutex);
    DBG("Got lock, setting buffer ptr to null");
    currentBuffer = nullptr;
}