#include "MainComponent.h"

MainComponent::MainComponent()
{
    addAndMakeVisible (openButton);
    openButton.setButtonText ("Open...");
    openButton.onClick = [this] { openButtonClicked(); };

    addAndMakeVisible (clearButton);
    clearButton.setButtonText ("Clear");
    clearButton.onClick = [this] { clearButtonClicked(); };

    addAndMakeVisible (&levelSlider);
    levelSlider.setSliderStyle(juce::Slider::LinearHorizontal);
    levelSlider.setRange(0.0, 1.0, 0.05);
    levelSlider.setValue(0.5);

    setSize (300, 200);

    formatManager.registerBasicFormats();
}

MainComponent::~MainComponent()
{
    shutdownAudio();
}

void MainComponent::prepareToPlay (int samplesPerBlockExpected, double sampleRate) {
    juce::Logger::getCurrentLogger()->writeToLog("prepareToPlay() triggered.");
}

void MainComponent::getNextAudioBlock (const juce::AudioSourceChannelInfo& bufferToFill)
{
    auto level = (float)levelSlider.getValue();
    auto numInputChannels  = fileBuffer.getNumChannels();
    auto numOutputChannels = bufferToFill.buffer->getNumChannels();

    auto outputSamplesOffset = bufferToFill.startSample;
    auto outputSamplesRemaining = bufferToFill.numSamples;

    while (outputSamplesRemaining > 0)
    {
        auto bufferSamplesRemaining = fileBuffer.getNumSamples() - position;
        auto samplesThisTime = juce::jmin(outputSamplesRemaining, bufferSamplesRemaining);

        for (auto channel = 0; channel < numOutputChannels; channel++)
        {
            bufferToFill.buffer->copyFrom(channel,
                                          outputSamplesOffset,
                                          fileBuffer,
                                          channel % numInputChannels,
                                          position,
                                          samplesThisTime);
        }

        outputSamplesRemaining -= samplesThisTime;
        outputSamplesOffset += samplesThisTime;
        position += samplesThisTime;

        if (position == fileBuffer.getNumSamples()) position = 0;
    }

    bufferToFill.buffer->applyGain(level);
}

void MainComponent::releaseResources()
{
    fileBuffer.setSize (0, 0);
}

void MainComponent::paint (juce::Graphics& g)
{
    g.fillAll (getLookAndFeel().findColour (juce::ResizableWindow::backgroundColourId));
}

void MainComponent::resized()
{
    openButton .setBounds (10, 10, getWidth() - 20, 20);
    clearButton.setBounds (10, 40, getWidth() - 20, 20);
    levelSlider.setBounds (10, 70, getWidth() - 20, 20);
}


void MainComponent::openButtonClicked()
{
    shutdownAudio();

    juce::Logger::getCurrentLogger()->writeToLog("Open button clicked, audio shut down done.");

    chooser = std::make_unique<juce::FileChooser> ("Select a Wave file shorter than 2 seconds to play...",
                                                   juce::File{},
                                                   "*.wav");
    auto chooserFlags = juce::FileBrowserComponent::openMode
                      | juce::FileBrowserComponent::canSelectFiles;
    
    chooser->launchAsync (chooserFlags, [this] (const juce::FileChooser& fc)
    {
        juce::Logger::getCurrentLogger()->writeToLog("File chooser launched.");

        auto file = fc.getResult();
        if (file == juce::File{}) return;

        juce::Logger::getCurrentLogger()->writeToLog("Picked file " + file.getFileName());

        std::unique_ptr<juce::AudioFormatReader> reader(formatManager.createReaderFor(file));
        if (reader.get() != nullptr)
        {
            auto duration = (float) reader->lengthInSamples / reader->sampleRate;

            if (duration > 2) return;

            juce::Logger::getCurrentLogger()->writeToLog("Reader not null, and file duration less that 2sec.");

            fileBuffer.setSize((int)reader->numChannels, (int)reader->lengthInSamples);
            reader->read(&fileBuffer,
                         0,
                         (int)reader->lengthInSamples,
                         0,
                         true,
                         true);
            position = 0;
            setAudioChannels(0, (int)reader->numChannels);
        }
    });
}

void MainComponent::clearButtonClicked()
{
    shutdownAudio();
}