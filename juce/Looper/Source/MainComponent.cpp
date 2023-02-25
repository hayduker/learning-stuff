#include "MainComponent.h"

MainComponent::MainComponent() :
    state(Stopped)
{
    addAndMakeVisible (recordButton);
    recordButton.setButtonText ("Record");
    recordButton.setColour(juce::TextButton::buttonColourId, juce::Colours::red);
    recordButton.onClick = [this] { recordButtonClicked(); };

    addAndMakeVisible (clearButton);
    clearButton.setButtonText ("Clear");
    clearButton.setColour(juce::TextButton::buttonColourId, juce::Colours::darkgrey);
    clearButton.onClick = [this] { clearButtonClicked(); };

    setSize (300, 200);

    loopBuffer.setSize(1, 44100 * 4);

    DBG("Constructor called, setting audio channels");
    setAudioChannels(1,2);
}

MainComponent::~MainComponent()
{
    shutdownAudio();
}

void MainComponent::prepareToPlay(int samplesPerBlockExpected, double sampleRate) {}

void MainComponent::getNextAudioBlock(const juce::AudioSourceChannelInfo& bufferToFill)
{
    auto outputSamplesOffset = bufferToFill.startSample;

    auto* loopReader = loopBuffer.getReadPointer(0, 0);
    auto* loopWriter = loopBuffer.getWritePointer(0, 0);

//    for (auto channel = 0; channel < bufferToFill.buffer->getNumChannels(); channel++)
//    {
        auto* inBuffer  = bufferToFill.buffer->getReadPointer (0, 0);
        auto* outBuffer = bufferToFill.buffer->getWritePointer(0, 0);

        for (auto sample = outputSamplesOffset; sample < bufferToFill.numSamples; sample++)
        {
            
            if (state == Recording)
            {
                if (!firstSampleAfterRecording)
                {
                    firstSampleAfterRecording = true;
                    DBG("State switched to Recording");
                }

                loopWriter[position] = inBuffer[sample];
                outBuffer[sample]    = inBuffer[sample];
                position++;
            }
            else if (state == Playing)
            {
                if (firstSampleAfterRecording)
                {
                    DBG("State switched to Playing");
                    loopLength = position;
                    position = 0;
                    firstSampleAfterRecording = false;
                }

                if (firstSampleAfterOverdubbing)
                {
                    DBG("State switched to Playing after overdub");
                    firstSampleAfterOverdubbing = false;
                }

                outBuffer[sample] = loopReader[position];
                position++;
                if (position == loopLength) position = 0;
            }
            else if (state == Overdubbing)
            {
                if (!firstSampleAfterOverdubbing)
                {
                    DBG("State switched to overdubbing");
                    firstSampleAfterOverdubbing = true;
                }

                loopWriter[position] = loopReader[position] + inBuffer[sample];
                outBuffer[sample]    = loopReader[position] + inBuffer[sample];
                position++; 
                if (position == loopLength) position = 0;

            }
            else // if (state == Stopped)
            {
                if (position > 0) position = 0;
                outBuffer[sample] = inBuffer[sample];
            }
            
        }
 //   }
}

void MainComponent::releaseResources()
{
    loopBuffer.setSize (0, 0);
}

void MainComponent::paint (juce::Graphics& g)
{
    g.fillAll (getLookAndFeel().findColour (juce::ResizableWindow::backgroundColourId));
}

void MainComponent::resized()
{
    recordButton.setBounds(10, 10, getWidth() - 20, 20);
    clearButton.setBounds(10, 40, getWidth() - 20, 20);
}

void MainComponent::recordButtonClicked()
{
    DBG("record button clicked");
    if (state == Stopped)
    {
        DBG("state = Stopped, start recording");
        state = Recording;
        recordButton.setButtonText ("Play");
        recordButton.setColour(juce::TextButton::buttonColourId, juce::Colours::green);
    }
    else if (state == Recording || state == Overdubbing)
    {
        DBG("state = Recording, start playing");
        state = Playing;

        recordButton.setButtonText ("Overdub");
        recordButton.setColour(juce::TextButton::buttonColourId, juce::Colours::darkgoldenrod);
        recordButton.setEnabled(true);
    }
    else if (state == Playing)
    {
        DBG("state = Stopped, start overdubbing");
        state = Overdubbing;
        recordButton.setButtonText ("Play");
        recordButton.setColour(juce::TextButton::buttonColourId, juce::Colours::green);        
    }
}

void MainComponent::clearButtonClicked()
{
    DBG("clear button clicked");
    state = Stopped;
    loopBuffer.clear(0,loopLength);
    loopLength = 0;

    recordButton.setButtonText ("Record");
    recordButton.setColour(juce::TextButton::buttonColourId, juce::Colours::red);
    recordButton.setEnabled(true);
}