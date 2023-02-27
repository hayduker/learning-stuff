#pragma once

#include <JuceHeader.h>

class MainComponent  : public  juce::Component,
                       private juce::MidiInputCallback
{
public:
    MainComponent();
    ~MainComponent() override;

    void paint (juce::Graphics& g) override;
    void resized() override;

private:
    // This is used to dispach an incoming message to the message thread
    class IncomingMessageCallback : public juce::CallbackMessage
    {
    public:
        IncomingMessageCallback(MainComponent* o, const juce::MidiMessage& m) : owner(o), message(m) {}

        void messageCallback() override
        {
            if (owner != nullptr) owner->addMessageToList(message);
        }

        Component::SafePointer<MainComponent> owner;
        juce::MidiMessage message;
    };

    void setMidiInput(int index);
    void handleIncomingMidiMessage(juce::MidiInput* /*source*/, const juce::MidiMessage& message) override;
    void postMessageToList (const juce::MidiMessage& message);
    void addMessageToList(const juce::MidiMessage& message);
    void logMessage(const juce::String& m);

    juce::AudioDeviceManager deviceManager;
    juce::ComboBox midiInputList;
    juce::Label midiInputListLabel;
    int lastInputIndex;
 
    juce::MidiKeyboardState keyboardState;
    juce::TextEditor midiMessagesBox;
    double startTime;
    juce::TextButton clearButton;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
