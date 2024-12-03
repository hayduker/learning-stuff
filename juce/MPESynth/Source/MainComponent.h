#pragma once

#include <JuceHeader.h>


class NoteComponent : public juce::Component
{
public:
    NoteComponent(const juce::MPENote& n) : note(n), colour(juce::Colours::white) {}

    void update(const juce::MPENote& newNote, juce::Point<float> newCentre)
    {
        note = newNote;
        centre = newCentre;

        setBounds(getSquareAroundCentre (juce::jmax (getNoteOnRadius(), getNoteOffRadius(), getPressureRadius()))
                     .getUnion (getTextRectangle())
                     .getSmallestIntegerContainer()
                     .expanded (3));

        repaint();
    }

    void paint(juce::Graphics& g) override
    {
        if (note.keyState == juce::MPENote::keyDown || note.keyState == juce::MPENote::keyDownAndSustained)
            drawPressedNoteCircle(g, colour);
        else if (note.keyState == juce::MPENote::sustained)
            drawSustainedNoteCircle(g, colour);
        else
            return;

        drawNoteLabel(g, colour);
    }

    juce::MPENote note;
    juce::Colour colour;
    juce::Point<float> centre;

private:
    void drawPressedNoteCircle (juce::Graphics& g, juce::Colour zoneColour)
    {
        g.setColour(zoneColour.withAlpha (0.3f));
        g.fillEllipse(translateToLocalBounds(getSquareAroundCentre(getNoteOnRadius())));
        g.setColour(zoneColour);
        g.drawEllipse(translateToLocalBounds(getSquareAroundCentre(getPressureRadius())), 2.0f);
    }

    void drawSustainedNoteCircle(juce::Graphics& g, juce::Colour zoneColour)
    {
        g.setColour(zoneColour);
        juce::Path circle, dashedCircle;
        circle.addEllipse(translateToLocalBounds(getSquareAroundCentre(getNoteOffRadius())));
        const float dashLengths[] = { 3.0f, 3.0f };
        juce::PathStrokeType(2.0, juce::PathStrokeType::mitered).createDashedStroke(dashedCircle, circle, dashLengths, 2);
        g.fillPath(dashedCircle);
    }

    void drawNoteLabel (juce::Graphics& g, juce::Colour)
    {
        auto textBounds = translateToLocalBounds (getTextRectangle()).getSmallestIntegerContainer();
        g.drawText ("+", textBounds, juce::Justification::centred);
        g.drawText (juce::MidiMessage::getMidiNoteName (note.initialNote, true, true, 3), textBounds, juce::Justification::centredBottom);
        g.setFont (juce::Font (22.0f, juce::Font::bold));
        g.drawText (juce::String (note.midiChannel), textBounds, juce::Justification::centredTop);
    }

    juce::Rectangle<float> getSquareAroundCentre (float radius) const noexcept
    {
        return juce::Rectangle<float> (radius * 2.0f, radius * 2.0f).withCentre (centre);
    }

    juce::Rectangle<float> translateToLocalBounds (juce::Rectangle<float> r) const noexcept
    {
        return r - getPosition().toFloat();
    }

    juce::Rectangle<float> getTextRectangle() const noexcept
    {
        return juce::Rectangle<float> (30.0f, 50.0f).withCentre (centre);
    }

    float getNoteOnRadius()   const noexcept   { return note.noteOnVelocity.asUnsignedFloat() * maxNoteRadius; }
    float getNoteOffRadius()  const noexcept   { return note.noteOffVelocity.asUnsignedFloat() * maxNoteRadius; }
    float getPressureRadius() const noexcept   { return note.pressure.asUnsignedFloat() * maxNoteRadius; }

    static constexpr auto maxNoteRadius = 100.0f;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (NoteComponent)
};


class Visualiser : public  juce::Component,
                   public  juce::MPEInstrument::Listener,
                   private juce::AsyncUpdater
{
public:
    Visualiser() {}

    void paint(juce::Graphics& g) override
    {
        g.fillAll(juce::Colours::darkgrey);

        g.setColour(juce::Colours::lightgrey);

        auto numRows = 8, numCols = 25;
        auto noteDistance = 36.0f;
        auto gapWidth = 2.0f;

        for (auto colIdx = 0; colIdx <= numCols; colIdx++)
        {
            auto x = noteDistance * (float)colIdx + gapWidth/2.0f;
            g.drawLine(x, 0.0f, x, noteDistance*numRows,2.0f);
        }

        for (auto rowIdx = 0; rowIdx <= numRows; rowIdx++)
        {
            auto y = noteDistance * (float)rowIdx + gapWidth/2.0f;
            g.drawLine(0.0f, y, noteDistance*numCols, y, 2.0f);
        }
    }

    void noteAdded(juce::MPENote newNote) override
    {
        const juce::ScopedLock sl(lock);
        activeNotes.add(newNote);
        triggerAsyncUpdate();
    }

    void notePressureChanged (juce::MPENote note) override { noteChanged(note); }
    void notePitchbendChanged(juce::MPENote note) override { noteChanged(note); }
    void noteTimbreChanged   (juce::MPENote note) override { noteChanged(note); }
    void noteKeyStateChanged (juce::MPENote note) override { noteChanged(note); }

    void noteChanged(juce::MPENote changedNote)
    {
        const juce::ScopedLock sl(lock);

        for (auto& note : activeNotes)
            if (note.noteID == changedNote.noteID) note = changedNote;

        triggerAsyncUpdate();
    }

    void noteReleased(juce::MPENote finishedNote) override
    {
        const juce::ScopedLock sl (lock);

        for (auto i = activeNotes.size(); --i >= 0;)
            if (activeNotes.getReference(i).noteID == finishedNote.noteID) activeNotes.remove (i);

        triggerAsyncUpdate();
    }


private:
    const juce::MPENote* findActiveNote(int noteID) const noexcept
    {
        for (auto& note : activeNotes)
            if (note.noteID == noteID) return &note;

        return nullptr;
    }

    juce::Array<NoteComponent*> findNoteComponents(int noteID) const noexcept
    {
        juce::Array<NoteComponent*> components;
        for (auto& noteComp : noteComponents)
            if (noteComp->note.noteID == noteID)
                components.add(noteComp);

        return components;
    }

    void handleAsyncUpdate() override
    {
        const juce::ScopedLock sl(lock);

        // For existing components, if active note no longer exists, remove component
        for (auto i = noteComponents.size(); --i >= 0;)
            if (findActiveNote(noteComponents.getUnchecked(i)->note.noteID) == nullptr)
                noteComponents.remove(i);

        // For existing active notes, if component doesn't exist, add component
        for (auto& note : activeNotes)
        {
            juce::Array<NoteComponent*> existingComponents = findNoteComponents(note.noteID);
            juce::Array<juce::Point<float>> centers = getCentrePositionsForNote(note);
            if (existingComponents.size() > centers.size())
            {
                std::cout << "More existing components than we have centers for. Not good!" << std::endl;
            }
            else if (existingComponents.size() < centers.size())
            {
                int numberToAdd = centers.size() - existingComponents.size();
                for (int i = 0; i < numberToAdd; i++)
                {
                    NoteComponent* component = new NoteComponent(note);
                    noteComponents.add(component);
                    existingComponents.add(component);
                    addAndMakeVisible(component);
                }
            }

            if (existingComponents.size() != centers.size())
            {
                std::cout << "Still have different number of components from centers, bailing." << std::endl;
                return;
            }

            for (int i = 0; i < existingComponents.size(); i++)
            {
                auto noteComp = existingComponents.getUnchecked(i);
                auto center = centers.getUnchecked(i);
                noteComp->update(note, center);
            }
        }
    }

    juce::Array<juce::Point<float>> getCentrePositionsForNote(juce::MPENote note) const
    {
        juce::Array<juce::Point<float>> centers;
        auto noteDistance = 36.0f;

        auto n = float(note.initialNote); // + float(note.totalPitchbendInSemitones*0.5f);
        std::cout << "DCS getCentrePositionsForNote = " << n << std::endl;

        for (auto row = 0; row < 8; row++)
        {
            auto c = n - (5*row) - 30;
            auto r = 7 - row;
            if (0 <= c && c <= 24 && 0 <= r && r <= 8)
            {
                auto x = noteDistance * ((float)c + 0.5f) + float(note.totalPitchbendInSemitones) * noteDistance * 0.5f;
                auto y = noteDistance * ((float)r + 0.5f) + noteDistance * (0.5f - note.timbre.asUnsignedFloat());
                std::cout << "DCS timbre = " << 0.5f - note.timbre.asUnsignedFloat() << ", y = " << y << std::endl;
                centers.add({x,y});
            }
        }

        return centers;
    }

    juce::OwnedArray<NoteComponent> noteComponents;
    juce::CriticalSection lock;
    juce::Array<juce::MPENote> activeNotes;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Visualiser)
};



class MPEDemoSynthVoice : public juce::MPESynthesiserVoice
{
public:
    MPEDemoSynthVoice();

    void noteStarted() override;
    void noteStopped(bool allowTailOff) override;
    void notePressureChanged() override;
    void notePitchbendChanged() override;
    void noteTimbreChanged() override;
    void noteKeyStateChanged() override;
    void setCurrentSampleRate (double newRate);

    void renderNextBlock(juce::AudioBuffer<float>& outputBuffer,
                         int startSample,
                         int numSamples) override;

private:
    float getNextSample() noexcept;

    juce::SmoothedValue<double> level, timbre, frequency;
    double phase, phaseDelta, tailOff;

    static constexpr auto maxLevel = 0.05;
    static constexpr auto maxLevelDb = 31.0;
    static constexpr auto smoothingLengthInSeconds = 0.01;
};


class MainComponent : public  juce::Component,
                      private juce::AudioIODeviceCallback,
                      private juce::MidiInputCallback            
{
public:
    MainComponent();
    ~MainComponent() override;

    //void paint (juce::Graphics& g) override;
    void resized() override;

    void audioDeviceStopped() override {}

private:
    // Start MidiInputCallback overrides
    void handleIncomingMidiMessage(juce::MidiInput* source,
                                   const juce::MidiMessage& message) override;
    // Start MidiInputCallback overrides

    // Start AudioIODeviceCallback overrides
    void audioDeviceAboutToStart(juce::AudioIODevice* device) override;

    void audioDeviceIOCallbackWithContext(const float* const* inputChannelData,
                                          int numInputchannels,
                                          float* const* outputChannelData,
                                          int numOutputChannels,
                                          int numSamples,
                                          const juce::AudioIODeviceCallbackContext& context) override;
    // End AudioIODeviceCallback overrides

    juce::AudioDeviceManager audioDeviceManager;
    juce::AudioDeviceSelectorComponent audioSetupComp;

    juce::ComboBox midiInputList;
    juce::Label midiInputListLabel;
    int lastInputIndex;

    Visualiser visualizerComp;
    juce::Viewport visualizerViewport;

    juce::MPEInstrument visualizerInstrument;
    juce::MPESynthesiser synth;
    juce::MidiMessageCollector midiCollector;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};