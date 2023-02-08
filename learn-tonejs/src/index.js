import * as Tone from 'tone';

document.getElementById('start-button').onclick = async () => {
    await Tone.start();
    console.log('audio is ready');
};

const synth = new Tone.Synth().toDestination();

let activeNote = "";


const keyToNote = {
    'a': 'C4',
    'w': 'C#4',
    's': 'D4',
    'e': 'D#4',
    'd': 'E4',
    'f': 'F4',
    't': 'F#4',
    'g': 'G4',
    'y': 'G#4',
    'h': 'A4',
    'u': 'A#4',
    'j': 'B4',
}


const playNote = noteName => {
    if (noteName == activeNote) return;
    const now = Tone.now();
    synth.triggerAttack(noteName, now);
    activeNote = noteName;
};

const stopNote = noteName => {
    if (noteName != activeNote) return;
    const now = Tone.now();
    synth.triggerRelease(now);
    activeNote = "";
};

const getNoteForKey = elem => {
    const key = elem.key;
    return (key in keyToNote) ? keyToNote[key] : null;
};

document.addEventListener('keydown', elem => {
    const note = getNoteForKey(elem);
    if (note) playNote(note);
});

document.addEventListener('keyup', elem => {
    const note = getNoteForKey(elem);
    if (note) stopNote(note);
});

const keysNodeList = document.querySelectorAll('.key');
const keysList = [...keysNodeList];
keysList.forEach(k => {
    if (k.classList.contains('invisible')) return;
});