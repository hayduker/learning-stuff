import * as Tone from 'tone';

document.getElementById('start-button').onclick = async () => await Tone.start();

const activeSynths = {}

let selectedTuning = 'equal';

let selectedPitchClass = 'C';
let selectedOctave = 4;
let currentRoot = selectedPitchClass + selectedOctave.toString();
let rootFreq = Tone.Frequency(currentRoot).toFrequency();

// Equal temperament math

const frs = Math.pow(2, 1/12);

const noteFreqEqual = semitonesFromRoot => {
    return rootFreq * Math.pow(frs, semitonesFromRoot);
};

// Just temperament math

const semitonesToRatio = {
    0:  1,
    2:  9/8,
    4:  5/4,
    5:  4/3,
    7:  3/2,
    9:  5/3,
    11: 15/8,
    12: 2
}

const noteFreqJust = semitonesFromRoot => {
    return rootFreq * semitonesToRatio[semitonesFromRoot];
};

const keyToInterval = {
    'z': 0,
    'x': 2,
    'c': 4,
    'v': 5,
    'b': 7,
    'n': 9,
    'm': 11,
}

const startNote = key => {
    if (key in activeSynths) return;
    const freq = getFreqForKey(key);
    const osc = new Tone.Oscillator(freq, 'triangle').toDestination().start();
    activeSynths[key] = osc;
};

const stopNote = key => {
    if (!(key in activeSynths)) return;
    activeSynths[key].stop();
    delete activeSynths[key];
};

const restartNotes = () => {
    console.log('active synths = ' + activeSynths)
    for (const key in activeSynths) {
        console.log('restarting synth for key ' + key)
        stopNote(key);
        startNote(key);
    }
}

const getFreqForKey = key => {
    const semitonesFromRoot = keyToInterval[key];
    
    if (selectedTuning == 'equal') {
        return noteFreqEqual(semitonesFromRoot);
    } else if (selectedTuning == 'just') {
        return noteFreqJust(semitonesFromRoot);
    }
};

document.addEventListener('keydown', elem => {
    elem.target.classList.add('active');
    const key = elem.key;
    if (key in keyToInterval) startNote(key);
});

document.addEventListener('keyup', elem => {
    elem.target.classList.remove('active');
    const key = elem.key;
    if (key in keyToInterval) stopNote(key);
});

const keysNodeList = document.querySelectorAll('.key');
const keysList = [...keysNodeList];
keysList.forEach(k => {
    if (k.classList.contains('invisible')) return;
});


const tuningButtons = document.querySelectorAll('input[name="tuning"]');
for (const button of tuningButtons) {
    button.onclick = e => {
        selectedTuning = e.target.value;
        console.log('radio button hit, new val = ' + selectedTuning);
        restartNotes();
    };
}

// Octave controls

document.querySelector('#octave-minus').onclick = e => {
    if (selectedOctave > 0) selectedOctave -= 1;
    currentRoot = selectedPitchClass + selectedOctave.toString();
    rootFreq = Tone.Frequency(currentRoot).toFrequency();
    restartNotes();
};

document.querySelector('#octave-plus').onclick = e => {
    if (selectedOctave < 8) selectedOctave += 1; 
    currentRoot = selectedPitchClass + selectedOctave.toString();
    rootFreq = Tone.Frequency(currentRoot).toFrequency();
    restartNotes();
};