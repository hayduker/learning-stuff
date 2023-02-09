import * as Tone from 'tone';

document.getElementById('start-button').onclick = async () => await Tone.start();


// Create keyboard GUI

const keyConfig = (n, c) => {
    return {
        'name': n,
        'class': c
    };
};

const createKeyElem = keyConfig => {
    const keyNameElem = document.createElement('div');
    keyNameElem.innerText = keyConfig.name;
    keyNameElem.classList.add('key-name');

    const pitchNameElem = document.createElement('div');
    pitchNameElem.innerText = '';
    pitchNameElem.classList.add('pitch-name');

    const textContainerElem = document.createElement('div');
    textContainerElem.classList.add('text-container');
    textContainerElem.appendChild(keyNameElem);
    textContainerElem.appendChild(pitchNameElem);

    const keyElem = document.createElement('div');
    keyElem.id = 'kbdkbd-' + keyConfig.name;
    keyElem.classList.add('key')
    keyElem.classList.add(keyConfig.class);
    keyElem.appendChild(textContainerElem);

    return keyElem;
};

const createKeyboardRowElem = keyConfigs => {
    const keyboardRowElem = document.createElement('div');
    keyboardRowElem.classList.add('keyboard-row');
    keyConfigs.forEach(keyConfig => {
        keyboardRowElem.appendChild(createKeyElem(keyConfig));
    });
    return keyboardRowElem;
};

const createKeyboardElem = keyboardConfigs => {
    const keyboardContainerElem = document.createElement('div');
    keyboardContainerElem.classList.add('keyboard-container');

    for (const row in keyboardConfigs) {
        const rowConfigs = keyboardConfigs[row];
        keyboardContainerElem.appendChild(createKeyboardRowElem(rowConfigs));
    }
    return keyboardContainerElem;
};


const keyConfigs = {
    row0: [
        keyConfig('1','inactive'),
        keyConfig('2','inactive'),
        keyConfig('3','inactive'),
        keyConfig('4','black'),
        keyConfig('5','black'),
        keyConfig('6','inactive'),
        keyConfig('7','black'),
        keyConfig('8','black'),
        keyConfig('9','black'),
        keyConfig('0','inactive'),
    ],
    row1: [
        keyConfig('~','invisible'),
        keyConfig('q','inactive'),
        keyConfig('w','inactive'),
        keyConfig('e','white'),
        keyConfig('r','white'),
        keyConfig('t','white'),
        keyConfig('y','white'),
        keyConfig('u','white'),
        keyConfig('i','white'),
        keyConfig('o','white'),
        keyConfig('p','inactive'),
    ],
    row2: [
        keyConfig('~','invisible'),
        keyConfig('a','inactive'),
        keyConfig('s','black'),
        keyConfig('d','black'),
        keyConfig('f','inactive'),
        keyConfig('g','black'),
        keyConfig('h','black'),
        keyConfig('j','black'),
        keyConfig('k','inactive'),
        keyConfig('l','inactive'),
    ],
    row3: [
        keyConfig('z','white'),
        keyConfig('x','white'),
        keyConfig('c','white'),
        keyConfig('v','white'),
        keyConfig('b','white'),
        keyConfig('n','white'),
        keyConfig('m','white'),
    ]
}

const getKeysFromKeyboard = keyboardElem => {
    const keysObject = {};
    const keysNodeList = keyboardElem.querySelectorAll("[id^='kbdkbd-']");
    [...keysNodeList].forEach(keyElem => {
        const id = keyElem.id;
        const keyName = id.slice(id.lastIndexOf('-') + 1);
        keysObject[keyName] = keyElem;
    });

    return keysObject;
};

const pageContainerElem = document.getElementById('page-container');
const keyboardElem = createKeyboardElem(keyConfigs)
pageContainerElem.appendChild(keyboardElem);

const keyElems = getKeysFromKeyboard(keyboardElem);
for (const keyName in keyElems) {
    const keyElem = keyElems[keyName];
    keyElem.onclick = () => {
        if (keyName in activeSynths) {
            stopNote(keyName);
        } else {
            startNote(keyName);
        }
    };
}


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
    1:  256/243,
    2:  9/8,
    3:  32/27,
    4:  5/4,
    5:  4/3,
    6:  729/512,
    7:  3/2,
    8:  128/81,
    9:  5/3,
    10: 16/9,
    11: 15/8,
    12: 2,
    13: 2 * (256/243),
    14: 2 * (9/8),
    15: 2 * (32/27),
    16: 2 * (5/4),
    17: 2 * (4/3),
    18: 2 * (729/512),
    19: 2 * (3/2),
    20: 2 * (128/81),
    21: 2 * (5/3),
    22: 2 * (16/9),
    23: 2 * (15/8)
}

const noteFreqJust = semitonesFromRoot => {
    return rootFreq * semitonesToRatio[semitonesFromRoot];
};

const keyToInterval = {
    'z': 0,
    's': 1,
    'x': 2,
    'd': 3,
    'c': 4,
    'v': 5,
    'g': 6,
    'b': 7,
    'h': 8,
    'n': 9,
    'j': 10,
    'm': 11,
    'e': 12,
    '4': 13,
    'r': 14,
    '5': 15,
    't': 16,
    'y': 17,
    '7': 18,
    'u': 19,
    '8': 20,
    'i': 21,
    '9': 22,
    'o': 23
}

const startNote = key => {
    if (key in activeSynths) return;

    keyElems[key].classList.add('playing');

    const freq = getFreqForKey(key);
    const osc = new Tone.Oscillator(freq, 'triangle').toDestination().start();
    activeSynths[key] = osc;
};

const stopNote = key => {
    if (!(key in activeSynths)) return;

    keyElems[key].classList.remove('playing');

    activeSynths[key].stop();
    delete activeSynths[key];
};

const restartNotes = () => {
    for (const key in activeSynths) {
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
    const key = elem.key;
    if (key in keyToInterval) startNote(key);
});

document.addEventListener('keyup', elem => {
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