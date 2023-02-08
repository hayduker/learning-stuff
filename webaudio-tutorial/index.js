// For legacy browsers
const AudioContext = window.AudioContext || window.webkitAudioContext;

const audioContext = new AudioContext();

// Get the audio element
const audioElement = document.querySelector('audio');

// Pass it into the audio context
const track = audioContext.createMediaElementSource(audioElement);


// Play/pause button
const playButton = document.querySelector('button');
playButton.addEventListener(
    'click',
    () => {
        // Check if context is in suspended state (autoplay policy)
        if (audioContext.state === 'suspended')
        {
            audioContext.resume();
        }
        
        // Play or pause track depending on state
        if (playButton.dataset.playing === 'false')
        {
            audioElement.play();
            playButton.dataset.playing = 'true';
        }
        else if (playButton.dataset.playing === 'true')
        {
            audioElement.pause();
            playButton.dataset.playing = 'false';
        }
    },
    false
);


// Listener for when file ends
audioElement.addEventListener(
    'ended',
    () => playButton.dataset.playing = 'false',
    false
);


// Create gain node
const gainNode = audioContext.createGain();

const volumeControl = document.querySelector('#volume');

volumeControl.addEventListener(
  'input',
  () => gainNode.gain.value = volumeControl.value,
  false
);


// Create panner node
const pannerOptions = { pan: 0 };
const panner = new StereoPannerNode(audioContext, pannerOptions);

const pannerControl = document.querySelector("#panner");

pannerControl.addEventListener(
  "input",
  () => {
    panner.pan.value = pannerControl.value;
  },
  false
);




// Connect input to gain node, and gain node to output
track.connect(gainNode).connect(panner).connect(audioContext.destination);