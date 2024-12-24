import sys
from scipy.io.wavfile import write
import numpy as np

def generate_wave(frequencies, duration=0.5, sample_rate=44100):
    audio = []
    for freq in frequencies:
        t = np.linspace(0, duration, int(sample_rate * duration), False)
        wave = np.sin(2 * np.pi * freq * t)
        audio.extend(wave)
    audio = np.array(audio)
    write("melody.wav", sample_rate, (audio * 32767).astype(np.int16))

#frequencies = [261.63, 293.66, 329.63, 349.23]  # Example frequencies
#generate_wave(frequencies)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        # Frequencies from command-line arguments
        frequencies = list(map(float, sys.argv[1:]))
    else:
        # Frequencies from stdin
        input_data = sys.stdin.read().strip()
        frequencies = list(map(float, input_data.split()))
    
    if not frequencies:
        print("No frequencies provided. Exiting.")
        sys.exit(1)
    
    generate_wave(frequencies)
    print("Generated melody.wav with frequencies:", frequencies)

