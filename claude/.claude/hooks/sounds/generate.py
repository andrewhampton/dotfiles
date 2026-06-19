#!/usr/bin/env python3
"""Generate the Claude Code tab-status audio cues.

Pure standard library (no numpy / sox / ffmpeg) so it runs anywhere a stock
Python 3 lives. Writes 16-bit mono WAVs that `afplay` plays directly.

Two cues, designed so you can tell them apart WITHOUT looking — they map to
speech prosody:

  question (❓)  a short two-note phrase that RISES and hangs unresolved —
                the melody of a spoken question. "I need you."
  review   (👀)  a brighter three-note arpeggio that resolves UP to the tonic
                with a sparkle tail — a soft "ta-da, ready."

Three timbral personalities are rendered into ./variants so you can audition
and pick (see audition.sh). The chosen pair is copied to ./review.wav and
./question.wav (see use.sh). The shipped pair is "arcade" — the most playful;
"musicbox" is the gentlest and "bell" the most ethereal.

Re-render after editing:  python3 generate.py
"""

import math
import os
import struct
import wave

SR = 44_100              # sample rate
BITS_PEAK = 32_767       # 16-bit signed max
HERE = os.path.dirname(os.path.abspath(__file__))
VARIANTS = os.path.join(HERE, "variants")

# Equal-tempered note frequencies (Hz). Kept in a bright-but-not-piercing band.
A4, D5, E5, G5, A5, C6, E6, G6, C7 = (
    440.00, 587.33, 659.25, 783.99, 880.00, 1046.50, 1318.51, 1567.98, 2093.00,
)

# Melodies (frequency, duration_seconds). Shared across personalities; only the
# timbre changes between variants.
#
#   review : E5 → G5 → C6  — ascending C-major triad landing on the tonic
#            octave = resolved + positive. Plus a faint C7 shimmer on the last
#            note for "sparkle".
#   question: D5 → A5      — a rising perfect fifth that ends on the 6th (la),
#            left gently hanging = "answer me?". Fewer notes + lower + no
#            celebratory tail, so it never gets confused with review.
REVIEW_MELODY = [(E5, 0.085), (G5, 0.085), (C6, 0.30)]
QUESTION_MELODY = [(D5, 0.11), (A5, 0.24)]

# Personalities: harmonic recipe + decay character.
#   harmonics : list of (partial_multiple, amplitude) summed per note
#   decay     : exponential decay rate (1/s) — higher = snappier/shorter ring
#   detune    : cents of a faint second voice for shimmer (0 = none)
PERSONALITIES = {
    # Pure-ish sine with a touch of upper partials — twinkly, the gentlest.
    "musicbox": dict(
        harmonics=[(1.0, 1.0), (2.0, 0.22), (3.0, 0.10), (4.0, 0.04)],
        decay=8.5, detune=4.0,
    ),
    # Softened triangle (odd partials) — snappier, a touch "arcade/coin",
    # the most playful without tipping into chiptune harshness.
    "arcade": dict(
        harmonics=[(1.0, 1.0), (3.0, 0.30), (5.0, 0.12), (7.0, 0.05)],
        decay=13.0, detune=0.0,
    ),
    # More partials + long ring + light detune — dreamy bell, the most "ethereal".
    "bell": dict(
        harmonics=[(1.0, 1.0), (2.0, 0.5), (3.0, 0.22), (4.76, 0.18), (6.0, 0.08)],
        decay=4.0, detune=7.0,
    ),
}

TARGET_PEAK = 0.5        # gentle: leave headroom, let system volume do the rest
ATTACK = 0.005           # 5 ms soft attack kills the click at note onset
OVERLAP = 0.82           # next note starts at 82% of the previous duration (legato)
ENDFADE = 0.004          # 4 ms fade-out on the final mix, anti-click


def _cents(freq, cents):
    return freq * (2.0 ** (cents / 1200.0))


def _render_note(freq, dur, spec, shimmer=False):
    """One plucked note: additive partials under a soft-attack / exp-decay env."""
    n = int(SR * dur)
    buf = [0.0] * n
    voices = [(freq, 1.0)]
    if spec["detune"]:
        voices.append((_cents(freq, spec["detune"]), 0.6))  # faint chorus voice
    if shimmer:
        voices.append((C7, 0.16))                            # bright sparkle on top
    for vfreq, vamp in voices:
        for mult, hamp in spec["harmonics"]:
            w = 2.0 * math.pi * vfreq * mult
            a = vamp * hamp
            for i in range(n):
                buf[i] += a * math.sin(w * (i / SR))
    # amplitude envelope
    for i in range(n):
        t = i / SR
        env = (t / ATTACK) if t < ATTACK else math.exp(-(t - ATTACK) * spec["decay"])
        buf[i] *= env
    return buf


def _render_phrase(melody, spec, sparkle_last):
    """Lay notes out with legato overlap and mix into one buffer."""
    notes = []
    start = 0
    starts = []
    for idx, (freq, dur) in enumerate(melody):
        shimmer = sparkle_last and idx == len(melody) - 1
        note = _render_note(freq, dur, spec, shimmer=shimmer)
        notes.append(note)
        starts.append(start)
        start += int(SR * dur * OVERLAP)
    total = max(s + len(b) for s, b in zip(starts, notes))
    mix = [0.0] * total
    for s, b in zip(starts, notes):
        for i, v in enumerate(b):
            mix[s + i] += v
    # normalize to target peak
    peak = max((abs(v) for v in mix), default=1.0) or 1.0
    g = TARGET_PEAK / peak
    mix = [v * g for v in mix]
    # final anti-click fade-out
    fade = int(SR * ENDFADE)
    for i in range(fade):
        mix[total - 1 - i] *= i / fade
    return mix


def _write_wav(path, samples):
    frames = b"".join(
        struct.pack("<h", max(-BITS_PEAK, min(BITS_PEAK, int(v * BITS_PEAK))))
        for v in samples
    )
    with wave.open(path, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(SR)
        w.writeframes(frames)


def main():
    os.makedirs(VARIANTS, exist_ok=True)
    for name, spec in PERSONALITIES.items():
        review = _render_phrase(REVIEW_MELODY, spec, sparkle_last=True)
        question = _render_phrase(QUESTION_MELODY, spec, sparkle_last=False)
        _write_wav(os.path.join(VARIANTS, f"{name}-review.wav"), review)
        _write_wav(os.path.join(VARIANTS, f"{name}-question.wav"), question)
        print(f"rendered {name}: review={len(review)/SR:.2f}s question={len(question)/SR:.2f}s")
    print(f"\nvariants written to {VARIANTS}")
    print("audition: ./audition.sh    |    choose: ./use.sh <musicbox|arcade|bell>")


if __name__ == "__main__":
    main()
