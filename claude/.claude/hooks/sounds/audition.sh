#!/bin/zsh
# Play every personality's cue pair so you can pick by ear.
# Usage: ./audition.sh [name ...]   (default: all)   e.g. ./audition.sh musicbox
emulate -L zsh
here="${0:A:h}"
names=("$@"); (( $#names )) || names=(musicbox arcade bell)
for n in $names; do
  for cue in question review; do
    f="$here/variants/$n-$cue.wav"
    [[ -f "$f" ]] || { print -r -- "missing: $f"; continue }
    [[ "$cue" == question ]] && glyph="❓" || glyph="👀"
    print -r -- "▶ $glyph  $n  $cue"
    afplay "$f"
    sleep 0.45
  done
  sleep 0.4
done
