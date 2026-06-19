#!/bin/zsh
# Make a personality the active cue pair the hook plays.
# Usage: ./use.sh <musicbox|arcade|bell>
emulate -L zsh
here="${0:A:h}"
name="${1:?usage: ./use.sh <musicbox|arcade|bell>}"
for cue in review question; do
  src="$here/variants/$name-$cue.wav"
  [[ -f "$src" ]] || { print -r -- "no such variant: $src"; exit 1 }
  cp -- "$src" "$here/$cue.wav"
done
print -r -- "active cues set to '$name'  (review.wav, question.wav)"
