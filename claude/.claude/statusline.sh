#!/usr/bin/env bash
# Claude Code status line.
# Segments: jj diff (+add -del ~mod) · bookmark↑ahead · cwd · ctx% · model effort · 5h% · id
#
# Receives the session JSON on stdin.

input=$(cat)

j() { printf '%s' "$input" | jq -r "$1" 2>/dev/null; }

# --- Colors (real escape bytes so length-stripping works) ---
RST=$'\e[0m'; DIM=$'\e[2m'
C_ADD=$'\e[32m'; C_DEL=$'\e[31m'; C_MOD=$'\e[33m'
C_BM=$'\e[35m'; C_CWD=$'\e[34m'; C_RATE=$'\e[36m'; C_MODEL=$'\e[96m'

# --- Context % (red when over the 200k threshold, else by usage) ---
ctx=$(j '.context_window.used_percentage // 0 | floor')
exceeds=$(j '.exceeds_200k_tokens // false')
if   [[ $exceeds == true ]]; then C_CTX=$'\e[31m'
elif (( ctx >= 90 ));        then C_CTX=$'\e[31m'
elif (( ctx >= 70 ));        then C_CTX=$'\e[33m'
else                              C_CTX=$'\e[32m'
fi

# --- model / effort (effort is absent when the model doesn't support it) ---
model=$(j '.model.display_name // empty')
effort=$(j '.effort.level // empty')

# --- 5h rate limit ---
rate=$(j '.rate_limits.five_hour.used_percentage // empty | floor')

# --- session id (short prefix) ---
sid=$(j '.session_id // empty'); sid=${sid:0:8}

# --- cwd (home-relative) ---
dir=$(j '.workspace.current_dir // .cwd')
cwd=${dir/#$HOME/\~}

# --- jj data (only when the working dir is inside a jj repo) ---
# --ignore-working-copy everywhere: without it every jj call takes the
# working-copy lock and snapshots, racing the session's own jj commands and
# stalling refreshes. Stats lag until the next real jj command snapshots.
in_jj=0; added=0; removed=0; bm=""; ahead=0
if [[ -n $dir ]] && cd "$dir" 2>/dev/null && jj root --ignore-working-copy >/dev/null 2>&1; then
  in_jj=1
  stat=$(jj diff --stat --ignore-working-copy 2>/dev/null | tail -1)
  [[ $stat =~ ([0-9]+)\ insertion ]] && added=${BASH_REMATCH[1]}
  [[ $stat =~ ([0-9]+)\ deletion  ]] && removed=${BASH_REMATCH[1]}

  closest='heads(::@ & bookmarks())'
  bm=$(jj log --ignore-working-copy --no-graph -r "$closest" -T 'bookmarks.map(|b| b.name()).join(",")' 2>/dev/null | head -1)
  if [[ -n $bm ]]; then
    ahead=$(jj log --ignore-working-copy --no-graph -r "($closest)..@" -T '"x\n"' 2>/dev/null | grep -c x)
  fi
fi
# Lines changed in place ≈ overlap of additions and deletions (diffs don't
# track "modified" as a distinct category; this is a best-effort heuristic).
modified=$(( added < removed ? added : removed ))

# --- Assemble segments ---
sep=" ${DIM}·${RST} "
seg_cwd="${C_CWD}${cwd}${RST}"
seg_ctx="${C_CTX}${ctx}%${RST}"

line="${seg_cwd}${sep}${seg_ctx}"
if (( in_jj )); then
  seg_diff="${C_ADD}+${added}${RST} ${C_DEL}-${removed}${RST} ${C_MOD}~${modified}${RST}"
  seg_bm="${C_BM}${bm:-(none)}${RST}${DIM}↑${ahead}${RST}"
  line="${seg_diff}${sep}${seg_bm}${sep}${line}"
fi
if [[ -n $model ]]; then
  seg_model="${C_MODEL}${model}${RST}"
  [[ -n $effort ]] && seg_model+=" ${DIM}${effort}${RST}"
  line="${line}${sep}${seg_model}"
fi
[[ -n $rate ]] && line="${line}${sep}${C_RATE}5h ${rate}%${RST}"
[[ -n $sid  ]] && line="${line}${sep}${DIM}${sid}${RST}"

# Left-aligned: Claude Code strips leading whitespace from status line output
# (see github.com/anthropics/claude-code/issues/29206), so right-alignment via
# padding isn't possible — print the line as-is.
printf '%s\n' "$line"
