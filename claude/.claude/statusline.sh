#!/usr/bin/env bash
# Claude Code status line.
# Segments: jj diff (+add -del ~mod) · bookmark↑ahead · cwd · ctx% · model effort · [5h%] · [wk%] · id
#
# The 5h (five_hour) and wk (seven_day) usage numbers are HIDDEN unless a
# trajectory projection says you're on pace to exhaust that window's limit:
#   yellow = may run out (projected finish 90-115% of limit)
#   red    = will run out (projected finish >=115% with enough evidence)
# See the rate-limit trajectory section below for the model.
#
# Receives the session JSON on stdin.

input=$(cat)

j() { printf '%s' "$input" | jq -r "$1" 2>/dev/null; }

# --- Colors (real escape bytes so length-stripping works) ---
RST=$'\e[0m'; DIM=$'\e[2m'
C_ADD=$'\e[32m'; C_DEL=$'\e[31m'; C_MOD=$'\e[33m'
C_BM=$'\e[35m'; C_CWD=$'\e[34m'; C_MODEL=$'\e[96m'

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

# --- Rate-limit usage + trajectory (5h "hourly" and 7d "weekly") ---
# Claude Code (Pro/Max) provides .rate_limits.{five_hour,seven_day}.{used_percentage,resets_at}.
# Both segments are hidden unless we project you'll run out of that window.
#
# Model: projected_end% = used% / elapsed_token_fraction.
#   - 5h window: elapsed fraction is flat over the 5h span (resets_at-5h .. resets_at).
#   - 7d window: elapsed fraction is weighted by how you actually spend tokens across
#     the week. From your Jun 3-18 usage, a weekday is ~18.1% of a typical week's tokens
#     and a weekend day ~4.7% (any rolling 7d window has 5 weekdays + 2 weekend days,
#     so these sum to ~100%). So burning hard on a Monday projects higher than the same
#     %-used reached on a quiet Sunday.
# State per window: 0=hidden, 1=yellow (may run out), 2=red (will run out).
five_used=$(j '.rate_limits.five_hour.used_percentage // empty')
five_reset=$(j '.rate_limits.five_hour.resets_at // empty')
seven_used=$(j '.rate_limits.seven_day.used_percentage // empty')
seven_reset=$(j '.rate_limits.seven_day.resets_at // empty')

TRAJ_PY='
import sys,time,datetime
WD=0.1813   # one weekday as a fraction of a typical weeks tokens (Jun 3-18)
WE=0.0467   # one weekend day, ditto
def num(x):
    try: return float(x)
    except: return None
fu,fr,su,sr=(num(a) for a in sys.argv[1:5])
now=time.time()
def state(used,ef):
    if used is None or ef<=0: return (0,int(used or 0))
    if used<10: return (0,int(used))
    proj=used/ef
    if proj>=115 and (ef>=0.08 or used>=40): s=2
    elif proj>=90: s=1
    else: s=0
    return (s,int(used))
# 5h: flat elapsed fraction across the 5-hour (18000s) span
ef5=max(0.0,min(1.0,(now-(fr-18000))/18000.0)) if fr else 0.0
s5,p5=state(fu,ef5)
# 7d: integrate weekday/weekend daily shares over the elapsed part of the window
def wk_elapsed(sr):
    start=sr-604800; frac=0.0; t=start
    while t<now and t<sr:
        dt=datetime.datetime.fromtimestamp(t)
        day0=dt.replace(hour=0,minute=0,second=0,microsecond=0)
        nxt=(day0+datetime.timedelta(days=1)).timestamp()
        seg=min(nxt,now)-t
        frac+=(WE if dt.weekday()>=5 else WD)*seg/86400.0
        t=nxt
    return frac
ef7=wk_elapsed(sr) if sr else 0.0
s7,p7=state(su,ef7)
print(int(s5),int(p5),int(s7),int(p7))
'
five_state=0; five_pct=0; seven_state=0; seven_pct=0
if [[ -n $five_used || -n $seven_used ]]; then
  read -r five_state five_pct seven_state seven_pct < <(
    python3 -c "$TRAJ_PY" "${five_used:-}" "${five_reset:-}" "${seven_used:-}" "${seven_reset:-}" 2>/dev/null)
fi

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
# Rate-limit segments: only shown when on a trajectory to run out.
if (( five_state >= 1 )); then
  col=$'\e[33m'; (( five_state == 2 )) && col=$'\e[31m'
  line="${line}${sep}${col}5h ${five_pct}%${RST}"
fi
if (( seven_state >= 1 )); then
  col=$'\e[33m'; (( seven_state == 2 )) && col=$'\e[31m'
  line="${line}${sep}${col}wk ${seven_pct}%${RST}"
fi
[[ -n $sid  ]] && line="${line}${sep}${DIM}${sid}${RST}"

# Left-aligned: Claude Code strips leading whitespace from status line output
# (see github.com/anthropics/claude-code/issues/29206), so right-alignment via
# padding isn't possible — print the line as-is.
printf '%s\n' "$line"
