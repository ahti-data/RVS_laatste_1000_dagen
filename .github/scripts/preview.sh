#!/usr/bin/env bash
# Runs on the GitHub runner (called by the preview workflows) and manages dashboard previews on the
# server over SSH, with the same FTP_* credentials the production deploy uses. Nothing has to be
# installed on the server.
#
#   preview.sh deploy    upload this PR as <base>/<APP_FOLDER>--pr-<n>/ and refresh the overview page
#   preview.sh delete    remove that preview and refresh the overview page
#   preview.sh prune     remove previews of this app whose PR is no longer open (needs OPEN_PRS)
#   preview.sh reindex   only rebuild the overview page
#
# Layout on the server:  <base>/index.html          overview page, served at /preview/
#                        <base>/<name>/             one Shiny app per preview (data/ -> live data)
#                        <base>/.rows/<name>.html   that preview's row in the overview page
# <base> must never contain an app.R: it is a plain folder, and every sub-folder is its own app.
#
# Environment: SSHPASS SSH_USER SSH_HOST APP_FOLDER, plus PR_NUMBER SRC_DIR [PR_TITLE PR_URL] for deploy,
#              PR_NUMBER for delete, OPEN_PRS (space separated numbers, may be empty) for prune.
set -euo pipefail

BASE="${PREVIEW_BASE:-/apps/preview}"
die() { echo "preview.sh: $*" >&2; exit 1; }

[[ "${APP_FOLDER:-}" =~ ^[A-Za-z0-9_]+$ ]] || die "APP_FOLDER missing or invalid"

# Run a command on the server. PREVIEW_LOCAL_TEST=1 runs it locally instead (used by the tests).
remote() {
  if [[ -n "${PREVIEW_LOCAL_TEST:-}" ]]; then
    bash -c "$1"
  else
    sshpass -e ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
      "${SSH_USER:?}@${SSH_HOST:?}" "$1"
  fi
}

html_escape() { sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g' -e 's/"/\&quot;/g'; }

page_header() {
  cat <<'HTML'
<!doctype html>
<html lang="nl"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Dashboard previews</title>
<style>
  body{font:16px/1.5 system-ui,sans-serif;margin:0 auto;max-width:56rem;padding:2rem 1rem;color:#1a1a1a}
  h1{margin:0 0 .25rem} p.lead{margin:0 0 1.5rem;color:#555}
  table{border-collapse:collapse;width:100%} th,td{text-align:left;padding:.5rem .75rem;border-bottom:1px solid #ddd}
  th{font-size:.85rem;color:#555} a{color:#0b5cad} .empty{color:#555;padding:1rem 0}
</style></head><body>
<h1>Dashboard previews</h1>
<p class="lead">Wijzigingen die nog niet in het echte dashboard staan. Een preview verdwijnt zodra de pull request is gesloten of samengevoegd.</p>
HTML
}

reindex() {
  local rows page
  rows="$(remote "cd '$BASE/.rows' 2>/dev/null && ls -1t *.html 2>/dev/null | head -200 | xargs -r cat || true")"
  page="$(mktemp)"
  {
    page_header
    if [[ -n "$rows" ]]; then
      echo '<table><tr><th>Dashboard</th><th>Wijziging</th><th>Bijgewerkt</th><th></th></tr>'
      printf '%s\n' "$rows"
      echo '</table>'
    else
      echo '<p class="empty">Er staan op dit moment geen previews online.</p>'
    fi
    echo '</body></html>'
  } > "$page"
  remote "mkdir -p '$BASE' && cat > '$BASE/.index.tmp' && chmod a+r '$BASE/.index.tmp' && mv '$BASE/.index.tmp' '$BASE/index.html'" < "$page"
  rm -f "$page"
}

write_row() {
  local name="$1" title url when row
  title="$(printf '%s' "${PR_TITLE:-}" | tr -d '\000-\037' | cut -c1-120)"
  [[ -n "$title" ]] || title="(geen titel)"
  url="${PR_URL:-}"
  [[ "$url" =~ ^https://github\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/pull/[0-9]+$ ]] || url=""
  when="$(TZ=Europe/Amsterdam date '+%Y-%m-%d %H:%M')"
  row="<tr><td>${APP_FOLDER}</td><td>#${PR_NUMBER} $(printf '%s' "$title" | html_escape)</td><td>${when}</td><td><a href=\"./${name}/\">Open preview</a>"
  [[ -n "$url" ]] && row+=" &middot; <a href=\"$(printf '%s' "$url" | html_escape)\">PR</a>"
  row+="</td></tr>"
  printf '%s\n' "$row" | remote "mkdir -p '$BASE/.rows' && cat > '$BASE/.rows/$name.html' && chmod a+r '$BASE/.rows/$name.html'"
}

deploy() {
  [[ "${PR_NUMBER:-}" =~ ^[0-9]+$ ]] || die "PR_NUMBER missing or invalid"
  [[ -n "${SRC_DIR:-}" ]] || die "SRC_DIR missing"
  local name="${APP_FOLDER}--pr-${PR_NUMBER}"

  # data/ is not uploaded: the preview is linked to the live data of the dashboard, which it only reads.
  # state/ is not uploaded either (runtime state, same as for the production deploy); a redeploy keeps
  # the preview's own state/, so favorites and dictionary edits made in a preview survive new pushes.
  tar -czf - -C "$SRC_DIR" app.R utils templates | remote "set -e
    base='$BASE'; name='$name'; app='$APP_FOLDER'
    mkdir -p \"\$base\"
    tmp=\$(mktemp -d \"\$base/.incoming-XXXXXX\")
    trap 'rm -rf \"\$tmp\"' EXIT
    tar -xzf - -C \"\$tmp\" --no-same-owner
    rm -rf \"\$tmp/data\"
    ln -s \"../../\$app/data\" \"\$tmp/data\"
    if [ -d \"\$base/\$name/state\" ]; then mv \"\$base/\$name/state\" \"\$tmp/state\"; else mkdir \"\$tmp/state\"; fi
    chmod -R a+rX \"\$tmp\"
    chmod -R a+rwX \"\$tmp/state\"
    rm -rf \"\$base/\$name\"
    mv \"\$tmp\" \"\$base/\$name\"
    echo deployed \"\$name\""

  write_row "$name"
  reindex
}

delete() {
  [[ "${PR_NUMBER:-}" =~ ^[0-9]+$ ]] || die "PR_NUMBER missing or invalid"
  local name="${APP_FOLDER}--pr-${PR_NUMBER}"
  remote "rm -rf '$BASE/$name' '$BASE/.rows/$name.html' && echo deleted '$name'"
  reindex
}

prune() {
  [[ -n "${OPEN_PRS+x}" ]] || die "OPEN_PRS must be set (it may be empty)"
  local n num
  for n in $(remote "ls -1 '$BASE' 2>/dev/null || true"); do
    [[ "$n" =~ ^${APP_FOLDER}--pr-([0-9]+)$ ]] || continue
    num="${BASH_REMATCH[1]}"
    case " $OPEN_PRS " in
      *" $num "*) ;;
      *) remote "rm -rf '$BASE/$n' '$BASE/.rows/$n.html' && echo pruned '$n'" ;;
    esac
  done
  reindex
}

case "${1:-}" in
  deploy) deploy ;;
  delete) delete ;;
  prune) prune ;;
  reindex) reindex ;;
  *) die "usage: preview.sh deploy|delete|prune|reindex" ;;
esac
