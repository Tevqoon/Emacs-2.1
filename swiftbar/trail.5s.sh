#!/bin/bash
# trail.5s.sh  ← filename controls refresh interval

result=$(/opt/homebrew/bin/emacsclient --eval "(and js/active-trail (org-roam-node-title (org-roam-node-from-id js/active-trail)))" 2>/dev/null)

if [[ "$result" == "nil" || -z "$result" ]]; then
  echo "⬡"
else
  # strip surrounding quotes emacsclient adds
  echo "⬡ ${result//\"/}"
fi
