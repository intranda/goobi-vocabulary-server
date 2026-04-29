#!/usr/bin/env bash
#
# Runs schemathesis against a locally-running vocabulary server, twice:
#
#   1. Anonymous   -> ./schemathesis-anon.log
#   2. Authed      -> ./schemathesis-auth.log    (requires VOCABULARY_API_TOKEN)
#
# Preconditions:
#   - The server is already running on $BASE_URL (default http://localhost:8081).
#   - VOCABULARY_API_TOKEN is exported and matches the server's `security.token`.
#   - Either `schemathesis` or `uvx` is on PATH. If neither, install one of:
#       pipx install schemathesis
#       # or rely on uvx (no install needed; runs schemathesis on demand)
#
# Notes:
#   - The anonymous run's pass/fail interpretation depends on the server's
#     `security.anonymous.{read,write}-allowed` configuration. With both off
#     (the default), every protected operation should return 401. With either
#     on, the corresponding methods are exercised as if authenticated.
#   - All schemathesis checks and phases are enabled. The TRACE -> 400 (vs the
#     spec-correct 405) failures are accepted as known noise; suppressing the
#     `unsupported_method` check would hide unrelated issues, so it stays on.
#
# Exit code: non-zero if either run fails.
#

set -u
set -o pipefail

BASE_URL="${SCHEMATHESIS_BASE_URL:-http://localhost:8081}"
SPEC_URL="${SCHEMATHESIS_SPEC_URL:-${BASE_URL}/docs/api}"
ANON_LOG="${ANON_LOG:-schemathesis-anon.log}"
AUTH_LOG="${AUTH_LOG:-schemathesis-auth.log}"

if command -v schemathesis >/dev/null 2>&1; then
    SCHEMATHESIS_CMD=(schemathesis)
elif command -v uvx >/dev/null 2>&1; then
    SCHEMATHESIS_CMD=(uvx schemathesis)
else
    echo "error: neither 'schemathesis' nor 'uvx' found on PATH" >&2
    echo "       try: pipx install schemathesis" >&2
    exit 2
fi

if [[ -z "${VOCABULARY_API_TOKEN:-}" ]]; then
    echo "error: VOCABULARY_API_TOKEN is not set; the authed run is mandatory" >&2
    echo "       export it from your application.properties (security.token) and retry" >&2
    exit 2
fi

anon_status=0
auth_status=0

echo ">>> [1/2] anonymous run -> ${ANON_LOG}"
"${SCHEMATHESIS_CMD[@]}" run "${SPEC_URL}" \
    --url "${BASE_URL}" \
    2>&1 | tee "${ANON_LOG}"
anon_status=${PIPESTATUS[0]}

echo
echo ">>> [2/2] authenticated run -> ${AUTH_LOG}"
"${SCHEMATHESIS_CMD[@]}" run "${SPEC_URL}" \
    --url "${BASE_URL}" \
    --header "Authorization: Bearer ${VOCABULARY_API_TOKEN}" \
    2>&1 | tee "${AUTH_LOG}"
auth_status=${PIPESTATUS[0]}

echo
echo ">>> done. anon=${anon_status} auth=${auth_status}"

if (( anon_status != 0 || auth_status != 0 )); then
    exit 1
fi
