#!/bin/sh

#
# app
#

export BITCOIN_DASHBOARD_TUI_LOG_ENV="dev"
export BITCOIN_DASHBOARD_TUI_LOG_FORMAT="Bracket" # Bracket | JSON
export BITCOIN_DASHBOARD_TUI_LOG_VERBOSITY="V3"
export BITCOIN_DASHBOARD_TUI_LIBPQ_CONN_STR="postgresql://nixbld1@localhost/bitcoin-dashboard-tui"
export BITCOIN_DASHBOARD_TUI_ENDPOINT_PORT="3000"
