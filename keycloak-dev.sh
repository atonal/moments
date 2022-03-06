#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
KC_PORT=9090
KC_DATA_DIR=${SCRIPT_DIR}/.kc-data
KC_ENV=${SCRIPT_DIR}/.kc-env

if [[ -f "${KC_ENV}" ]]; then
    source "${KC_ENV}"
fi

docker run \
    -p "${KC_PORT}":8080 \
    -v "${KC_DATA_DIR}":/opt/keycloak/data \
    -e KEYCLOAK_ADMIN=admin \
    -e KEYCLOAK_ADMIN_PASSWORD="${KC_ADMIN_PASSWORD}" \
    -d \
    --rm \
    quay.io/keycloak/keycloak \
    start-dev
