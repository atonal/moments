moments
=====

An OTP application

Build
-----

    $ rebar3 compile

Run
---

    MOMENTS_OIDC_CLIENT_ID=moments MOMENTS_OIDC_CLIENT_SECRET=<redacted> rebar3 run

Authentication Server
---------------------

    ./keycloak-dev.sh

Load Test Data
--------------

    mnesia:load_textfile("../../../../apps/moments/test/db_data.txt").
