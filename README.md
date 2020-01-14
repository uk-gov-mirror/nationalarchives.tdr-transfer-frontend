# tdr-transfer-frontend
Repository for TDR transfer code

## Running locally

* Start the auth server

`docker run -d  --name keycloak -p 8080:8080 -e KEYCLOAK_USER=admin -e KEYCLOAK_PASSWORD=admin nationalarchives/tdr-auth-server:intg`
* Go to http://localhost:8080/auth/admin and log in with username admin and password admin.

* Set up a [realm](https://www.keycloak.org/docs/latest/getting_started/index.html#creating-a-realm-and-user) called tdr. You can set the display name to something else if you want as this will show on the login page.

* Set up a [client](https://www.keycloak.org/docs/latest/server_admin/#oidc-clients) called tdr.

* In the client settings, change the login theme to govuk.

* Generate a [secret](https://www.keycloak.org/docs/latest/server_admin/#_client-credentials)

* Set the secret in auth.secret in [application.conf](conf/application.conf).

* Create a new [user](https://www.keycloak.org/docs/latest/getting_started/index.html#_create-new-user) in the tdr realm.

* Start redis locally.

`docker run -d --name redis -p 6379:6379 redis`

* Start the application using `sbt run`

## Notes
* Each environment has its own secret for the auth server. These cannot be generated inside aws in any way and so it's difficult to get them into the terraform scripts. At the moment, these are stored in a parameter store variable called /${env}/auth/secret although this may change.