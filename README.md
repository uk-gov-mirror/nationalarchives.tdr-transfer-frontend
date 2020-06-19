# tdr-transfer-frontend
Repository for TDR transfer code

## Running locally

There are two ways to develop this project:

- Frontend development only, using the AWS integration environment for everything else
- Full stack local development, using a local dev copy of the API, Keycloak, etc.

### Prerequisites

Regardless of how you set up the development environment, you will need:

- IntelliJ with the Scala plugin (or equivalent Scala dev environment)
- Docker

### Frontend development only

Follow these instructions if you want to make changes to the frontend application without needing to set up a full
development environment for the other TDR services.

- Run redis using Docker:
  ```
  docker run -d --name redis -p 6379:6379 redis
  ```
- Get the TDR client secret for integration by logging into the [Keycloak admin site][auth-admin], and going to Clients,
  then "tdr", then Credentials, and copying the UUID in the Secret field
- In IntelliJ, create a new sbt run configuration:
  - Set the Tasks parameter to `run`
  - Configure environment variables:
    - AUTH_URL=https://auth.tdr-integration.nationalarchives.gov.uk
    - AUTH_SECRET=\<insert the secret for the tdr client that you copied above\>
    - API_URL=https://api.tdr-integration.nationalarchives.gov.uk/graphql
    - TDR_IDENTITY_POOL_ID=secret-from-/mgmt/identitypoolid_intg in the management account
- Follow the Static Assets steps below to build the CSS and JS
- Run the project from IntelliJ
- Visit `http://localhost:9000`

When you log into the site, you will need to log in as a user from the Integration environment.

[auth-admin]: https://auth.tdr-integration.nationalarchives.gov.uk/auth/admin

### Full stack local development

Follow these instructions if you want to make changes to the API, database and/or auth system at the same time as
updating the frontend.

#### Auth server

-  Start the auth server
  ```
  docker run -d  --name keycloak -p 8081:8080 -e KEYCLOAK_USER=admin -e KEYCLOAK_PASSWORD=admin nationalarchives/tdr-auth-server:intg
  ```
- Go to `http://localhost:8081/auth/admin` and log in with username admin and password admin.
- Set up a [realm](https://www.keycloak.org/docs/latest/getting_started/index.html#creating-a-realm-and-user) called tdr. You can set the display name to something else if you want as this will show on the login page.
- Set up a [client](https://www.keycloak.org/docs/latest/server_admin/#oidc-clients) called tdr:
  - In the client settings, change the "Login Theme" to govuk.
  - Set "Access Type" to `confidential`
  - Set "Root URL" to `http://localhost:9000`
  - Set "Valid redirect URIs" to `http://localhost:9000/*`
  - Click Save
  - In newly appeared "Credentials" tab, generate a [secret](https://www.keycloak.org/docs/latest/server_admin/#_client-credentials)
- Configure the token mappers for the client
  - In the tdr client settings, click the Mappers tab
  - Add two mappings:
    - User ID mapping:
      - Name = `user_id`
      - Mapper Type = User Property
      - Property = `id`
      - Token Claim Name = `user_id`
      - Claim JSON Type = String
    - Transferring body mapping:
      - Name = `body`
      - Mapper Type = User Attribute
      - Property = `body`
      - Token Claim Name = `body`
      - Claim JSON Type = String 
- Configure the roles:
  - Click Roles in the menu on the left
  - Add a new role called `tdr_user`
- Create a user:
  - Click Users in the menu on the left
  - Click Add User
  - Set a Username (all the other fields are optional) and click Save
  - Click the Role Mappings tab
  - Move the `tdr_user` role into the Assigned Roles
  - Click the Attributes tab
  - Add an attribute called `body` with value matching one of the transferring bodies in the database, e.g.
    `MOCK1 Department`
  - Click the Credentials tab
  - Set a non-temporary password for the user
- Set AUTH_SECRET as an environment variable in IntelliJ and/or the command line (depending on how you plan to run the
  frontend project) with the secret as its value:
  ```
  AUTH_SECRET=[secret value]
  ```
  
If you want to upload to the aws s3 bucket, you can no longer use your local keycloak instance so you'll have to set at least these environment variables
- API_URL=http://localhost:8080/graphql
- AUTH_URL=https://auth.tdr-integration.nationalarchives.gov.uk
- AUTH_SECRET=\<insert the secret for the tdr client in the integration account\>
- IDENTITY_POOL_ID=secret-from-/mgmt/identitypoolid_intg in the management account

#### Frontend project

* Set up another [client](https://www.keycloak.org/docs/latest/server_admin/#oidc-clients) called tdr-fe. This will be a public client as the configuration is available publicly in the browser. This means there are no client credentials.

* Set "Valid redirect URIs" to `http://localhost:9000/*` Leave everything else on the defaults.

* Start redis locally.

    `docker run -d --name redis -p 6379:6379 redis`
* Run the frontend with `sbt run` from IntelliJ or the command line
* Visit `http://localhost:9000`

### Static assets

* If npm is not installed install [nvm](https://github.com/nvm-sh/nvm) in root directory.

* Once nvm is installed:
    `nvm install 13.6`

* `cd` into tdr-transfer-frontend in terminal

* run  `npm install` then `npm run build`

If you're working on the Sass files and just want to regenerate the CSS without rebuilding all the JavaScript code, you
can run `npm run sass-watch` to monitor and rebuild the CSS files, or run `npm run build-css` to run a single build.

## Generated GraphQL classes

There is a separate repository which contains the generated case classes needed to query the consignment API. 
These classes will be needed by more than one project which is why they are in a separate project.
If you need to add a new query:

* Run `git clone https://github.com/nationalarchives/tdr-generated-graphql.git`
* Add the new query to the `src/main/graphql` directory
* Run `sbt package publishLocal`
* Set the version for `tdr-generated-graphql` in this projects build.sbt to be the snapshot version.

## Notes
* Each environment has its own secret for the auth server. These cannot be generated inside aws in any way and so it's difficult to get them into the terraform scripts. At the moment, these are stored in a parameter store variable called /${env}/auth/secret although this may change.
