# tdr-transfer-frontend
Repository for TDR transfer code

## Running locally

There are two ways to develop this project:

- Frontend development only, using the AWS integration environment for everything else. This is the default
- Full stack local development, using a local dev copy of the API, Keycloak, etc


When moving between full local and frontend only development you should clear your browsers' cookies and local storage as the browser can cache the local Cognito ID, which is incompatible with non-local Cognito. An example of the error you will see if this happens is below:

``` 
POST https://cognito-identity.eu-west-2.amazonaws.com 400

Uncaught (in promise) ValidationException: 1 validation error detected: Value 'some-fake-identity-id' at 'identityId' failed to satisfy constraint: Member must satisy regular expression pattern: [\w-]+:[0-9a-f-]+
```

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
- If you don't already have an admin user account for the [Integration Keycloak][auth-admin] site, ask another member of
  the TDR development team to create one for you
- Look up the integration environment variables in the AWS Integration account:
  - Keycloak client secret
    - In the AWS console:
      - Go the Systems Manager service
      - Go the Parameter Store in the left-hand menu
      - Find the `/intg/auth/secret` parameter
      - Copy the parameter's value
    - With the AWS CLI:
      - Run:
        ```
        aws ssm get-parameter --name "/intg/auth/secret" --with-decryption
        ```
      - Copy the `Value` from the object returned
  - Cognito identity pool ID
    - In the AWS console:
      - Go to the Cognito service
      - Click "Manage Identity Pools"
      - Click on the TDR Frontend identity pool
      - Click "Edit identity pool"
      - Copy the full identity pool ID, including the AWS region prefix
    - With the AWS CLI:
      - Run:
        ```
        aws cognito-identity list-identity-pools --max-results 20
        ```
      - Copy the `IdentityPoolId` of the pool named "TDR Frontend Identity Intg"
- In IntelliJ, create a new sbt run configuration:
  - Set the Tasks parameter to `run`
  - Configure the environment variables:
    - AUTH_SECRET=\<the secret for the Keycloak client that you copied above\>
    - IDENTITY_POOL_ID=\<the identity pool ID you copied above\>
- Follow the Static Assets steps below to build the CSS and JS
- Run the project from IntelliJ
- Visit `http://localhost:9000`

When you log into the site, you will need to log in as a user from the Integration environment.

[auth-admin]: https://auth.tdr-integration.nationalarchives.gov.uk/auth/admin

### Full stack local development

Follow these instructions if you want to make changes to the API, database and/or auth system at the same time as
updating the frontend.

#### Local auth server

-  Log into docker with credentials from ECR and start the auth server. This will need AWS CLI version 2 to work.
  ```
  export MANAGEMENT_ACCOUNT=management_account_number
  aws ecr get-login-password --region eu-west-2 --profile management | docker login --username AWS --password-stdin $MANAGEMENT_ACCOUNT.dkr.ecr.eu-west-2.amazonaws.com
  docker run -d --name keycloak -p 8081:8080 -e KEYCLOAK_USER=admin -e KEYCLOAK_PASSWORD=admin -e KEYCLOAK_IMPORT=/tmp/tdr-realm.json -e CLIENT_SECRET=[some value] -e BACKEND_CHECKS_CLIENT_SECRET=[some value] -e REALM_ADMIN_CLIENT_SECRET=[some value] -e KEYCLOAK_CONFIGURATION_PROPERTIES=intg_properties.json -e USER_ADMIN_CLIENT_SECRET=[some value] $MANAGEMENT_ACCOUNT.dkr.ecr.eu-west-2.amazonaws.com/auth-server:intg
  ```
- Go to `http://localhost:8081/auth/admin` and log in with username *admin* and password *admin*.  
- Create a transferring body user:
  - Click Users in the menu on the left
  - Click Add User
  - Set a Username (all the other fields are optional) and click Save
  - Click the Groups tab
  - In the "Available Groups" box select the `Mock 1 Department` sub-group, and click `Join`
    The `transferring_body_user/Mock 1 Department` group should now appear in the "Group Membership" box
  - Click the Credentials tab
  - Set a non-temporary password for the user
  - For full details about managing transferring body users and transferring body groups see: [Tdr User Administrator Manual](https://github.com/nationalarchives/tdr-dev-documentation/blob/master/tdr-admins/tdr-user-administrator.md)
- Set AUTH_SECRET as an environment variable in IntelliJ and/or the command line (depending on how you plan to run the
  frontend project) with the secret as its value:
  ```
  AUTH_SECRET=[CLIENT_SECRET value from the docker run command]
  ```

#### Local API

Clone and run the [tdr-consignment-api] project.

[tdr-consignment-api]: https://github.com/nationalarchives/tdr-consignment-api

#### Local S3 emulator

Create a new empty directory that the S3 emulator will save files in.

**If you are running Linux**, change the owner of this directory to user 2000, to give the user in the S3 ninja Docker
container permission to save files there. Do not do this on a Mac, because Docker handles file permissions differently
on Linux and Mac OS, and this step will prevent uploads from working.

```
sudo chown 2000:2000 /your/new/upload/directory
```

Run an [S3 ninja] Docker container, specifying a local directory in which to save the files:

```
docker run -d -p 9444:9000 -v /your/new/upload/directory:/home/sirius/data --name=s3ninja scireum/s3-ninja:6.4
```

Visit http://localhost:9444/ui and check you can create a bucket and upload a test file through the S3 ninja UI. Check
that the file appears in the folder that you mounted.

Requests to S3 ninja need to be authenticated with a Cognito token. To emulate this endpoint, clone the
[tdr-local-aws] project and follow the instructions there to run the `FakeCognitoServer` application.

[S3 ninja]: https://s3ninja.net/
[tdr-local-aws]: https://github.com/nationalarchives/tdr-local-aws

#### Local backend checks

Follow the instructions in [tdr-local-aws] to run the `FakeBackendChecker` application, making sure you set
the environment variable for the monitored folder to the same path as the mount directory that you set in
the `docker run` command when you started the S3 ninja container. This lets the fake backend checker detect
and scan files as they are uploaded to the S3 emulator.

#### Frontend project

* Start redis locally.

    `docker run -d --name redis -p 6379:6379 redis`
* Ensure you have set the `AUTH_SECRET` environment variable, as described above. Set it in the command line or in the
  IntelliJ run configuration
* Run the frontend, specifying the local full stack configuration file:
  ```
  sbt -Dconfig.file=conf/application.local-full-stack.conf run
  ```
  or set the IntelliJ SBT run configuration to `-Dconfig.file=conf/application.local-full-stack.conf run`
* Visit `http://localhost:9000`

### Static assets

The TDR static assets, including the sass, are stored in a separate repo to allow for reuse by different applications: https://github.com/nationalarchives/tdr-frontend-styles

To enable local development of the sass and other static assets:
1. Clone the TDR Frontend Style repo locally

2. Navigate to the root of the TDR Frontend Styles repo and run the following command:
```
[tdr-frontend-styles root] $ npm link
```
* From the output of this command copy the following line: `@nationalarchives/tdr-frontend-styles`.

3. Navigate to the npm directory of this repo

* If npm is not installed install [nvm](https://github.com/nvm-sh/nvm) in root directory.

4. Once nvm is installed run:
    `nvm install 14.9`

5. Run the following commands in the npm directory  `npm install` then `npm run build`

6. Run the following command in the npm directory: `npm link @nationalarchives/tdr-frontend-styles`

* This creates a symbolic link to the version of tdr-frontend-styles library on your machine.

7. Run the following command: `npm run sass-watch`

8. Make changes to the sass files in the src directory of the tdr-frontend-styles: tdr-frontend-styles/src/tdr/sass
* Changes made in the tdr-frontend-styles src will be picked up in the locally running application

9. Once any changes have been made to the sass, you can create a new version of the tdr-frontend-styles package and publish following the instructions in tdr-frontend-style repo:  https://github.com/nationalarchives/tdr-frontend-styles/blob/main/README.md

See the following blog post for more information on npm link and instructions to undo the symbolic link: https://medium.com/@alexishevia/the-magic-behind-npm-link-d94dcb3a81af 

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
