Local Builds with Nix
=====================

The simplest way to get the server building is currently to use [nix](https://nixos.org/manual/nixos/stable/).

Once you've installed nix, from the root of the project, run:

~~~bash
nix-shell --pure
cabal new-build
~~~

Local Development with docker-compose
=====================================

The easiest way to run local aftok infrastructure is using Docker and
docker-compose. Several steps are necessary to set up your local environment
for development.

Obtaining Docker Images
-----------------------

The docker-compose setup requires 4 images:

* `aftok/aftok-server:latest`
* `nginx/nginx:latest`
* `postgres/postgres:9.4`
* `electriccoinco/zcashd:v4.0.0`

You can obtain these images by running the following. This assumes the use of
`sudo` under Linux; this may not be necessary on other platforms (e.g. OSX).

~~~bash
make build-image # build the aftok-server image(s)
docker pull nginx/nginx:lates
docker pull postgres/postgres:9.4
docker pull electriccoinco/zcashd:v4.0.0
~~~

If you have problems building the aftok-server image, you can also obtain
a prebuilt image by running

~~~bash
docker pull aftok/aftok-server:latest
~~~

Configuration Files
-------------------

A number of configuration files are required for docker-compose to be able
to run all of the necessary containers successfully. You should create
a `local` directory, which will have the following contents:

~~~
local
├── conf
│   ├── nginx
│   │   ├── mime.types
│   │   └── nginx.conf
│   ├── nginx-certs
│   │   ├── aftok.crt
│   │   └── aftok.key
│   ├── server
│   │   ├── aftok.bip70-chain.cert.pem
│   │   ├── aftok.bip70.key.pem
│   │   ├── aftok.cfg
│   │   ├── aftok-migrations.cfg
│   │   └── snap-site-key
│   └── zcashd
│       ├── zcash-data
│       │   └── zcash.conf
│       └── zcash-params
└── db-dumps
    └── aftok.sample.plsql
~~~

Sample default versions of each of these files can be found in the `conf`
directory; you can simply use the following to set up your local environment:

~~~bash
mkdir local
cp -r conf local

# The aftok-zcashd container runs zcashd as user 2001, so we change the
# owner of the zcashd configuration directories to this user.
sudo chown -R 2001.2001 local/conf/zcashd
~~~

Database Initialization
=======
Config files
------------

Sample config files are available in the `conf` directory.
Copy these files to `local/conf/server` and remove the .example 
extension from the filenames.

You will also need to generate a self-signed SSL certificate for
local development.

TODO: self-signed cert instructions


Database initialization
-----------------------

When you first set up your local docker environment for aftok development, the
database that is created by `docker-compose up` will not be initialized.  The
easiest way to get it set up is to bootstrap from an existing database dump. 

First, you'll need to start the servers. Expect aftok-server and aftok-nginx to
both fail to start; they won't work properly until the database is initialized,
but that's okay.

Also, if you have a copy of postgres already running at localhost:5432 you may
need to change the exposed port in docker-compose.yml so as to avoid conflicts.

If you're using docker on OSX, the `sudo` in the lines below (which is required
on linux) should be omitted.

~~~bash
sudo docker-compose up
sudo docker ps
~~~

At this point, the `aftok-db` container should be the only one that's running;
the other two will have failed on startup.

Assuming that you have such a dump at `local/db-dumps/aftok.dump`, use the
following commands to initialize the database. The postgres user's password is
specified in the docker-compose file.

~~~bash
createuser -h localhost -U postgres -W -P aftok
createdb -h localhost -U postgres -W -O aftok aftok 
psql -h localhost -U aftok -W aftok < local/db-dumps/aftok.dump
~~~

Now, you should be able to shut down docker-compose using ^C and 
restart it with `docker-compose up`.

Database Configuration
----------------------

All database DDL state is handled using the Haskell dbmigrations tool.

Once all the containers are up, you'll need to run the existing database
migrations as follows:

~~~bash
stack install dbmigrations-postgresql
moo-postgresql upgrade --config-file ./local/conf/server/aftok-migrations.cfg
~~~

New migrations can be created with:

~~~bash
moo-postgresql new --config-file ./local/conf/server/aftok-migrations.cfg kebab-case-descriptive-name
~~~
