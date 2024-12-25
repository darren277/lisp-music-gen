# LISP Music Generator

## Database Initialization

```shell
$ sudo -u postgres psql -c "ALTER USER myusername CREATEDB;"
==> ALTER ROLE

$ PGPASSWORD=mypassword psql -U myusername -d postgres -c "CREATE DATABASE musicgen;"
==> CREATE DATABASE

$ PGPASSWORD=mypassword psql -U myusername -d musicgen -c "CREATE TABLE IF NOT EXISTS melodies (id SERIAL PRIMARY KEY, root_frequency FLOAT NOT NULL CHECK (root_frequency > 0), melody TEXT NOT NULL);"
==> CREATE TABLE
```
