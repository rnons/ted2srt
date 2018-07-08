# ted2srt

This is the source code of [ted2srt.org](https://ted2srt.org), a website to download bilingual subtitles of TED talks. The backend is written in Haskell, while the frontend is in PureScript.

## Set up development environment

### Frontend

Setup [purescript](https://github.com/purescript/purescript) and [pulp](https://github.com/purescript-contrib/pulp), then

```
cd frontend
yarn
pulp -w build
yarn start
```

You need to run `yarn build` once, so that backend can start correctly.

### Backend

First, setup [nix](https://nixos.org/nix/) and [stack](https://docs.haskellstack.org/en/stable/README/) the way you like.

Then

```
cd backend
stack build
```

While `stack` is running, you can setup the database. Postgres and Redis are needed, you can either use your system wide version or nix installed version. Following is how to use the nix version.

```
# enter nix shell to use postgres and redis installed by nix
nix-shell

# start redis
redis-server --daemonize yes

# start postgres, you only need to initdb and createdb for the first time
initdb -D data -U postgres
pg_ctl -D data -l logfile start
createdb -U postgres ted2srt
```

One last step, create your own `.env` file and modify it to your needs.

```
cp .env.example .env
```

If `stack build` has finished now, run

```
stack exec ted2srt
```

to start the server.

Then navigate to http://localhost:8080, try searching some keywords, cheers.
