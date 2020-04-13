# ted2srt

[Donate](https://liberapay.com/rnons/)

This is the source code of [ted2srt.org](https://ted2srt.org), a website to download bilingual subtitles of TED talks. The backend is written in Haskell, while the frontend is in PureScript.

## Set up development environment

[Nix](https://nixos.org/nix/) is required for development.

### Frontend

```
cd frontend
nix-shell
spago build -w
yarn
yarn start
```

Run `yarn build` once, so that backend can start correctly.

### Backend

```
cd backend
nix-shell
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

Then navigate to http://localhost:3001, try paste a TED talk url to the search bar, cheers.
