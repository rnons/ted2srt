#!/bin/sh
rm -rf dist
NODE_ENV=production yarn build
cp src/search.xml dist
tar czf dist.tar.gz dist
scp dist.tar.gz ted2srt:/tmp/
ssh ted2srt /bin/sh <<'ENDSSH'
cd /tmp
tar xf dist.tar.gz
rsync -a --delete dist/ /var/www/ted2srt/dist
ENDSSH
rm dist.tar.gz
