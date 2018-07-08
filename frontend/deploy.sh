#!/bin/sh
cd frontend && yarn build && cd ..
tar czf dist.tar.gz backend/dist
scp dist.tar.gz ted2srt:/tmp/
ssh ted2srt /bin/sh <<'ENDSSH'
cd /tmp
tar xf dist.tar.gz
rsync -a --delete backend/dist/ /var/www/ted2srt/dist
ENDSSH
rm dist.tar.gz
