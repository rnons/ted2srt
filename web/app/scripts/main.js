import {Router} from 'director';

import Http from './models/http';
import Utils from './models/utils';
import {TalksProvider} from './models/talks';

import {HomeView} from './views/home';
import {TalkView} from './views/talk';
import {SearchView} from './views/search';

import {HomeController} from './controllers/home';
import {TalkController} from './controllers/talk';
import {SearchController} from './controllers/search';

let Talks = new TalksProvider(Http);
let $container = document.getElementById('container');

document.getElementById('random-talk').addEventListener('click', () => {
  Talks.random().then((talk) => {
    document.location = '#/talks/' + talk.slug;
  });
});

let routes = {
  '/': function() {
    Talks.fetch().then((talks) => {
      $container.innerHTML = document.getElementById('home.html').innerHTML;
      window.scrollTo(0, 0);
      let view = new HomeView();
      new HomeController(talks, view);
    });
  },
  '/talks/:slug': function(slug) {
    Talks.fetchBySlug(slug).then((talk) => {
      $container.innerHTML = document.getElementById('talk.html').innerHTML;
      window.scrollTo(0, 0);
      let view = new TalkView();
      new TalkController(talk, view);
    });
  },
  '/search': function() {
    const TED_URL_REGEX = /^https?:\/\/www.ted.com\/talks\/(\w+)/;
    let match, params, query;
    params = Utils.parseQueryString();
    match = TED_URL_REGEX.exec(decodeURIComponent(params.q));
    query = params.q.replace(/\+/g, ' ');
    if (match) {
      document.location = '#/talks/' + match[1];
    }

    $container.innerHTML = document.getElementById('search.html').innerHTML;
    Talks.search(query).then((talks) => {
      window.scrollTo(0, 0);
      let view = new SearchView();
      setTimeout(() => {
        new SearchController(talks, view, query);
      }, 2000);
      // new SearchController(talks, view, query);
    });
  },
};

let router = new Router(routes);

router.notfound = function() {
  console.log('not found');
};

router.init('/');
