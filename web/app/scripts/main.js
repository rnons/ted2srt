import {Router} from 'director';

import {TalksProvider} from './models/talks';
import $http from './http';
import utils from './models/util';

import {HomeView} from './views/home';
import {TalkView} from './views/talk';
import {SearchView} from './views/search';

import {HomeController} from './controllers/home';
import {TalkController} from './controllers/talk';
import {SearchController} from './controllers/search';

let Talks = new TalksProvider();
let $container = document.getElementById('container');

document.getElementById('random-talk').addEventListener('click', () => {
  Talks.random().then((talk) => {
    document.location = '#/talks/' + talk.slug;
  });
});

let routes = {
  '/': function() {
    $container.innerHTML = document.getElementById('home.html').innerHTML;
    let view = new HomeView();
    new HomeController(Talks, view);
  },
  '/talks/:slug': function(slug) {
    Talks.fetchBySlug(slug).then((talk) => {
      $container.innerHTML = document.getElementById('talk.html').innerHTML;
      let view = new TalkView();
      new TalkController(talk, view);
    })
  },
  '/search': function() {
    const TED_URL_REGEX = /^https?:\/\/www.ted.com\/talks\/(\w+)/;
    let match, params, query;
    params = utils.parseQueryString();
    match = TED_URL_REGEX.exec(decodeURIComponent(params.q));
    query = params.q.replace(/\+/g, ' ');
    if (match) {
      document.location = '#/talks/' + match[1];
    }

    Talks.search(query).then((talks) => {
      $container.innerHTML = document.getElementById('search.html').innerHTML;
      let view = new SearchView();
      new SearchController(talks, view, query);
    });
  },
};

let router = new Router(routes);

router.notfound = function() {
  console.log('not found');
};

router.init('/');
