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
const headerTpl = document.getElementById('header.html').innerHTML;
const loadingTpl = document.getElementById('loading.html').innerHTML;

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
    $container.innerHTML = document.getElementById('talk.html')
                                   .innerHTML
                                   .replace('{{>header}}', headerTpl)
                                   .replace('{{>loading}}', loadingTpl);
    window.scrollTo(0, 0);
    Talks.fetchBySlug(slug).then((talk) => {
      let view = new TalkView();
      new TalkController(talk, view);
      document.querySelector('.Loading').classList.add('is-hidden');
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

    $container.innerHTML = document.getElementById('search.html')
                                   .innerHTML
                                   .replace('{{>header}}', headerTpl)
                                   .replace('{{>loading}}', loadingTpl);
    window.scrollTo(0, 0);
    Talks.search(query).then((talks) => {
      let view = new SearchView();
      new SearchController(talks, view, query);
      document.querySelector('.Loading').classList.add('is-hidden');
    });
  },
};

let router = new Router(routes);

router.notfound = function() {
  console.log('not found');
};

router.init('/');
