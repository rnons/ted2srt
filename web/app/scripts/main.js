import {Router} from 'director';

import {TalksProvider} from './models/talks';
import $http from './http';
import utils from './models/util';

import {HomeView} from './views/home';
import {TalkView} from './views/talk';

import {HomeController} from './controllers/home';
import {TalkController} from './controllers/talk';
import {searchPageHandler} from './search-page';

let talks = new TalksProvider();

var $container = document.getElementById('container');
var routes = {
  '/': function() {
    $container.innerHTML = document.getElementById('home.html').innerHTML;
    let view = new HomeView();
    new HomeController(talks, view);
  },
  '/talks/:slug': function(slug) {
    talks.fetchBySlug(slug).then((talk) => {
      $container.innerHTML = document.getElementById('talk.html').innerHTML;
      let view = new TalkView();
      new TalkController(talk, view);
    })
  },
  '/search': function() {
    var params = utils.parseQueryString();
    $container.innerHTML = [
      '<header>',
        '<div class="container">',
          '<div id="logo"><a href="/">:: TED -> [SRT]</a></div>',
          '<form id="search" method="GET" action="/search">',
            '<input type="text" name="q" placeholder="TED talk url or keywords" required>',
            '<input type="submit">',
          '</form>',
        '</div>',
      '</header>',
      '<div id="search-page" class="container">',
        '<ul id="result"></ul>',
      '</div>',
      ].join('\n');
    searchPageHandler($http, params);
  },
  '/random': function() {
    $http.get('/api/talks/random').then((data) => {
      document.location = 'talks/' + data.slug;
    }).catch(err => {
      console.log(err);
    });
  }
};

var router = new Router(routes).configure({html5history: true});

router.notfound = function() {
  console.log('not found');
};

router.init();
