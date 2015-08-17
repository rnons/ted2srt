import {Router} from 'director';

import {TalksProvider} from './models/talks';
import $http from './http';
import utils from './util';

import {HomeView} from './views/home';

import {HomeController} from './controllers/home';
import {talkPageHandler} from './talk-page';
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
      '<div id="talk-page" class="container">',
        '<div id="main">',
          '<div id="talk-info"></div>',
          '<div id="languages" class="Panel">',
            '<h4 class="Panel-title">Languages</h4>',
            '<ul class="Panel-body"></ul>',
          '</div>',
        '</div>',
        '<div id="downloads">',
          '<div id="video" class="Panel"></div>',
          '<div id="subtitles" class="Panel">',
            '<h4 class="Panel-title">Transcripts</h4>',
            '<ul class="Panel-body"><li>',
              '<a id="srt" href="javascript:void(0)">SRT</a>',
            '</li><li>',
              '<a id="txt" href="javascript:void(0)">TXT</a>',
            '</li><li>',
              '<a id="lrc" href="javascript:void(0)">LRC</a>',
            '</li></ul>',
          '</div>',
          '<div id="watch"><a href="javascript:void(0)">▶︎ Play</a></div>',
        '</div>',
      '</div>',
      '<div id="player-container">',
      '</div>',
      ].join('\n');
    talkPageHandler($http, slug);
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
