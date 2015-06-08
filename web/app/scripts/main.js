/* jshint devel:true */
/* global Router */
(function(){
  'use strict';
var homepageHandler = function() {
  var request = new XMLHttpRequest();
  request.open('GET', '/api/talks', true);

  request.onload = function() {
    if (request.status >= 200 && request.status < 400) {
      var data = JSON.parse(request.responseText);
      data.slice(0, 5).forEach(addTalk);
    }
  };
  request.send();

  var $talks = document.getElementById('talks');
  function addTalk(talk) {
    var template = [
      '<a href="/talks/{{slug}}">',
        '<img src="{{src}}">',
        '<div class="info">',
          '<p class="title">{{title}}</p>',
          '<p class="speaker">{{speaker}}</p>',
        '</div>',
      '</a>'
      ].join('\n');
    var div = document.createElement('div');
    div.innerHTML = template.replace('{{slug}}', talk.slug)
                            .replace('{{src}}', talk.image)
                            .replace('{{speaker}}', talk.name.split(':')[0])
                            .replace('{{title}}', talk.name.split(':')[1]);
    div.className = 'tile';
    $talks.appendChild(div);
  }
};

var searchPageHandler = function(params) {
  var request = new XMLHttpRequest();
  request.open('GET', '/api/search?q=' + params.q, true);

  request.onload = function() {
    if (request.status >= 200 && request.status < 400) {
      document.title = params.q + ' - TED2srt search';
      var data = JSON.parse(request.responseText);
      data.forEach(addSearchResult);
    }
  };
  request.send();

  var addSearchResult;
  var $result;
  $result = document.getElementById('result');
  addSearchResult = function(talk) {
    var template = [
      '<h3><a href="/talks/{{slug}}">{{title}}</a></h3>',
      '<div class="talk-info-body">',
        '<a href="/talks/{{slug}}"><img src="{{src}}"></a>',
        '<p>{{description}}</p>',
      '</div>'
      ].join('\n');
    var li = document.createElement('li');
    li.innerHTML = template.replace(/{{slug}}/g, talk.slug)
                           .replace('{{src}}', talk.image)
                           .replace('{{title}}', talk.name)
                           .replace('{{description}}', talk.description);
    $result.appendChild(li);
  };
};

var $container = document.getElementById('container');
var routes = {
  '/': function() {
    $container.innerHTML = [
      '<div id="homepage" class="container">',
        '<div id="logo">:: TED -> [SRT]</div>',
        '<form id="search" method="GET" action="/search">',
          '<input type="text" name="q" required>',
          '<input type="submit">',
        '</form>',
        '<div id="talks"></div>',
      '</div>'
      ].join('\n');
    homepageHandler();
  },
  '/talks/:slug': function(slug) {
    $container.innerHTML = [
      '<header>',
        '<div class="container">',
          '<div id="logo"><a href="/">:: TED -> [SRT]</a></div>',
          '<form id="search" method="GET" action="/search">',
            '<input type="text" name="q" required>',
            '<input type="submit">',
          '</form>',
        '</div>',
      '</header>',
      '<div id="talk-page" class="container">',
        '<div id="main">',
          '<div id="talk-info"></div>',
          '<div id="languages"><ul><h4>Languages</h4></ul></div>',
        '</div>',
        '<div id="downloads">',
          '<div id="video"></div>',
          '<div id="subtitles">',
            '<ul><h4>Transcripts</h4><li>',
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
      '</div>'
      ].join('\n');
    app.talkPageHandler(slug);
  },
  '/search': function() {
    var querys = window.location.search.split('?')[1].split('&');
    var params = {};
    querys.forEach(function(q) {
      var qs = q.split('=');
      params[qs[0]] = qs[1];
    });
    $container.innerHTML = [
      '<header>',
        '<div class="container">',
          '<div id="logo"><a href="/">:: TED -> [SRT]</a></div>',
          '<form id="search" method="GET" action="/search">',
            '<input type="text" name="q" required>',
            '<input type="submit">',
          '</form>',
        '</div>',
      '</header>',
      '<div id="search-page" class="container">',
        '<ul id="result"></ul>',
      '</div>'
      ].join('\n');
    searchPageHandler(params);
  }
};

var router = new Router(routes).configure({html5history: true});

router.notfound = function() {
  console.log('not found');
};

router.init();
})();
