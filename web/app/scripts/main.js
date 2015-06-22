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
                            .replace('{{src}}', talk.images.medium)
                            .replace('{{speaker}}', talk.name.split(':')[0])
                            .replace('{{title}}', talk.name.split(':')[1]);
    div.className = 'tile';
    $talks.appendChild(div);
  }
};

var $container = document.getElementById('container');
var routes = {
  '/': function() {
    $container.innerHTML = [
      '<div id="homepage" class="container">',
        '<div id="logo">:: TED -> [SRT]</div>',
        '<form id="search" method="GET" action="/search">',
          '<input type="text" name="q" placeholder="TED talk url or keywords" required>',
          '<input type="submit">',
        '</form>',
        '<div id="talks"></div>',
      '</div>',
      '<footer class="Footer Footer--absolute">',
        'TED2srt by <a href="https://twitter.com/rnons" target="_blank">rnons</a> | <a href="https://github.com/rnons/ted2srt" target="_blank">source code</a> | <a href="http://ted2srt.org/atom.xml" target="_blank">feed</a>',
      '</footer>'
      ].join('\n');
    homepageHandler();
  },
  '/talks/:slug': function(slug) {
    var params = app.utils.parseQueryString();
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
      '</div>',
      '<footer class="Footer">',
        'TED2srt by <a href="https://twitter.com/rnons" target="_blank">rnons</a> | <a href="https://github.com/rnons/ted2srt" target="_blank">source code</a> | <a href="http://ted2srt.org/atom.xml" target="_blank">feed</a>',
      '</footer>'
      ].join('\n');
    app.talkPageHandler(slug, params);
  },
  '/search': function() {
    var params = app.utils.parseQueryString();
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
      '<footer class="Footer">',
        'TED2srt by <a href="https://twitter.com/rnons" target="_blank">rnons</a> | <a href="https://github.com/rnons/ted2srt" target="_blank">source code</a> | <a href="http://ted2srt.org/atom.xml" target="_blank">feed</a>',
      '</footer>'
      ].join('\n');
    app.searchPageHandler(params);
  }
};

var router = new Router(routes).configure({html5history: true});

router.notfound = function() {
  console.log('not found');
};

router.init();
})();
