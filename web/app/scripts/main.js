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
    var template = '<a href="/talks/{{slug}}"><img src="{{src}}"><p>{{title}}</p></a>';
    var div = document.createElement('div');
    div.innerHTML = template.replace('{{slug}}', talk.slug)
                            .replace('{{src}}', talk.image)
                            .replace('{{title}}', talk.name);
    div.className = 'tile';
    $talks.appendChild(div);
  }
};

var talkPageHandler = function(slug) {
  var request = new XMLHttpRequest();
  var url = '/api/talks/' + slug + '?languages=true';
  var talk = null;

  request.open('GET', url, true);
  request.onload = function() {
    if (request.status >= 200 && request.status < 400) {
      var data = JSON.parse(request.responseText);
      talk = data.talk;
      addTalkInfo(data.talk);
      data.languages.forEach(addLanguage);
      addVideoDownloads(data.talk.mSlug);
      addTranscriptsHandler(data.talk.id);
    }
  };
  request.send();

  var addTalkInfo = function(talk) {
    var template = [
      '<h3><a href="{{slug}}">{{title}}</a></h3>',
      '<div class="talk-info-body">',
        '<a href="{{slug}}"><img src="{{src}}"></a>',
        '<p>{{description}}</p>',
      '</div>'
      ].join('\n');
    var mkTalkSrc = function(slug) {
      return 'https://www.ted.com/talks/' + slug;
    };
    document.getElementById('talk-info')
      .innerHTML = template.replace(/{{slug}}/g, mkTalkSrc(talk.slug))
                           .replace('{{src}}', talk.image)
                           .replace('{{title}}', talk.name)
                           .replace('{{description}}', talk.description);

  };

  var $languages = document.querySelector('#languages ul');
  var addLanguage, setSelected;
  var selected = [];
  setSelected = function(e) {
    var li, languageCode, length, index;
    li= e.target;
    languageCode = li.dataset.code;
    length = selected.length;
    index = selected.indexOf(languageCode);
    if (length < 2) {
      li.classList.toggle('selected');
      if (index === -1) {
        selected.push(languageCode);
      } else {
        selected.splice(index, 1);
      }
    } else if (length === 2) {
      li.classList.remove('selected');
      if (index !== -1) { selected.splice(index, 1); }
    }
  };

  addLanguage = function(language) {
    var li = document.createElement('li');
    li.dataset.code = language.code;
    li.innerHTML = language.name;
    li.addEventListener('click', setSelected);
    $languages.appendChild(li);
  };

  var downloadUrl = 'http://download.ted.com/talks/';
  var mkVideoUrl = function(slug, quality) {
    return downloadUrl + slug + '-' + quality + '.mp4';
  };
  var addVideoDownloads = function (mediaSlug) {
    var template = [
      '<ul><h4>Video</h4><li>',
        '<a href="{{720p}}" title="Right click to save (1280x720)" target="_blank">720p</a>',
      '</li><li>',
        '<a href="{{480p}}" title="Right click to save (854x480)" target="_blank">480p</a>',
      '</li><li>',
        '<a href="{{360p}}" title="Right click to save (640x360)" target="_blank">360p</a>',
      '</li><li>',
        '<a href="{{288p}}" title="Right click to save (512x288)" target="_blank">288p</a>',
      '</li></ul>'
      ].join('\n');
    var $video = document.getElementById('video');
    $video.innerHTML = template.replace('{{720p}}', mkVideoUrl(mediaSlug, '1500k'))
                               .replace('{{480p}}', mkVideoUrl(mediaSlug, '950k'))
                               .replace('{{360p}}', mkVideoUrl(mediaSlug, '600k'))
                               .replace('{{288p}}', mkVideoUrl(mediaSlug, '320k'));
  };

  var mkTranscriptUrl = function(tid, format, download) {
    var pathSlug = '/transcripts/';
    if (download) {
      pathSlug += 'download/';
    }
    var queryString = '';
    if (selected.length === 0) {
      queryString = 'lang=en';
    } else {
      queryString = selected.map(function(code) {
        return 'lang=' + code;
      }).join('&');
    }
    return '/api/talks/' + tid + pathSlug + format + '?' + queryString;
  };

  var addTranscriptsHandler = function(tid) {
    document.querySelector('#subtitles ul').addEventListener('click', function(e) {
      if (e.target.id) {
        window.location = mkTranscriptUrl(tid, e.target.id, true);
      }
    });
  };

  document.getElementById('watch').addEventListener('click', function() {
    var template = [
      '<video controls autoplay>',
        '<source src="{{video_src}}" type="video/mp4">',
        '<track kind="captions" src="{{vtt_src}}" default>',
      '</video>',
      ].join('\n');
    var $playerContainer = document.getElementById('player-container');
    $playerContainer.style.display = 'flex';
    $playerContainer.innerHTML =
      template.replace('{{video_src}}', mkVideoUrl(talk.mSlug, '950k'))
              .replace('{{vtt_src}}', mkTranscriptUrl(talk.id, 'vtt', false));
  });

  document.getElementById('player-container').addEventListener('click', function(e) {
    if (e.target === this) {
      this.style.display = 'none';
      this.innerHTML = '';
    }
  });
};

var searchPageHandler = function(params) {
  var request = new XMLHttpRequest();
  request.open('GET', '/api/search?q=' + params.q, true);

  request.onload = function() {
    if (request.status >= 200 && request.status < 400) {
      var data = JSON.parse(request.responseText);
      console.log(data);
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
    talkPageHandler(slug);
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
