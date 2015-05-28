/* jshint devel:true */
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
  request.open('GET', url, true);

  request.onload = function() {
    if (request.status >= 200 && request.status < 400) {
      var data = JSON.parse(request.responseText);
      data.languages.forEach(addLanguage);
      addVideoDownloads(data.talk.mSlug);
      addTranscriptsHandler(data.talk.id);
    }
  };
  request.send();
  var $languages = document.querySelector('#languages ul');
  var languageTemplate = '<li data-lang="{{code}}">{{name}}</li>';
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
      index === -1 ? selected.push(languageCode) : selected.splice(index, 1);
    } else if (length === 2) {
      li.classList.remove('selected');
      if (index !== -1) selected.splice(index, 1);
    }
  };

  addLanguage = function(language) {
    var li = document.createElement('li');
    li.dataset.code = language.code;
    li.innerHTML = language.name;
    li.addEventListener('click', setSelected);
    $languages.appendChild(li);
  }

  var downloadUrl = "http://download.ted.com/talks/";
  var mkVideoUrl = function(slug, quality) {
    return downloadUrl + slug + '-' + quality + '.mp4';
  };
  var addVideoDownloads = function (mediaSlug) {
    var template = [
      '<ul><li>',
        '<a href="{{720p}}" title="Right click to save (1280x720)" target="_blank">720p</a>',
      '</li><li>',
        '<a href="{{480p}}" title="Right click to save (854x480)" target="_blank">480p</a>',
      '</li><li>',
        '<a href="{{360p}}" title="Right click to save (640x360)" target="_blank">360p</a>',
      '</li><li>',
        '<a href="{{288p}}" title="Right click to save (512x288)" target="_blank">288p</a>',
      '</li></ul>'
      ].join('\n');
    $video = document.getElementById('video');
    $video.innerHTML = template.replace('{{720p}}', mkVideoUrl(mediaSlug, '1500k'))
                               .replace('{{480p}}', mkVideoUrl(mediaSlug, '950k'))
                               .replace('{{360p}}', mkVideoUrl(mediaSlug, '600k'))
                               .replace('{{288p}}', mkVideoUrl(mediaSlug, '320k'));
  };

  var mkTranscriptUrl = function(tid, format) {
    var queryString = '';
    if (selected.length === 0) {
      queryString = 'lang=en';
    } else {
      queryString = selected.map(function(code) {
        return 'lang=' + code
      }).join('&')
    }
    return '/api/talks/' + tid + '/downloads/transcripts/' + format + '?' + queryString;
  }

  var addTranscriptsHandler = function(tid) {
    document.querySelector('#subtitles ul').addEventListener('click', function(e) {
      if (e.target.id) {
        window.location = mkTranscriptUrl(tid, e.target.id);
      }
    });
  };
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
      '<a href="/talks/{{slug}}"><img src="{{src}}"></a>',
      '<h3><a href="/talks/{{slug}}">{{title}}</a></h3>',
      '<p>{{description}}</p>',
      ].join('\n');
    var li = document.createElement('li');
    li.innerHTML = template.replace('{{slug}}', talk.slug)
                           .replace('{{src}}', talk.image)
                           .replace('{{title}}', talk.name)
                           .replace('{{description}}', talk.description);
    $result.appendChild(li);
  }
};

var $container = document.getElementById('container');
var routes = {
  '/': function() {
    $container.innerHTML = [
      '<div id="homepage">',
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
      '<div id="talk">',
        '<div id="downloads">',
          '<div id="video"></div>',
          '<div id="subtitles">',
            '<ul><li>',
              '<a id="srt">SRT</a>',
            '</li><li>',
              '<a id="txt">TXT</a>',
            '</li><li>',
              '<a id="lrc">LRC</a>',
            '</li><ul>',
          '</div>',
        '</div>',
        '<div id="languages"><ul></ul></div>',
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
      '<div id="search-page">',
        '<ul id="result"></ul>',
      '</div>'
      ].join('\n');
    searchPageHandler(params);
  }
};

var router = Router(routes).configure({html5history: true});

router.notfound = function() {
  console.log('not found');
};

router.init();
