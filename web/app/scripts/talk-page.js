import utils from './util.js';

const LOCAL_STORAGE_KEY = 'selected';
var selected = [];

var addTalkInfo = function(talk) {
  const talkUrl = `https://www.ted.com/talks/${talk.slug}`,
        publishedAt = utils.pprDate(talk.publishedAt);
  document.getElementById('talk-info').innerHTML = `
    <h3>
      <a href="${talkUrl}" target="_blank">${talk.name}</a>
    </h3>
    <div class="talk-info-body">
      <a href="${talkUrl}" target="_blank">
        <img src="${talk.images.medium}">
      </a>
      <p>
        ${talk.description}
        <span class="Time">Published: ${publishedAt}</span>
      </p>
    </div>
    `
};

var mkQueryString = function() {
  var queryString = '';
  if (selected.length === 0) {
    queryString = 'lang=en';
  } else {
    queryString = selected.map(function(code) {
      return 'lang=' + code;
    }).join('&');
  }
  return '?' + queryString;
};

var saveSelected = function() {
  if (window.localStorage) {
    window.localStorage.setItem(LOCAL_STORAGE_KEY, selected);
  }
};

var setSelected = function(e) {
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
    saveSelected();
  } else if (length === 2) {
    li.classList.remove('selected');
    if (index !== -1) { selected.splice(index, 1); }
    saveSelected();
  }
};

var addLanguage = function(language, queryLangs) {
  var li = document.createElement('li');
  li.dataset.code = language.code;
  li.innerHTML = language.name;
  li.addEventListener('click', setSelected);
  document.querySelector('#languages ul').appendChild(li);
  if (queryLangs.indexOf(language.code) !== -1) {
    li.click();
  }
};

let mkVideoUrl = (mediaSlug, codeRate) => {
  return `http://download.ted.com/talks/${mediaSlug}-${codeRate}.mp4`
}

var addVideoDownloads = function (mediaSlug) {
  document.getElementById('video').innerHTML =`
    <h4 class="Panel-title">Video</h4>
    <ul class="Panel-body"><li>
     <a href="${mkVideoUrl('1500k')}" title="Right click to save (1280x720)" target="_blank">720p</a>
    </li><li>
     <a href="${mkVideoUrl('950k')}" title="Right click to save (854x480)" target="_blank">480p</a>
    </li><li>
     <a href="${mkVideoUrl('600k')}" title="Right click to save (640x360)" target="_blank">360p</a>
    </li><li>
     <a href="${mkVideoUrl('320k')}" title="Right click to save (512x288)" target="_blank">288p</a>
    </li></ul>
    `
};

var mkTranscriptUrl = function(tid, format, download) {
  var pathSlug = '/transcripts/';
  if (download) {
    pathSlug += 'download/';
  }
  return '/api/talks/' + tid + pathSlug + format + mkQueryString();
};

var addTranscriptsHandler = function(tid) {
  document.querySelector('#subtitles ul').addEventListener('click', function(e) {
    if (e.target.id) {
      document.location = mkTranscriptUrl(tid, e.target.id, true);
    }
  });
};

var bindEvents = function(talk) {
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

export function talkPageHandler($http, slug) {
  let queryLangs = [];
  if (window.localStorage) {
    let selected = window.localStorage.getItem(LOCAL_STORAGE_KEY);
    if (selected) {
      queryLangs = selected.split(',');
    }
  }
  if (queryLangs.length > 2) {
    queryLangs = queryLangs.slice(0, 2);
  }

  $http.get(`/api/talks/${slug}`).then((data) => {
    document.title = data.name + ' - TED2srt';
    addTalkInfo(data);
    data.languages.forEach(function(lang) {
      addLanguage(lang, queryLangs);
    });
    addVideoDownloads(data.mSlug);
    addTranscriptsHandler(data.id);
    bindEvents(data);
  }).catch(err => {
    console.log(err);
  });
}
