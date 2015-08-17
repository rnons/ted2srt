function parseQueryString() {
  if (!document.location.search) return;
  let querys = document.location.search.split('?')[1].split('&');
  let params = {};
  querys.forEach(function(q) {
    let qs, key, value;
    qs = q.split('=');
    key = qs[0];
    value = qs[1];
    if (params[key]) {
      if (!(params[key] instanceof Array)) {
        params[key] = [params[key]];
      }
      if (params[key].indexOf(value) === -1) {
        params[key].push(value);
      }
    } else {
      params[key] = value;
    }
  });
  return params;
}

function pprDate(dateString) {
  let date = new Date(dateString);
  let year, month, dayOfMonth;
  year = date.getFullYear();
  month = date.getMonth() + 1;
  dayOfMonth = date.getDate();
  return [year, month, dayOfMonth].join('-');
}

function mkVideoUrl(mediaSlug, codeRate) {
  return `http://download.ted.com/talks/${mediaSlug}-${codeRate}.mp4`;
}

function mkTranscriptUrl(tid, selected, format, download) {
  let mkQueryString = function() {
    let queryString = '';
    if (selected.length === 0) {
      queryString = 'lang=en';
    } else {
      queryString = selected.map(function(code) {
        return 'lang=' + code;
      }).join('&');
    }
    return '?' + queryString;
  };

  let pathSlug = '/transcripts/';
  if (download) {
    pathSlug += 'download/';
  }
  return '/api/talks/' + tid + pathSlug + format + mkQueryString();
}

function isSafari() {
  let ua = navigator.userAgent;
  if (ua.indexOf('AppleWebKit') !== -1 && ua.indexOf('Chrome') === -1) {
    return true;
  } else {
    return false;
  }
}


export default {
  parseQueryString,
  pprDate,
  isSafari,
  mkVideoUrl,
  mkTranscriptUrl
};
