(function() {
  'use strict';
  var TED_URL_REGEX = /^https?:\/\/www.ted.com\/talks\/(\w+)/;

  var app = window.app || (window.app = {});
  app.searchPageHandler = function(params) {
    var match;
    if (match = TED_URL_REGEX.exec(decodeURIComponent(params.q))) {
      document.location = '/talks/' + match[1];
    }
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
})();
