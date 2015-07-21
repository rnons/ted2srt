import utils from './util.js';
var TED_URL_REGEX = /^https?:\/\/www.ted.com\/talks\/(\w+)/;

export function searchPageHandler($http, params) {
  let match = TED_URL_REGEX.exec(decodeURIComponent(params.q)),
      query = params.q.replace(/\+/g, ' ');
  if (match) {
    document.location = '/talks/' + match[1];
  }
  var $input = document.querySelector('#search input[name=q]');
  $input.value = query;
  $input.focus();

  $http.get(`/api/search?q=${query}`).then((data) => {
    document.title = query + ' - TED2srt search';
    data.forEach(addSearchResult);
  }).catch(err => {
    console.log(err);
  });

  var addSearchResult;
  var $result;
  $result = document.getElementById('result');
  addSearchResult = function(talk) {
    var template = [
      '<h3><a href="/talks/{{slug}}">{{title}}</a></h3>',
      '<div class="talk-info-body">',
        '<a href="/talks/{{slug}}"><img src="{{src}}"></a>',
        '<p>',
          '{{description}}',
          '<span class="Time">Published: {{publishedAt}}</span>',
        '</p>',
      '</div>'
      ].join('\n');
    var li = document.createElement('li');
    li.innerHTML = template.replace(/{{slug}}/g, talk.slug)
                           .replace('{{src}}', talk.images.medium)
                           .replace('{{title}}', talk.name)
                           .replace('{{description}}', talk.description)
                           .replace('{{publishedAt}}', utils.pprDate(talk.publishedAt));
    $result.appendChild(li);
  };
}
