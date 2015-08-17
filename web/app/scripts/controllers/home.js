export function homepageHandler(Talks) {
  Talks.fetch().then((data) => {
    data.forEach(addTalk);
  });

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
                            .replace('{{speaker}}', talk.speaker)
                            .replace('{{title}}', talk.title);
    div.className = 'tile';
    $talks.appendChild(div);
  }
};
