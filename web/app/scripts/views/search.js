import Utils from '../models/utils.js';

export class SearchView {
  constructor() {
    this.$input = document.querySelector('#search input[name=q]');
    this.$result = document.getElementById('result');
    this.template = document.getElementById('search-item.html').innerHTML;
  }

  renderTalk(talk) {
    let li = document.createElement('li');
    li.classList.add('u-margin-vm');
    li.innerHTML =
      this.template.replace(/{{slug}}/g, talk.slug)
                   .replace('{{src}}', talk.image)
                   .replace('{{title}}', talk.name)
                   .replace('{{description}}', talk.description)
                   .replace('{{publishedAt}}', Utils.pprDate(talk.publishedAt));
    this.$result.appendChild(li);
  }

  render(talks, query) {
    document.title = query + ' - TED2srt search';

    talks.forEach(this.renderTalk, this);
    this.$input.value = query;
    this.$input.focus();
  }
}
