export class HomeView {
  constructor() {
    this.$talks = document.getElementById('talks');
    this.template = `
      <a href="#/talks/{{slug}}">
        <img src="{{src}}">
        <div class="info">
          <p class="title">{{title}}</p>
          <p class="speaker">{{speaker}}</p>
        </div>
      </a>
      `;
  }

  renderTalk(talk) {
    let div = document.createElement('div');
    div.innerHTML = this.template.replace('{{slug}}', talk.slug)
                                 .replace('{{src}}', talk.images.medium)
                                 .replace('{{speaker}}', talk.speaker)
                                 .replace('{{title}}', talk.title);
    div.className = 'tile';
    this.$talks.appendChild(div);
  }

  render(talks) {
    talks.forEach(this.renderTalk, this);
  }
};
