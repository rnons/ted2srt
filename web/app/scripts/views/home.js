export class HomeView {
  constructor() {
    this.$talks = document.getElementById('talks');
    this.template = `
      <a href="#/talks/{{slug}}">
        <img class="FeaturedTalks-image" src="{{src}}">
        <div class="FeaturedTalks-info">
          <div class="FeaturedTalks-title">{{title}}</div>
          <div class="FeaturedTalks-speaker">{{speaker}}</div>
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
    div.className = 'FeaturedTalks-tile';
    this.$talks.appendChild(div);
  }

  render(talks) {
    talks.forEach(this.renderTalk, this);
  }
}
