import Talk from '../models/talk';
import Footer from '../components/footer';
import SearchForm from '../components/search-form';
import * as styles from './index.css';

class Home {
  searchForm = new SearchForm();
  footer = new Footer();

  constructor(private talks: Talk[]) {}

  renderTalk(talk) {
    const {
      slug,
      image,
      title,
      speaker
    } = talk;
    return `
      <a class="${styles.tile}" href="#/talks/${slug}">
        <div class="${styles.image}" style="background-image: url(${image})"></div>
        <div class="${styles.info}">
          <div class="${styles.title}">${title}</div>
          <div class="${styles.speaker}">${speaker}</div>
        </div>
      </a>
    `;
  }

  render() {
    const form = this.searchForm.render();
    const list = this.talks.map(this.renderTalk).join('');
    const footer = this.footer.render();
    return `
      <div class="${styles.root}">
        <div class="${styles.logo}">:: TED -> [SRT]</div>
        ${form}
        <div class="${styles.list}">
          ${list}
        </div>
      </div>
      ${footer}
    `;
  }
}

export default Home;
