import Talk from '../models/talk';
import Header from '../components/header';
import Footer from '../components/footer';
import Sidebar from './sidebar';
import * as styles from './index.css';


class TalkPage {
  sidebar: Sidebar;
  selectedLanguages: string[] = [];
  header = new Header();
  footer = new Footer();

  constructor(private talk: Talk, private rerender: () => void) {

    this.sidebar = new Sidebar(talk);
  }

  delegate(target, selector, type, handler) {
    function dispatchEvent(event) {
      var targetElement = event.target;
      var potentialElements = target.querySelectorAll(selector);
      var hasMatch = Array.prototype.indexOf.call(potentialElements, targetElement) !== -1;

      if (hasMatch) {
        handler(targetElement, event);
      }
    }

    target.addEventListener(type, dispatchEvent);
  }

  mounted() {
    const $languages = document.querySelector('.js-languages');
    this.delegate($languages, 'a', 'click', this.handleSelectLanguage);
  }

  handleSelectLanguage = (element) => {
    const code = element.dataset.code;
    const index = this.selectedLanguages.indexOf(code);
    if (index === -1) {
      this.selectedLanguages.push(code);
    } else {
      this.selectedLanguages.splice(index, 1);
    }
    this.rerender();
  }

  renderInfo() {
    const {
      name,
      slug,
      image,
      description,
      publishedAt
    } = this.talk;
    const tedUrl = `https://www.ted.com/talks/${slug}`;
    return `
      <h3>
        <a href="${tedUrl}" target="_blank">${name}</a>
      </h3>
      <div class="${styles.info}">
        <a class="${styles.cover}"
           href="${tedUrl}"
           style="background-image: url(${image}")
           target="_blank">
        </a>
        <p class="${styles.description}">
          ${description}
          <span class="${styles.date}">Published: ${publishedAt.toDateString()}</span>
        </p>
      </div>
    `
  }

  render() {
    const info = this.renderInfo();
    const sidebar = this.sidebar.render(this.selectedLanguages);
    const header = this.header.render();
    const footer = this.footer.render();
    return `
      ${header}
      <div class="${styles.root}">
        <main class="${styles.main}">
          <div class="u-margin-bm">${info}</div>
        </main>
        ${sidebar}
      </div>
      ${footer}
    `;
  }
}

export default TalkPage;
