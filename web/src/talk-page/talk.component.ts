import Header from '../components/header';
import Footer from '../components/footer';
import TalkService from './talk.service';
import Sidebar from './sidebar';
import * as styles from './index.css';


class TalkComponent {
  sidebar: Sidebar;
  selectedLanguages: string[] = [];
  header = new Header();

  constructor(private service: TalkService, private footer: Footer) {
    this.sidebar = new Sidebar(service);
  }

  delegate(target, selector, type, handler) {
    function dispatchEvent(event) {
      const targetElement = event.target;
      const potentialElements = target.querySelectorAll(selector);
      const hasMatch = Array.prototype.indexOf.call(potentialElements, targetElement) !== -1;

      if (hasMatch) {
        handler(targetElement, event);
      }
    }

    target.addEventListener(type, dispatchEvent);
  }

  mounted() {
    const $languages = document.querySelector('.js-languages');
    this.delegate($languages, 'a', 'click', this.handleSelectLanguage);
    this.footer.mounted();
    const $player = <HTMLVideoElement>document.querySelector('.js-player');
    const $cover = <HTMLElement>document.querySelector('.js-cover');
    const $info = <HTMLElement>document.querySelector('.js-info')
    $cover.addEventListener('click', () => {
      $info.style.display = 'none';
      $player.style.display = 'block';
      $player.play();
    })
  }

  handleSelectLanguage = (element) => {
    const code = element.dataset.code;
    this.service.selectLanguage(code);
  }

  makeQuery() {
    const { selectedLanguages } = this.service;
    let query = '';
    if (selectedLanguages.length === 0) {
      query = 'lang=en';
    } else {
      query = selectedLanguages.map(function(code) {
        return 'lang=' + code;
      }).join('&');
    }
    return query;
  }

  renderInfo() {
    const {
      id,
      name,
      slug,
      mediaSlug,
      image,
      description,
      publishedAt
    } = this.service.talk;
    const tedUrl = `https://www.ted.com/talks/${slug}`;
    const videoUrl = `https://download.ted.com/talks/${mediaSlug}-950k.mp4`;
    const vttUrl = `/api/talks/${id}/transcripts/vtt?${this.makeQuery()}`;
    return `
      <h3 class="${styles.title}">
        <a href="${tedUrl}" target="_blank">${name}</a>
      </h3>
      <video class="js-player"
        onclick="this.paused ? this.play() : this.pause()"
        controls hidden>
          <source src="${videoUrl}" type="video/mp4">
        <track kind="captions" src="${vttUrl}" default>
      </video>
      <div class="${styles.info} js-info">
        <div class="${styles.cover} js-cover"
           href="${tedUrl}"
           style="background-image: url(${image})"
           target="_blank">
          <span class="${styles.playButton}"></span>
        </div>
        <p class="${styles.description}">
          ${description}
          <span class="${styles.date}">Published: ${publishedAt.toDateString()}</span>
        </p>
      </div>
    `
  }

  renderTranscript() {
    if (this.service.talk.languages.length === 0) {
      return `
        <div class="${styles.help}">
          Sorry, no transcript yet.
        </div>
      `;
    }
    if (this.service.selectedLanguages.length === 0) {
      return `
        <div class="${styles.help}">
          You can read the transcripts by selecting one or two languages from the sidebar.
        </div>
      `;
    }
    const [ lang1, lang2 ] = this.service.selectedLanguages;
    if (!this.service.transcripts[lang1]) {
      return '';
    }
    const transcript1 = this.service.transcripts[lang1].split('\n');
    let transcript2, rows;
    if (lang2 && this.service.transcripts[lang2]) {
      transcript2 = this.service.transcripts[lang2].split('\n');
      rows = transcript1.map((p1, index) => {
        const p2 = transcript2[index];
        return `
          <div class="${styles.row}">
            <p>${p1}</p>
            <p>${p2}</p>
          </div>
        `;
      }).join('');
    } else {
      rows = transcript1.map(p => {
        return `
          <p>${p}</p>
        `;
      }).join('');
    }

    return `
      <article class="${styles.article}">
        ${rows}
      </article>
    `;
  }

  render() {
    const header = this.header.render();
    const info = this.renderInfo();
    const transcript = this.renderTranscript();
    const sidebar = this.sidebar.render();
    const footer = this.footer.render();
    return `
      ${header}
      <div class="${styles.root}">
        <main class="${styles.main}">
          <div class="u-margin-bm">${info}</div>
          <div>${transcript}</div>
        </main>
        ${sidebar}
      </div>
      ${footer}
    `;
  }
}

export default TalkComponent;
