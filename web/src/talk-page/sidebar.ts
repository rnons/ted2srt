import TalkService from './talk.service';
import * as styles from './sidebar.css';


class Sidebar {
  constructor(private service: TalkService) {}

  getVideoUrl(codeRate) {
    const slug = this.service.talk.mediaSlug;
    return `https://download.ted.com/talks/${slug}-${codeRate}.mp4`;
  }

  renderAudio() {
    return '';
  }

  renderVideo() {
    return `
      <div class="${styles.panel}">
        <h4 class="${styles.panelTitle}">Download Video</h4>
        <ul class="${styles.list}">
          <li>
            <a href="${this.getVideoUrl('1500k')}"
               title="Right click to save (1280x720)"
               download>720p</a>
          </li>
          <li>
            <a href="${this.getVideoUrl('950k')}"
               title="Right click to save (854x480)"
               download>480p</a>
          </li>
          <li>
            <a href="${this.getVideoUrl('600k')}"
               title="Right click to save (640x360)"
               download>360p</a>
          </li>
          <li>
            <a href="${this.getVideoUrl('320k')}"
               title="Right click to save (512x288)"
               download>288p</a>
          </li>
        </ul>
      </div>
    `;
  }

  renderTranscripts() {
    return `
      <li>
        <a href="${this.service.makeTranscriptDownloadUrl('srt')}">SRT</a>
      </li>
      <li>
        <a href="${this.service.makeTranscriptDownloadUrl('txt')}">TXT</a>
      </li>
      <li>
        <a href="${this.service.makeTranscriptDownloadUrl('lrc')}">LRC</a>
      </li>
    `;
  }

  renderLanguages() {
    const { talk, selectedLanguages } = this.service;
    return talk.languages.map(({
      languageCode, languageName, endonym
    }) => {
      const className = selectedLanguages.indexOf(languageCode) === -1
        ? styles.link : styles.linkActive;
      return `
        <li>
          <a class="${className}"
            title="${languageName}"
            data-code="${languageCode}">${endonym}</a>
        </li>
      `;
    }).join('');
  }

  rerender() {
    const $transcripts = <HTMLElement>document.querySelector('.js-transcripts-download');
    $transcripts.innerHTML = this.renderTranscripts();

    const $languages = <HTMLElement>document.querySelector('.js-languages');
    $languages.innerHTML = this.renderLanguages();
  }

  render() {
    const video = this.renderVideo();
    const transcript = this.renderTranscripts();
    const languages = this.renderLanguages();
    return `
      <div>
        ${video}
        <div class="${styles.panel}">
          <h4 class="${styles.panelTitle}">Download transcript</h4>
          <ul class="${styles.list} js-transcripts-download">${transcript}</ul>
        </div>
        <div class="${styles.panel}">
          <h4 class="${styles.panelTitle}">Select languages</h4>
          <ul class="${styles.list} js-languages">${languages}</ul>
        </div>
      </div>
    `;
  }
}

export default Sidebar;
