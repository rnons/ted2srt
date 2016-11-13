import TalkService from './talk.service';
import * as styles from './sidebar.css';


class Sidebar {
  constructor(private service: TalkService) {}

  getVideoUrl(codeRate) {
    const slug = this.service.talk.mediaSlug;
    return `https://download.ted.com/talks/${slug}-${codeRate}.mp4`;
  }

  getTranscriptUrl(selectedLanguages, format) {
    const { id } = this.service.talk;
    let query = '';
    if (selectedLanguages.length === 0) {
      query = 'lang=en';
    } else {
      query = selectedLanguages.map(function(code) {
        return 'lang=' + code;
      }).join('&');
    }
    return `/api/talks/${id}/transcripts/download/${format}?${query}`;
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

  renderTranscript(selectedLanguages) {
    return `
      <div class="${styles.panel}">
        <h4 class="${styles.panelTitle}">Download transcript</h4>
        <ul class="${styles.list}">
          <li>
            <a href="${this.getTranscriptUrl(selectedLanguages, 'srt')}">SRT</a>
          </li>
          <li>
            <a href="${this.getTranscriptUrl(selectedLanguages, 'txt')}">TXT</a>
          </li>
          <li>
            <a href="${this.getTranscriptUrl(selectedLanguages, 'lrc')}">LRC</a>
          </li>
        </ul>
      </div>
    `;
  }

  renderLanguages(selectedLanguages) {
    const list = this.service.talk.languages.map(({languageCode, endonym}) => {
      const className = selectedLanguages.indexOf(languageCode) === -1
        ? styles.link : styles.linkActive;
      return `
        <li>
          <a class="${className}" data-code="${languageCode}">${endonym}</a>
        </li>
      `;
    });
    return `
      <div class="${styles.panel}">
        <h4 class="${styles.panelTitle}">Select languages</h4>
        <ul class="${styles.list} js-languages">${list.join('')}</ul>
      </div>
    `;
  }

  render() {
    const selectedLanguages = this.service.selectedLanguages;
    const video = this.renderVideo();
    const transcript = this.renderTranscript(selectedLanguages);
    const languages = this.renderLanguages(selectedLanguages);
    return `
      <aside class="${styles.aside}">
        ${video}
        ${transcript}
        ${languages}
      </aside>
    `;
  }
}

export default Sidebar;
