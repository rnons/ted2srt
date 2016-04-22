import Utils from '../models/utils';

export class TalkView {
  constructor() {
    this.$talkInfo = document.getElementById('talk-info');
    this.$languages = document.querySelector('#languages ul');
    this.$watch = document.getElementById('watch');
    this.$playerContainer = document.getElementById('player-container');
    this.$audio = document.getElementById('audio');
    this.$video = document.getElementById('video');
    this.$transcripts = document.querySelector('#subtitles ul');

    this.templateInfo = document.getElementById('talk-info.html').innerHTML;
    this.templateAudio = document.getElementById('audio-download.html').innerHTML;
    this.templateVideo = document.getElementById('video-downloads.html').innerHTML;

    this.$playerContainer.addEventListener('click', this.closeVideo);
  }

  bind(event, handler) {
    if (event === 'selectLang') {
      this.delegate(this.$languages, 'li', 'click', function () {
        handler(this.dataset.code);
      });
    } else if (event === 'watch') {
      this.$watch.addEventListener('click', handler);
    } else if (event === 'download') {
      this.$transcripts.addEventListener('click', (e) => {
        handler(e.target.id);
      });
    }
  }

  playVideo(talk, selected) {
    var template = `
      <video controls autoplay>
        <source src="{{video_src}}" type="video/mp4">
        <track kind="captions" src="{{vtt_src}}" default>
      </video>
      `;
    if (Utils.isSafari()) {
      this.$playerContainer.style.display = '-webkit-flex';
    } else {
      this.$playerContainer.style.display = 'flex';
    }
    this.$playerContainer.innerHTML =
      template.replace('{{video_src}}', Utils.mkVideoUrl(talk.mediaSlug, '950k'))
              .replace('{{vtt_src}}', Utils.mkTranscriptUrl(talk.id, selected, 'vtt', false));
  }

  closeVideo(event) {
    if (event.target === this) {
      this.style.display = 'none';
      this.innerHTML = '';
    }
  }

  delegate(target, selector, type, handler) {
    function dispatchEvent(event) {
      var targetElement = event.target;
      var potentialElements = target.querySelectorAll(selector);
      var hasMatch = Array.prototype.indexOf.call(potentialElements, targetElement) !== -1;

      if (hasMatch) {
        handler.call(targetElement, event);
      }
    }

    target.addEventListener(type, dispatchEvent);
  }

  toggle(langCode) {
    let li = document.querySelector(`li[data-code="${langCode}"]`);
    li.classList.toggle('selected');
  }

  downloadTranscript(talk, selected, format) {
    document.location = Utils.mkTranscriptUrl(talk.id, selected, format, true);
  }

  renderInfo(talk) {
    const talkUrl = `https://www.ted.com/talks/${talk.slug}`,
          publishedAt = Utils.pprDate(talk.publishedAt);

    this.$talkInfo.innerHTML =
      this.templateInfo.replace(/{{talkUrl}}/g, talkUrl)
                       .replace('{{name}}', talk.name)
                       .replace('{{src}}', talk.image)
                       .replace('{{description}}', talk.description)
                       .replace('{{publishedAt}}', publishedAt);
  }

  renderLanguages(language, selected) {
    let li = document.createElement('li');
    li.dataset.code = language.code;
    li.innerHTML = language.name;
    this.$languages.appendChild(li);
    if (selected.indexOf(language.code) !== -1) {
      li.click();
    }
  }

  renderAudioDownload(mediaSlug) {
    let src = Utils.mkAudioUrl(mediaSlug);
    this.$audio.classList.remove('is-hidden');
    this.$audio.innerHTML = this.templateAudio.replace('{{src}}', src);
  }

  renderVideoDownloads(mediaSlug) {
    let mkVideoUrl = Utils.mkVideoUrl.bind(this, mediaSlug);
    this.$video.innerHTML =
      this.templateVideo.replace('{{1500k}}', mkVideoUrl('1500k'))
                        .replace('{{950k}}', mkVideoUrl('950k'))
                        .replace('{{600k}}', mkVideoUrl('600k'))
                        .replace('{{320k}}', mkVideoUrl('320k'));
  }

  render(talk, selected) {
    document.title = talk.name + ' - TED2srt';
    this.renderInfo(talk);
    talk.languages.forEach((lang) => {
      this.renderLanguages(lang, selected);
    });
    if (talk.hasAudio) {
      this.renderAudioDownload(talk.mediaSlug);
    }
    this.renderVideoDownloads(talk.mediaSlug);
  }
}
