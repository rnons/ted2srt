const LOCAL_STORAGE_KEY = 'selected';

export class TalkController {
  constructor(Talk, View) {
    this.Talk = Talk;
    this.View = View;
    this.selected = [];

    this.View.bind('selectLang', (langCode) => {
      this.selectLang(langCode);
    });

    this.View.bind('watch', () => {
      this.View.playVideo(this.Talk, this.selected);
    });

    this.View.bind('download', (format) => {
      this.View.downloadTranscript(this.Talk, this.selected, format);
    });

    this.View.render(Talk, this.getSelected());
  }

  selectLang(langCode) {
    let index, length;
    length = this.selected.length;
    index = this.selected.indexOf(langCode);
    if (length < 2) {
      if (index === -1) {
        this.selected.push(langCode);
      } else {
        this.selected.splice(index, 1);
      }
      this.View.toggle(langCode);
      this.saveSelected();
    } else if (length === 2) {
      if (index !== -1) {
        this.selected.splice(index, 1);
        this.View.toggle(langCode);
        this.saveSelected();
      }
    }
  }

  getSelected() {
    let selected = [];
    if (window.localStorage) {
      let stored = window.localStorage.getItem(LOCAL_STORAGE_KEY);
      if (stored) {
        selected = stored.split(',');
      }
    }
    if (selected.length > 2) {
      selected = selected.slice(0, 2);
    }
    return selected;
  }

  saveSelected() {
    if (window.localStorage) {
      window.localStorage.setItem(LOCAL_STORAGE_KEY, this.selected);
    }
  }
}
