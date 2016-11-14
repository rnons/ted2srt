import Talk from '../models/talk';
import Http from '../services/http';

class TalkService {
  slugToTalk = {};
  subscribers: (() => any)[] = [];
  selectedLanguages: string[] = [];
  talk: Talk;
  transcripts = {};

  constructor(private http: Http) {}

  readLanguages() {
    const languages = localStorage.getItem('languages');
    return languages ? JSON.parse(languages) : [];
  }

  saveLanguages() {
    localStorage.setItem('languages', JSON.stringify(this.selectedLanguages));
  }

  getBySlug(slug) {
    let talk = this.slugToTalk[slug];
    if (talk) {
      return Promise.resolve(talk);
    } else {
      return this.http.getJson(`/api/talks/${slug}`).then(data => {
        talk = new Talk(data);
        this.slugToTalk[talk.slug] = talk;
        this.talk = talk;
        this.restoreLanguages();
        return Promise.resolve();
      }).catch(err => {
        console.log(err);
      });
    }
  }

  getTranscript(format, lang) {
    const { id } = this.talk;
    if (this.transcripts[lang]) {
      this.inform();
      return Promise.resolve();
    } else {
      return this.http.get(`/api/talks/${id}/transcripts/${format}?lang=${lang}`)
        .then((transcript: string) => {
          this.transcripts[lang] = transcript;
          this.inform();
        });
    }
  }

  restoreLanguages() {
    const stored = this.readLanguages();
    const languageCodes = this.talk.languages.map(lang => lang.languageCode);
    stored.map(code => {
      if (languageCodes.indexOf(code) !== -1) {
        this.selectedLanguages.push(code);
        this.getTranscript('txt', code);
      }
    });
    this.inform();
  }

  selectLanguage(code) {
    const index = this.selectedLanguages.indexOf(code);
    if (index === -1) {
      if (this.selectedLanguages.length !== 2) {
        this.selectedLanguages.push(code);
        this.getTranscript('txt', code);
      }
    } else {
      this.selectedLanguages.splice(index, 1);
      this.inform();
    }
    this.saveLanguages();
  }

  subscribe(callback: () => any) {
    if (this.subscribers.indexOf(callback) === -1) {
      this.subscribers.push(callback);
    }
  }

  inform() {
    this.subscribers.forEach(callback => callback());
  }
}

export default TalkService;
