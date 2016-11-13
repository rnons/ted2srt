import Talk from '../models/talk';
import Http from '../services/http';

class TalkService {
  slugToTalk = {};
  subscribers: (() => any)[] = [];
  selectedLanguages: string[] = [];
  talk: Talk;
  transcripts = {};

  constructor(private http: Http) {}

  add(data) {
    return new Talk(data);
  }

  getBySlug(slug) {
    let talk = this.slugToTalk[slug];
    if (talk) {
      return Promise.resolve(talk);
    } else {
      return this.http.getJson(`/api/talks/${slug}`).then(data => {
        talk = this.add(data);
        this.slugToTalk[talk.slug] = talk;
        this.talk = talk;
        this.selectLanguage('en');
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

  selectLanguage(code) {
    const index = this.selectedLanguages.indexOf(code);
    if (index === -1) {
      this.selectedLanguages.push(code);
    } else {
      this.selectedLanguages.splice(index, 1);
    }
    this.getTranscript('txt', code);
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
